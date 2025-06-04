
;; title: AgriSure


(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_POLICY_NOT_FOUND (err u101))
(define-constant ERR_POLICY_EXPIRED (err u102))
(define-constant ERR_POLICY_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_PREMIUM (err u104))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u105))
(define-constant ERR_INVALID_WEATHER_DATA (err u106))
(define-constant ERR_ORACLE_NOT_AUTHORIZED (err u107))
(define-constant ERR_INSUFFICIENT_FUNDS (err u108))

(define-map policies
  { policy-id: uint }
  {
    farmer: principal,
    crop-type: (string-ascii 50),
    coverage-amount: uint,
    premium-paid: uint,
    start-block: uint,
    end-block: uint,
    location: (string-ascii 100),
    min-rainfall: uint,
    max-temperature: uint,
    active: bool,
    claimed: bool
  }
)

(define-map weather-data
  { location: (string-ascii 100), stacks-block-height: uint }
  {
    rainfall: uint,
    temperature: uint,
    oracle: principal,
    timestamp: uint
  }
)

(define-map authorized-oracles
  { oracle: principal }
  { authorized: bool }
)

(define-data-var policy-counter uint u0)
(define-data-var contract-balance uint u0)

(define-public (create-policy 
  (crop-type (string-ascii 50))
  (coverage-amount uint)
  (duration-blocks uint)
  (location (string-ascii 100))
  (min-rainfall uint)
  (max-temperature uint))
  (let (
    (policy-id (+ (var-get policy-counter) u1))
    (premium (/ coverage-amount u10))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (>= (stx-get-balance tx-sender) premium) ERR_INSUFFICIENT_PREMIUM)
    (asserts! (is-none (map-get? policies { policy-id: policy-id })) ERR_POLICY_ALREADY_EXISTS)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    
    (map-set policies
      { policy-id: policy-id }
      {
        farmer: tx-sender,
        crop-type: crop-type,
        coverage-amount: coverage-amount,
        premium-paid: premium,
        start-block: start-block,
        end-block: end-block,
        location: location,
        min-rainfall: min-rainfall,
        max-temperature: max-temperature,
        active: true,
        claimed: false
      }
    )
    
    (var-set policy-counter policy-id)
    (var-set contract-balance (+ (var-get contract-balance) premium))
    (ok policy-id)
  )
)

(define-public (submit-weather-data
  (location (string-ascii 100))
  (rainfall uint)
  (temperature uint))
  (begin
    (asserts! (is-oracle-authorized tx-sender) ERR_ORACLE_NOT_AUTHORIZED)
    (asserts! (and (>= rainfall u0) (<= rainfall u1000)) ERR_INVALID_WEATHER_DATA)
    (asserts! (and (>= temperature u0) (<= temperature u60)) ERR_INVALID_WEATHER_DATA)
    
    (map-set weather-data
      { location: location, stacks-block-height: stacks-block-height }
      {
        rainfall: rainfall,
        temperature: temperature,
        oracle: tx-sender,
        timestamp: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (file-claim (policy-id uint))
  (let (
    (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (farmer (get farmer policy))
    (location (get location policy))
    (end-block (get end-block policy))
    (min-rainfall (get min-rainfall policy))
    (max-temperature (get max-temperature policy))
    (coverage-amount (get coverage-amount policy))
    (active (get active policy))
    (claimed (get claimed policy))
  )
    (asserts! (is-eq tx-sender farmer) ERR_UNAUTHORIZED)
    (asserts! active ERR_POLICY_EXPIRED)
    (asserts! (not claimed) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (>= stacks-block-height end-block) ERR_POLICY_EXPIRED)
    
    (let (
      (weather (unwrap! (get-weather-data-for-claim location end-block) ERR_INVALID_WEATHER_DATA))
      (rainfall (get rainfall weather))
      (temperature (get temperature weather))
      (payout-eligible (or 
        (< rainfall min-rainfall)
        (> temperature max-temperature)
      ))
    )
      (if payout-eligible
        (begin
          (asserts! (>= (stx-get-balance (as-contract tx-sender)) coverage-amount) ERR_INSUFFICIENT_FUNDS)
          (try! (as-contract (stx-transfer? coverage-amount tx-sender farmer)))
          (map-set policies
            { policy-id: policy-id }
            (merge policy { claimed: true, active: false })
          )
          (var-set contract-balance (- (var-get contract-balance) coverage-amount))
          (ok { payout: coverage-amount, reason: "weather-conditions-met" })
        )
        (begin
          (map-set policies
            { policy-id: policy-id }
            (merge policy { active: false })
          )
          (ok { payout: u0, reason: "weather-conditions-not-met" })
        )
      )
    )
  )
)

(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles
      { oracle: oracle }
      { authorized: true }
    )
    (ok true)
  )
)

(define-public (revoke-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles
      { oracle: oracle }
      { authorized: false }
    )
    (ok true)
  )
)

(define-public (fund-contract)
  (let (
    (amount (stx-get-balance tx-sender))
  )
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok amount)
  )
)

(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-weather-data (location (string-ascii 100)) (stacks-block uint))
  (map-get? weather-data { location: location, stacks-block-height: stacks-block })
)

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to false (get authorized (map-get? authorized-oracles { oracle: oracle })))
)

(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

(define-read-only (get-policy-count)
  (var-get policy-counter)
)

(define-private (get-weather-data-for-claim (location (string-ascii 100)) (target-block uint))
  (let (
    (search-range u10)
    (start-block (if (>= target-block search-range) (- target-block search-range) u0))
  )
    (get-latest-weather-in-range location start-block target-block)
  )
)

(define-private (get-latest-weather-in-range 
  (location (string-ascii 100)) 
  (start-block uint) 
  (end-block uint))
  (let (
    (weather-1 (map-get? weather-data { location: location, stacks-block-height: end-block }))
    (weather-2 (map-get? weather-data { location: location, stacks-block-height: (- end-block u1) }))
    (weather-3 (map-get? weather-data { location: location, stacks-block-height: (- end-block u2) }))
    (weather-4 (map-get? weather-data { location: location, stacks-block-height: (- end-block u3) }))
    (weather-5 (map-get? weather-data { location: location, stacks-block-height: (- end-block u4) }))
  )
    (if (is-some weather-1) weather-1
      (if (is-some weather-2) weather-2
        (if (is-some weather-3) weather-3
          (if (is-some weather-4) weather-4
            weather-5
          )
        )
      )
    )
  )
)

(define-read-only (calculate-premium (coverage-amount uint))
  (/ coverage-amount u10)
)

(define-read-only (get-policy-status (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (ok {
      active: (get active policy),
      claimed: (get claimed policy),
      expired: (>= stacks-block-height (get end-block policy))
    })
    ERR_POLICY_NOT_FOUND
  )
)