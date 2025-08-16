
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
(define-constant ERR_PORTFOLIO_NOT_FOUND (err u109))
(define-constant ERR_PORTFOLIO_ALREADY_EXISTS (err u110))
(define-constant ERR_INVALID_ALLOCATION (err u111))
(define-constant ERR_CROP_NOT_FOUND (err u112))
(define-constant ERR_INSUFFICIENT_DIVERSITY (err u113))
(define-constant ERR_INVALID_CROP_TYPE (err u114))
(define-constant ERR_PORTFOLIO_LOCKED (err u115))
(define-constant ERR_FARMER_NOT_REGISTERED (err u116))
(define-constant ERR_INVALID_RATING (err u117))
(define-constant ERR_INSUFFICIENT_HISTORY (err u118))
(define-constant ERR_FARM_ALREADY_REGISTERED (err u119))
(define-constant ERR_INVALID_FARM_DATA (err u120))

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
(define-data-var portfolio-counter uint u0)
(define-data-var farmer-counter uint u0)

(define-map farmer-profiles
  { farmer: principal }
  {
    registration-block: uint,
    total-policies: uint,
    total-claims: uint,
    total-premiums-paid: uint,
    total-payouts-received: uint,
    current-risk-score: uint,
    reputation-score: uint,
    active: bool
  }
)

(define-map farm-characteristics
  { farmer: principal }
  {
    farm-size: uint,
    soil-quality-score: uint,
    irrigation-type: uint,
    years-experience: uint,
    organic-certified: bool,
    climate-zone: uint,
    elevation: uint,
    water-access-score: uint
  }
)

(define-map performance-history
  { farmer: principal, year: uint }
  {
    policies-created: uint,
    claims-filed: uint,
    successful-harvests: uint,
    total-yield: uint,
    premium-discount: uint,
    performance-rating: uint
  }
)

(define-map premium-adjustments
  { farmer: principal }
  {
    base-multiplier: uint,
    risk-multiplier: uint,
    reputation-multiplier: uint,
    experience-multiplier: uint,
    final-multiplier: uint,
    last-updated: uint
  }
)

(define-map crop-portfolios
  { portfolio-id: uint }
  {
    farmer: principal,
    name: (string-ascii 50),
    total-land-area: uint,
    target-risk-score: uint,
    current-risk-score: uint,
    creation-block: uint,
    last-rebalance-block: uint,
    locked: bool,
    active: bool
  }
)

(define-map portfolio-allocations
  { portfolio-id: uint, crop-type: (string-ascii 50) }
  {
    allocated-area: uint,
    target-percentage: uint,
    current-percentage: uint,
    expected-yield: uint,
    risk-factor: uint,
    seasonal-multiplier: uint
  }
)

(define-map crop-risk-factors
  { crop-type: (string-ascii 50) }
  {
    base-risk: uint,
    weather-sensitivity: uint,
    market-volatility: uint,
    growth-cycle: uint,
    water-requirement: uint
  }
)

(define-map portfolio-performance
  { portfolio-id: uint, season: uint }
  {
    total-yield: uint,
    total-revenue: uint,
    risk-adjusted-return: uint,
    diversity-score: uint,
    rebalance-needed: bool
  }
)

(define-map seasonal-factors
  { season: uint, crop-type: (string-ascii 50) }
  {
    yield-multiplier: uint,
    price-multiplier: uint,
    risk-multiplier: uint
  }
)

(define-public (create-policy 
  (crop-type (string-ascii 50))
  (coverage-amount uint)
  (duration-blocks uint)
  (location (string-ascii 100))
  (min-rainfall uint)
  (max-temperature uint))
  (let (
    (policy-id (+ (var-get policy-counter) u1))
    (base-premium (/ coverage-amount u10))
    (adjusted-premium (calculate-adjusted-premium tx-sender base-premium))
    (start-block stacks-block-height)
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (>= (stx-get-balance tx-sender) adjusted-premium) ERR_INSUFFICIENT_PREMIUM)
    (asserts! (is-none (map-get? policies { policy-id: policy-id })) ERR_POLICY_ALREADY_EXISTS)
    
    (try! (stx-transfer? adjusted-premium tx-sender (as-contract tx-sender)))
    
    (map-set policies
      { policy-id: policy-id }
      {
        farmer: tx-sender,
        crop-type: crop-type,
        coverage-amount: coverage-amount,
        premium-paid: adjusted-premium,
        start-block: start-block,
        end-block: end-block,
        location: location,
        min-rainfall: min-rainfall,
        max-temperature: max-temperature,
        active: true,
        claimed: false
      }
    )
    
    (unwrap-panic (update-farmer-stats tx-sender adjusted-premium))
    (var-set policy-counter policy-id)
    (var-set contract-balance (+ (var-get contract-balance) adjusted-premium))
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

(define-public (create-crop-portfolio 
  (name (string-ascii 50))
  (total-land-area uint)
  (target-risk-score uint))
  (let (
    (portfolio-id (+ (var-get portfolio-counter) u1))
  )
    (asserts! (> total-land-area u0) ERR_INVALID_ALLOCATION)
    (asserts! (and (>= target-risk-score u1) (<= target-risk-score u100)) ERR_INVALID_ALLOCATION)
    (asserts! (is-none (map-get? crop-portfolios { portfolio-id: portfolio-id })) ERR_PORTFOLIO_ALREADY_EXISTS)
    
    (map-set crop-portfolios
      { portfolio-id: portfolio-id }
      {
        farmer: tx-sender,
        name: name,
        total-land-area: total-land-area,
        target-risk-score: target-risk-score,
        current-risk-score: u0,
        creation-block: stacks-block-height,
        last-rebalance-block: stacks-block-height,
        locked: false,
        active: true
      }
    )
    
    (var-set portfolio-counter portfolio-id)
    (ok portfolio-id)
  )
)

(define-public (allocate-crop-to-portfolio 
  (portfolio-id uint)
  (crop-type (string-ascii 50))
  (allocated-area uint)
  (target-percentage uint)
  (expected-yield uint))
  (let (
    (portfolio (unwrap! (map-get? crop-portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND))
    (farmer (get farmer portfolio))
    (total-area (get total-land-area portfolio))
    (locked (get locked portfolio))
    (risk-factors (default-to { base-risk: u50, weather-sensitivity: u30, market-volatility: u20, growth-cycle: u10, water-requirement: u25 } 
                   (map-get? crop-risk-factors { crop-type: crop-type })))
    (calculated-risk (calculate-crop-risk-factor risk-factors))
  )
    (asserts! (is-eq tx-sender farmer) ERR_UNAUTHORIZED)
    (asserts! (not locked) ERR_PORTFOLIO_LOCKED)
    (asserts! (> allocated-area u0) ERR_INVALID_ALLOCATION)
    (asserts! (<= allocated-area total-area) ERR_INVALID_ALLOCATION)
    (asserts! (and (>= target-percentage u1) (<= target-percentage u100)) ERR_INVALID_ALLOCATION)
    
    (map-set portfolio-allocations
      { portfolio-id: portfolio-id, crop-type: crop-type }
      {
        allocated-area: allocated-area,
        target-percentage: target-percentage,
        current-percentage: (/ (* allocated-area u100) total-area),
        expected-yield: expected-yield,
        risk-factor: calculated-risk,
        seasonal-multiplier: u100
      }
    )
    
    (let (
      (updated-portfolio (merge portfolio { current-risk-score: (calculate-portfolio-risk portfolio-id) }))
    )
      (map-set crop-portfolios { portfolio-id: portfolio-id } updated-portfolio)
      (ok true)
    )
  )
)

(define-public (rebalance-portfolio (portfolio-id uint))
  (let (
    (portfolio (unwrap! (map-get? crop-portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND))
    (farmer (get farmer portfolio))
    (locked (get locked portfolio))
    (active (get active portfolio))
    (last-rebalance (get last-rebalance-block portfolio))
    (min-rebalance-interval u1008)
  )
    (asserts! (is-eq tx-sender farmer) ERR_UNAUTHORIZED)
    (asserts! active ERR_PORTFOLIO_NOT_FOUND)
    (asserts! (not locked) ERR_PORTFOLIO_LOCKED)
    (asserts! (>= (- stacks-block-height last-rebalance) min-rebalance-interval) ERR_INVALID_ALLOCATION)
    
    (let (
      (diversity-score (calculate-portfolio-diversity portfolio-id))
      (rebalance-needed (< diversity-score u60))
    )
      (if rebalance-needed
        (begin
          (map-set crop-portfolios
            { portfolio-id: portfolio-id }
            (merge portfolio { 
              last-rebalance-block: stacks-block-height,
              current-risk-score: (calculate-portfolio-risk portfolio-id)
            })
          )
          (ok { rebalanced: true, diversity-score: diversity-score })
        )
        (ok { rebalanced: false, diversity-score: diversity-score })
      )
    )
  )
)

(define-public (set-crop-risk-factors 
  (crop-type (string-ascii 50))
  (base-risk uint)
  (weather-sensitivity uint)
  (market-volatility uint)
  (growth-cycle uint)
  (water-requirement uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (<= base-risk u100) (<= weather-sensitivity u100) (<= market-volatility u100)) ERR_INVALID_ALLOCATION)
    (asserts! (and (<= growth-cycle u365) (<= water-requirement u100)) ERR_INVALID_ALLOCATION)
    
    (map-set crop-risk-factors
      { crop-type: crop-type }
      {
        base-risk: base-risk,
        weather-sensitivity: weather-sensitivity,
        market-volatility: market-volatility,
        growth-cycle: growth-cycle,
        water-requirement: water-requirement
      }
    )
    (ok true)
  )
)

(define-public (update-seasonal-factors 
  (season uint)
  (crop-type (string-ascii 50))
  (yield-multiplier uint)
  (price-multiplier uint)
  (risk-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= season u1) (<= season u4)) ERR_INVALID_ALLOCATION)
    (asserts! (and (>= yield-multiplier u50) (<= yield-multiplier u200)) ERR_INVALID_ALLOCATION)
    (asserts! (and (>= price-multiplier u50) (<= price-multiplier u200)) ERR_INVALID_ALLOCATION)
    (asserts! (and (>= risk-multiplier u50) (<= risk-multiplier u200)) ERR_INVALID_ALLOCATION)
    
    (map-set seasonal-factors
      { season: season, crop-type: crop-type }
      {
        yield-multiplier: yield-multiplier,
        price-multiplier: price-multiplier,
        risk-multiplier: risk-multiplier
      }
    )
    (ok true)
  )
)

(define-public (record-portfolio-performance 
  (portfolio-id uint)
  (season uint)
  (total-yield uint)
  (total-revenue uint))
  (let (
    (portfolio (unwrap! (map-get? crop-portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND))
    (farmer (get farmer portfolio))
    (diversity-score (calculate-portfolio-diversity portfolio-id))
    (risk-score (get current-risk-score portfolio))
    (risk-adjusted-return (calculate-risk-adjusted-return total-revenue risk-score))
    (rebalance-needed (< diversity-score u60))
  )
    (asserts! (is-eq tx-sender farmer) ERR_UNAUTHORIZED)
    (asserts! (and (>= season u1) (<= season u4)) ERR_INVALID_ALLOCATION)
    
    (map-set portfolio-performance
      { portfolio-id: portfolio-id, season: season }
      {
        total-yield: total-yield,
        total-revenue: total-revenue,
        risk-adjusted-return: risk-adjusted-return,
        diversity-score: diversity-score,
        rebalance-needed: rebalance-needed
      }
    )
    (ok true)
  )
)

(define-read-only (get-portfolio (portfolio-id uint))
  (map-get? crop-portfolios { portfolio-id: portfolio-id })
)

(define-read-only (get-portfolio-allocation (portfolio-id uint) (crop-type (string-ascii 50)))
  (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: crop-type })
)

(define-read-only (get-crop-risk-factors (crop-type (string-ascii 50)))
  (map-get? crop-risk-factors { crop-type: crop-type })
)

(define-read-only (get-portfolio-performance (portfolio-id uint) (season uint))
  (map-get? portfolio-performance { portfolio-id: portfolio-id, season: season })
)

(define-read-only (get-seasonal-factors (season uint) (crop-type (string-ascii 50)))
  (map-get? seasonal-factors { season: season, crop-type: crop-type })
)

(define-read-only (get-portfolio-count)
  (var-get portfolio-counter)
)

(define-private (calculate-crop-risk-factor (risk-factors { base-risk: uint, weather-sensitivity: uint, market-volatility: uint, growth-cycle: uint, water-requirement: uint }))
  (let (
    (base (get base-risk risk-factors))
    (weather (get weather-sensitivity risk-factors))
    (market (get market-volatility risk-factors))
    (cycle (get growth-cycle risk-factors))
    (water (get water-requirement risk-factors))
    (weighted-risk (+ (* base u3) (* weather u2) (* market u2) (* cycle u1) (* water u1)))
  )
    (/ weighted-risk u9)
  )
)

(define-private (calculate-portfolio-risk (portfolio-id uint))
  (let (
    (portfolio (default-to { farmer: tx-sender, name: "", total-land-area: u0, target-risk-score: u0, current-risk-score: u0, creation-block: u0, last-rebalance-block: u0, locked: false, active: false } 
               (map-get? crop-portfolios { portfolio-id: portfolio-id })))
    (total-area (get total-land-area portfolio))
  )
    (if (> total-area u0)
      (calculate-weighted-risk portfolio-id total-area)
      u0
    )
  )
)

(define-private (calculate-weighted-risk (portfolio-id uint) (total-area uint))
  (let (
    (crop-1-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "wheat" }))
    (crop-2-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "corn" }))
    (crop-3-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "soybeans" }))
    (weighted-risk-1 (if (is-some crop-1-alloc) 
                       (/ (* (get risk-factor (unwrap-panic crop-1-alloc)) (get allocated-area (unwrap-panic crop-1-alloc))) total-area)
                       u0))
    (weighted-risk-2 (if (is-some crop-2-alloc) 
                       (/ (* (get risk-factor (unwrap-panic crop-2-alloc)) (get allocated-area (unwrap-panic crop-2-alloc))) total-area)
                       u0))
    (weighted-risk-3 (if (is-some crop-3-alloc) 
                       (/ (* (get risk-factor (unwrap-panic crop-3-alloc)) (get allocated-area (unwrap-panic crop-3-alloc))) total-area)
                       u0))
  )
    (+ weighted-risk-1 weighted-risk-2 weighted-risk-3)
  )
)

(define-private (calculate-portfolio-diversity (portfolio-id uint))
  (let (
    (crop-1-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "wheat" }))
    (crop-2-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "corn" }))
    (crop-3-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "soybeans" }))
    (crop-count (+ (if (is-some crop-1-alloc) u1 u0) (if (is-some crop-2-alloc) u1 u0) (if (is-some crop-3-alloc) u1 u0)))
    (base-diversity (* crop-count u20))
    (allocation-balance (calculate-allocation-balance portfolio-id))
  )
    (+ base-diversity allocation-balance)
  )
)

(define-private (calculate-allocation-balance (portfolio-id uint))
  (let (
    (crop-1-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "wheat" }))
    (crop-2-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "corn" }))
    (crop-3-alloc (map-get? portfolio-allocations { portfolio-id: portfolio-id, crop-type: "soybeans" }))
    (pct-1 (if (is-some crop-1-alloc) (get current-percentage (unwrap-panic crop-1-alloc)) u0))
    (pct-2 (if (is-some crop-2-alloc) (get current-percentage (unwrap-panic crop-2-alloc)) u0))
    (pct-3 (if (is-some crop-3-alloc) (get current-percentage (unwrap-panic crop-3-alloc)) u0))
    (ideal-pct u33)
    (deviation-1 (if (> pct-1 ideal-pct) (- pct-1 ideal-pct) (- ideal-pct pct-1)))
    (deviation-2 (if (> pct-2 ideal-pct) (- pct-2 ideal-pct) (- ideal-pct pct-2)))
    (deviation-3 (if (> pct-3 ideal-pct) (- pct-3 ideal-pct) (- ideal-pct pct-3)))
    (total-deviation (+ deviation-1 deviation-2 deviation-3))
  )
    (if (< total-deviation u30) u40 (- u40 (/ total-deviation u3)))
  )
)

(define-private (calculate-risk-adjusted-return (revenue uint) (risk-score uint))
  (if (> risk-score u0)
    (/ (* revenue u100) risk-score)
    revenue
  )
)

(define-public (register-farmer 
  (farm-size uint)
  (soil-quality-score uint)
  (irrigation-type uint)
  (years-experience uint)
  (organic-certified bool)
  (climate-zone uint)
  (elevation uint)
  (water-access-score uint))
  (begin
    (asserts! (is-none (map-get? farmer-profiles { farmer: tx-sender })) ERR_FARM_ALREADY_REGISTERED)
    (asserts! (and (> farm-size u0) (<= farm-size u10000)) ERR_INVALID_FARM_DATA)
    (asserts! (and (>= soil-quality-score u1) (<= soil-quality-score u100)) ERR_INVALID_FARM_DATA)
    (asserts! (<= irrigation-type u3) ERR_INVALID_FARM_DATA)
    (asserts! (<= years-experience u100) ERR_INVALID_FARM_DATA)
    (asserts! (and (>= climate-zone u1) (<= climate-zone u10)) ERR_INVALID_FARM_DATA)
    (asserts! (<= elevation u5000) ERR_INVALID_FARM_DATA)
    (asserts! (and (>= water-access-score u1) (<= water-access-score u100)) ERR_INVALID_FARM_DATA)
    
    (map-set farmer-profiles
      { farmer: tx-sender }
      {
        registration-block: stacks-block-height,
        total-policies: u0,
        total-claims: u0,
        total-premiums-paid: u0,
        total-payouts-received: u0,
        current-risk-score: u50,
        reputation-score: u50,
        active: true
      }
    )
    
    (map-set farm-characteristics
      { farmer: tx-sender }
      {
        farm-size: farm-size,
        soil-quality-score: soil-quality-score,
        irrigation-type: irrigation-type,
        years-experience: years-experience,
        organic-certified: organic-certified,
        climate-zone: climate-zone,
        elevation: elevation,
        water-access-score: water-access-score
      }
    )
    
    (try! (calculate-initial-premium-adjustment tx-sender))
    (var-set farmer-counter (+ (var-get farmer-counter) u1))
    (ok true)
  )
)

(define-public (update-performance-record 
  (year uint)
  (policies-created uint)
  (claims-filed uint)
  (successful-harvests uint)
  (total-yield uint))
  (let (
    (farmer-profile (unwrap! (map-get? farmer-profiles { farmer: tx-sender }) ERR_FARMER_NOT_REGISTERED))
    (performance-rating (calculate-performance-rating claims-filed policies-created successful-harvests))
    (premium-discount (calculate-premium-discount performance-rating))
  )
    (asserts! (and (>= year u2020) (<= year u2050)) ERR_INVALID_FARM_DATA)
    (asserts! (<= claims-filed policies-created) ERR_INVALID_FARM_DATA)
    
    (map-set performance-history
      { farmer: tx-sender, year: year }
      {
        policies-created: policies-created,
        claims-filed: claims-filed,
        successful-harvests: successful-harvests,
        total-yield: total-yield,
        premium-discount: premium-discount,
        performance-rating: performance-rating
      }
    )
    
    (try! (update-risk-score tx-sender))
    (try! (update-reputation-score tx-sender))
    (try! (recalculate-premium-adjustment tx-sender))
    (ok true)
  )
)

(define-public (update-farmer-stats (farmer principal) (premium-paid uint))
  (let (
    (profile (default-to 
      { registration-block: stacks-block-height, total-policies: u0, total-claims: u0, total-premiums-paid: u0, total-payouts-received: u0, current-risk-score: u50, reputation-score: u50, active: true }
      (map-get? farmer-profiles { farmer: farmer })))
  )
    (map-set farmer-profiles
      { farmer: farmer }
      (merge profile {
        total-policies: (+ (get total-policies profile) u1),
        total-premiums-paid: (+ (get total-premiums-paid profile) premium-paid)
      })
    )
    (ok true)
  )
)

(define-public (record-claim-payout (farmer principal) (payout-amount uint))
  (let (
    (profile (unwrap! (map-get? farmer-profiles { farmer: farmer }) ERR_FARMER_NOT_REGISTERED))
  )
    (map-set farmer-profiles
      { farmer: farmer }
      (merge profile {
        total-claims: (+ (get total-claims profile) u1),
        total-payouts-received: (+ (get total-payouts-received profile) payout-amount)
      })
    )
    (try! (update-risk-score farmer))
    (try! (recalculate-premium-adjustment farmer))
    (ok true)
  )
)

(define-public (calculate-initial-premium-adjustment (farmer principal))
  (let (
    (farm-chars (unwrap! (map-get? farm-characteristics { farmer: farmer }) ERR_FARMER_NOT_REGISTERED))
    (experience-mult (calculate-experience-multiplier (get years-experience farm-chars)))
    (farm-quality-mult (calculate-farm-quality-multiplier farm-chars))
  )
    (map-set premium-adjustments
      { farmer: farmer }
      {
        base-multiplier: u100,
        risk-multiplier: u100,
        reputation-multiplier: u100,
        experience-multiplier: experience-mult,
        final-multiplier: (/ (* u100 experience-mult farm-quality-mult) u10000),
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (recalculate-premium-adjustment (farmer principal))
  (let (
    (profile (unwrap! (map-get? farmer-profiles { farmer: farmer }) ERR_FARMER_NOT_REGISTERED))
    (farm-chars (unwrap! (map-get? farm-characteristics { farmer: farmer }) ERR_FARMER_NOT_REGISTERED))
    (risk-mult (calculate-risk-multiplier (get current-risk-score profile)))
    (reputation-mult (calculate-reputation-multiplier (get reputation-score profile)))
    (experience-mult (calculate-experience-multiplier (get years-experience farm-chars)))
    (farm-quality-mult (calculate-farm-quality-multiplier farm-chars))
    (final-mult (/ (* risk-mult reputation-mult experience-mult farm-quality-mult) u1000000))
  )
    (map-set premium-adjustments
      { farmer: farmer }
      {
        base-multiplier: u100,
        risk-multiplier: risk-mult,
        reputation-multiplier: reputation-mult,
        experience-multiplier: experience-mult,
        final-multiplier: final-mult,
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-read-only (get-farmer-profile (farmer principal))
  (map-get? farmer-profiles { farmer: farmer })
)

(define-read-only (get-farm-characteristics (farmer principal))
  (map-get? farm-characteristics { farmer: farmer })
)

(define-read-only (get-performance-history (farmer principal) (year uint))
  (map-get? performance-history { farmer: farmer, year: year })
)

(define-read-only (get-premium-adjustment (farmer principal))
  (map-get? premium-adjustments { farmer: farmer })
)

(define-read-only (get-farmer-count)
  (var-get farmer-counter)
)

(define-read-only (calculate-adjusted-premium (farmer principal) (base-premium uint))
  (let (
    (adjustment (map-get? premium-adjustments { farmer: farmer }))
  )
    (if (is-some adjustment)
      (/ (* base-premium (get final-multiplier (unwrap-panic adjustment))) u100)
      base-premium
    )
  )
)

(define-private (update-risk-score (farmer principal))
  (let (
    (profile (unwrap! (map-get? farmer-profiles { farmer: farmer }) ERR_FARMER_NOT_REGISTERED))
    (total-policies (get total-policies profile))
    (total-claims (get total-claims profile))
    (claim-ratio (if (> total-policies u0) (/ (* total-claims u100) total-policies) u0))
    (new-risk-score (+ u30 (/ claim-ratio u2)))
  )
    (map-set farmer-profiles
      { farmer: farmer }
      (merge profile { current-risk-score: (if (> new-risk-score u100) u100 new-risk-score) })
    )
    (ok true)
  )
)

(define-private (update-reputation-score (farmer principal))
  (let (
    (profile (unwrap! (map-get? farmer-profiles { farmer: farmer }) ERR_FARMER_NOT_REGISTERED))
    (blocks-since-registration (- stacks-block-height (get registration-block profile)))
    (total-policies (get total-policies profile))
    (total-claims (get total-claims profile))
    (longevity-bonus (if (> blocks-since-registration u52560) u10 u0))
    (activity-bonus (if (> total-policies u5) u10 u0))
    (claim-penalty (if (> total-claims u0) (* total-claims u5) u0))
    (new-reputation (+ u50 longevity-bonus activity-bonus (- claim-penalty)))
  )
    (map-set farmer-profiles
      { farmer: farmer }
      (merge profile { 
        reputation-score: (if (> new-reputation u100) u100 (if (< new-reputation u1) u1 new-reputation))
      })
    )
    (ok true)
  )
)

(define-private (calculate-performance-rating (claims-filed uint) (policies-created uint) (successful-harvests uint))
  (let (
    (claim-ratio (if (> policies-created u0) (/ (* claims-filed u100) policies-created) u0))
    (harvest-success-ratio (if (> policies-created u0) (/ (* successful-harvests u100) policies-created) u0))
    (base-rating u50)
    (claim-penalty (/ claim-ratio u2))
    (harvest-bonus (/ harvest-success-ratio u4))
  )
    (let ((final-rating (+ base-rating harvest-bonus (- claim-penalty))))
      (if (> final-rating u100) u100 (if (< final-rating u1) u1 final-rating))
    )
  )
)

(define-private (calculate-premium-discount (performance-rating uint))
  (if (>= performance-rating u80)
    u20
    (if (>= performance-rating u60)
      u10
      (if (>= performance-rating u40)
        u5
        u0
      )
    )
  )
)

(define-private (calculate-risk-multiplier (risk-score uint))
  (if (<= risk-score u30)
    u80
    (if (<= risk-score u50)
      u100
      (if (<= risk-score u70)
        u120
        u150
      )
    )
  )
)

(define-private (calculate-reputation-multiplier (reputation-score uint))
  (if (>= reputation-score u80)
    u85
    (if (>= reputation-score u60)
      u95
      (if (>= reputation-score u40)
        u105
        u115
      )
    )
  )
)

(define-private (calculate-experience-multiplier (years-experience uint))
  (if (>= years-experience u20)
    u85
    (if (>= years-experience u10)
      u90
      (if (>= years-experience u5)
        u95
        u105
      )
    )
  )
)

(define-private (calculate-farm-quality-multiplier 
  (farm-chars { farm-size: uint, soil-quality-score: uint, irrigation-type: uint, years-experience: uint, organic-certified: bool, climate-zone: uint, elevation: uint, water-access-score: uint }))
  (let (
    (soil-bonus (if (>= (get soil-quality-score farm-chars) u80) u95 u100))
    (irrigation-bonus (if (<= (get irrigation-type farm-chars) u1) u95 u100))
    (water-bonus (if (>= (get water-access-score farm-chars) u80) u95 u100))
    (organic-bonus (if (get organic-certified farm-chars) u90 u100))
  )
    (/ (* soil-bonus irrigation-bonus water-bonus organic-bonus) u1000000)
  )
)



