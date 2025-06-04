# 🌾 AgriSure - Crop Insurance Smart Contract

## 📋 Overview

AgriSure is a decentralized crop insurance platform built on Stacks blockchain using Clarity smart contracts. Farmers can purchase insurance policies that automatically trigger payouts based on adverse weather conditions reported by authorized oracles.

## ✨ Features

- 🚜 **Policy Creation**: Farmers can create customized insurance policies
- 🌦️ **Weather Oracle Integration**: Authorized oracles submit real-time weather data
- 💰 **Automatic Payouts**: Claims are processed automatically based on weather conditions
- 🔒 **Secure & Transparent**: All transactions recorded on blockchain
- 📊 **Flexible Coverage**: Customizable rainfall and temperature thresholds

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd agrisure
clarinet check
```

## 📖 Usage Guide

### 1. 👨‍🌾 Creating a Policy (Farmer)

```clarity
(contract-call? .AgriSure create-policy 
  "corn"           ;; crop type
  u10000           ;; coverage amount (10,000 STX)
  u1000            ;; duration in blocks
  "iowa-farm-001"  ;; location identifier
  u50              ;; minimum rainfall (mm)
  u35              ;; maximum temperature (°C)
)
```

### 2. 🌡️ Submitting Weather Data (Oracle)

```clarity
(contract-call? .AgriSure submit-weather-data
  "iowa-farm-001"  ;; location
  u30              ;; rainfall (mm)
  u40              ;; temperature (°C)
)
```

### 3. 💸 Filing a Claim (Farmer)

```clarity
(contract-call? .AgriSure file-claim u1) ;; policy ID
```

### 4. 🔐 Managing Oracles (Contract Owner)

```clarity
;; Authorize oracle
(contract-call? .AgriSure authorize-oracle 'SP1234...)

;; Revoke oracle
(contract-call? .AgriSure revoke-oracle 'SP1234...)
```

## 📊 Read-Only Functions

### Policy Information
- `get-policy(policy-id)` - Get policy details
- `get-policy-status(policy-id)` - Check policy status
- `get-policy-count()` - Total policies created

### Weather Data
- `get-weather-data(location, block-height)` - Get weather data
- `is-oracle-authorized(oracle)` - Check oracle authorization

### Contract Stats
- `get-contract-balance()` - Contract STX balance
- `calculate-premium(coverage-amount)` - Calculate required premium

## 🏗️ Contract Architecture

### Data Structures

**Policy Map**
- Farmer address
- Crop type and location
- Coverage amount and premium
- Weather thresholds
- Policy duration

**Weather Data Map**
- Location and timestamp
- Rainfall and temperature readings
- Oracle information

### Payout Logic

Payouts are triggered when:
- 🌧️ Rainfall falls below minimum threshold, OR
- 🌡️ Temperature exceeds maximum threshold

## 🧪 Testing

```bash
clarinet test
```

### Test Scenarios
- ✅ Policy creation and premium payment
- ✅ Weather data submission by oracles
- ✅ Successful claim processing
- ✅ Failed claims (conditions not met)
- ✅ Authorization management

## 🔧 Configuration

### Premium Calculation
Current rate: 10% of coverage amount
```clarity
(define-read-only (calculate-premium (coverage-amount uint))
  (/ coverage-amount u10))
```

### Weather Data Search
Claims check weather data within 10 blocks of policy end date.

## 🚨 Error Codes

- `u100` - Unauthorized access
- `u101` - Policy not found
- `u102` - Policy expired
- `u103` - Policy already exists
- `u104` - Insufficient premium
- `u105` - Claim already processed
- `u106` - Invalid weather data
- `u107` - Oracle not authorized
- `u108` - Insufficient contract funds

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Submit pull request

## 📄 License

MIT License - see LICENSE file for details

## 🆘 Support

For questions and support, please open an issue in the repository.

---


