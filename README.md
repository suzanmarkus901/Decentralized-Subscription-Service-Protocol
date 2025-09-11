# Decentralized Subscription Service Protocol

A Clarity smart contract that enables decentralized subscription services with automated recurring payments on the Stacks blockchain.

## Overview

This protocol allows service providers to create subscription plans and users to subscribe to these plans with STX tokens. The protocol handles payment processing, subscription management, and access control.

## Features

- Service provider registration
- Subscription plan creation and management
- User subscription handling
- Automatic payment processing
- Protocol fee collection
- Subscription renewal and cancellation

## Contract Functions

### Administrative Functions

- `set-protocol-fee`: Set the protocol fee percentage (in basis points)
- `set-fee-recipient`: Set the recipient of protocol fees

### Service Provider Functions

- `register-service-provider`: Register as a service provider
- `create-subscription-plan`: Create a new subscription plan
- `toggle-plan-status`: Activate or deactivate a subscription plan

### User Functions

- `subscribe`: Subscribe to a plan
- `renew-subscription`: Renew an existing subscription
- `cancel-subscription`: Cancel an active subscription

### Read-Only Functions

- `get-protocol-fee`: Get the current protocol fee
- `get-fee-recipient`: Get the current fee recipient
- `get-service-provider`: Get details about a service provider
- `get-subscription-plan`: Get details about a subscription plan
- `get-subscription`: Get details about a subscription
- `get-user-subscription`: Get a user's subscription for a specific plan
- `is-subscription-active`: Check if a subscription is active
- `has-active-subscription`: Check if a user has an active subscription for a plan

## Usage Example

### Register as a Service Provider

```clarity
(contract-call? .SS-protocol register-service-provider "Premium Content Provider")
```

### Create a Subscription Plan

```clarity
;; Parameters: provider-id, name, description, price (in STX), period (in blocks)
(contract-call? .SS-protocol create-subscription-plan u1 "Premium Plan" "Access to premium content" u10000000 u144) ;; ~1 day period (144 blocks)
```

### Subscribe to a Plan

```clarity
(contract-call? .SS-protocol subscribe u1)
```

### Renew a Subscription

```clarity
(contract-call? .SS-protocol renew-subscription u1)
```

### Cancel a Subscription

```clarity
(contract-call? .SS-protocol cancel-subscription u1)
```

## Integration

Service providers can integrate with this protocol by checking subscription status before granting access to their services:

```clarity
;; Check if a user has an active subscription
(contract-call? .SS-protocol has-active-subscription tx-sender u1)
```

## Protocol Fees

The protocol charges a small fee on each subscription payment. The default fee is 0.5% (50 basis points) and can be adjusted by the contract owner.