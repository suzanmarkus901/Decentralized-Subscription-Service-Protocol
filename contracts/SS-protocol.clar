(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-subscription (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-subscription-inactive (err u106))
(define-constant err-subscription-expired (err u107))
(define-constant err-invalid-period (err u108))

(define-data-var protocol-fee uint u50)
(define-data-var fee-recipient principal contract-owner)

(define-map service-providers
  { provider-id: uint }
  {
    principal: principal,
    name: (string-ascii 64),
    active: bool,
    created-at: uint
  }
)

(define-map subscription-plans
  { plan-id: uint }
  {
    provider-id: uint,
    name: (string-ascii 64),
    description: (string-ascii 256),
    price: uint,
    period: uint,
    active: bool,
    created-at: uint
  }
)

(define-map subscriptions
  { subscription-id: uint }
  {
    subscriber: principal,
    plan-id: uint,
    start-time: uint,
    end-time: uint,
    last-payment: uint,
    active: bool
  }
)

(define-map user-subscriptions
  { user: principal, plan-id: uint }
  { subscription-id: uint }
)

(define-map subscription-counters
  { provider-id: uint }
  { counter: uint }
)

(define-data-var next-provider-id uint u1)
(define-data-var next-plan-id uint u1)
(define-data-var next-subscription-id uint u1)

(define-read-only (get-protocol-fee)
  (var-get protocol-fee)
)

(define-read-only (get-fee-recipient)
  (var-get fee-recipient)
)

(define-read-only (get-service-provider (provider-id uint))
  (map-get? service-providers { provider-id: provider-id })
)

(define-read-only (get-subscription-plan (plan-id uint))
  (map-get? subscription-plans { plan-id: plan-id })
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-user-subscription (user principal) (plan-id uint))
  (map-get? user-subscriptions { user: user, plan-id: plan-id })
)

(define-read-only (is-subscription-active (subscription-id uint))
  (match (map-get? subscriptions { subscription-id: subscription-id })
    subscription (and (get active subscription) (< stacks-block-height (get end-time subscription)))
    false
  )
)

(define-read-only (has-active-subscription (user principal) (plan-id uint))
  (match (map-get? user-subscriptions { user: user, plan-id: plan-id })
    user-sub (is-subscription-active (get subscription-id user-sub))
    false
  )
)

(define-public (set-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set protocol-fee new-fee))
  )
)

(define-public (set-fee-recipient (new-recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set fee-recipient new-recipient))
  )
)

(define-public (register-service-provider (name (string-ascii 64)))
  (let
    (
      (provider-id (var-get next-provider-id))
    )
    ;; (asserts! (not (map-get? service-providers { provider-id: provider-id })) err-already-exists)
    (map-set service-providers
      { provider-id: provider-id }
      {
        principal: tx-sender,
        name: name,
        active: true,
        created-at: stacks-block-height
      }
    )
    (var-set next-provider-id (+ provider-id u1))
    (ok provider-id)
  )
)

(define-public (create-subscription-plan 
    (provider-id uint)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (price uint)
    (period uint)
  )
  (let
    (
      (plan-id (var-get next-plan-id))
      (provider (unwrap! (map-get? service-providers { provider-id: provider-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (asserts! (> period u0) err-invalid-period)
    
    (map-set subscription-plans
      { plan-id: plan-id }
      {
        provider-id: provider-id,
        name: name,
        description: description,
        price: price,
        period: period,
        active: true,
        created-at: stacks-block-height
      }
    )
    (var-set next-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

(define-public (toggle-plan-status (plan-id uint) (active bool))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    
    (map-set subscription-plans
      { plan-id: plan-id }
      (merge plan { active: active })
    )
    (ok true)
  )
)

(define-public (subscribe (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) err-not-found))
      (subscription-id (var-get next-subscription-id))
      (fee-amount (/ (* (get price plan) (var-get protocol-fee)) u10000))
      (provider-amount (- (get price plan) fee-amount))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
      (current-time stacks-block-height)
      (end-time (+ current-time (get period plan)))
    )
    (asserts! (get active plan) err-invalid-subscription)
    
    (try! (stx-transfer? (get price plan) tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? fee-amount tx-sender (var-get fee-recipient))))
    (try! (as-contract (stx-transfer? provider-amount tx-sender (get principal provider))))
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      {
        subscriber: tx-sender,
        plan-id: plan-id,
        start-time: current-time,
        end-time: end-time,
        last-payment: current-time,
        active: true
      }
    )
    
    (map-set user-subscriptions
      { user: tx-sender, plan-id: plan-id }
      { subscription-id: subscription-id }
    )
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-public (renew-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (fee-amount (/ (* (get price plan) (var-get protocol-fee)) u10000))
      (provider-amount (- (get price plan) fee-amount))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
      (new-end-time (+ (get end-time subscription) (get period plan)))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (get active subscription) err-subscription-inactive)
    (asserts! (get active plan) err-invalid-subscription)
    
    (try! (stx-transfer? (get price plan) tx-sender (as-contract tx-sender)))
    (try! (as-contract (stx-transfer? fee-amount tx-sender (var-get fee-recipient))))
    (try! (as-contract (stx-transfer? provider-amount tx-sender (get principal provider))))
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription 
        { 
          end-time: new-end-time,
          last-payment: stacks-block-height
        }
      )
    )
    
    (ok true)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
    )
    (asserts! (or 
                (is-eq tx-sender (get subscriber subscription))
                (is-eq tx-sender contract-owner)
              ) 
              err-unauthorized)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription { active: false })
    )
    
    (ok true)
  )
)