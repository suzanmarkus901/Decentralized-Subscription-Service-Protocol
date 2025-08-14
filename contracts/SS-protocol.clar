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

(define-map subscription-usage
  { subscription-id: uint }
  {
    current-usage: uint,
    usage-limit: uint,
    overage-rate: uint,
    last-reset: uint,
    total-overages: uint
  }
)

(define-map usage-records
  { subscription-id: uint, period: uint }
  {
    usage-count: uint,
    overage-charges: uint,
    period-start: uint,
    period-end: uint
  }
)

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


(define-public (get-subscription-status (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
    )
    (ok (get active subscription))
  )
)
(define-public (get-subscription-expiry (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
    )
    (ok (get end-time subscription))
  )
)
(define-public (get-subscription-last-payment (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
    )
    (ok (get last-payment subscription))
  )
)


(define-map subscription-tiers
  { tier-id: uint, plan-id: uint }
  {
    name: (string-ascii 32),
    benefits: (list 10 (string-ascii 64)),
    multiplier: uint
  }
)

(define-data-var next-tier-id uint u1)

(define-read-only (get-tier-details (tier-id uint) (plan-id uint))
  (map-get? subscription-tiers { tier-id: tier-id, plan-id: plan-id })
)

(define-public (add-subscription-tier 
    (plan-id uint)
    (name (string-ascii 32))
    (benefits (list 10 (string-ascii 64)))
    (price-multiplier uint)
  )
  (let
    (
      (tier-id (var-get next-tier-id))
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (map-set subscription-tiers
      { tier-id: tier-id, plan-id: plan-id }
      {
        name: name,
        benefits: benefits,
        multiplier: price-multiplier
      }
    )
    (var-set next-tier-id (+ tier-id u1))
    (ok tier-id)
  )
)


(define-map referral-codes
  { code: (string-ascii 16) }
  {
    owner: principal,
    uses: uint,
    active: bool
  }
)

(define-map referral-rewards
  { user: principal }
  { total-earned: uint }
)

(define-constant referral-reward-percentage u500)

(define-read-only (get-referral-stats (code (string-ascii 16)))
  (map-get? referral-codes { code: code })
)

(define-public (create-referral-code (code (string-ascii 16)))
  (begin
    (asserts! (is-none (map-get? referral-codes { code: code })) err-already-exists)
    (map-set referral-codes
      { code: code }
      {
        owner: tx-sender,
        uses: u0,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (subscribe-with-referral (plan-id uint) (referral-code (string-ascii 16)))
  (let
    (
      (referral (unwrap! (map-get? referral-codes { code: referral-code }) err-not-found))
      (subscription-result (try! (subscribe plan-id)))
      (plan (unwrap! (map-get? subscription-plans { plan-id: plan-id }) err-not-found))
      (reward-amount (/ (* (get price plan) referral-reward-percentage) u10000))
    )
    (asserts! (get active referral) err-invalid-subscription)
    (try! (as-contract (stx-transfer? reward-amount tx-sender (get owner referral))))
    (map-set referral-codes
      { code: referral-code }
      (merge referral { uses: (+ (get uses referral) u1) })
    )
    (ok subscription-result)
  )
)



(define-map provider-metrics
  { provider-id: uint, period: uint }
  {
    total-subscriptions: uint,
    active-subscriptions: uint,
    total-revenue: uint,
    new-subscribers: uint,
    churned-subscribers: uint,
    renewal-count: uint,
    last-updated: uint
  }
)

(define-map subscription-events
  { event-id: uint }
  {
    provider-id: uint,
    subscription-id: uint,
    event-type: (string-ascii 16),
    amount: uint,
    timestamp: uint,
    subscriber: principal
  }
)

(define-map provider-revenue-history
  { provider-id: uint, block-height: uint }
  {
    period-revenue: uint,
    cumulative-revenue: uint,
    subscriber-count: uint
  }
)

(define-map churn-analytics
  { provider-id: uint }
  {
    total-churned: uint,
    churn-rate: uint,
    avg-subscription-length: uint,
    last-calculated: uint
  }
)

(define-data-var next-event-id uint u1)

(define-read-only (get-provider-metrics (provider-id uint) (period uint))
  (map-get? provider-metrics { provider-id: provider-id, period: period })
)



(define-read-only (get-churn-analytics (provider-id uint))
  (map-get? churn-analytics { provider-id: provider-id })
)

(define-read-only (get-subscription-event (event-id uint))
  (map-get? subscription-events { event-id: event-id })
)

(define-read-only (calculate-mrr (provider-id uint))
  (let
    (
      (current-period (/ stacks-block-height u144))
      (metrics (default-to 
        { total-subscriptions: u0, active-subscriptions: u0, total-revenue: u0, new-subscribers: u0, churned-subscribers: u0, renewal-count: u0, last-updated: u0 }
        (map-get? provider-metrics { provider-id: provider-id, period: current-period })
      ))
    )
    (ok (get total-revenue metrics))
  )
)

(define-read-only (calculate-growth-rate (provider-id uint))
  (let
    (
      (current-period (/ stacks-block-height u144))
      (previous-period (- current-period u1))
      (current-metrics (default-to 
        { total-subscriptions: u0, active-subscriptions: u0, total-revenue: u0, new-subscribers: u0, churned-subscribers: u0, renewal-count: u0, last-updated: u0 }
        (map-get? provider-metrics { provider-id: provider-id, period: current-period })
      ))
      (previous-metrics (default-to 
        { total-subscriptions: u0, active-subscriptions: u0, total-revenue: u0, new-subscribers: u0, churned-subscribers: u0, renewal-count: u0, last-updated: u0 }
        (map-get? provider-metrics { provider-id: provider-id, period: previous-period })
      ))
      (current-subs (get active-subscriptions current-metrics))
      (previous-subs (get active-subscriptions previous-metrics))
    )
    (if (is-eq previous-subs u0)
      (ok u0)
      (ok (/ (* (- current-subs previous-subs) u10000) previous-subs))
    )
  )
)

(define-public (record-subscription-event 
    (provider-id uint)
    (subscription-id uint)
    (event-type (string-ascii 16))
    (amount uint)
    (subscriber principal)
  )
  (let
    (
      (event-id (var-get next-event-id))
      (current-period (/ stacks-block-height u144))
    )
    (map-set subscription-events
      { event-id: event-id }
      {
        provider-id: provider-id,
        subscription-id: subscription-id,
        event-type: event-type,
        amount: amount,
        timestamp: stacks-block-height,
        subscriber: subscriber
      }
    )
    (var-set next-event-id (+ event-id u1))
    ;; (try! (update-provider-metrics provider-id current-period event-type amount))
    (ok event-id)
  )
)

(define-private (update-provider-metrics (provider-id uint) (period uint) (event-type (string-ascii 16)) (amount uint))
  (let
    (
      (current-metrics (default-to 
        { total-subscriptions: u0, active-subscriptions: u0, total-revenue: u0, new-subscribers: u0, churned-subscribers: u0, renewal-count: u0, last-updated: u0 }
        (map-get? provider-metrics { provider-id: provider-id, period: period })
      ))
      (updated-metrics (if (is-eq event-type "subscribe")
        (merge current-metrics {
          total-subscriptions: (+ (get total-subscriptions current-metrics) u1),
          active-subscriptions: (+ (get active-subscriptions current-metrics) u1),
          total-revenue: (+ (get total-revenue current-metrics) amount),
          new-subscribers: (+ (get new-subscribers current-metrics) u1),
          last-updated: stacks-block-height
        })
        (if (is-eq event-type "renew")
          (merge current-metrics {
            total-revenue: (+ (get total-revenue current-metrics) amount),
            renewal-count: (+ (get renewal-count current-metrics) u1),
            last-updated: stacks-block-height
          })
          (if (is-eq event-type "cancel")
            (merge current-metrics {
              active-subscriptions: (- (get active-subscriptions current-metrics) u1),
              churned-subscribers: (+ (get churned-subscribers current-metrics) u1),
              last-updated: stacks-block-height
            })
            current-metrics
          )
        )
      ))
    )
    (map-set provider-metrics
      { provider-id: provider-id, period: period }
      updated-metrics
    )
    (ok true)
  )
)

(define-public (update-revenue-snapshot (provider-id uint))
  (let
    (
      (current-period (/ stacks-block-height u144))
      (current-metrics (default-to 
        { total-subscriptions: u0, active-subscriptions: u0, total-revenue: u0, new-subscribers: u0, churned-subscribers: u0, renewal-count: u0, last-updated: u0 }
        (map-get? provider-metrics { provider-id: provider-id, period: current-period })
      ))
      (previous-snapshot (default-to 
        { period-revenue: u0, cumulative-revenue: u0, subscriber-count: u0 }
        (map-get? provider-revenue-history { provider-id: provider-id, block-height: (- stacks-block-height u144) })
      ))
    )
    (map-set provider-revenue-history
      { provider-id: provider-id, block-height: stacks-block-height }
      {
        period-revenue: (get total-revenue current-metrics),
        cumulative-revenue: (+ (get cumulative-revenue previous-snapshot) (get total-revenue current-metrics)),
        subscriber-count: (get active-subscriptions current-metrics)
      }
    )
    (ok true)
  )
)


(define-constant err-usage-limit-exceeded (err u109))
(define-constant err-invalid-usage (err u110))

(define-read-only (get-subscription-usage (subscription-id uint))
  (map-get? subscription-usage { subscription-id: subscription-id })
)

(define-read-only (get-usage-record (subscription-id uint) (period uint))
  (map-get? usage-records { subscription-id: subscription-id, period: period })
)

(define-read-only (get-current-usage-period)
  (/ stacks-block-height u1008)
)

(define-public (initialize-usage-tracking 
    (subscription-id uint)
    (usage-limit uint)
    (overage-rate uint)
  )
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (asserts! (> usage-limit u0) err-invalid-usage)
    (map-set subscription-usage
      { subscription-id: subscription-id }
      {
        current-usage: u0,
        usage-limit: usage-limit,
        overage-rate: overage-rate,
        last-reset: stacks-block-height,
        total-overages: u0
      }
    )
    (ok true)
  )
)

(define-public (record-usage (subscription-id uint) (usage-amount uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
      (usage-data (unwrap! (map-get? subscription-usage { subscription-id: subscription-id }) err-not-found))
      (current-period (get-current-usage-period))
      (new-usage (+ (get current-usage usage-data) usage-amount))
      (overage-amount (if (> new-usage (get usage-limit usage-data))
                        (- new-usage (get usage-limit usage-data))
                        u0))
      (overage-cost (if (> overage-amount u0)
                      (* overage-amount (get overage-rate usage-data))
                      u0))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (asserts! (is-subscription-active subscription-id) err-subscription-inactive)
    (asserts! (> usage-amount u0) err-invalid-usage)
    
    (if (> overage-cost u0)
      (begin
        (try! (stx-transfer? overage-cost (get subscriber subscription) (get principal provider)))
        (map-set subscription-usage
          { subscription-id: subscription-id }
          (merge usage-data {
            current-usage: new-usage,
            total-overages: (+ (get total-overages usage-data) overage-cost)
          })
        )
      )
      (map-set subscription-usage
        { subscription-id: subscription-id }
        (merge usage-data { current-usage: new-usage })
      )
    )
    
    (let
      (
        (current-record (default-to 
          { usage-count: u0, overage-charges: u0, period-start: stacks-block-height, period-end: (+ stacks-block-height u1008) }
          (map-get? usage-records { subscription-id: subscription-id, period: current-period })
        ))
      )
      (map-set usage-records
        { subscription-id: subscription-id, period: current-period }
        (merge current-record {
          usage-count: (+ (get usage-count current-record) usage-amount),
          overage-charges: (+ (get overage-charges current-record) overage-cost)
        })
      )
    )
    
    (ok overage-cost)
  )
)

(define-public (reset-usage-period (subscription-id uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
      (usage-data (unwrap! (map-get? subscription-usage { subscription-id: subscription-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (map-set subscription-usage
      { subscription-id: subscription-id }
      (merge usage-data {
        current-usage: u0,
        last-reset: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-public (update-usage-limit (subscription-id uint) (new-limit uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
      (usage-data (unwrap! (map-get? subscription-usage { subscription-id: subscription-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (asserts! (> new-limit u0) err-invalid-usage)
    (map-set subscription-usage
      { subscription-id: subscription-id }
      (merge usage-data { usage-limit: new-limit })
    )
    (ok true)
  )
)

(define-read-only (calculate-projected-overage (subscription-id uint) (projected-usage uint))
  (match (map-get? subscription-usage { subscription-id: subscription-id })
    usage-data 
      (let
        (
          (total-projected (+ (get current-usage usage-data) projected-usage))
          (overage-amount (if (> total-projected (get usage-limit usage-data))
                            (- total-projected (get usage-limit usage-data))
                            u0))
        )
        (ok (* overage-amount (get overage-rate usage-data)))
      )
    err-not-found
  )
)

(define-map subscription-sla
  { subscription-id: uint }
  {
    uptime-guarantee: uint,
    response-time-max: uint,
    penalty-rate: uint,
    escrow-multiplier: uint,
    active: bool,
    created-at: uint
  }
)

(define-map escrow-deposits
  { subscription-id: uint }
  {
    total-deposited: uint,
    available-balance: uint,
    locked-balance: uint,
    last-deposit: uint,
    auto-refill: bool
  }
)

(define-map sla-violations
  { violation-id: uint }
  {
    subscription-id: uint,
    violation-type: (string-ascii 32),
    severity: uint,
    reported-at: uint,
    verified: bool,
    compensation-amount: uint,
    processed: bool,
    reporter: principal
  }
)

(define-map provider-sla-stats
  { provider-id: uint }
  {
    total-violations: uint,
    uptime-score: uint,
    avg-response-time: uint,
    reputation-score: uint,
    last-updated: uint,
    penalty-balance: uint
  }
)

(define-map sla-claim-periods
  { subscription-id: uint, period: uint }
  {
    uptime-reported: uint,
    downtime-incidents: uint,
    compensation-paid: uint,
    period-start: uint,
    period-end: uint
  }
)

(define-data-var next-violation-id uint u1)
(define-constant err-insufficient-escrow (err u111))
(define-constant err-sla-not-found (err u112))
(define-constant err-violation-already-processed (err u113))
(define-constant err-invalid-sla-params (err u114))
(define-constant err-escrow-locked (err u115))

(define-read-only (get-subscription-sla (subscription-id uint))
  (map-get? subscription-sla { subscription-id: subscription-id })
)

(define-read-only (get-escrow-balance (subscription-id uint))
  (map-get? escrow-deposits { subscription-id: subscription-id })
)

(define-read-only (get-sla-violation (violation-id uint))
  (map-get? sla-violations { violation-id: violation-id })
)

(define-read-only (get-provider-reputation (provider-id uint))
  (match (map-get? provider-sla-stats { provider-id: provider-id })
    stats (get reputation-score stats)
    u5000
  )
)

(define-read-only (calculate-escrow-requirement (subscription-id uint))
  (match (get-subscription-sla subscription-id)
    sla-data
      (match (get-subscription subscription-id)
        subscription
          (match (get-subscription-plan (get plan-id subscription))
            plan
              (ok (* (get price plan) (get escrow-multiplier sla-data)))
            err-not-found
          )
        err-not-found
      )
    err-sla-not-found
  )
)

(define-public (create-subscription-sla 
    (subscription-id uint)
    (uptime-guarantee uint)
    (response-time-max uint)
    (penalty-rate uint)
    (escrow-multiplier uint)
  )
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (asserts! (and 
      (>= uptime-guarantee u9000)
      (<= uptime-guarantee u10000)
      (> response-time-max u0)
      (<= penalty-rate u5000)
      (and (>= escrow-multiplier u1) (<= escrow-multiplier u12))
    ) err-invalid-sla-params)
    
    (map-set subscription-sla
      { subscription-id: subscription-id }
      {
        uptime-guarantee: uptime-guarantee,
        response-time-max: response-time-max,
        penalty-rate: penalty-rate,
        escrow-multiplier: escrow-multiplier,
        active: true,
        created-at: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (deposit-to-escrow (subscription-id uint) (amount uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (sla-data (unwrap! (map-get? subscription-sla { subscription-id: subscription-id }) err-sla-not-found))
      (current-escrow (default-to 
        { total-deposited: u0, available-balance: u0, locked-balance: u0, last-deposit: u0, auto-refill: false }
        (map-get? escrow-deposits { subscription-id: subscription-id })
      ))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (get active sla-data) err-sla-not-found)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrow-deposits
      { subscription-id: subscription-id }
      {
        total-deposited: (+ (get total-deposited current-escrow) amount),
        available-balance: (+ (get available-balance current-escrow) amount),
        locked-balance: (get locked-balance current-escrow),
        last-deposit: stacks-block-height,
        auto-refill: (get auto-refill current-escrow)
      }
    )
    (ok true)
  )
)

(define-public (report-sla-violation 
    (subscription-id uint)
    (violation-type (string-ascii 32))
    (severity uint)
  )
  (let
    (
      (violation-id (var-get next-violation-id))
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (sla-data (unwrap! (map-get? subscription-sla { subscription-id: subscription-id }) err-sla-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-deposits { subscription-id: subscription-id }) err-insufficient-escrow))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (get active sla-data) err-sla-not-found)
    (asserts! (and (>= severity u1) (<= severity u5)) err-invalid-sla-params)
    
    (let
      (
        (base-compensation (/ (* (get price plan) (get penalty-rate sla-data)) u10000))
        (severity-multiplier (+ u100 (* severity u50)))
        (compensation-amount (/ (* base-compensation severity-multiplier) u100))
      )
      
      (map-set sla-violations
        { violation-id: violation-id }
        {
          subscription-id: subscription-id,
          violation-type: violation-type,
          severity: severity,
          reported-at: stacks-block-height,
          verified: false,
          compensation-amount: compensation-amount,
          processed: false,
          reporter: tx-sender
        }
      )
      
      (var-set next-violation-id (+ violation-id u1))
      (ok violation-id)
    )
  )
)

(define-public (process-sla-violation (violation-id uint))
  (let
    (
      (violation (unwrap! (map-get? sla-violations { violation-id: violation-id }) err-not-found))
      (subscription (unwrap! (map-get? subscriptions { subscription-id: (get subscription-id violation) }) err-not-found))
      (plan (unwrap! (map-get? subscription-plans { plan-id: (get plan-id subscription) }) err-not-found))
      (provider (unwrap! (map-get? service-providers { provider-id: (get provider-id plan) }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-deposits { subscription-id: (get subscription-id violation) }) err-insufficient-escrow))
    )
    (asserts! (is-eq tx-sender (get principal provider)) err-unauthorized)
    (asserts! (not (get processed violation)) err-violation-already-processed)
    (asserts! (>= (get available-balance escrow-data) (get compensation-amount violation)) err-insufficient-escrow)
    
    (try! (as-contract (stx-transfer? 
      (get compensation-amount violation) 
      tx-sender 
      (get subscriber subscription)
    )))
    
    (map-set escrow-deposits
      { subscription-id: (get subscription-id violation) }
      (merge escrow-data {
        available-balance: (- (get available-balance escrow-data) (get compensation-amount violation))
      })
    )
    
    (map-set sla-violations
      { violation-id: violation-id }
      (merge violation { verified: true, processed: true })
    )
    
    (let
      (
        (provider-stats (default-to 
          { total-violations: u0, uptime-score: u10000, avg-response-time: u0, reputation-score: u10000, last-updated: u0, penalty-balance: u0 }
          (map-get? provider-sla-stats { provider-id: (get provider-id plan) })
        ))
        (new-violation-count (+ (get total-violations provider-stats) u1))
        (reputation-penalty (* (get severity violation) u200))
        (new-reputation (if (> (get reputation-score provider-stats) reputation-penalty)
                          (- (get reputation-score provider-stats) reputation-penalty)
                          u0))
      )
      (map-set provider-sla-stats
        { provider-id: (get provider-id plan) }
        (merge provider-stats {
          total-violations: new-violation-count,
          reputation-score: new-reputation,
          last-updated: stacks-block-height,
          penalty-balance: (+ (get penalty-balance provider-stats) (get compensation-amount violation))
        })
      )
    )
    
    (ok (get compensation-amount violation))
  )
)

(define-public (withdraw-escrow (subscription-id uint) (amount uint))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-deposits { subscription-id: subscription-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    (asserts! (>= (get available-balance escrow-data) amount) err-insufficient-escrow)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get subscriber subscription))))
    
    (map-set escrow-deposits
      { subscription-id: subscription-id }
      (merge escrow-data {
        available-balance: (- (get available-balance escrow-data) amount)
      })
    )
    (ok true)
  )
)

(define-public (toggle-escrow-auto-refill (subscription-id uint) (enable bool))
  (let
    (
      (subscription (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found))
      (escrow-data (unwrap! (map-get? escrow-deposits { subscription-id: subscription-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get subscriber subscription)) err-unauthorized)
    
    (map-set escrow-deposits
      { subscription-id: subscription-id }
      (merge escrow-data { auto-refill: enable })
    )
    (ok true)
  )
)



