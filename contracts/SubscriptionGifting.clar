;; SubscriptionGifting.clar - Gift subscription functionality
;; Allows users to purchase subscriptions as gifts for others

(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u600))
(define-constant err-already-redeemed (err u601))
(define-constant err-gift-expired (err u602))
(define-constant err-invalid-recipient (err u603))
(define-constant err-self-gift (err u604))
(define-constant err-insufficient-funds (err u605))
(define-constant err-plan-inactive (err u606))
(define-constant err-gift-cancelled (err u607))

(define-data-var next-gift-id uint u0)

;; Maps to store gift subscription data
(define-map gift-subscriptions
  { gift-id: uint }
  {
    giver: principal,
    recipient: (optional principal), ;; none for open gifts
    plan-id: uint,
    amount-paid: uint,
    created-at: uint,
    expires-at: uint,
    redeemed: bool,
    redeemed-at: (optional uint),
    redeemed-by: (optional principal),
    cancelled: bool
  }
)

;; Read-only functions
(define-read-only (get-gift-details (gift-id uint))
  (map-get? gift-subscriptions { gift-id: gift-id })
)

(define-read-only (get-next-gift-id)
  (var-get next-gift-id)
)

;; Create a gift subscription
(define-public (create-gift-subscription 
    (recipient (optional principal))
    (plan-id uint)
    (expiry-blocks uint)
  )
  (let
    (
      (gift-id (var-get next-gift-id))
      (current-time stacks-block-height)
      (expires-at (+ current-time expiry-blocks))
      ;; For demo, assuming a fixed plan price - in reality would cross-contract call
      (plan-price u1000000) ;; 1 STX
    )
    ;; Basic validations
    (asserts! (> expiry-blocks u0) err-invalid-recipient)
    (asserts! 
      (match recipient
        some-recipient (not (is-eq tx-sender some-recipient))
        true
      )
      err-self-gift
    )
    
    ;; Transfer payment to contract
    (try! (stx-transfer? plan-price tx-sender (as-contract tx-sender)))
    
    ;; Create gift entry
    (map-set gift-subscriptions
      { gift-id: gift-id }
      {
        giver: tx-sender,
        recipient: recipient,
        plan-id: plan-id,
        amount-paid: plan-price,
        created-at: current-time,
        expires-at: expires-at,
        redeemed: false,
        redeemed-at: none,
        redeemed-by: none,
        cancelled: false
      }
    )
    
    ;; Increment gift ID counter
    (var-set next-gift-id (+ gift-id u1))
    (ok gift-id)
  )
)

;; Redeem a gift subscription
(define-public (redeem-gift (gift-id uint))
  (let
    (
      (gift (unwrap! (map-get? gift-subscriptions { gift-id: gift-id }) err-not-found))
    )
    ;; Validations
    (asserts! (not (get redeemed gift)) err-already-redeemed)
    (asserts! (not (get cancelled gift)) err-gift-cancelled)
    (asserts! (<= stacks-block-height (get expires-at gift)) err-gift-expired)
    
    ;; Check if gift is for specific recipient
    (match (get recipient gift)
      some-recipient (asserts! (is-eq tx-sender some-recipient) err-invalid-recipient)
      true ;; Open gift, anyone can redeem
    )
    
    ;; Mark as redeemed
    (map-set gift-subscriptions
      { gift-id: gift-id }
      (merge gift {
        redeemed: true,
        redeemed-at: (some stacks-block-height),
        redeemed-by: (some tx-sender)
      })
    )
    
    ;; In a full implementation, would create subscription via cross-contract call
    ;; For now, just return success
    (ok true)
  )
)

;; Cancel a gift subscription (only giver can cancel)
(define-public (cancel-gift (gift-id uint))
  (let
    (
      (gift (unwrap! (map-get? gift-subscriptions { gift-id: gift-id }) err-not-found))
    )
    ;; Only giver can cancel
    (asserts! (is-eq tx-sender (get giver gift)) err-invalid-recipient)
    ;; Can't cancel if already redeemed
    (asserts! (not (get redeemed gift)) err-already-redeemed)
    ;; Can't cancel if already cancelled
    (asserts! (not (get cancelled gift)) err-gift-cancelled)
    
    ;; Mark as cancelled
    (map-set gift-subscriptions
      { gift-id: gift-id }
      (merge gift { cancelled: true })
    )
    
    ;; Refund the giver
    (try! (as-contract (stx-transfer? (get amount-paid gift) tx-sender (get giver gift))))
    
    (ok true)
  )
)

;; Check if a gift is valid for redemption
(define-read-only (is-gift-redeemable (gift-id uint) (potential-redeemer principal))
  (match (map-get? gift-subscriptions { gift-id: gift-id })
    gift
      (and
        (not (get redeemed gift))
        (not (get cancelled gift))
        (<= stacks-block-height (get expires-at gift))
        (match (get recipient gift)
          some-recipient (is-eq potential-redeemer some-recipient)
          true
        )
      )
    false
  )
)




