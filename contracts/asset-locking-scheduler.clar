;; ============================================================
;; Contract: asset-locking-scheduler
;; Purpose : Time-based asset locking with penalties
;; ============================================================

;; ------------------------------------------------------------
;; Error Codes
;; ------------------------------------------------------------
(define-constant ERR-NOT-ADMIN           (err u1200))
(define-constant ERR-NOT-BENEFICIARY     (err u1201))
(define-constant ERR-LOCK-NOT-FOUND      (err u1202))
(define-constant ERR-LOCK-ACTIVE         (err u1203))
(define-constant ERR-LOCK-EXPIRED        (err u1204))
(define-constant ERR-NOTHING-TO-WITHDRAW (err u1205))
(define-constant ERR-INVALID-PARAMS      (err u1206))

;; ------------------------------------------------------------
;; Data Storage
;; ------------------------------------------------------------

(define-data-var admin principal tx-sender)
(define-data-var lock-counter uint u0)

(define-map locks
  { id: uint }
  {
    creator: principal,
    beneficiary: principal,
    amount: uint,
    unlock-height: uint,
    penalty-rate: uint, ;; percentage (0-100) replaced en-dash with hyphen
    withdrawn: bool
  }
)

;; ------------------------------------------------------------
;; Read-Only Functions
;; ------------------------------------------------------------

(define-read-only (get-lock (id uint))
  (map-get? locks { id: id })
)

(define-read-only (is-unlocked (id uint))
  (match (map-get? locks { id: id })
    l
    (>= stacks-block-height (get unlock-height l))
    false
  )
)

(define-read-only (penalty-amount (id uint))
  (match (map-get? locks { id: id })
    l
    (/ (* (get amount l) (get penalty-rate l)) u100)
    u0
  )
)

;; ------------------------------------------------------------
;; Public Functions - Admin Actions replaced em-dash with hyphen
;; ------------------------------------------------------------

(define-public (create-lock
  (beneficiary principal)
  (amount uint)
  (unlock-height uint)
  (penalty-rate uint)
)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
    (asserts! (> amount u0) ERR-INVALID-PARAMS)
    (asserts! (> unlock-height stacks-block-height) ERR-INVALID-PARAMS)
    (asserts! (<= penalty-rate u100) ERR-INVALID-PARAMS)

    ;; Transfer funds into contract custody
    (try!
      (stx-transfer?
        amount
        tx-sender
        (as-contract tx-sender)
      )
    )

    (let ((id (+ (var-get lock-counter) u1)))
      (var-set lock-counter id)

      (map-set locks
        { id: id }
        {
          creator: tx-sender,
          beneficiary: beneficiary,
          amount: amount,
          unlock-height: unlock-height,
          penalty-rate: penalty-rate,
          withdrawn: false
        }
      )

      (ok id)
    )
  )
)

(define-public (admin-release (id uint))
  (match (map-get? locks { id: id })
    l
    (begin
      (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-ADMIN)
      (asserts! (not (get withdrawn l)) ERR-NOTHING-TO-WITHDRAW)

      (try!
        (stx-transfer?
          (get amount l)
          (as-contract tx-sender)
          (get beneficiary l)
        )
      )

      (map-set locks
        { id: id }
        {
          creator: (get creator l),
          beneficiary: (get beneficiary l),
          amount: (get amount l),
          unlock-height: (get unlock-height l),
          penalty-rate: (get penalty-rate l),
          withdrawn: true
        }
      )

      (ok true)
    )
    ERR-LOCK-NOT-FOUND
  )
)

;; ------------------------------------------------------------
;; Public Functions - Beneficiary Actions replaced em-dash with hyphen
;; ------------------------------------------------------------

(define-public (withdraw (id uint))
  (match (map-get? locks { id: id })
    l
    (begin
      (asserts! (is-eq tx-sender (get beneficiary l)) ERR-NOT-BENEFICIARY)
      (asserts! (not (get withdrawn l)) ERR-NOTHING-TO-WITHDRAW)

      (let (
        (penalty
          (if (is-unlocked id)
            u0
            (penalty-amount id)
          )
        )
        (payout (- (get amount l) penalty))
      )
        (try!
          (stx-transfer?
            payout
            (as-contract tx-sender)
            tx-sender
          )
        )

        ;; Penalty retained by admin
        (if (> penalty u0)
          (try!
            (stx-transfer?
              penalty
              (as-contract tx-sender)
              (var-get admin)
            )
          )
          true
        )

        (map-set locks
          { id: id }
          {
            creator: (get creator l),
            beneficiary: (get beneficiary l),
            amount: (get amount l),
            unlock-height: (get unlock-height l),
            penalty-rate: (get penalty-rate l),
            withdrawn: true
          }
        )

        (ok payout)
      )
    )
    ERR-LOCK-NOT-FOUND
  )
)
