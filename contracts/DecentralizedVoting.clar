;; DecentralizedVoting - A secure voting platform for organizations

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-INVALID-PROPOSAL (err u102))
(define-constant ERR-VOTING-ENDED (err u103))
(define-constant ERR-VOTING-NOT-ENDED (err u104))
(define-constant ERR-NOT-ELIGIBLE (err u105))

;; Data Variables
(define-data-var total-proposals uint u0)
(define-data-var min-tokens uint u100) ;; Minimum tokens required to vote

;; Data Maps
(define-map proposals 
    uint 
    {
        title: (string-ascii 50),
        description: (string-ascii 500),
        creator: principal,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 10)
    }
)

(define-map voter-registry
    { proposal-id: uint, voter: principal }
    { voted: bool }
)

(define-map eligible-voters
    principal
    uint  ;; token balance
)

;; Public Functions

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 500)) (blocks uint))
    (let
        (
            (new-id (+ (var-get total-proposals) u1))
            (end-block (+ stacks-block-height blocks))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set proposals new-id
            {
                title: title,
                description: description,
                creator: tx-sender,
                end-block: end-block,
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            }
        )
        (var-set total-proposals new-id)
        (ok new-id)
    )
)

;; Register eligible voter with token balance
(define-public (register-voter (voter principal) (token-balance uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set eligible-voters voter token-balance)
        (ok true)
    )
)

;; Cast a vote
(define-public (vote (proposal-id uint) (vote-bool bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (voter-key { proposal-id: proposal-id, voter: tx-sender })
            (voter-balance (default-to u0 (map-get? eligible-voters tx-sender)))
        )
        (asserts! (>= voter-balance (var-get min-tokens)) ERR-NOT-ELIGIBLE)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-VOTING-ENDED)
        (asserts! (is-none (map-get? voter-registry voter-key)) ERR-ALREADY-VOTED)
        
        (map-set voter-registry voter-key { voted: true })
        
        (if vote-bool
            (map-set proposals proposal-id 
                (merge proposal { yes-votes: (+ (get yes-votes proposal) u1) }))
            (map-set proposals proposal-id 
                (merge proposal { no-votes: (+ (get no-votes proposal) u1) }))
        )
        (ok true)
    )
)

;; Finalize proposal
(define-public (finalize-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
        )
        (asserts! (>= stacks-block-height (get end-block proposal)) ERR-VOTING-NOT-ENDED)
        (asserts! (is-eq (get status proposal) "active") ERR-VOTING-ENDED)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    status: (if (> (get yes-votes proposal) (get no-votes proposal))
                        "passed"
                        "rejected"
                    )
                }
            )
        )
        (ok true)
    )
)

;; Read-only Functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

;; Check if address has voted
(define-read-only (has-voted (proposal-id uint) (voter principal))
    (default-to 
        false
        (get voted (map-get? voter-registry { proposal-id: proposal-id, voter: voter }))
    )
)

;; Get voter eligibility and balance
(define-read-only (get-voter-info (voter principal))
    (map-get? eligible-voters voter)
)

;; Get total proposals
(define-read-only (get-total-proposals)
    (var-get total-proposals)
)
