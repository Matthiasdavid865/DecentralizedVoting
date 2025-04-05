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



(define-map weighted-votes 
    { proposal-id: uint, voter: principal }
    { weight: uint }
)

(define-public (weighted-vote (proposal-id uint) (vote-bool bool))
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
        (map-set weighted-votes voter-key { weight: voter-balance })
        
        (if vote-bool
            (map-set proposals proposal-id 
                (merge proposal { yes-votes: (+ (get yes-votes proposal) voter-balance) }))
            (map-set proposals proposal-id 
                (merge proposal { no-votes: (+ (get no-votes proposal) voter-balance) }))
        )
        (ok true)
    )
)


(define-map proposal-categories
    uint
    (string-ascii 20)
)

(define-public (create-categorized-proposal (title (string-ascii 50)) (description (string-ascii 500)) (blocks uint) (category (string-ascii 20)))
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
        (map-set proposal-categories new-id category)
        (var-set total-proposals new-id)
        (ok new-id)
    )
)

(define-read-only (get-proposal-category (proposal-id uint))
    (map-get? proposal-categories proposal-id)
)


(define-map delegations
    principal
    principal
)

(define-constant ERR-INVALID-DELEGATE (err u106))
(define-constant ERR-ALREADY-DELEGATED (err u107))

(define-public (delegate-vote (delegate-to principal))
    (begin
        (asserts! (not (is-eq tx-sender delegate-to)) ERR-INVALID-DELEGATE)
        (asserts! (is-none (map-get? delegations tx-sender)) ERR-ALREADY-DELEGATED)
        (map-set delegations tx-sender delegate-to)
        (ok true)
    )
)

(define-public (remove-delegation)
    (begin
        (map-delete delegations tx-sender)
        (ok true)
    )
)


(define-constant ERR-INVALID-EXTENSION (err u108))

(define-public (extend-voting-period (proposal-id uint) (additional-blocks uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-VOTING-ENDED)
        (asserts! (> additional-blocks u0) ERR-INVALID-EXTENSION)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    end-block: (+ (get end-block proposal) additional-blocks)
                }
            )
        )
        (ok true)
    )
)


(define-data-var quorum-requirement uint u100)

(define-public (set-quorum-requirement (new-quorum uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set quorum-requirement new-quorum)
        (ok true)
    )
)

(define-public (finalize-proposal-with-quorum (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        )
        (asserts! (>= stacks-block-height (get end-block proposal)) ERR-VOTING-NOT-ENDED)
        (asserts! (is-eq (get status proposal) "active") ERR-VOTING-ENDED)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    status: (if (and
                        (>= total-votes (var-get quorum-requirement))
                        (> (get yes-votes proposal) (get no-votes proposal)))
                        "passed"
                        "rejected"
                    )
                }
            )
        )
        (ok true)
    )
)


(define-data-var contract-paused bool false)
(define-constant ERR-CONTRACT-PAUSED (err u109))

(define-public (toggle-contract-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set contract-paused (not (var-get contract-paused)))
        (ok true)
    )
)

(define-public (vote-with-pause-check (proposal-id uint) (vote-bool bool))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
        (vote proposal-id vote-bool)
    )
)


(define-map proposal-comments
    { proposal-id: uint, commenter: principal }
    { comment: (string-ascii 200), timestamp: uint }
)

(define-public (add-comment (proposal-id uint) (comment (string-ascii 200)))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (voter-balance (default-to u0 (map-get? eligible-voters tx-sender)))
        )
        (asserts! (>= voter-balance (var-get min-tokens)) ERR-NOT-ELIGIBLE)
        (map-set proposal-comments
            { proposal-id: proposal-id, commenter: tx-sender }
            { comment: comment, timestamp: stacks-block-height }
        )
        (ok true)
    )
)

(define-read-only (get-comment (proposal-id uint) (commenter principal))
    (map-get? proposal-comments { proposal-id: proposal-id, commenter: commenter })
)


