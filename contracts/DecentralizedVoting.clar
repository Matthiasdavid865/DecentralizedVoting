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

(define-constant ERR-EXECUTION-FAILED (err u112))
(define-constant ERR-EXECUTION-UNAUTHORIZED (err u113))
(define-constant ERR-EXECUTION-ALREADY-DONE (err u114))
(define-constant ERR-INVALID-EXECUTION-TARGET (err u115))
(define-constant ERR-EXECUTION-WINDOW-EXPIRED (err u116))

(define-map execution-targets
    (string-ascii 30)
    {
        contract-address: principal,
        function-name: (string-ascii 30),
        authorized: bool
    }
)

(define-map executable-proposals
    uint
    {
        execution-target: (string-ascii 30),
        execution-parameters: (list 5 uint),
        execution-window: uint,
        auto-execute: bool
    }
)

(define-map execution-history
    uint
    {
        executed: bool,
        execution-block: uint,
        execution-result: (string-ascii 20),
        executor: principal
    }
)

(define-public (register-execution-target 
    (target-name (string-ascii 30)) 
    (contract-address principal) 
    (function-name (string-ascii 30)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set execution-targets target-name
            {
                contract-address: contract-address,
                function-name: function-name,
                authorized: true
            }
        )
        (ok true)
    )
)

(define-public (create-executable-proposal 
    (title (string-ascii 50)) 
    (description (string-ascii 500)) 
    (blocks uint)
    (execution-target (string-ascii 30))
    (execution-parameters (list 5 uint))
    (execution-window uint)
    (auto-execute bool))
    (let
        (
            (new-id (+ (var-get total-proposals) u1))
            (end-block (+ stacks-block-height blocks))
            (target-info (map-get? execution-targets execution-target))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-some target-info) ERR-INVALID-EXECUTION-TARGET)
        (asserts! (get authorized (unwrap-panic target-info)) ERR-EXECUTION-UNAUTHORIZED)
        
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
        
        (map-set executable-proposals new-id
            {
                execution-target: execution-target,
                execution-parameters: execution-parameters,
                execution-window: execution-window,
                auto-execute: auto-execute
            }
        )
        
        (var-set total-proposals new-id)
        (ok new-id)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (executable-info (unwrap! (map-get? executable-proposals proposal-id) ERR-INVALID-EXECUTION-TARGET))
            (execution-record (map-get? execution-history proposal-id))
            (target-info (unwrap! (map-get? execution-targets (get execution-target executable-info)) ERR-INVALID-EXECUTION-TARGET))
            (execution-deadline (+ (get end-block proposal) (get execution-window executable-info)))
        )
        (asserts! (is-eq (get status proposal) "passed") ERR-EXECUTION-UNAUTHORIZED)
        (asserts! (< stacks-block-height execution-deadline) ERR-EXECUTION-WINDOW-EXPIRED)
        (asserts! (is-none execution-record) ERR-EXECUTION-ALREADY-DONE)
        (asserts! (get authorized target-info) ERR-EXECUTION-UNAUTHORIZED)
        
        (map-set execution-history proposal-id
            {
                executed: true,
                execution-block: stacks-block-height,
                execution-result: "success",
                executor: tx-sender
            }
        )
        
        (ok true)
    )
)

(define-public (schedule-execution (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (executable-info (unwrap! (map-get? executable-proposals proposal-id) ERR-INVALID-EXECUTION-TARGET))
        )
        (asserts! (is-eq (get status proposal) "passed") ERR-EXECUTION-UNAUTHORIZED)
        (asserts! (get auto-execute executable-info) ERR-EXECUTION-UNAUTHORIZED)
        
        (execute-proposal proposal-id)
    )
)

(define-public (revoke-execution-target (target-name (string-ascii 30)))
    (let
        (
            (target-info (unwrap! (map-get? execution-targets target-name) ERR-INVALID-EXECUTION-TARGET))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set execution-targets target-name
            (merge target-info { authorized: false })
        )
        (ok true)
    )
)

(define-read-only (get-execution-target (target-name (string-ascii 30)))
    (map-get? execution-targets target-name)
)

(define-read-only (get-executable-proposal-info (proposal-id uint))
    (map-get? executable-proposals proposal-id)
)

(define-read-only (get-execution-history (proposal-id uint))
    (map-get? execution-history proposal-id)
)

(define-read-only (is-execution-window-active (proposal-id uint))
    (let
        (
            (proposal (map-get? proposals proposal-id))
            (executable-info (map-get? executable-proposals proposal-id))
        )
        (match proposal
            prop-data
                (match executable-info
                    exec-data
                        (let
                            (
                                (execution-deadline (+ (get end-block prop-data) (get execution-window exec-data)))
                            )
                            (and
                                (is-eq (get status prop-data) "passed")
                                (< stacks-block-height execution-deadline)
                                (is-none (map-get? execution-history proposal-id))
                            )
                        )
                    false
                )
            false
        )
    )
)

(define-read-only (get-pending-executions)
    (let
        (
            (total-props (var-get total-proposals))
        )
        (filter is-execution-pending (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
    )
)

(define-read-only (is-execution-pending (proposal-id uint))
    (is-execution-window-active proposal-id)
)

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





(define-map token-lock-time
    principal
    uint
)

(define-constant WEIGHT-MULTIPLIER u2)
(define-constant MAX-WEIGHT-MULTIPLIER u5)
(define-constant BLOCKS-PER-MULTIPLIER u1000)

(define-public (lock-tokens (lock-period uint))
    (begin
        (asserts! (>= (default-to u0 (map-get? eligible-voters tx-sender)) (var-get min-tokens)) ERR-NOT-ELIGIBLE)
        (map-set token-lock-time tx-sender (+ stacks-block-height lock-period))
        (ok true)
    )
)

(define-public (time-weighted-vote (proposal-id uint) (vote-bool bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (voter-key { proposal-id: proposal-id, voter: tx-sender })
            (voter-balance (default-to u0 (map-get? eligible-voters tx-sender)))
            (lock-time (default-to u0 (map-get? token-lock-time tx-sender)))
            (blocks-locked (if (> lock-time stacks-block-height) 
                (- lock-time stacks-block-height) 
                u0))
            (weight-multiplier (if (> (+ WEIGHT-MULTIPLIER (/ blocks-locked BLOCKS-PER-MULTIPLIER)) MAX-WEIGHT-MULTIPLIER)
                MAX-WEIGHT-MULTIPLIER
                (+ WEIGHT-MULTIPLIER (/ blocks-locked BLOCKS-PER-MULTIPLIER))))
            (weighted-balance (* voter-balance weight-multiplier))
        )
        (asserts! (>= voter-balance (var-get min-tokens)) ERR-NOT-ELIGIBLE)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-VOTING-ENDED)
        (asserts! (is-none (map-get? voter-registry voter-key)) ERR-ALREADY-VOTED)
        
        (map-set voter-registry voter-key { voted: true })
        
        (if vote-bool
            (map-set proposals proposal-id 
                (merge proposal { yes-votes: (+ (get yes-votes proposal) weighted-balance) }))
            (map-set proposals proposal-id 
                (merge proposal { no-votes: (+ (get no-votes proposal) weighted-balance) }))
        )
        (ok true)
    )
)


(define-map proposal-tags
    { proposal-id: uint, tag: (string-ascii 20) }
    bool
)

(define-map tag-registry
    (string-ascii 20)
    bool
)

(define-public (register-tag (tag (string-ascii 20)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set tag-registry tag true)
        (ok true)
    )
)

(define-public (add-proposal-tag (proposal-id uint) (tag (string-ascii 20)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? proposals proposal-id)) ERR-INVALID-PROPOSAL)
        (asserts! (default-to false (map-get? tag-registry tag)) ERR-NOT-AUTHORIZED)
        (map-set proposal-tags { proposal-id: proposal-id, tag: tag } true)
        (ok true)
    )
)

(define-read-only (has-tag (proposal-id uint) (tag (string-ascii 20)))
    (default-to false (map-get? proposal-tags { proposal-id: proposal-id, tag: tag }))
)


(define-constant ERR-INVALID-STAGE (err u110))
(define-constant ERR-STAGE-NOT-READY (err u111))

(define-map proposal-stages
    uint
    {
        current-stage: uint,
        stage-end-block: uint,
        total-stages: uint,
        stage-names: (list 5 (string-ascii 20))
    }
)

(define-map stage-votes
    { proposal-id: uint, stage: uint, voter: principal }
    { vote: bool, weight: uint }
)

(define-map stage-results
    { proposal-id: uint, stage: uint }
    { yes-votes: uint, no-votes: uint, status: (string-ascii 10) }
)

(define-public (create-multi-stage-proposal 
    (title (string-ascii 50)) 
    (description (string-ascii 500)) 
    (stage-durations (list 5 uint))
    (stage-names (list 5 (string-ascii 20))))
    (let
        (
            (new-id (+ (var-get total-proposals) u1))
            (first-stage-duration (unwrap! (element-at stage-durations u0) ERR-INVALID-PROPOSAL))
            (end-block (+ stacks-block-height first-stage-duration))
            (total-stages (len stage-durations))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> total-stages u0) ERR-INVALID-PROPOSAL)
        (asserts! (is-eq (len stage-names) total-stages) ERR-INVALID-PROPOSAL)
        

        
        (map-set proposal-stages new-id
            {
                current-stage: u0,
                stage-end-block: end-block,
                total-stages: total-stages,
                stage-names: stage-names
            }
        )
        
        (map-set stage-results 
            { proposal-id: new-id, stage: u0 }
            { yes-votes: u0, no-votes: u0, status: "active" }
        )
        
        (var-set total-proposals new-id)
        (ok new-id)
    )
)

(define-public (vote-in-stage (proposal-id uint) (vote-bool bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (stage-info (unwrap! (map-get? proposal-stages proposal-id) ERR-INVALID-STAGE))
            (current-stage (get current-stage stage-info))
            (voter-balance (default-to u0 (map-get? eligible-voters tx-sender)))
            (stage-voter-key { proposal-id: proposal-id, stage: current-stage, voter: tx-sender })
            (stage-result-key { proposal-id: proposal-id, stage: current-stage })
            (current-results (unwrap! (map-get? stage-results stage-result-key) ERR-INVALID-STAGE))
        )
        (asserts! (>= voter-balance (var-get min-tokens)) ERR-NOT-ELIGIBLE)
        (asserts! (< stacks-block-height (get stage-end-block stage-info)) ERR-VOTING-ENDED)
        (asserts! (is-eq (get status proposal) "multi-stage") ERR-INVALID-STAGE)
        (asserts! (is-none (map-get? stage-votes stage-voter-key)) ERR-ALREADY-VOTED)
        
        (map-set stage-votes stage-voter-key 
            { vote: vote-bool, weight: voter-balance }
        )
        
        (if vote-bool
            (map-set stage-results stage-result-key
                (merge current-results 
                    { yes-votes: (+ (get yes-votes current-results) voter-balance) }
                )
            )
            (map-set stage-results stage-result-key
                (merge current-results 
                    { no-votes: (+ (get no-votes current-results) voter-balance) }
                )
            )
        )
        (ok true)
    )
)

(define-public (advance-to-next-stage (proposal-id uint) (next-stage-duration uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (stage-info (unwrap! (map-get? proposal-stages proposal-id) ERR-INVALID-STAGE))
            (current-stage (get current-stage stage-info))
            (next-stage (+ current-stage u1))
            (stage-result-key { proposal-id: proposal-id, stage: current-stage })
            (current-results (unwrap! (map-get? stage-results stage-result-key) ERR-INVALID-STAGE))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (>= stacks-block-height (get stage-end-block stage-info)) ERR-STAGE-NOT-READY)
        (asserts! (< next-stage (get total-stages stage-info)) ERR-INVALID-STAGE)
        
        (map-set stage-results stage-result-key
            (merge current-results { status: "completed" })
        )
        
        (map-set proposal-stages proposal-id
            (merge stage-info 
                {
                    current-stage: next-stage,
                    stage-end-block: (+ stacks-block-height next-stage-duration)
                }
            )
        )
        
        (map-set stage-results 
            { proposal-id: proposal-id, stage: next-stage }
            { yes-votes: u0, no-votes: u0, status: "active" }
        )
        
        (ok true)
    )
)

(define-public (finalize-multi-stage-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-INVALID-PROPOSAL))
            (stage-info (unwrap! (map-get? proposal-stages proposal-id) ERR-INVALID-STAGE))
            (current-stage (get current-stage stage-info))
            (final-stage (- (get total-stages stage-info) u1))
            (stage-result-key { proposal-id: proposal-id, stage: current-stage })
            (final-results (unwrap! (map-get? stage-results stage-result-key) ERR-INVALID-STAGE))
        )
        (asserts! (>= stacks-block-height (get stage-end-block stage-info)) ERR-VOTING-NOT-ENDED)
        (asserts! (is-eq current-stage final-stage) ERR-INVALID-STAGE)
        (asserts! (is-eq (get status proposal) "multi-stage") ERR-VOTING-ENDED)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    status: (if (> (get yes-votes final-results) (get no-votes final-results))
                        "passed"
                        "rejected"
                    ),
                    yes-votes: (get yes-votes final-results),
                    no-votes: (get no-votes final-results)
                }
            )
        )
        
        (map-set stage-results stage-result-key
            (merge final-results { status: "finalized" })
        )
        
        (ok true)
    )
)

(define-read-only (get-proposal-stage-info (proposal-id uint))
    (map-get? proposal-stages proposal-id)
)

(define-read-only (get-stage-results (proposal-id uint) (stage uint))
    (map-get? stage-results { proposal-id: proposal-id, stage: stage })
)

(define-read-only (get-stage-vote (proposal-id uint) (stage uint) (voter principal))
    (map-get? stage-votes { proposal-id: proposal-id, stage: stage, voter: voter })
)

(define-read-only (has-voted-in-current-stage (proposal-id uint) (voter principal))
    (let
        (
            (stage-info (map-get? proposal-stages proposal-id))
        )
        (match stage-info
            stage-data 
                (is-some (map-get? stage-votes 
                    { 
                        proposal-id: proposal-id, 
                        stage: (get current-stage stage-data), 
                        voter: voter 
                    }
                ))
            false
        )
    )
)