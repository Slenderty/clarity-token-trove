;; Token Trove Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-exists (err u101))
(define-constant err-not-found (err u102))
(define-constant err-already-voted (err u103))

;; Data structures
(define-map projects
  { project-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    description: (string-utf8 500),
    website: (string-ascii 100),
    verified: bool,
    votes: uint,
    created-at: uint
  })

(define-map votes
  { project-id: uint, voter: principal }
  { voted: bool })

(define-data-var next-project-id uint u1)

;; Public functions
(define-public (list-project 
  (name (string-ascii 50))
  (description (string-utf8 500))
  (website (string-ascii 100)))
  (let ((project-id (var-get next-project-id)))
    (map-insert projects
      { project-id: project-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        website: website,
        verified: false,
        votes: u0,
        created-at: block-height
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)))

(define-public (vote (project-id uint))
  (let ((vote-key { project-id: project-id, voter: tx-sender }))
    (asserts! (is-none (map-get? votes vote-key))
      err-already-voted)
    (map-set votes vote-key { voted: true })
    (map-set projects
      { project-id: project-id }
      (merge (unwrap! (map-get? projects { project-id: project-id }) err-not-found)
        { votes: (+ (get votes (unwrap! (map-get? projects { project-id: project-id }) err-not-found)) u1) }
      ))
    (ok true)))

;; Read only functions
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id }))

(define-read-only (has-voted (project-id uint) (voter principal))
  (is-some (map-get? votes { project-id: project-id, voter: voter })))
