;; companion-registry.clar
;; Core companion management for doctor visit support

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-COMPANION-EXISTS (err u101))
(define-constant ERR-COMPANION-NOT-FOUND (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-PATIENT-NOT-FOUND (err u104))

(define-data-var contract-owner principal tx-sender)

(define-map companions
  { companion-id: uint }
  {
    companion: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    availability: bool,
    rating: uint,
    completed-visits: uint,
    created-at: uint
  })

(define-map patients
  { patient-id: uint }
  {
    patient: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    medical-needs: (string-ascii 200),
    emergency-contact: (string-ascii 100),
    created-at: uint
  })

(define-map visit-requests
  { request-id: uint }
  {
    patient-id: uint,
    companion-id: (optional uint),
    appointment-date: uint,
    doctor-location: (string-ascii 100),
    special-requirements: (string-ascii 200),
    status: (string-ascii 20),
    created-at: uint,
    updated-at: uint
  })

(define-data-var next-companion-id uint u1)
(define-data-var next-patient-id uint u1)
(define-data-var next-request-id uint u1)

(define-public (register-companion (name (string-ascii 50)) (location (string-ascii 100)))
  (let ((companion-id (var-get next-companion-id)))
    (map-set companions
      { companion-id: companion-id }
      {
        companion: tx-sender,
        name: name,
        location: location,
        availability: true,
        rating: u50,
        completed-visits: u0,
        created-at: stacks-block-height
      })
    (var-set next-companion-id (+ companion-id u1))
    (ok companion-id)))

(define-public (register-patient (name (string-ascii 50)) (location (string-ascii 100))
                                (medical-needs (string-ascii 200)) (emergency-contact (string-ascii 100)))
  (let ((patient-id (var-get next-patient-id)))
    (map-set patients
      { patient-id: patient-id }
      {
        patient: tx-sender,
        name: name,
        location: location,
        medical-needs: medical-needs,
        emergency-contact: emergency-contact,
        created-at: stacks-block-height
      })
    (var-set next-patient-id (+ patient-id u1))
    (ok patient-id)))

(define-public (create-visit-request (patient-id uint) (appointment-date uint)
                                   (doctor-location (string-ascii 100)) (special-requirements (string-ascii 200)))
  (let ((request-id (var-get next-request-id))
        (patient-data (map-get? patients { patient-id: patient-id })))
    (asserts! (is-some patient-data) ERR-PATIENT-NOT-FOUND)
    (asserts! (is-eq tx-sender (get patient (unwrap-panic patient-data))) ERR-NOT-AUTHORIZED)

    (map-set visit-requests
      { request-id: request-id }
      {
        patient-id: patient-id,
        companion-id: none,
        appointment-date: appointment-date,
        doctor-location: doctor-location,
        special-requirements: special-requirements,
        status: "pending",
        created-at: stacks-block-height,
        updated-at: stacks-block-height
      })
    (var-set next-request-id (+ request-id u1))
    (ok request-id)))

(define-public (accept-visit-request (request-id uint) (companion-id uint))
  (let ((request-data (map-get? visit-requests { request-id: request-id }))
        (companion-data (map-get? companions { companion-id: companion-id })))
    (asserts! (is-some request-data) ERR-COMPANION-NOT-FOUND)
    (asserts! (is-some companion-data) ERR-COMPANION-NOT-FOUND)
    (asserts! (is-eq tx-sender (get companion (unwrap-panic companion-data))) ERR-NOT-AUTHORIZED)
    (asserts! (get availability (unwrap-panic companion-data)) ERR-INVALID-STATUS)

    (map-set visit-requests
      { request-id: request-id }
      (merge (unwrap-panic request-data)
             { companion-id: (some companion-id),
               status: "accepted",
               updated-at: stacks-block-height }))
    (ok true)))

(define-public (complete-visit (request-id uint) (rating uint))
  (let ((request-data (map-get? visit-requests { request-id: request-id }))
        (companion-id (get companion-id (unwrap-panic (map-get? visit-requests { request-id: request-id }))))
        (companion-data (map-get? companions { companion-id: (unwrap-panic companion-id) })))
    (asserts! (is-some request-data) ERR-COMPANION-NOT-FOUND)
    (asserts! (is-some companion-id) ERR-COMPANION-NOT-FOUND)
    (asserts! (<= rating u100) ERR-INVALID-STATUS)

    (map-set visit-requests
      { request-id: request-id }
      (merge (unwrap-panic request-data)
             { status: "completed", updated-at: stacks-block-height }))

    (map-set companions
      { companion-id: (unwrap-panic companion-id) }
      (merge (unwrap-panic companion-data)
             { completed-visits: (+ (get completed-visits (unwrap-panic companion-data)) u1),
               rating: (/ (+ (* (get rating (unwrap-panic companion-data)) (get completed-visits (unwrap-panic companion-data))) rating)
                         (+ (get completed-visits (unwrap-panic companion-data)) u1)) }))
    (ok true)))

(define-read-only (get-companion (companion-id uint))
  (map-get? companions { companion-id: companion-id }))

(define-read-only (get-patient (patient-id uint))
  (map-get? patients { patient-id: patient-id }))

(define-read-only (get-visit-request (request-id uint))
  (map-get? visit-requests { request-id: request-id }))

(define-read-only (get-available-companions)
  (ok "Use off-chain indexing for companion filtering"))

;; visit-coordinator.clar
;; Transportation and visit coordination support

(define-constant ERR-REQUEST-NOT-FOUND (err u200))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u201))
(define-constant ERR-INVALID-TRANSPORT-TYPE (err u202))

(define-map transportation-requests
  { transport-id: uint }
  {
    visit-request-id: uint,
    transport-type: (string-ascii 20),
    pickup-location: (string-ascii 100),
    pickup-time: uint,
    estimated-duration: uint,
    special-needs: (string-ascii 200),
    status: (string-ascii 20),
    created-at: uint
  })

(define-map medical-notes
  { note-id: uint }
  {
    visit-request-id: uint,
    author: principal,
    note-type: (string-ascii 30),
    content: (string-ascii 500),
    is-shared: bool,
    created-at: uint
  })

(define-data-var next-transport-id uint u1)
(define-data-var next-note-id uint u1)

(define-public (schedule-transportation (visit-request-id uint) (transport-type (string-ascii 20))
                                      (pickup-location (string-ascii 100)) (pickup-time uint)
                                      (estimated-duration uint) (special-needs (string-ascii 200)))
  (let ((transport-id (var-get next-transport-id)))
    (asserts! (or (is-eq transport-type "taxi")
                  (is-eq transport-type "medical-transport")
                  (is-eq transport-type "companion-drive")) ERR-INVALID-TRANSPORT-TYPE)

    (map-set transportation-requests
      { transport-id: transport-id }
      {
        visit-request-id: visit-request-id,
        transport-type: transport-type,
        pickup-location: pickup-location,
        pickup-time: pickup-time,
        estimated-duration: estimated-duration,
        special-needs: special-needs,
        status: "scheduled",
        created-at: stacks-block-height
      })
    (var-set next-transport-id (+ transport-id u1))
    (ok transport-id)))

(define-public (add-medical-note (visit-request-id uint) (note-type (string-ascii 30))
                                (content (string-ascii 500)) (is-shared bool))
  (let ((note-id (var-get next-note-id)))
    (map-set medical-notes
      { note-id: note-id }
      {
        visit-request-id: visit-request-id,
        author: tx-sender,
        note-type: note-type,
        content: content,
        is-shared: is-shared,
        created-at: stacks-block-height
      })
    (var-set next-note-id (+ note-id u1))
    (ok note-id)))

(define-public (update-transport-status (transport-id uint) (status (string-ascii 20)))
  (let ((transport-data (map-get? transportation-requests { transport-id: transport-id })))
    (asserts! (is-some transport-data) ERR-REQUEST-NOT-FOUND)

    (map-set transportation-requests
      { transport-id: transport-id }
      (merge (unwrap-panic transport-data) { status: status }))
    (ok true)))

(define-read-only (get-transportation-request (transport-id uint))
  (map-get? transportation-requests { transport-id: transport-id }))

(define-read-only (get-medical-note (note-id uint))
  (map-get? medical-notes { note-id: note-id }))

(define-read-only (get-shared-notes-for-visit (visit-request-id uint))
  (ok "Use off-chain indexing for note filtering"))
