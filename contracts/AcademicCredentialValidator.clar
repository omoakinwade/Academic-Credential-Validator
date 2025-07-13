;; AcademicCredentialValidator - Educational achievement verification system

(define-map academic-credentials uint {
  student: principal,
  degree-title: (string-utf8 64),
  curriculum-details: (string-utf8 256),
  graduation-date: uint,
  institution-name: (string-utf8 64),
  accreditation-verified: bool
})

(define-map student-credentials principal (list 100 uint))
(define-map accreditation-bodies principal bool)
(define-data-var credential-id-sequence uint u0)

;; Error definitions
(define-constant err-unauthorized-student (err u500))
(define-constant err-unauthorized-accreditor (err u501))
(define-constant err-credential-not-found (err u502))
(define-constant err-access-restricted (err u403))
(define-constant err-credential-limit-exceeded (err u504))
(define-constant err-invalid-principal-address (err u505))
(define-constant err-empty-degree-title (err u506))
(define-constant err-empty-curriculum-details (err u507))
(define-constant err-invalid-graduation-date (err u508))
(define-constant err-empty-institution-name (err u509))
(define-constant err-invalid-credential-id (err u510))

;; Academic registry administrator
(define-constant academic-admin tx-sender)

;; Register accreditation body
(define-public (register-accreditation-body (accreditor principal))
  (begin
    (asserts! (is-eq tx-sender academic-admin) err-access-restricted)
    (asserts! (not (is-eq accreditor 'SP000000000000000000002Q6VF78)) err-invalid-principal-address)
    (ok (map-set accreditation-bodies accreditor true))
  ))

;; Submit academic credential
(define-public (submit-academic-credential
  (degree-title (string-utf8 64))
  (curriculum-details (string-utf8 256))
  (graduation-date uint)
  (institution-name (string-utf8 64)))
  (let
    ((credential-id (var-get credential-id-sequence))
     (student tx-sender)
     (existing-credentials (default-to (list) (map-get? student-credentials student))))
    
    (asserts! (> (len degree-title) u0) err-empty-degree-title)
    (asserts! (> (len curriculum-details) u0) err-empty-curriculum-details)
    (asserts! (> graduation-date u0) err-invalid-graduation-date)
    (asserts! (> (len institution-name) u0) err-empty-institution-name)
    (asserts! (< (len existing-credentials) u100) err-credential-limit-exceeded)
    
    (map-set academic-credentials credential-id {
      student: student,
      degree-title: degree-title,
      curriculum-details: curriculum-details,
      graduation-date: graduation-date,
      institution-name: institution-name,
      accreditation-verified: false
    })
    
    (let
      ((updated-credentials (unwrap-panic (as-max-len? (concat (list credential-id) existing-credentials) u100))))
      (map-set student-credentials student updated-credentials)
    )
    
    (var-set credential-id-sequence (+ credential-id u1))
    (ok credential-id)))

;; Verify accreditation
(define-public (verify-accreditation (credential-id uint))
  (begin
    (asserts! (< credential-id (var-get credential-id-sequence)) err-invalid-credential-id)
    (let
      ((credential (unwrap! (map-get? academic-credentials credential-id) err-credential-not-found)))
      (asserts! (default-to false (map-get? accreditation-bodies tx-sender)) err-unauthorized-accreditor)
      (ok (map-set academic-credentials credential-id (merge credential {accreditation-verified: true})))
    )
  ))

;; Get credential details
(define-read-only (get-credential-details (credential-id uint))
  (map-get? academic-credentials credential-id))

;; Get student credentials
(define-read-only (get-student-credentials (student principal))
  (default-to (list) (map-get? student-credentials student)))

;; Check accreditation body status
(define-read-only (is-accreditation-body (address principal))
  (default-to false (map-get? accreditation-bodies address)))
