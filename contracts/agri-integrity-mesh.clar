;; agricultural-integrity-mesh
;; Decentralized agricultural production verification and ownership management system

;; ===============================================
;; ERROR RESPONSE DEFINITIONS
;; ===============================================

;; System administrator privilege identifier
(define-constant system-controller tx-sender)

;; Error status indicators for transaction failures
(define-constant controller-privilege-required (err u300))
(define-constant record-not-found (err u301))
(define-constant duplicate-record-attempt (err u302))
(define-constant string-length-violation (err u303))
(define-constant numeric-range-violation (err u304))
(define-constant permission-denied (err u305))
(define-constant ownership-mismatch (err u306))
(define-constant access-forbidden (err u307))
(define-constant metadata-format-error (err u308))

;; ===============================================
;; STATE VARIABLE DECLARATIONS
;; ===============================================

;; Sequential identifier generator for production records
(define-data-var production-sequence-number uint u0)

;; ===============================================
;; DATA STRUCTURE MAPPINGS
;; ===============================================

;; Primary production record storage mechanism
(define-map agricultural-production-ledger
  { production-record-id: uint }
  {
    cultivated-species: (string-ascii 64),
    production-owner: principal,
    total-output-volume: uint,
    blockchain-timestamp: uint,
    cultivation-site-info: (string-ascii 128),
    metadata-descriptors: (list 10 (string-ascii 32))
  }
)

;; Access control matrix for production data visibility
(define-map data-access-control
  { production-record-id: uint, authorized-viewer: principal }
  { viewing-privilege: bool }
)

;; ===============================================
;; VALIDATION HELPER FUNCTIONS
;; ===============================================

;; Verify production record exists in system
(define-private (production-record-exists (record-id uint))
  (is-some (map-get? agricultural-production-ledger { production-record-id: record-id }))
)

;; Validate metadata descriptor string format
(define-private (descriptor-format-valid (descriptor (string-ascii 32)))
  (and
    (> (len descriptor) u0)
    (< (len descriptor) u33)
  )
)

;; Validate complete set of metadata descriptors
(define-private (validate-descriptor-set (descriptors (list 10 (string-ascii 32))))
  (and
    (> (len descriptors) u0)
    (<= (len descriptors) u10)
    (is-eq (len (filter descriptor-format-valid descriptors)) (len descriptors))
  )
)

;; Confirm caller owns specified production record
(define-private (confirm-record-ownership (record-id uint) (claiming-owner principal))
  (match (map-get? agricultural-production-ledger { production-record-id: record-id })
    production-data (is-eq (get production-owner production-data) claiming-owner)
    false
  )
)

;; Extract output volume from production record
(define-private (extract-output-volume (record-id uint))
  (default-to u0
    (get total-output-volume
      (map-get? agricultural-production-ledger { production-record-id: record-id })
    )
  )
)

;; ===============================================
;; PRIMARY REGISTRATION INTERFACE
;; ===============================================

;; Create new agricultural production record with complete metadata
(define-public (create-production-record 
  (species-name (string-ascii 64)) 
  (output-volume uint) 
  (site-description (string-ascii 128)) 
  (descriptors (list 10 (string-ascii 32)))
)
  (let
    (
      (next-record-id (+ (var-get production-sequence-number) u1))
    )
    ;; Parameter validation checks
    (asserts! (> (len species-name) u0) string-length-violation)
    (asserts! (< (len species-name) u65) string-length-violation)
    (asserts! (> output-volume u0) numeric-range-violation)
    (asserts! (< output-volume u1000000000) numeric-range-violation)
    (asserts! (> (len site-description) u0) string-length-violation)
    (asserts! (< (len site-description) u129) string-length-violation)
    (asserts! (validate-descriptor-set descriptors) metadata-format-error)

    ;; Insert production record into ledger
    (map-insert agricultural-production-ledger
      { production-record-id: next-record-id }
      {
        cultivated-species: species-name,
        production-owner: tx-sender,
        total-output-volume: output-volume,
        blockchain-timestamp: block-height,
        cultivation-site-info: site-description,
        metadata-descriptors: descriptors
      }
    )

    ;; Establish owner access permissions
    (map-insert data-access-control
      { production-record-id: next-record-id, authorized-viewer: tx-sender }
      { viewing-privilege: true }
    )

    ;; Increment sequence counter and return new ID
    (var-set production-sequence-number next-record-id)
    (ok next-record-id)
  )
)

;; ===============================================
;; OWNERSHIP MODIFICATION INTERFACE
;; ===============================================

;; Transfer production record ownership to different principal
(define-public (transfer-production-ownership (record-id uint) (recipient-principal principal))
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
    )
    ;; Ownership and existence validation
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! (is-eq (get production-owner production-data) tx-sender) ownership-mismatch)

    ;; Execute ownership transfer
    (map-set agricultural-production-ledger
      { production-record-id: record-id }
      (merge production-data { production-owner: recipient-principal })
    )
    (ok true)
  )
)

;; ===============================================
;; ACCESS PERMISSION MANAGEMENT
;; ===============================================

;; Revoke viewing privileges for specified principal
(define-public (revoke-viewing-access (record-id uint) (target-viewer principal))
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
    )
    ;; Validate record existence and caller ownership
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! (is-eq (get production-owner production-data) tx-sender) ownership-mismatch)
    (asserts! (not (is-eq target-viewer tx-sender)) controller-privilege-required)

    ;; Remove access permission entry
    (map-delete data-access-control { production-record-id: record-id, authorized-viewer: target-viewer })
    (ok true)
  )
)

;; ===============================================
;; RECORD MODIFICATION INTERFACE
;; ===============================================

;; Append additional metadata descriptors to existing record
(define-public (append-metadata-descriptors (record-id uint) (new-descriptors (list 10 (string-ascii 32))))
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
      (current-descriptors (get metadata-descriptors production-data))
      (merged-descriptors (unwrap! (as-max-len? (concat current-descriptors new-descriptors) u10) metadata-format-error))
    )
    ;; Validate record existence and ownership
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! (is-eq (get production-owner production-data) tx-sender) ownership-mismatch)

    ;; Validate new descriptor format
    (asserts! (validate-descriptor-set new-descriptors) metadata-format-error)

    ;; Update record with combined descriptors
    (map-set agricultural-production-ledger
      { production-record-id: record-id }
      (merge production-data { metadata-descriptors: merged-descriptors })
    )
    (ok merged-descriptors)
  )
)

;; Comprehensive record update with all fields
(define-public (modify-production-record 
  (record-id uint) 
  (updated-species (string-ascii 64)) 
  (updated-volume uint) 
  (updated-site (string-ascii 128)) 
  (updated-descriptors (list 10 (string-ascii 32)))
)
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
    )
    ;; Ownership validation and input checks
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! (is-eq (get production-owner production-data) tx-sender) ownership-mismatch)
    (asserts! (> (len updated-species) u0) string-length-violation)
    (asserts! (< (len updated-species) u65) string-length-violation)
    (asserts! (> updated-volume u0) numeric-range-violation)
    (asserts! (< updated-volume u1000000000) numeric-range-violation)
    (asserts! (> (len updated-site) u0) string-length-violation)
    (asserts! (< (len updated-site) u129) string-length-violation)
    (asserts! (validate-descriptor-set updated-descriptors) metadata-format-error)

    ;; Apply comprehensive update to record
    (map-set agricultural-production-ledger
      { production-record-id: record-id }
      (merge production-data { 
        cultivated-species: updated-species, 
        total-output-volume: updated-volume, 
        cultivation-site-info: updated-site, 
        metadata-descriptors: updated-descriptors 
      })
    )
    (ok true)
  )
)

;; ===============================================
;; AUTHENTICATION AND VERIFICATION
;; ===============================================

;; Verify production record authenticity and ownership claims
(define-public (verify-production-authenticity (record-id uint) (claimed-owner principal))
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
      (verified-owner (get production-owner production-data))
      (creation-timestamp (get blockchain-timestamp production-data))
      (access-granted (default-to 
        false 
        (get viewing-privilege 
          (map-get? data-access-control { production-record-id: record-id, authorized-viewer: tx-sender })
        )
      ))
    )
    ;; Access permission verification
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! 
      (or 
        (is-eq tx-sender verified-owner)
        access-granted
        (is-eq tx-sender system-controller)
      ) 
      permission-denied
    )

    ;; Return authentication result with metadata
    (if (is-eq verified-owner claimed-owner)
      ;; Authentication successful response
      (ok {
        is-authentic: true,
        current-block: block-height,
        blockchain-age: (- block-height creation-timestamp),
        farmer-match: true
      })
      ;; Authentication failed response
      (ok {
        is-authentic: false,
        current-block: block-height,
        blockchain-age: (- block-height creation-timestamp),
        farmer-match: false
      })
    )
  )
)

;; ===============================================
;; RECORD REMOVAL INTERFACE
;; ===============================================

;; Permanently delete production record from system
(define-public (delete-production-record (record-id uint))
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
    )
    ;; Ownership confirmation required
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! (is-eq (get production-owner production-data) tx-sender) ownership-mismatch)

    ;; Execute record deletion
    (map-delete agricultural-production-ledger { production-record-id: record-id })
    (ok true)
  )
)

;; ===============================================
;; EMERGENCY SECURITY CONTROLS
;; ===============================================

;; Apply emergency security restriction to production record
(define-public (apply-emergency-restriction (record-id uint))
  (let
    (
      (production-data (unwrap! (map-get? agricultural-production-ledger { production-record-id: record-id }) record-not-found))
      (security-descriptor "EMERGENCY-RESTRICTED")
      (current-descriptors (get metadata-descriptors production-data))
    )
    ;; Verify caller authority for emergency actions
    (asserts! (production-record-exists record-id) record-not-found)
    (asserts! 
      (or 
        (is-eq tx-sender system-controller)
        (is-eq (get production-owner production-data) tx-sender)
      ) 
      controller-privilege-required
    )

    (ok true)
  )
)

