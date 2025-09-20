;; CHRONOS VAULT - TIME-LOCKED STX ESCROW SMART CONTRACT

;; A comprehensive smart contract system for creating time-locked STX vaults with advanced features:
;; - Customizable time-lock periods with minimum security requirements
;; - Multi-vault portfolio management per user account
;; - Partial and complete withdrawal capabilities
;; - Comprehensive audit trails and transaction logging
;; - Emergency pause functionality for contract security
;; - Service fee collection for sustainable operations
;; Perfect for savings goals, vesting schedules, delayed payments, and secure escrow services.

;; ERROR CONSTANTS - COMPREHENSIVE ERROR CODE SYSTEM
(define-constant ERR-UNAUTHORIZED-ACCESS-DENIED (err u1001))
(define-constant ERR-VAULT-NOT-FOUND-IN-SYSTEM (err u1002))
(define-constant ERR-VAULT-STILL-TIME-LOCKED (err u1003))
(define-constant ERR-INSUFFICIENT-STX-BALANCE (err u1004))
(define-constant ERR-INVALID-DEPOSIT-AMOUNT (err u1005))
(define-constant ERR-INVALID-LOCK-DURATION (err u1006))
(define-constant ERR-VAULT-CREATION-LIMIT-EXCEEDED (err u1007))
(define-constant ERR-ZERO-BALANCE-OPERATION (err u1008))
(define-constant ERR-WITHDRAWAL-EXCEEDS-BALANCE (err u1009))
(define-constant ERR-INVALID-VAULT-IDENTIFIER (err u1010))
(define-constant ERR-INVALID-STRING-INPUT (err u1011))
(define-constant ERR-CONTRACT-PAUSED (err u1012))
(define-constant ERR-VAULT-LIMIT-EXCEEDED (err u1013))
(define-constant ERR-ACTIVITY-LOG-CREATION-FAILED (err u1014))


;; SYSTEM CONFIGURATION CONSTANTS

(define-constant contract-deployer-address tx-sender)
(define-constant maximum-vaults-per-user u100)
(define-constant minimum-lock-duration-blocks u144) ;; Approximately 24 hours at 10min/block
(define-constant vault-creation-fee u1000000) ;; 1 STX service fee for vault creation
(define-constant maximum-vault-identifier u999999999) ;; Upper limit for vault IDs
(define-constant vault-description-max-length u50) ;; Maximum characters for vault labels
(define-constant default-vault-description "Default Chronos Time-Locked Vault")

;; GLOBAL CONTRACT STATE VARIABLES
(define-data-var global-vault-counter uint u0)
(define-data-var total-locked-stx-amount uint u0)
(define-data-var contract-pause-status bool false)
(define-data-var total-service-fees-collected uint u0)

;; PRIMARY DATA STRUCTURES AND STORAGE MAPS
;; Main vault registry storing comprehensive vault metadata and state information
(define-map vault-registry-database
  { vault-id: uint }
  {
    vault-owner: principal,
    locked-amount: uint,
    unlock-block-height: uint,
    creation-block-height: uint,
    last-activity-block: uint,
    vault-label: (string-ascii 50),
    vault-status: (string-ascii 20)
  }
)

;; User portfolio management system for tracking owned vaults and aggregated statistics
(define-map user-vault-portfolio
  { user-address: principal }
  { 
    owned-vault-ids: (list 100 uint),
    total-locked-amount: uint,
    active-vault-count: uint,
    last-portfolio-update: uint
  }
)

;; Comprehensive transaction activity logging for transparency and audit requirements
(define-map vault-activity-log
  { vault-id: uint, activity-index: uint }
  {
    action-type: (string-ascii 30),
    transaction-amount: uint,
    block-timestamp: uint,
    initiator-address: principal,
    additional-context: (string-ascii 100)
  }
)

;; Activity sequence tracking for each vault to maintain proper logging order
(define-map vault-activity-counter
  { vault-id: uint }
  { next-activity-index: uint }
)

;; INPUT VALIDATION AND UTILITY FUNCTIONS
;; Comprehensive vault identifier validation with range and existence checks
(define-private (is-valid-vault-id (vault-identifier uint))
  (and 
    (> vault-identifier u0) 
    (<= vault-identifier maximum-vault-identifier)
  )
)

;; String input validation to prevent empty or null vault descriptions
(define-private (is-valid-vault-description (description (string-ascii 50)))
  (and 
    (> (len description) u0)
    (<= (len description) vault-description-max-length)
  )
)

;; Enhanced vault data retrieval with comprehensive validation and error handling
(define-private (get-vault-data-safely (vault-identifier uint))
  (if (is-valid-vault-id vault-identifier)
    (map-get? vault-registry-database { vault-id: vault-identifier })
    none
  )
)

;; Sanitize and validate vault description input with strict validation and fallback
(define-private (sanitize-vault-description (input-description (string-ascii 50)))
  (let 
    (
      (description-length (len input-description))
      (is-valid-length (and (> description-length u0) (<= description-length vault-description-max-length)))
    )
    (if is-valid-length
      input-description
      default-vault-description
    )
  )
)

;; Check if contract is currently operational (not in emergency pause state)
(define-private (is-contract-operational)
  (not (var-get contract-pause-status))
)

;; Validate user authorization for vault operations
(define-private (is-vault-owner (vault-data {vault-owner: principal, locked-amount: uint, unlock-block-height: uint, creation-block-height: uint, last-activity-block: uint, vault-label: (string-ascii 50), vault-status: (string-ascii 20)}) (requesting-user principal))
  (is-eq (get vault-owner vault-data) requesting-user)
)

;; VAULT CREATION AND MANAGEMENT FUNCTIONS
;; Create new time-locked vault with comprehensive validation and error handling
(define-public (create-time-locked-vault 
  (initial-deposit-amount uint) 
  (lock-duration-blocks uint) 
  (vault-description (string-ascii 50)))
  (let
    (
      (new-vault-id (+ (var-get global-vault-counter) u1))
      (unlock-target-height (+ stacks-block-height lock-duration-blocks))
      (vault-creator tx-sender)
      (current-user-portfolio (default-to 
        { 
          owned-vault-ids: (list), 
          total-locked-amount: u0, 
          active-vault-count: u0,
          last-portfolio-update: u0
        }
        (map-get? user-vault-portfolio { user-address: vault-creator })))
      (total-required-stx (+ initial-deposit-amount vault-creation-fee))
      ;; Validate description input before sanitizing
      (description-length (len vault-description))
      (is-description-valid (and (> description-length u0) (<= description-length vault-description-max-length)))
      (final-description (if is-description-valid vault-description default-vault-description))
    )
    
    ;; Comprehensive pre-flight validation checks
    (asserts! (is-contract-operational) ERR-CONTRACT-PAUSED)
    (asserts! (> initial-deposit-amount u0) ERR-INVALID-DEPOSIT-AMOUNT)
    (asserts! (>= lock-duration-blocks minimum-lock-duration-blocks) ERR-INVALID-LOCK-DURATION)
    (asserts! (>= (stx-get-balance vault-creator) total-required-stx) ERR-INSUFFICIENT-STX-BALANCE)
    (asserts! (< (get active-vault-count current-user-portfolio) maximum-vaults-per-user) ERR-VAULT-CREATION-LIMIT-EXCEEDED)
    (asserts! (is-valid-vault-id new-vault-id) ERR-INVALID-VAULT-IDENTIFIER)
    
    ;; Execute STX transfers for vault funding and service fee payment
    (try! (stx-transfer? initial-deposit-amount vault-creator (as-contract tx-sender)))
    (try! (stx-transfer? vault-creation-fee vault-creator contract-deployer-address))
    
    ;; Register the new vault in the system database
    (map-set vault-registry-database
      { vault-id: new-vault-id }
      {
        vault-owner: vault-creator,
        locked-amount: initial-deposit-amount,
        unlock-block-height: unlock-target-height,
        creation-block-height: stacks-block-height,
        last-activity-block: stacks-block-height,
        vault-label: final-description,
        vault-status: "ACTIVE"
      }
    )
    
    ;; Update user's vault portfolio with new vault information
    (map-set user-vault-portfolio
      { user-address: vault-creator }
      {
        owned-vault-ids: (unwrap! (as-max-len? 
          (append (get owned-vault-ids current-user-portfolio) new-vault-id) u100)
          ERR-VAULT-LIMIT-EXCEEDED),
        total-locked-amount: (+ (get total-locked-amount current-user-portfolio) initial-deposit-amount),
        active-vault-count: (+ (get active-vault-count current-user-portfolio) u1),
        last-portfolio-update: stacks-block-height
      }
    )
    
    ;; Initialize activity counter for the new vault
    (map-set vault-activity-counter
      { vault-id: new-vault-id }
      { next-activity-index: u1 }
    )
    
    ;; Update global contract state variables
    (var-set global-vault-counter new-vault-id)
    (var-set total-locked-stx-amount (+ (var-get total-locked-stx-amount) initial-deposit-amount))
    (var-set total-service-fees-collected (+ (var-get total-service-fees-collected) vault-creation-fee))
    
    ;; Log vault creation activity for audit trail
    (try! (log-vault-activity new-vault-id "VAULT_CREATED" initial-deposit-amount vault-creator "Initial vault creation with time-lock"))
    
    (ok new-vault-id)
  )
)

;; Add additional STX funds to existing vault with comprehensive validation
(define-public (add-funds-to-vault (target-vault-id uint) (additional-amount uint))
  (let
    (
      (requesting-user tx-sender)
      (vault-data (unwrap! (get-vault-data-safely target-vault-id) ERR-VAULT-NOT-FOUND-IN-SYSTEM))
      (updated-vault-balance (+ (get locked-amount vault-data) additional-amount))
    )
    
    ;; Comprehensive validation for fund addition
    (asserts! (is-contract-operational) ERR-CONTRACT-PAUSED)
    (asserts! (is-vault-owner vault-data requesting-user) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (> additional-amount u0) ERR-INVALID-DEPOSIT-AMOUNT)
    (asserts! (>= (stx-get-balance requesting-user) additional-amount) ERR-INSUFFICIENT-STX-BALANCE)
    (asserts! (is-eq (get vault-status vault-data) "ACTIVE") ERR-VAULT-NOT-FOUND-IN-SYSTEM)
    
    ;; Transfer additional STX to the contract
    (try! (stx-transfer? additional-amount requesting-user (as-contract tx-sender)))
    
    ;; Update vault record with increased balance
    (map-set vault-registry-database
      { vault-id: target-vault-id }
      (merge vault-data { 
        locked-amount: updated-vault-balance,
        last-activity-block: stacks-block-height
      })
    )
    
    ;; Update global locked amount tracking
    (var-set total-locked-stx-amount (+ (var-get total-locked-stx-amount) additional-amount))
    
    ;; Update user's portfolio total locked amount
    (match (map-get? user-vault-portfolio { user-address: requesting-user })
      existing-portfolio (map-set user-vault-portfolio
        { user-address: requesting-user }
        (merge existing-portfolio { 
          total-locked-amount: (+ (get total-locked-amount existing-portfolio) additional-amount),
          last-portfolio-update: stacks-block-height
        }))
      false
    )
    
    ;; Log fund addition activity
    (try! (log-vault-activity target-vault-id "FUNDS_ADDED" additional-amount requesting-user "Additional STX deposited to vault"))
    
    (ok updated-vault-balance)
  )
)

;; WITHDRAWAL FUNCTIONS WITH COMPREHENSIVE SECURITY
;; Complete vault withdrawal when time-lock has expired
(define-public (withdraw-complete-balance (target-vault-id uint))
  (let 
    (
      (requesting-user tx-sender)
      (vault-data (unwrap! (get-vault-data-safely target-vault-id) ERR-VAULT-NOT-FOUND-IN-SYSTEM))
      (withdrawal-amount (get locked-amount vault-data))
    )
    
    ;; Comprehensive validation for complete withdrawal
    (asserts! (is-contract-operational) ERR-CONTRACT-PAUSED)
    (asserts! (is-vault-owner vault-data requesting-user) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (>= stacks-block-height (get unlock-block-height vault-data)) ERR-VAULT-STILL-TIME-LOCKED)
    (asserts! (> withdrawal-amount u0) ERR-ZERO-BALANCE-OPERATION)
    (asserts! (is-eq (get vault-status vault-data) "ACTIVE") ERR-VAULT-NOT-FOUND-IN-SYSTEM)
    
    ;; Transfer complete vault balance back to owner
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender requesting-user)))
    
    ;; Update vault to emptied state
    (map-set vault-registry-database
      { vault-id: target-vault-id }
      (merge vault-data { 
        locked-amount: u0,
        last-activity-block: stacks-block-height,
        vault-status: "EMPTIED"
      })
    )
    
    ;; Update global contract state
    (var-set total-locked-stx-amount (- (var-get total-locked-stx-amount) withdrawal-amount))
    
    ;; Update user's portfolio statistics
    (match (map-get? user-vault-portfolio { user-address: requesting-user })
      existing-portfolio (map-set user-vault-portfolio
        { user-address: requesting-user }
        (merge existing-portfolio { 
          total-locked-amount: (- (get total-locked-amount existing-portfolio) withdrawal-amount),
          active-vault-count: (- (get active-vault-count existing-portfolio) u1),
          last-portfolio-update: stacks-block-height
        }))
      false
    )
    
    ;; Log complete withdrawal activity
    (try! (log-vault-activity target-vault-id "COMPLETE_WITHDRAWAL" withdrawal-amount requesting-user "Full vault balance withdrawn"))
    
    (ok withdrawal-amount)
  )
)

;; Partial withdrawal from unlocked vault with specified amount
(define-public (withdraw-partial-amount (target-vault-id uint) (withdrawal-amount uint))
  (let
    (
      (requesting-user tx-sender)
      (vault-data (unwrap! (get-vault-data-safely target-vault-id) ERR-VAULT-NOT-FOUND-IN-SYSTEM))
      (remaining-balance (- (get locked-amount vault-data) withdrawal-amount))
    )
    
    ;; Comprehensive validation for partial withdrawal
    (asserts! (is-contract-operational) ERR-CONTRACT-PAUSED)
    (asserts! (is-vault-owner vault-data requesting-user) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (>= stacks-block-height (get unlock-block-height vault-data)) ERR-VAULT-STILL-TIME-LOCKED)
    (asserts! (> withdrawal-amount u0) ERR-INVALID-DEPOSIT-AMOUNT)
    (asserts! (>= (get locked-amount vault-data) withdrawal-amount) ERR-WITHDRAWAL-EXCEEDS-BALANCE)
    (asserts! (is-eq (get vault-status vault-data) "ACTIVE") ERR-VAULT-NOT-FOUND-IN-SYSTEM)
    
    ;; Transfer requested amount to vault owner
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender requesting-user)))
    
    ;; Update vault with remaining balance
    (map-set vault-registry-database
      { vault-id: target-vault-id }
      (merge vault-data { 
        locked-amount: remaining-balance,
        last-activity-block: stacks-block-height,
        vault-status: (if (is-eq remaining-balance u0) "EMPTIED" "ACTIVE")
      })
    )
    
    ;; Update global contract state
    (var-set total-locked-stx-amount (- (var-get total-locked-stx-amount) withdrawal-amount))
    
    ;; Update user's portfolio statistics
    (match (map-get? user-vault-portfolio { user-address: requesting-user })
      existing-portfolio (map-set user-vault-portfolio
        { user-address: requesting-user }
        (merge existing-portfolio { 
          total-locked-amount: (- (get total-locked-amount existing-portfolio) withdrawal-amount),
          active-vault-count: (if (is-eq remaining-balance u0) 
                                (- (get active-vault-count existing-portfolio) u1)
                                (get active-vault-count existing-portfolio)),
          last-portfolio-update: stacks-block-height
        }))
      false
    )
    
    ;; Log partial withdrawal activity
    (try! (log-vault-activity target-vault-id "PARTIAL_WITHDRAWAL" withdrawal-amount requesting-user "Partial amount withdrawn from vault"))
    
    (ok remaining-balance)
  )
)

;; QUERY AND ANALYTICS READ-ONLY FUNCTIONS
;; Retrieve complete vault information with comprehensive details
(define-read-only (get-vault-details (vault-identifier uint))
  (get-vault-data-safely vault-identifier)
)

;; Get user's complete vault portfolio and comprehensive statistics
(define-read-only (get-user-portfolio (user-address principal))
  (map-get? user-vault-portfolio { user-address: user-address })
)

;; Check if vault time-lock has expired and is ready for withdrawal
(define-read-only (is-vault-unlocked (vault-identifier uint))
  (match (get-vault-data-safely vault-identifier)
    vault-data (>= stacks-block-height (get unlock-block-height vault-data))
    false
  )
)

;; Calculate exact remaining lock time in blocks for specific vault
(define-read-only (get-remaining-lock-time (vault-identifier uint))
  (match (get-vault-data-safely vault-identifier)
    vault-data (if (>= stacks-block-height (get unlock-block-height vault-data))
                  u0
                  (- (get unlock-block-height vault-data) stacks-block-height))
    u0
  )
)

;; Comprehensive contract analytics and system-wide statistics
(define-read-only (get-contract-analytics)
  {
    total-vaults-created: (var-get global-vault-counter),
    total-stx-locked: (var-get total-locked-stx-amount),
    current-block-height: stacks-block-height,
    contract-balance: (stx-get-balance (as-contract tx-sender)),
    operational-status: (if (var-get contract-pause-status) "PAUSED" "OPERATIONAL"),
    total-fees-collected: (var-get total-service-fees-collected),
    deployer-address: contract-deployer-address
  }
)

;; Calculate user's total locked STX across all active vaults
(define-read-only (get-user-total-locked-stx (user-address principal))
  (match (map-get? user-vault-portfolio { user-address: user-address })
    portfolio-data (get total-locked-amount portfolio-data)
    u0
  )
)

;; Get vault activity history for specific activity entry
(define-read-only (get-vault-activity (vault-identifier uint) (activity-index uint))
  (if (is-valid-vault-id vault-identifier)
    (map-get? vault-activity-log { vault-id: vault-identifier, activity-index: activity-index })
    none
  )
)

;; Get next activity index for a vault (useful for pagination)
(define-read-only (get-vault-next-activity-index (vault-identifier uint))
  (match (map-get? vault-activity-counter { vault-id: vault-identifier })
    counter-data (get next-activity-index counter-data)
    u0
  )
)

;; UTILITY AND ADMINISTRATIVE FUNCTIONS
;; Enhanced activity logging with comprehensive information capture
(define-private (log-vault-activity 
  (vault-identifier uint) 
  (action-type (string-ascii 30)) 
  (transaction-amount uint) 
  (initiator principal)
  (context (string-ascii 100)))
  (let
    (
      (current-activity-index (match (map-get? vault-activity-counter { vault-id: vault-identifier })
                                counter-data (get next-activity-index counter-data)
                                u0))
    )
    (asserts! (is-valid-vault-id vault-identifier) ERR-INVALID-VAULT-IDENTIFIER)
    
    ;; Record the activity in the log
    (map-set vault-activity-log
      { vault-id: vault-identifier, activity-index: current-activity-index }
      {
        action-type: action-type,
        transaction-amount: transaction-amount,
        block-timestamp: stacks-block-height,
        initiator-address: initiator,
        additional-context: context
      }
    )
    
    ;; Update the activity counter for next entry
    (map-set vault-activity-counter
      { vault-id: vault-identifier }
      { next-activity-index: (+ current-activity-index u1) }
    )
    
    (ok true)
  )
)

;; Emergency pause functionality for contract security (deployer only)
(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-eq tx-sender contract-deployer-address) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (var-set contract-pause-status (not (var-get contract-pause-status)))
    (ok (var-get contract-pause-status))
  )
)

;; Update vault label/description (vault owner only)
(define-public (update-vault-label (vault-identifier uint) (new-description (string-ascii 50)))
  (let
    (
      (requesting-user tx-sender)
      (vault-data (unwrap! (get-vault-data-safely vault-identifier) ERR-VAULT-NOT-FOUND-IN-SYSTEM))
      ;; Validate description input before sanitizing
      (description-length (len new-description))
      (is-description-valid (and (> description-length u0) (<= description-length vault-description-max-length)))
      (final-description (if is-description-valid new-description default-vault-description))
    )
    
    (asserts! (is-contract-operational) ERR-CONTRACT-PAUSED)
    (asserts! (is-vault-owner vault-data requesting-user) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! is-description-valid ERR-INVALID-STRING-INPUT)
    
    ;; Update vault with new description
    (map-set vault-registry-database
      { vault-id: vault-identifier }
      (merge vault-data { 
        vault-label: final-description,
        last-activity-block: stacks-block-height
      })
    )
    
    ;; Log label update activity
    (try! (log-vault-activity vault-identifier "LABEL_UPDATED" u0 requesting-user "Vault description updated"))
    
    (ok final-description)
  )
)

;; Get contract configuration parameters (read-only)
(define-read-only (get-contract-config)
  {
    max-vaults-per-user: maximum-vaults-per-user,
    min-lock-duration: minimum-lock-duration-blocks,
    vault-creation-fee: vault-creation-fee,
    max-vault-id: maximum-vault-identifier,
    description-max-length: vault-description-max-length
  }
)