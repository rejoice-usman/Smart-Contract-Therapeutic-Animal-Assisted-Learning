;; =============================================================================
;; SMART CONTRACT THERAPEUTIC ANIMAL ASSISTED LEARNING SYSTEM
;; =============================================================================
;; A comprehensive system for coordinating educational programs with animal partners
;; including session scheduling, learning outcome tracking, and animal welfare monitoring
;; with special needs accommodation and nature-based education approaches.

;; =============================================================================
;; CONSTANTS AND ERROR CODES
;; =============================================================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_INPUT (err u102))
(define-constant ERR_SESSION_CONFLICT (err u103))
(define-constant ERR_ANIMAL_UNAVAILABLE (err u104))
(define-constant ERR_STUDENT_NOT_ENROLLED (err u105))
(define-constant ERR_SESSION_EXPIRED (err u106))
(define-constant ERR_ANIMAL_WELFARE_CONCERN (err u107))
(define-constant ERR_CAPACITY_EXCEEDED (err u108))
(define-constant ERR_INVALID_CREDENTIALS (err u109))
(define-constant ERR_PROGRAM_INACTIVE (err u110))

;; Session status constants
(define-constant STATUS_SCHEDULED u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_CANCELLED u4)

;; Animal welfare status
(define-constant WELFARE_EXCELLENT u5)
(define-constant WELFARE_GOOD u4)
(define-constant WELFARE_FAIR u3)
(define-constant WELFARE_POOR u2)
(define-constant WELFARE_CRITICAL u1)

;; Learning outcome levels
(define-constant OUTCOME_EXCEPTIONAL u5)
(define-constant OUTCOME_PROFICIENT u4)
(define-constant OUTCOME_DEVELOPING u3)
(define-constant OUTCOME_BEGINNING u2)
(define-constant OUTCOME_NEEDS_SUPPORT u1)

;; Special needs accommodation types
(define-constant ACCOMMODATION_MOBILITY u1)
(define-constant ACCOMMODATION_SENSORY u2)
(define-constant ACCOMMODATION_COGNITIVE u3)
(define-constant ACCOMMODATION_BEHAVIORAL u4)
(define-constant ACCOMMODATION_COMMUNICATION u5)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var session-id-nonce uint u0)
(define-data-var animal-id-nonce uint u0)
(define-data-var student-id-nonce uint u0)
(define-data-var program-id-nonce uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; Educational Programs Map
(define-map programs
  { program-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    duration-weeks: uint,
    max-students: uint,
    min-age: uint,
    max-age: uint,
    special-needs-compatible: bool,
    nature-based-focus: bool,
    animal-ethics-integration: bool,
    coordinator: principal,
    active: bool,
    created-at: uint
  }
)

;; Animal Partners Map
(define-map animals
  { animal-id: uint }
  {
    name: (string-ascii 32),
    species: (string-ascii 32),
    breed: (string-ascii 32),
    age: uint,
    temperament: (string-ascii 64),
    certifications: (list 10 (string-ascii 32)),
    welfare-status: uint,
    available: bool,
    max-sessions-per-week: uint,
    special-needs-trained: bool,
    handler: principal,
    last-wellness-check: uint,
    created-at: uint
  }
)

;; Students Map
(define-map students
  { student-id: uint }
  {
    guardian: principal,
    age: uint,
    grade-level: uint,
    special-needs: (list 5 uint),
    accommodation-requirements: (list 10 (string-ascii 64)),
    animal-allergies: (list 5 (string-ascii 32)),
    emergency-contact: principal,
    medical-notes: (string-ascii 256),
    enrolled-programs: (list 10 uint),
    active: bool,
    created-at: uint
  }
)

;; Learning Sessions Map
(define-map sessions
  { session-id: uint }
  {
    program-id: uint,
    animal-id: uint,
    student-ids: (list 8 uint),
    instructor: principal,
    scheduled-start: uint,
    scheduled-end: uint,
    actual-start: (optional uint),
    actual-end: (optional uint),
    status: uint,
    location: (string-ascii 64),
    session-notes: (string-ascii 512),
    learning-objectives: (list 5 (string-ascii 128)),
    created-at: uint
  }
)

;; Learning Outcomes Tracking Map
(define-map learning-outcomes
  { session-id: uint, student-id: uint }
  {
    cognitive-development: uint,
    social-skills: uint,
    emotional-regulation: uint,
    motor-skills: uint,
    communication: uint,
    empathy-development: uint,
    nature-connection: uint,
    animal-ethics-understanding: uint,
    overall-progress: uint,
    notes: (string-ascii 256),
    assessed-by: principal,
    assessment-date: uint
  }
)

;; Animal Welfare Monitoring Map
(define-map welfare-reports
  { animal-id: uint, report-date: uint }
  {
    physical-health: uint,
    behavioral-indicators: uint,
    stress-level: uint,
    appetite: uint,
    social-interaction: uint,
    exercise-level: uint,
    environmental-comfort: uint,
    overall-welfare: uint,
    concerns: (string-ascii 256),
    recommendations: (string-ascii 256),
    veterinarian: principal,
    next-check-date: uint
  }
)

;; Curriculum Development Map
(define-map curriculum-modules
  { program-id: uint, module-id: uint }
  {
    title: (string-ascii 64),
    description: (string-ascii 256),
    learning-objectives: (list 5 (string-ascii 128)),
    duration-minutes: uint,
    recommended-animals: (list 3 uint),
    special-needs-adaptations: (list 5 (string-ascii 128)),
    nature-activities: (list 5 (string-ascii 128)),
    ethics-components: (list 3 (string-ascii 128)),
    assessment-criteria: (list 5 (string-ascii 64)),
    created-by: principal,
    approved: bool,
    created-at: uint
  }
)

;; Staff Credentials Map
(define-map staff-credentials
  { staff-member: principal }
  {
    name: (string-ascii 64),
    role: (string-ascii 32),
    certifications: (list 10 (string-ascii 64)),
    specializations: (list 5 (string-ascii 32)),
    experience-years: uint,
    animal-handling-certified: bool,
    special-needs-trained: bool,
    active: bool,
    last-training-update: uint,
    supervisor: (optional principal)
  }
)

;; =============================================================================
;; ADMINISTRATIVE FUNCTIONS
;; =============================================================================

;; Register a new educational program
(define-public (register-program
  (name (string-ascii 64))
  (description (string-ascii 256))
  (duration-weeks uint)
  (max-students uint)
  (min-age uint)
  (max-age uint)
  (special-needs-compatible bool)
  (nature-based-focus bool)
  (animal-ethics-integration bool)
  (coordinator principal))
  (let ((program-id (+ (var-get program-id-nonce) u1)))
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (and (> (len name) u0) (> duration-weeks u0) (> max-students u0)) ERR_INVALID_INPUT)

    (map-set programs
      { program-id: program-id }
      {
        name: name,
        description: description,
        duration-weeks: duration-weeks,
        max-students: max-students,
        min-age: min-age,
        max-age: max-age,
        special-needs-compatible: special-needs-compatible,
        nature-based-focus: nature-based-focus,
        animal-ethics-integration: animal-ethics-integration,
        coordinator: coordinator,
        active: true,
        created-at: stacks-block-height
      }
    )

    (var-set program-id-nonce program-id)
    (ok program-id)
  )
)

;; Register a new animal partner
(define-public (register-animal
  (name (string-ascii 32))
  (species (string-ascii 32))
  (breed (string-ascii 32))
  (age uint)
  (temperament (string-ascii 64))
  (certifications (list 10 (string-ascii 32)))
  (max-sessions-per-week uint)
  (special-needs-trained bool)
  (handler principal))
  (let ((animal-id (+ (var-get animal-id-nonce) u1)))
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (and (> (len name) u0) (> (len species) u0)) ERR_INVALID_INPUT)

    (map-set animals
      { animal-id: animal-id }
      {
        name: name,
        species: species,
        breed: breed,
        age: age,
        temperament: temperament,
        certifications: certifications,
        welfare-status: WELFARE_GOOD,
        available: true,
        max-sessions-per-week: max-sessions-per-week,
        special-needs-trained: special-needs-trained,
        handler: handler,
        last-wellness-check: stacks-block-height,
        created-at: stacks-block-height
      }
    )

    (var-set animal-id-nonce animal-id)
    (ok animal-id)
  )
)

;; Register a new student
(define-public (register-student
  (guardian principal)
  (age uint)
  (grade-level uint)
  (special-needs (list 5 uint))
  (accommodation-requirements (list 10 (string-ascii 64)))
  (animal-allergies (list 5 (string-ascii 32)))
  (emergency-contact principal)
  (medical-notes (string-ascii 256)))
  (let ((student-id (+ (var-get student-id-nonce) u1)))
    (asserts! (or (is-eq tx-sender guardian) (is-authorized-admin tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (and (> age u0) (<= age u18)) ERR_INVALID_INPUT)

    (map-set students
      { student-id: student-id }
      {
        guardian: guardian,
        age: age,
        grade-level: grade-level,
        special-needs: special-needs,
        accommodation-requirements: accommodation-requirements,
        animal-allergies: animal-allergies,
        emergency-contact: emergency-contact,
        medical-notes: medical-notes,
        enrolled-programs: (list),
        active: true,
        created-at: stacks-block-height
      }
    )

    (var-set student-id-nonce student-id)
    (ok student-id)
  )
)

;; =============================================================================
;; SESSION MANAGEMENT FUNCTIONS
;; =============================================================================

;; Schedule a learning session
(define-public (schedule-session
  (program-id uint)
  (animal-id uint)
  (student-ids (list 8 uint))
  (instructor principal)
  (scheduled-start uint)
  (scheduled-end uint)
  (location (string-ascii 64))
  (learning-objectives (list 5 (string-ascii 128))))
  (let ((session-id (+ (var-get session-id-nonce) u1)))
    (asserts! (is-authorized-instructor tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-program-active program-id) ERR_PROGRAM_INACTIVE)
    (asserts! (is-animal-available animal-id scheduled-start scheduled-end) ERR_ANIMAL_UNAVAILABLE)
    (asserts! (validate-student-enrollment program-id student-ids) ERR_STUDENT_NOT_ENROLLED)
    (asserts! (< scheduled-start scheduled-end) ERR_INVALID_INPUT)
    (asserts! (>= scheduled-start stacks-block-height) ERR_INVALID_INPUT)

    (map-set sessions
      { session-id: session-id }
      {
        program-id: program-id,
        animal-id: animal-id,
        student-ids: student-ids,
        instructor: instructor,
        scheduled-start: scheduled-start,
        scheduled-end: scheduled-end,
        actual-start: none,
        actual-end: none,
        status: STATUS_SCHEDULED,
        location: location,
        session-notes: "",
        learning-objectives: learning-objectives,
        created-at: stacks-block-height
      }
    )

    (var-set session-id-nonce session-id)
    (ok session-id)
  )
)

;; Start a scheduled session
(define-public (start-session (session-id uint))
  (let ((session-data (unwrap! (map-get? sessions { session-id: session-id }) ERR_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender (get instructor session-data)) (is-authorized-admin tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status session-data) STATUS_SCHEDULED) ERR_INVALID_INPUT)
    (asserts! (<= (get scheduled-start session-data) stacks-block-height) ERR_SESSION_EXPIRED)

    (map-set sessions
      { session-id: session-id }
      (merge session-data {
        actual-start: (some stacks-block-height),
        status: STATUS_IN_PROGRESS
      })
    )

    (ok true)
  )
)

;; Complete a session and add notes
(define-public (complete-session
  (session-id uint)
  (session-notes (string-ascii 512)))
  (let ((session-data (unwrap! (map-get? sessions { session-id: session-id }) ERR_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender (get instructor session-data)) (is-authorized-admin tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status session-data) STATUS_IN_PROGRESS) ERR_INVALID_INPUT)

    (map-set sessions
      { session-id: session-id }
      (merge session-data {
        actual-end: (some stacks-block-height),
        status: STATUS_COMPLETED,
        session-notes: session-notes
      })
    )

    (ok true)
  )
)

;; =============================================================================
;; LEARNING OUTCOME TRACKING FUNCTIONS
;; =============================================================================

;; Record learning outcomes for a student in a session
(define-public (record-learning-outcomes
  (session-id uint)
  (student-id uint)
  (cognitive-development uint)
  (social-skills uint)
  (emotional-regulation uint)
  (motor-skills uint)
  (communication uint)
  (empathy-development uint)
  (nature-connection uint)
  (animal-ethics-understanding uint)
  (overall-progress uint)
  (notes (string-ascii 256)))
  (begin
    (asserts! (is-authorized-instructor tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-valid-session session-id) ERR_NOT_FOUND)
    (asserts! (is-student-in-session session-id student-id) ERR_STUDENT_NOT_ENROLLED)
    (asserts! (validate-outcome-scores cognitive-development social-skills emotional-regulation
                                     motor-skills communication empathy-development
                                     nature-connection animal-ethics-understanding overall-progress) ERR_INVALID_INPUT)

    (map-set learning-outcomes
      { session-id: session-id, student-id: student-id }
      {
        cognitive-development: cognitive-development,
        social-skills: social-skills,
        emotional-regulation: emotional-regulation,
        motor-skills: motor-skills,
        communication: communication,
        empathy-development: empathy-development,
        nature-connection: nature-connection,
        animal-ethics-understanding: animal-ethics-understanding,
        overall-progress: overall-progress,
        notes: notes,
        assessed-by: tx-sender,
        assessment-date: stacks-block-height
      }
    )

    (ok true)
  )
)

;; =============================================================================
;; ANIMAL WELFARE MONITORING FUNCTIONS
;; =============================================================================

;; Submit animal welfare report
(define-public (submit-welfare-report
  (animal-id uint)
  (physical-health uint)
  (behavioral-indicators uint)
  (stress-level uint)
  (appetite uint)
  (social-interaction uint)
  (exercise-level uint)
  (environmental-comfort uint)
  (overall-welfare uint)
  (concerns (string-ascii 256))
  (recommendations (string-ascii 256))
  (next-check-date uint))
  (begin
    (asserts! (is-authorized-veterinarian tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-valid-animal animal-id) ERR_NOT_FOUND)
    (asserts! (validate-welfare-scores physical-health behavioral-indicators stress-level
                                     appetite social-interaction exercise-level
                                     environmental-comfort overall-welfare) ERR_INVALID_INPUT)
    (asserts! (> next-check-date stacks-block-height) ERR_INVALID_INPUT)

    (map-set welfare-reports
      { animal-id: animal-id, report-date: stacks-block-height }
      {
        physical-health: physical-health,
        behavioral-indicators: behavioral-indicators,
        stress-level: stress-level,
        appetite: appetite,
        social-interaction: social-interaction,
        exercise-level: exercise-level,
        environmental-comfort: environmental-comfort,
        overall-welfare: overall-welfare,
        concerns: concerns,
        recommendations: recommendations,
        veterinarian: tx-sender,
        next-check-date: next-check-date
      }
    )

    ;; Update animal welfare status
    (update-animal-welfare-status animal-id overall-welfare)
  )
)

;; =============================================================================
;; CURRICULUM DEVELOPMENT FUNCTIONS
;; =============================================================================

;; Create curriculum module
(define-public (create-curriculum-module
  (program-id uint)
  (module-id uint)
  (title (string-ascii 64))
  (description (string-ascii 256))
  (learning-objectives (list 5 (string-ascii 128)))
  (duration-minutes uint)
  (recommended-animals (list 3 uint))
  (special-needs-adaptations (list 5 (string-ascii 128)))
  (nature-activities (list 5 (string-ascii 128)))
  (ethics-components (list 3 (string-ascii 128)))
  (assessment-criteria (list 5 (string-ascii 64))))
  (begin
    (asserts! (is-authorized-curriculum-developer tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-program-active program-id) ERR_PROGRAM_INACTIVE)
    (asserts! (and (> (len title) u0) (> duration-minutes u0)) ERR_INVALID_INPUT)

    (map-set curriculum-modules
      { program-id: program-id, module-id: module-id }
      {
        title: title,
        description: description,
        learning-objectives: learning-objectives,
        duration-minutes: duration-minutes,
        recommended-animals: recommended-animals,
        special-needs-adaptations: special-needs-adaptations,
        nature-activities: nature-activities,
        ethics-components: ethics-components,
        assessment-criteria: assessment-criteria,
        created-by: tx-sender,
        approved: false,
        created-at: stacks-block-height
      }
    )

    (ok true)
  )
)

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================

;; Check if user is authorized admin
(define-private (is-authorized-admin (user principal))
  (or (is-eq user CONTRACT_OWNER)
      (default-to false (get active (map-get? staff-credentials { staff-member: user })))
  )
)

;; Check if user is authorized instructor
(define-private (is-authorized-instructor (user principal))
  (or (is-authorized-admin user)
      (match (map-get? staff-credentials { staff-member: user })
        staff-data (and (get active staff-data)
                       (or (is-eq (get role staff-data) "instructor")
                           (is-eq (get role staff-data) "coordinator")))
        false
      )
  )
)

;; Check if user is authorized veterinarian
(define-private (is-authorized-veterinarian (user principal))
  (or (is-authorized-admin user)
      (match (map-get? staff-credentials { staff-member: user })
        staff-data (and (get active staff-data)
                       (is-eq (get role staff-data) "veterinarian"))
        false
      )
  )
)

;; Check if user is authorized curriculum developer
(define-private (is-authorized-curriculum-developer (user principal))
  (or (is-authorized-admin user)
      (match (map-get? staff-credentials { staff-member: user })
        staff-data (and (get active staff-data)
                       (or (is-eq (get role staff-data) "curriculum-developer")
                           (is-eq (get role staff-data) "coordinator")))
        false
      )
  )
)

;; Validate that a program is active
(define-private (is-program-active (program-id uint))
  (default-to false (get active (map-get? programs { program-id: program-id })))
)

;; Check if animal is available for scheduling
(define-private (is-animal-available (animal-id uint) (start-time uint) (end-time uint))
  (match (map-get? animals { animal-id: animal-id })
    animal-data (and (get available animal-data)
                    (>= (get welfare-status animal-data) WELFARE_FAIR))
    false
  )
)

;; Validate student enrollment in program
(define-private (validate-student-enrollment (program-id uint) (student-ids (list 8 uint)))
  (fold check-student-enrollment student-ids true)
)

(define-private (check-student-enrollment (student-id uint) (acc bool))
  (and acc
       (match (map-get? students { student-id: student-id })
         student-data (get active student-data)
         false
       )
  )
)

;; Validate that outcome scores are within valid range (1-5)
(define-private (validate-outcome-scores (score1 uint) (score2 uint) (score3 uint)
                                        (score4 uint) (score5 uint) (score6 uint)
                                        (score7 uint) (score8 uint) (score9 uint))
  (and (<= score1 u5) (>= score1 u1)
       (<= score2 u5) (>= score2 u1)
       (<= score3 u5) (>= score3 u1)
       (<= score4 u5) (>= score4 u1)
       (<= score5 u5) (>= score5 u1)
       (<= score6 u5) (>= score6 u1)
       (<= score7 u5) (>= score7 u1)
       (<= score8 u5) (>= score8 u1)
       (<= score9 u5) (>= score9 u1)
  )
)

;; Validate that welfare scores are within valid range (1-5)
(define-private (validate-welfare-scores (score1 uint) (score2 uint) (score3 uint)
                                        (score4 uint) (score5 uint) (score6 uint)
                                        (score7 uint) (score8 uint))
  (and (<= score1 u5) (>= score1 u1)
       (<= score2 u5) (>= score2 u1)
       (<= score3 u5) (>= score3 u1)
       (<= score4 u5) (>= score4 u1)
       (<= score5 u5) (>= score5 u1)
       (<= score6 u5) (>= score6 u1)
       (<= score7 u5) (>= score7 u1)
       (<= score8 u5) (>= score8 u1)
  )
)

;; Check if session exists and is valid
(define-private (is-valid-session (session-id uint))
  (is-some (map-get? sessions { session-id: session-id }))
)

;; Check if animal exists
(define-private (is-valid-animal (animal-id uint))
  (is-some (map-get? animals { animal-id: animal-id }))
)

;; Check if student is enrolled in a specific session
(define-private (is-student-in-session (session-id uint) (student-id uint))
  (match (map-get? sessions { session-id: session-id })
    session-data (is-some (index-of (get student-ids session-data) student-id))
    false
  )
)

;; Update animal welfare status
(define-private (update-animal-welfare-status (animal-id uint) (welfare-score uint))
  (match (map-get? animals { animal-id: animal-id })
    animal-data (begin
                  (map-set animals
                    { animal-id: animal-id }
                    (merge animal-data {
                      welfare-status: welfare-score,
                      available: (>= welfare-score WELFARE_FAIR),
                      last-wellness-check: stacks-block-height
                    })
                  )
                  (ok true)
                )
    ERR_NOT_FOUND
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

;; Get program information
(define-read-only (get-program (program-id uint))
  (map-get? programs { program-id: program-id })
)

;; Get animal information
(define-read-only (get-animal (animal-id uint))
  (map-get? animals { animal-id: animal-id })
)

;; Get student information
(define-read-only (get-student (student-id uint))
  (map-get? students { student-id: student-id })
)

;; Get session information
(define-read-only (get-session (session-id uint))
  (map-get? sessions { session-id: session-id })
)

;; Get learning outcomes for a student in a session
(define-read-only (get-learning-outcomes (session-id uint) (student-id uint))
  (map-get? learning-outcomes { session-id: session-id, student-id: student-id })
)

;; Get welfare report for an animal on a specific date
(define-read-only (get-welfare-report (animal-id uint) (report-date uint))
  (map-get? welfare-reports { animal-id: animal-id, report-date: report-date })
)

;; Get curriculum module
(define-read-only (get-curriculum-module (program-id uint) (module-id uint))
  (map-get? curriculum-modules { program-id: program-id, module-id: module-id })
)

;; Get staff credentials
(define-read-only (get-staff-credentials (staff-member principal))
  (map-get? staff-credentials { staff-member: staff-member })
)

;; Get current nonce values
(define-read-only (get-nonces)
  {
    session-id-nonce: (var-get session-id-nonce),
    animal-id-nonce: (var-get animal-id-nonce),
    student-id-nonce: (var-get student-id-nonce),
    program-id-nonce: (var-get program-id-nonce)
  }
)

;; =============================================================================
;; STAFF MANAGEMENT FUNCTIONS
;; =============================================================================

;; Register staff member
(define-public (register-staff
  (staff-member principal)
  (name (string-ascii 64))
  (role (string-ascii 32))
  (certifications (list 10 (string-ascii 64)))
  (specializations (list 5 (string-ascii 32)))
  (experience-years uint)
  (animal-handling-certified bool)
  (special-needs-trained bool)
  (supervisor (optional principal)))
  (begin
    (asserts! (is-authorized-admin tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR_INVALID_INPUT)

    (map-set staff-credentials
      { staff-member: staff-member }
      {
        name: name,
        role: role,
        certifications: certifications,
        specializations: specializations,
        experience-years: experience-years,
        animal-handling-certified: animal-handling-certified,
        special-needs-trained: special-needs-trained,
        active: true,
        last-training-update: stacks-block-height,
        supervisor: supervisor
      }
    )

    (ok true)
  )
)
