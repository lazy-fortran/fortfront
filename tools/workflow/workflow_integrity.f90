module workflow_integrity
    use error_handling
    implicit none
    private

    ! Technical evidence categories
    integer, parameter, public :: EVIDENCE_CI_RUN = 1
    integer, parameter, public :: EVIDENCE_TEST_OUTPUT = 2
    integer, parameter, public :: EVIDENCE_BUILD_SUCCESS = 3
    integer, parameter, public :: EVIDENCE_PERFORMANCE_METRICS = 4
    integer, parameter, public :: EVIDENCE_COVERAGE_REPORT = 5
    integer, parameter, public :: EVIDENCE_REVIEW_APPROVAL = 6

    ! Evidence validation status
    integer, parameter, public :: VALIDATION_PENDING = 0
    integer, parameter, public :: VALIDATION_VERIFIED = 1
    integer, parameter, public :: VALIDATION_FAILED = 2
    integer, parameter, public :: VALIDATION_FRAUD = 3

    ! Technical evidence record
    type, public :: technical_evidence_t
        integer :: evidence_type = 0
        character(len=:), allocatable :: evidence_url
        character(len=:), allocatable :: evidence_hash
        character(len=:), allocatable :: timestamp
        character(len=:), allocatable :: metadata
        integer :: validation_status = VALIDATION_PENDING
        character(len=:), allocatable :: validator_id
    contains
        procedure :: validate_evidence
        procedure :: is_valid
        procedure :: get_verification_proof
        procedure :: mark_as_fraud
        procedure :: clear_evidence
    end type technical_evidence_t

    ! Completion verification record
    type, public :: completion_verification_t
        character(len=:), allocatable :: task_id
        character(len=:), allocatable :: completion_claim
        type(technical_evidence_t), allocatable :: evidence(:)
        integer :: evidence_count = 0
        logical :: verification_complete = .false.
        character(len=:), allocatable :: verification_timestamp
        character(len=:), allocatable :: fraud_detection_result
    contains
        procedure :: add_evidence
        procedure :: verify_completion
        procedure :: detect_fraud_patterns
        procedure :: get_verification_status
        procedure :: require_evidence_type
        procedure :: clear_verification
        final :: cleanup_verification
    end type completion_verification_t

    ! Workflow integrity manager
    type, public :: workflow_integrity_manager_t
        type(completion_verification_t), allocatable :: verifications(:)
        integer :: verification_count = 0
        integer :: verification_capacity = 0
        logical :: fraud_prevention_active = .true.
        character(len=:), allocatable :: audit_log_path
    contains
        procedure :: initialize_manager
        procedure :: create_completion_record
        procedure :: enforce_technical_evidence
        procedure :: validate_all_completions
        procedure :: detect_systematic_fraud
        procedure :: generate_audit_report
        procedure :: get_fraud_metrics
        procedure :: clear_manager
        final :: cleanup_manager
    end type workflow_integrity_manager_t

    ! Public interface
    public :: create_workflow_manager, create_evidence_record
    public :: create_completion_verification

contains

    ! Technical evidence methods
    function validate_evidence(this, validator_id) result(validation_result)
        class(technical_evidence_t), intent(inout) :: this
        character(len=*), intent(in) :: validator_id
        type(result_t) :: validation_result

        ! Basic validation checks
        if (.not. allocated(this%evidence_url)) then
            validation_result = create_error_result( &
                "Evidence URL is required for validation", &
                component="workflow_integrity", &
                suggestion="Provide valid CI run URL or test output location" &
            )
            this%validation_status = VALIDATION_FAILED
            return
        end if

        if (.not. allocated(this%timestamp)) then
            validation_result = create_error_result( &
                "Evidence timestamp is required for validation", &
                component="workflow_integrity", &
                suggestion="Include timestamp when evidence was generated" &
            )
            this%validation_status = VALIDATION_FAILED
            return
        end if

        ! Mark as verified with validator
        this%validation_status = VALIDATION_VERIFIED
        this%validator_id = trim(validator_id)
        validation_result = success_result()
    end function validate_evidence

    pure function is_valid(this) result(valid)
        class(technical_evidence_t), intent(in) :: this
        logical :: valid
        valid = this%validation_status == VALIDATION_VERIFIED
    end function is_valid

    function get_verification_proof(this) result(proof)
        class(technical_evidence_t), intent(in) :: this
        character(len=:), allocatable :: proof

        if (.not. this%is_valid()) then
            proof = "UNVERIFIED: No valid technical evidence"
            return
        end if

        proof = "VERIFIED: Type=" // char(this%evidence_type + 48)
        if (allocated(this%evidence_url)) then
            proof = proof // ", URL=" // this%evidence_url
        end if
        if (allocated(this%timestamp)) then
            proof = proof // ", Timestamp=" // this%timestamp
        end if
        if (allocated(this%validator_id)) then
            proof = proof // ", Validator=" // this%validator_id
        end if
    end function get_verification_proof

    subroutine mark_as_fraud(this, fraud_reason)
        class(technical_evidence_t), intent(inout) :: this
        character(len=*), intent(in) :: fraud_reason

        this%validation_status = VALIDATION_FRAUD
        if (allocated(this%metadata)) then
            this%metadata = this%metadata // " | FRAUD: " // trim(fraud_reason)
        else
            this%metadata = "FRAUD: " // trim(fraud_reason)
        end if
    end subroutine mark_as_fraud

    subroutine clear_evidence(this)
        class(technical_evidence_t), intent(inout) :: this

        this%evidence_type = 0
        this%validation_status = VALIDATION_PENDING
        if (allocated(this%evidence_url)) deallocate(this%evidence_url)
        if (allocated(this%evidence_hash)) deallocate(this%evidence_hash)
        if (allocated(this%timestamp)) deallocate(this%timestamp)
        if (allocated(this%metadata)) deallocate(this%metadata)
        if (allocated(this%validator_id)) deallocate(this%validator_id)
    end subroutine clear_evidence

    ! Completion verification methods
    subroutine add_evidence(this, evidence)
        class(completion_verification_t), intent(inout) :: this
        type(technical_evidence_t), intent(in) :: evidence

        type(technical_evidence_t), allocatable :: temp_evidence(:)
        integer :: new_capacity, i

        ! Grow array if needed
        if (.not. allocated(this%evidence)) then
            allocate(this%evidence(4))
            this%evidence_count = 0
        else if (this%evidence_count >= size(this%evidence)) then
            new_capacity = size(this%evidence) * 2
            allocate(temp_evidence(new_capacity))
            
            do i = 1, this%evidence_count
                temp_evidence(i) = this%evidence(i)
            end do
            
            call move_alloc(temp_evidence, this%evidence)
        end if

        this%evidence_count = this%evidence_count + 1
        this%evidence(this%evidence_count) = evidence
    end subroutine add_evidence

    function verify_completion(this) result(verification_result)
        class(completion_verification_t), intent(inout) :: this
        type(result_t) :: verification_result

        integer :: i, verified_count
        logical :: has_ci_evidence, has_test_evidence

        if (this%evidence_count == 0) then
            verification_result = create_error_result( &
                "No technical evidence provided for completion claim", &
                component="workflow_integrity", &
                context=this%completion_claim, &
                suggestion="Provide CI run URL and test output evidence" &
            )
            return
        end if

        ! Count verified evidence and check required types
        verified_count = 0
        has_ci_evidence = .false.
        has_test_evidence = .false.

        do i = 1, this%evidence_count
            if (this%evidence(i)%is_valid()) then
                verified_count = verified_count + 1
                
                if (this%evidence(i)%evidence_type == EVIDENCE_CI_RUN) then
                    has_ci_evidence = .true.
                end if
                if (this%evidence(i)%evidence_type == EVIDENCE_TEST_OUTPUT) then
                    has_test_evidence = .true.
                end if
            end if
        end do

        ! Require CI evidence for completion claims
        if (.not. has_ci_evidence) then
            verification_result = create_error_result( &
                "CI run evidence is required for completion verification", &
                component="workflow_integrity", &
                context=this%completion_claim, &
                suggestion="Provide valid CI run URL showing tests pass" &
            )
            return
        end if

        ! Mark verification complete
        this%verification_complete = .true.
        this%verification_timestamp = "verified"  ! In real implementation, use actual timestamp
        verification_result = success_result()
    end function verify_completion

    function detect_fraud_patterns(this) result(fraud_result)
        class(completion_verification_t), intent(in) :: this
        type(result_t) :: fraud_result

        integer :: i, fraud_count
        logical :: suspicious_patterns

        fraud_count = 0
        suspicious_patterns = .false.

        ! Check for fraud markers in evidence
        do i = 1, this%evidence_count
            if (this%evidence(i)%validation_status == VALIDATION_FRAUD) then
                fraud_count = fraud_count + 1
            end if

            ! Check for suspicious patterns
            if (allocated(this%evidence(i)%metadata)) then
                if (index(this%evidence(i)%metadata, "FRAUD") > 0) then
                    suspicious_patterns = .true.
                end if
            end if
        end do

        if (fraud_count > 0 .or. suspicious_patterns) then
            fraud_result = create_error_result( &
                "Fraud patterns detected in completion verification", &
                component="workflow_integrity", &
                context=this%completion_claim, &
                suggestion="Review evidence integrity and re-submit valid proof" &
            )
        else
            fraud_result = success_result()
        end if
    end function detect_fraud_patterns

    function get_verification_status(this) result(status)
        class(completion_verification_t), intent(in) :: this
        character(len=:), allocatable :: status

        if (.not. this%verification_complete) then
            status = "PENDING_VERIFICATION"
        else if (this%detect_fraud_patterns()%is_failure()) then
            status = "FRAUD_DETECTED"
        else
            status = "VERIFIED_COMPLETE"
        end if
    end function get_verification_status

    function require_evidence_type(this, evidence_type) result(has_type)
        class(completion_verification_t), intent(in) :: this
        integer, intent(in) :: evidence_type
        logical :: has_type

        integer :: i

        has_type = .false.
        do i = 1, this%evidence_count
            if (this%evidence(i)%evidence_type == evidence_type .and. &
                this%evidence(i)%is_valid()) then
                has_type = .true.
                return
            end if
        end do
    end function require_evidence_type

    subroutine clear_verification(this)
        class(completion_verification_t), intent(inout) :: this

        integer :: i

        if (allocated(this%evidence)) then
            do i = 1, this%evidence_count
                call this%evidence(i)%clear_evidence()
            end do
            deallocate(this%evidence)
        end if

        this%evidence_count = 0
        this%verification_complete = .false.
        
        if (allocated(this%task_id)) deallocate(this%task_id)
        if (allocated(this%completion_claim)) deallocate(this%completion_claim)
        if (allocated(this%verification_timestamp)) deallocate(this%verification_timestamp)
        if (allocated(this%fraud_detection_result)) deallocate(this%fraud_detection_result)
    end subroutine clear_verification

    subroutine cleanup_verification(this)
        type(completion_verification_t), intent(inout) :: this
        call this%clear_verification()
    end subroutine cleanup_verification

    ! Workflow integrity manager methods
    subroutine initialize_manager(this, audit_log_path)
        class(workflow_integrity_manager_t), intent(inout) :: this
        character(len=*), intent(in), optional :: audit_log_path

        this%verification_capacity = 16
        allocate(this%verifications(this%verification_capacity))
        this%verification_count = 0
        this%fraud_prevention_active = .true.

        if (present(audit_log_path)) then
            this%audit_log_path = trim(audit_log_path)
        else
            this%audit_log_path = "workflow_integrity_audit.log"
        end if
    end subroutine initialize_manager

    function create_completion_record(this, task_id, completion_claim) result(record_result)
        class(workflow_integrity_manager_t), intent(inout) :: this
        character(len=*), intent(in) :: task_id, completion_claim
        type(result_t) :: record_result

        type(completion_verification_t), allocatable :: temp_verifications(:)
        integer :: new_capacity, i

        ! Grow array if needed
        if (this%verification_count >= this%verification_capacity) then
            new_capacity = this%verification_capacity * 2
            allocate(temp_verifications(new_capacity))
            
            do i = 1, this%verification_count
                temp_verifications(i) = this%verifications(i)
            end do
            
            call move_alloc(temp_verifications, this%verifications)
            this%verification_capacity = new_capacity
        end if

        ! Create new verification record
        this%verification_count = this%verification_count + 1
        this%verifications(this%verification_count)%task_id = trim(task_id)
        this%verifications(this%verification_count)%completion_claim = trim(completion_claim)
        
        record_result = success_result()
    end function create_completion_record

    function enforce_technical_evidence(this, task_id, required_evidence_types) result(enforcement_result)
        class(workflow_integrity_manager_t), intent(in) :: this
        character(len=*), intent(in) :: task_id
        integer, intent(in) :: required_evidence_types(:)
        type(result_t) :: enforcement_result

        integer :: i, j, verification_index
        logical :: found_verification, has_required_type

        ! Find verification record
        found_verification = .false.
        do i = 1, this%verification_count
            if (allocated(this%verifications(i)%task_id)) then
                if (this%verifications(i)%task_id == task_id) then
                    verification_index = i
                    found_verification = .true.
                    exit
                end if
            end if
        end do

        if (.not. found_verification) then
            enforcement_result = create_error_result( &
                "No verification record found for task", &
                component="workflow_integrity", &
                context=task_id, &
                suggestion="Create completion record before enforcing evidence" &
            )
            return
        end if

        ! Check for required evidence types
        do i = 1, size(required_evidence_types)
            has_required_type = this%verifications(verification_index)%require_evidence_type(required_evidence_types(i))
            
            if (.not. has_required_type) then
                enforcement_result = create_error_result( &
                    "Required technical evidence type missing", &
                    component="workflow_integrity", &
                    context=task_id, &
                    suggestion="Provide all required evidence types before completion" &
                )
                return
            end if
        end do

        enforcement_result = success_result()
    end function enforce_technical_evidence

    function validate_all_completions(this) result(validation_result)
        class(workflow_integrity_manager_t), intent(inout) :: this
        type(result_t) :: validation_result

        type(error_collection_t) :: errors
        integer :: i
        type(result_t) :: single_result

        errors = create_error_collection()

        do i = 1, this%verification_count
            single_result = this%verifications(i)%verify_completion()
            if (single_result%is_failure()) then
                call errors%add_result(single_result)
            end if
        end do

        if (errors%has_errors()) then
            validation_result = create_error_result( &
                "Validation failures in completion verifications: " // errors%get_summary(), &
                component="workflow_integrity", &
                suggestion="Address all validation failures before proceeding" &
            )
        else
            validation_result = success_result()
        end if
    end function validate_all_completions

    function detect_systematic_fraud(this) result(fraud_result)
        class(workflow_integrity_manager_t), intent(in) :: this
        type(result_t) :: fraud_result

        integer :: i, fraud_count, total_verified
        real :: fraud_rate
        type(result_t) :: single_fraud_result

        fraud_count = 0
        total_verified = 0

        do i = 1, this%verification_count
            if (this%verifications(i)%verification_complete) then
                total_verified = total_verified + 1
                
                single_fraud_result = this%verifications(i)%detect_fraud_patterns()
                if (single_fraud_result%is_failure()) then
                    fraud_count = fraud_count + 1
                end if
            end if
        end do

        if (total_verified > 0) then
            fraud_rate = real(fraud_count) / real(total_verified)
            
            if (fraud_rate > 0.1) then  ! More than 10% fraud rate
                fraud_result = create_error_result( &
                    "Systematic fraud detected in workflow completions", &
                    component="workflow_integrity", &
                    suggestion="Investigate and remediate fraud patterns immediately" &
                )
            else
                fraud_result = success_result()
            end if
        else
            fraud_result = success_result()
        end if
    end function detect_systematic_fraud

    function generate_audit_report(this) result(report)
        class(workflow_integrity_manager_t), intent(in) :: this
        character(len=:), allocatable :: report

        integer :: i, verified_count, fraud_count
        character(len=20) :: count_str

        verified_count = 0
        fraud_count = 0

        do i = 1, this%verification_count
            if (this%verifications(i)%verification_complete) then
                verified_count = verified_count + 1
            end if
            
            if (this%verifications(i)%detect_fraud_patterns()%is_failure()) then
                fraud_count = fraud_count + 1
            end if
        end do

        write(count_str, '(I0)') this%verification_count
        report = "WORKFLOW INTEGRITY AUDIT REPORT" // char(10)
        report = report // "Total verifications: " // trim(count_str) // char(10)
        
        write(count_str, '(I0)') verified_count
        report = report // "Completed verifications: " // trim(count_str) // char(10)
        
        write(count_str, '(I0)') fraud_count
        report = report // "Fraud detections: " // trim(count_str) // char(10)
        
        if (fraud_count > 0) then
            report = report // "STATUS: FRAUD DETECTED - IMMEDIATE REVIEW REQUIRED" // char(10)
        else
            report = report // "STATUS: No fraud patterns detected" // char(10)
        end if
    end function generate_audit_report

    function get_fraud_metrics(this) result(metrics)
        class(workflow_integrity_manager_t), intent(in) :: this
        character(len=:), allocatable :: metrics

        integer :: total_evidence, verified_evidence, fraud_evidence
        integer :: i, j
        character(len=20) :: num_str
        real :: verification_rate, fraud_rate

        total_evidence = 0
        verified_evidence = 0
        fraud_evidence = 0

        do i = 1, this%verification_count
            total_evidence = total_evidence + this%verifications(i)%evidence_count
            
            do j = 1, this%verifications(i)%evidence_count
                if (this%verifications(i)%evidence(j)%is_valid()) then
                    verified_evidence = verified_evidence + 1
                end if
                
                if (this%verifications(i)%evidence(j)%validation_status == VALIDATION_FRAUD) then
                    fraud_evidence = fraud_evidence + 1
                end if
            end do
        end do

        if (total_evidence > 0) then
            verification_rate = real(verified_evidence) / real(total_evidence) * 100.0
            fraud_rate = real(fraud_evidence) / real(total_evidence) * 100.0
        else
            verification_rate = 0.0
            fraud_rate = 0.0
        end if

        write(num_str, '(F6.1)') verification_rate
        metrics = "Verification Rate: " // trim(num_str) // "%" // char(10)
        
        write(num_str, '(F6.1)') fraud_rate
        metrics = metrics // "Fraud Rate: " // trim(num_str) // "%" // char(10)
        
        write(num_str, '(I0)') total_evidence
        metrics = metrics // "Total Evidence: " // trim(num_str) // char(10)
        
        write(num_str, '(I0)') verified_evidence
        metrics = metrics // "Verified Evidence: " // trim(num_str) // char(10)
        
        write(num_str, '(I0)') fraud_evidence
        metrics = metrics // "Fraud Evidence: " // trim(num_str)
    end function get_fraud_metrics

    subroutine clear_manager(this)
        class(workflow_integrity_manager_t), intent(inout) :: this

        integer :: i

        if (allocated(this%verifications)) then
            do i = 1, this%verification_count
                call this%verifications(i)%clear_verification()
            end do
            deallocate(this%verifications)
        end if

        this%verification_count = 0
        this%verification_capacity = 0
        this%fraud_prevention_active = .false.
        
        if (allocated(this%audit_log_path)) deallocate(this%audit_log_path)
    end subroutine clear_manager

    subroutine cleanup_manager(this)
        type(workflow_integrity_manager_t), intent(inout) :: this
        call this%clear_manager()
    end subroutine cleanup_manager

    ! Factory functions
    function create_workflow_manager(audit_log_path) result(manager)
        character(len=*), intent(in), optional :: audit_log_path
        type(workflow_integrity_manager_t) :: manager

        if (present(audit_log_path)) then
            call manager%initialize_manager(audit_log_path)
        else
            call manager%initialize_manager()
        end if
    end function create_workflow_manager

    function create_evidence_record(evidence_type, evidence_url, timestamp, metadata) result(evidence)
        integer, intent(in) :: evidence_type
        character(len=*), intent(in) :: evidence_url
        character(len=*), intent(in), optional :: timestamp, metadata
        type(technical_evidence_t) :: evidence

        evidence%evidence_type = evidence_type
        evidence%evidence_url = trim(evidence_url)
        evidence%validation_status = VALIDATION_PENDING

        if (present(timestamp)) then
            evidence%timestamp = trim(timestamp)
        end if

        if (present(metadata)) then
            evidence%metadata = trim(metadata)
        end if
    end function create_evidence_record

    function create_completion_verification(task_id, completion_claim) result(verification)
        character(len=*), intent(in) :: task_id, completion_claim
        type(completion_verification_t) :: verification

        verification%task_id = trim(task_id)
        verification%completion_claim = trim(completion_claim)
        verification%verification_complete = .false.
        verification%evidence_count = 0
    end function create_completion_verification

end module workflow_integrity