module completion_verification_framework
    use error_handling
    use workflow_integrity
    implicit none
    private

    ! Verification framework status
    integer, parameter, public :: FRAMEWORK_INACTIVE = 0
    integer, parameter, public :: FRAMEWORK_ACTIVE = 1
    integer, parameter, public :: FRAMEWORK_FRAUD_DETECTION = 2
    integer, parameter, public :: FRAMEWORK_EMERGENCY_STOP = 3

    ! Completion claim types
    integer, parameter, public :: CLAIM_IMPLEMENTATION = 1
    integer, parameter, public :: CLAIM_BUG_FIX = 2
    integer, parameter, public :: CLAIM_FEATURE_COMPLETE = 3
    integer, parameter, public :: CLAIM_REFACTORING = 4
    integer, parameter, public :: CLAIM_TESTING = 5
    integer, parameter, public :: CLAIM_DOCUMENTATION = 6

    ! Verification rule definitions
    type, public :: verification_rule_t
        integer :: claim_type = 0
        integer :: required_evidence_types(6) = 0
        integer :: required_evidence_count = 0
        integer :: minimum_ci_passes = 1
        logical :: requires_manual_review = .false.
        character(len=:), allocatable :: rule_description
    contains
        procedure :: matches_claim_type
        procedure :: check_evidence_requirements
        procedure :: get_rule_summary
        procedure :: clear_rule
    end type verification_rule_t

    ! Automated validation engine
    type, public :: automated_validation_engine_t
        type(verification_rule_t), allocatable :: rules(:)
        integer :: rule_count = 0
        integer :: framework_status = FRAMEWORK_INACTIVE
        type(workflow_integrity_manager_t) :: integrity_manager
        logical :: strict_enforcement = .true.
        character(len=:), allocatable :: audit_trail_path
        integer :: total_validations = 0
        integer :: successful_validations = 0
        integer :: fraud_detections = 0
    contains
        procedure :: initialize_engine
        procedure :: add_verification_rule
        procedure :: validate_completion_claim
        procedure :: run_automated_validation
        procedure :: detect_completion_fraud
        procedure :: enforce_verification_gate
        procedure :: generate_validation_report
        procedure :: get_framework_metrics
        procedure :: emergency_stop_framework
        procedure :: restart_framework
        procedure :: clear_engine
        final :: cleanup_engine
    end type automated_validation_engine_t

    ! Completion gate controller
    type, public :: completion_gate_controller_t
        type(automated_validation_engine_t) :: validation_engine
        logical :: gate_active = .true.
        logical :: allow_emergency_override = .false.
        integer :: consecutive_failures = 0
        integer :: max_consecutive_failures = 3
        character(len=:), allocatable :: gate_status_message
    contains
        procedure :: initialize_gate
        procedure :: process_completion_request
        procedure :: check_gate_status
        procedure :: handle_validation_failure
        procedure :: reset_failure_counter
        procedure :: emergency_gate_override
        procedure :: get_gate_metrics
        procedure :: clear_gate
    end type completion_gate_controller_t

    ! Public interface
    public :: create_validation_engine, create_verification_rule
    public :: create_completion_gate, initialize_default_rules

contains

    ! Verification rule methods
    pure function matches_claim_type(this, claim_type) result(matches)
        class(verification_rule_t), intent(in) :: this
        integer, intent(in) :: claim_type
        logical :: matches
        matches = this%claim_type == claim_type
    end function matches_claim_type

    function check_evidence_requirements(this, verification) result(check_result)
        class(verification_rule_t), intent(in) :: this
        type(completion_verification_t), intent(in) :: verification
        type(result_t) :: check_result

        integer :: i, found_count
        logical :: requirement_met

        found_count = 0

        ! Check each required evidence type
        do i = 1, this%required_evidence_count
            requirement_met = verification%require_evidence_type(this%required_evidence_types(i))
            if (requirement_met) then
                found_count = found_count + 1
            end if
        end do

        if (found_count < this%required_evidence_count) then
            check_result = create_error_result( &
                "Missing required evidence types for completion claim", &
                component="completion_verification_framework", &
                context=verification%completion_claim, &
                suggestion="Provide all required evidence types before claiming completion" &
            )
        else
            check_result = success_result()
        end if
    end function check_evidence_requirements

    function get_rule_summary(this) result(summary)
        class(verification_rule_t), intent(in) :: this
        character(len=:), allocatable :: summary

        character(len=20) :: type_str, count_str, ci_str

        write(type_str, '(I0)') this%claim_type
        write(count_str, '(I0)') this%required_evidence_count
        write(ci_str, '(I0)') this%minimum_ci_passes

        summary = "Claim Type: " // trim(type_str)
        summary = summary // ", Required Evidence: " // trim(count_str)
        summary = summary // ", Min CI Passes: " // trim(ci_str)
        
        if (this%requires_manual_review) then
            summary = summary // ", Manual Review: Required"
        else
            summary = summary // ", Manual Review: Not Required"
        end if
        
        if (allocated(this%rule_description)) then
            summary = summary // " | " // this%rule_description
        end if
    end function get_rule_summary

    subroutine clear_rule(this)
        class(verification_rule_t), intent(inout) :: this

        this%claim_type = 0
        this%required_evidence_types = 0
        this%required_evidence_count = 0
        this%minimum_ci_passes = 1
        this%requires_manual_review = .false.
        
        if (allocated(this%rule_description)) deallocate(this%rule_description)
    end subroutine clear_rule

    ! Automated validation engine methods
    subroutine initialize_engine(this, audit_trail_path, strict_enforcement)
        class(automated_validation_engine_t), intent(inout) :: this
        character(len=*), intent(in), optional :: audit_trail_path
        logical, intent(in), optional :: strict_enforcement

        call this%integrity_manager%initialize_manager()
        
        this%framework_status = FRAMEWORK_ACTIVE
        this%rule_count = 0
        this%total_validations = 0
        this%successful_validations = 0
        this%fraud_detections = 0

        if (present(audit_trail_path)) then
            this%audit_trail_path = trim(audit_trail_path)
        else
            this%audit_trail_path = "completion_verification_audit.log"
        end if

        if (present(strict_enforcement)) then
            this%strict_enforcement = strict_enforcement
        else
            this%strict_enforcement = .true.
        end if

        ! Initialize with default rules
        call this%add_default_rules()
    end subroutine initialize_engine

    subroutine add_verification_rule(this, rule)
        class(automated_validation_engine_t), intent(inout) :: this
        type(verification_rule_t), intent(in) :: rule

        type(verification_rule_t), allocatable :: temp_rules(:)
        integer :: new_capacity, i

        ! Grow array if needed
        if (.not. allocated(this%rules)) then
            allocate(this%rules(4))
            this%rule_count = 0
        else if (this%rule_count >= size(this%rules)) then
            new_capacity = size(this%rules) * 2
            allocate(temp_rules(new_capacity))
            
            do i = 1, this%rule_count
                temp_rules(i) = this%rules(i)
            end do
            
            call move_alloc(temp_rules, this%rules)
        end if

        this%rule_count = this%rule_count + 1
        this%rules(this%rule_count) = rule
    end subroutine add_verification_rule

    function validate_completion_claim(this, claim_type, verification) result(validation_result)
        class(automated_validation_engine_t), intent(inout) :: this
        integer, intent(in) :: claim_type
        type(completion_verification_t), intent(inout) :: verification
        type(result_t) :: validation_result

        integer :: i, rule_index
        logical :: found_rule
        type(result_t) :: rule_check_result, fraud_check_result

        this%total_validations = this%total_validations + 1

        if (this%framework_status /= FRAMEWORK_ACTIVE) then
            validation_result = create_error_result( &
                "Validation framework is not active", &
                component="completion_verification_framework", &
                suggestion="Restart framework before validating completions" &
            )
            return
        end if

        ! Find matching rule
        found_rule = .false.
        do i = 1, this%rule_count
            if (this%rules(i)%matches_claim_type(claim_type)) then
                rule_index = i
                found_rule = .true.
                exit
            end if
        end do

        if (.not. found_rule) then
            if (this%strict_enforcement) then
                validation_result = create_error_result( &
                    "No verification rule found for claim type", &
                    component="completion_verification_framework", &
                    context=verification%completion_claim, &
                    suggestion="Define verification rule for this claim type" &
                )
                return
            else
                ! Allow with warning if not in strict mode
                validation_result = warning_result( &
                    "No specific verification rule for claim type", &
                    component="completion_verification_framework" &
                )
                this%successful_validations = this%successful_validations + 1
                return
            end if
        end if

        ! Check rule requirements
        rule_check_result = this%rules(rule_index)%check_evidence_requirements(verification)
        if (rule_check_result%is_failure()) then
            validation_result = rule_check_result
            return
        end if

        ! Run fraud detection
        fraud_check_result = verification%detect_fraud_patterns()
        if (fraud_check_result%is_failure()) then
            this%fraud_detections = this%fraud_detections + 1
            this%framework_status = FRAMEWORK_FRAUD_DETECTION
            validation_result = critical_result( &
                "Fraud detected in completion claim", &
                component="completion_verification_framework", &
                context=verification%completion_claim, &
                suggestion="Investigate fraud patterns and provide legitimate evidence" &
            )
            return
        end if

        ! Validation successful
        this%successful_validations = this%successful_validations + 1
        validation_result = success_result()
    end function validate_completion_claim

    function run_automated_validation(this) result(automation_result)
        class(automated_validation_engine_t), intent(inout) :: this
        type(result_t) :: automation_result

        type(result_t) :: integrity_result, fraud_result

        if (this%framework_status == FRAMEWORK_EMERGENCY_STOP) then
            automation_result = create_error_result( &
                "Framework in emergency stop mode", &
                component="completion_verification_framework", &
                suggestion="Restart framework after addressing emergency conditions" &
            )
            return
        end if

        ! Validate all completions in integrity manager
        integrity_result = this%integrity_manager%validate_all_completions()
        if (integrity_result%is_failure()) then
            automation_result = integrity_result
            return
        end if

        ! Detect systematic fraud
        fraud_result = this%integrity_manager%detect_systematic_fraud()
        if (fraud_result%is_failure()) then
            this%framework_status = FRAMEWORK_FRAUD_DETECTION
            this%fraud_detections = this%fraud_detections + 1
            automation_result = fraud_result
            return
        end if

        automation_result = success_result()
    end function run_automated_validation

    function detect_completion_fraud(this) result(fraud_detection_result)
        class(automated_validation_engine_t), intent(inout) :: this
        type(result_t) :: fraud_detection_result

        integer :: fraud_threshold
        real :: fraud_rate

        if (this%total_validations == 0) then
            fraud_detection_result = success_result()
            return
        end if

        fraud_rate = real(this%fraud_detections) / real(this%total_validations)
        fraud_threshold = 10  ! 10% fraud rate threshold

        if (fraud_rate * 100.0 > real(fraud_threshold)) then
            this%framework_status = FRAMEWORK_FRAUD_DETECTION
            fraud_detection_result = critical_result( &
                "Systematic completion fraud detected", &
                component="completion_verification_framework", &
                suggestion="Emergency review required - investigate all recent completions" &
            )
        else
            fraud_detection_result = success_result()
        end if
    end function detect_completion_fraud

    function enforce_verification_gate(this, task_id, claim_type) result(gate_result)
        class(automated_validation_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: task_id
        integer, intent(in) :: claim_type
        type(result_t) :: gate_result

        type(result_t) :: enforcement_result
        integer :: required_types(2)

        if (this%framework_status /= FRAMEWORK_ACTIVE) then
            gate_result = create_error_result( &
                "Verification gate is not active", &
                component="completion_verification_framework", &
                suggestion="Activate framework before attempting completion" &
            )
            return
        end if

        ! Define basic required evidence types
        required_types(1) = EVIDENCE_CI_RUN
        required_types(2) = EVIDENCE_TEST_OUTPUT

        enforcement_result = this%integrity_manager%enforce_technical_evidence(task_id, required_types)
        if (enforcement_result%is_failure()) then
            gate_result = enforcement_result
        else
            gate_result = success_result()
        end if
    end function enforce_verification_gate

    function generate_validation_report(this) result(report)
        class(automated_validation_engine_t), intent(in) :: this
        character(len=:), allocatable :: report

        character(len=20) :: num_str
        real :: success_rate, fraud_rate

        if (this%total_validations > 0) then
            success_rate = real(this%successful_validations) / real(this%total_validations) * 100.0
            fraud_rate = real(this%fraud_detections) / real(this%total_validations) * 100.0
        else
            success_rate = 0.0
            fraud_rate = 0.0
        end if

        report = "=== COMPLETION VERIFICATION FRAMEWORK REPORT ===" // char(10)
        
        select case (this%framework_status)
        case (FRAMEWORK_INACTIVE)
            report = report // "Framework Status: INACTIVE" // char(10)
        case (FRAMEWORK_ACTIVE)
            report = report // "Framework Status: ACTIVE" // char(10)
        case (FRAMEWORK_FRAUD_DETECTION)
            report = report // "Framework Status: FRAUD DETECTION MODE" // char(10)
        case (FRAMEWORK_EMERGENCY_STOP)
            report = report // "Framework Status: EMERGENCY STOP" // char(10)
        end select

        write(num_str, '(I0)') this%total_validations
        report = report // "Total Validations: " // trim(num_str) // char(10)
        
        write(num_str, '(I0)') this%successful_validations
        report = report // "Successful Validations: " // trim(num_str) // char(10)
        
        write(num_str, '(F6.1)') success_rate
        report = report // "Success Rate: " // trim(num_str) // "%" // char(10)
        
        write(num_str, '(I0)') this%fraud_detections
        report = report // "Fraud Detections: " // trim(num_str) // char(10)
        
        write(num_str, '(F6.1)') fraud_rate
        report = report // "Fraud Rate: " // trim(num_str) // "%" // char(10)
        
        write(num_str, '(I0)') this%rule_count
        report = report // "Active Rules: " // trim(num_str) // char(10)
        
        if (this%strict_enforcement) then
            report = report // "Enforcement Mode: STRICT" // char(10)
        else
            report = report // "Enforcement Mode: PERMISSIVE" // char(10)
        end if

        ! Add integrity manager report
        report = report // char(10) // this%integrity_manager%generate_audit_report()
    end function generate_validation_report

    function get_framework_metrics(this) result(metrics)
        class(automated_validation_engine_t), intent(in) :: this
        character(len=:), allocatable :: metrics

        character(len=20) :: num_str

        write(num_str, '(I0)') this%total_validations
        metrics = "TotalValidations=" // trim(num_str)
        
        write(num_str, '(I0)') this%successful_validations
        metrics = metrics // ",SuccessfulValidations=" // trim(num_str)
        
        write(num_str, '(I0)') this%fraud_detections
        metrics = metrics // ",FraudDetections=" // trim(num_str)
        
        write(num_str, '(I0)') this%rule_count
        metrics = metrics // ",ActiveRules=" // trim(num_str)
        
        write(num_str, '(I0)') this%framework_status
        metrics = metrics // ",FrameworkStatus=" // trim(num_str)
    end function get_framework_metrics

    subroutine emergency_stop_framework(this, reason)
        class(automated_validation_engine_t), intent(inout) :: this
        character(len=*), intent(in) :: reason

        this%framework_status = FRAMEWORK_EMERGENCY_STOP
        ! In real implementation, would log emergency stop reason
    end subroutine emergency_stop_framework

    subroutine restart_framework(this)
        class(automated_validation_engine_t), intent(inout) :: this

        if (this%framework_status == FRAMEWORK_EMERGENCY_STOP .or. &
            this%framework_status == FRAMEWORK_FRAUD_DETECTION) then
            this%framework_status = FRAMEWORK_ACTIVE
        end if
    end subroutine restart_framework

    subroutine clear_engine(this)
        class(automated_validation_engine_t), intent(inout) :: this

        integer :: i

        if (allocated(this%rules)) then
            do i = 1, this%rule_count
                call this%rules(i)%clear_rule()
            end do
            deallocate(this%rules)
        end if

        call this%integrity_manager%clear_manager()
        
        this%rule_count = 0
        this%framework_status = FRAMEWORK_INACTIVE
        this%total_validations = 0
        this%successful_validations = 0
        this%fraud_detections = 0
        this%strict_enforcement = .true.
        
        if (allocated(this%audit_trail_path)) deallocate(this%audit_trail_path)
    end subroutine clear_engine

    subroutine cleanup_engine(this)
        type(automated_validation_engine_t), intent(inout) :: this
        call this%clear_engine()
    end subroutine cleanup_engine

    ! Add default rules to engine
    subroutine add_default_rules(this)
        class(automated_validation_engine_t), intent(inout) :: this

        type(verification_rule_t) :: implementation_rule, bug_fix_rule, feature_rule

        ! Implementation completion rule
        implementation_rule%claim_type = CLAIM_IMPLEMENTATION
        implementation_rule%required_evidence_types(1) = EVIDENCE_CI_RUN
        implementation_rule%required_evidence_types(2) = EVIDENCE_TEST_OUTPUT
        implementation_rule%required_evidence_types(3) = EVIDENCE_BUILD_SUCCESS
        implementation_rule%required_evidence_count = 3
        implementation_rule%minimum_ci_passes = 1
        implementation_rule%requires_manual_review = .false.
        implementation_rule%rule_description = "Code implementation requires CI, test, and build evidence"
        call this%add_verification_rule(implementation_rule)

        ! Bug fix completion rule
        bug_fix_rule%claim_type = CLAIM_BUG_FIX
        bug_fix_rule%required_evidence_types(1) = EVIDENCE_CI_RUN
        bug_fix_rule%required_evidence_types(2) = EVIDENCE_TEST_OUTPUT
        bug_fix_rule%required_evidence_count = 2
        bug_fix_rule%minimum_ci_passes = 1
        bug_fix_rule%requires_manual_review = .true.
        bug_fix_rule%rule_description = "Bug fixes require CI and test evidence with manual review"
        call this%add_verification_rule(bug_fix_rule)

        ! Feature completion rule
        feature_rule%claim_type = CLAIM_FEATURE_COMPLETE
        feature_rule%required_evidence_types(1) = EVIDENCE_CI_RUN
        feature_rule%required_evidence_types(2) = EVIDENCE_TEST_OUTPUT
        feature_rule%required_evidence_types(3) = EVIDENCE_BUILD_SUCCESS
        feature_rule%required_evidence_types(4) = EVIDENCE_PERFORMANCE_METRICS
        feature_rule%required_evidence_count = 4
        feature_rule%minimum_ci_passes = 2
        feature_rule%requires_manual_review = .true.
        feature_rule%rule_description = "Feature completion requires comprehensive evidence and review"
        call this%add_verification_rule(feature_rule)
    end subroutine add_default_rules

    ! Completion gate controller methods
    subroutine initialize_gate(this, strict_enforcement)
        class(completion_gate_controller_t), intent(inout) :: this
        logical, intent(in), optional :: strict_enforcement

        if (present(strict_enforcement)) then
            call this%validation_engine%initialize_engine(strict_enforcement=strict_enforcement)
        else
            call this%validation_engine%initialize_engine()
        end if

        this%gate_active = .true.
        this%allow_emergency_override = .false.
        this%consecutive_failures = 0
        this%gate_status_message = "Gate operational - ready for completion requests"
    end subroutine initialize_gate

    function process_completion_request(this, task_id, claim_type, verification) result(gate_decision)
        class(completion_gate_controller_t), intent(inout) :: this
        character(len=*), intent(in) :: task_id
        integer, intent(in) :: claim_type
        type(completion_verification_t), intent(inout) :: verification
        type(result_t) :: gate_decision

        type(result_t) :: validation_result

        if (.not. this%gate_active) then
            gate_decision = create_error_result( &
                "Completion gate is not active", &
                component="completion_gate_controller", &
                suggestion="Activate gate before processing completion requests" &
            )
            return
        end if

        ! Process through validation engine
        validation_result = this%validation_engine%validate_completion_claim(claim_type, verification)
        
        if (validation_result%is_failure()) then
            call this%handle_validation_failure()
            gate_decision = validation_result
        else
            call this%reset_failure_counter()
            gate_decision = success_result()
        end if
    end function process_completion_request

    function check_gate_status(this) result(status_result)
        class(completion_gate_controller_t), intent(in) :: this
        type(result_t) :: status_result

        if (.not. this%gate_active) then
            status_result = create_error_result( &
                "Completion gate is inactive", &
                component="completion_gate_controller", &
                context=this%gate_status_message, &
                suggestion="Activate gate to process completion requests" &
            )
        else if (this%consecutive_failures >= this%max_consecutive_failures) then
            status_result = critical_result( &
                "Gate blocked due to consecutive failures", &
                component="completion_gate_controller", &
                context=this%gate_status_message, &
                suggestion="Review failures and reset gate before continuing" &
            )
        else
            status_result = success_result()
        end if
    end function check_gate_status

    subroutine handle_validation_failure(this)
        class(completion_gate_controller_t), intent(inout) :: this

        this%consecutive_failures = this%consecutive_failures + 1
        
        if (this%consecutive_failures >= this%max_consecutive_failures) then
            this%gate_status_message = "Gate blocked - too many consecutive validation failures"
        else
            this%gate_status_message = "Validation failure recorded - gate remains operational"
        end if
    end subroutine handle_validation_failure

    subroutine reset_failure_counter(this)
        class(completion_gate_controller_t), intent(inout) :: this

        this%consecutive_failures = 0
        this%gate_status_message = "Gate operational - validation successful"
    end subroutine reset_failure_counter

    function emergency_gate_override(this, override_reason) result(override_result)
        class(completion_gate_controller_t), intent(inout) :: this
        character(len=*), intent(in) :: override_reason
        type(result_t) :: override_result

        if (.not. this%allow_emergency_override) then
            override_result = create_error_result( &
                "Emergency override not permitted", &
                component="completion_gate_controller", &
                suggestion="Enable emergency override capability first" &
            )
            return
        end if

        this%consecutive_failures = 0
        this%gate_status_message = "EMERGENCY OVERRIDE ACTIVE: " // trim(override_reason)
        override_result = warning_result( &
            "Emergency override activated", &
            component="completion_gate_controller", &
            context=override_reason &
        )
    end function emergency_gate_override

    function get_gate_metrics(this) result(metrics)
        class(completion_gate_controller_t), intent(in) :: this
        character(len=:), allocatable :: metrics

        character(len=20) :: num_str

        if (this%gate_active) then
            metrics = "GateActive=true"
        else
            metrics = "GateActive=false"
        end if

        write(num_str, '(I0)') this%consecutive_failures
        metrics = metrics // ",ConsecutiveFailures=" // trim(num_str)
        
        write(num_str, '(I0)') this%max_consecutive_failures
        metrics = metrics // ",MaxConsecutiveFailures=" // trim(num_str)
        
        if (this%allow_emergency_override) then
            metrics = metrics // ",EmergencyOverride=enabled"
        else
            metrics = metrics // ",EmergencyOverride=disabled"
        end if

        ! Add validation engine metrics
        metrics = metrics // "," // this%validation_engine%get_framework_metrics()
    end function get_gate_metrics

    subroutine clear_gate(this)
        class(completion_gate_controller_t), intent(inout) :: this

        call this%validation_engine%clear_engine()
        this%gate_active = .false.
        this%allow_emergency_override = .false.
        this%consecutive_failures = 0
        this%max_consecutive_failures = 3
        
        if (allocated(this%gate_status_message)) deallocate(this%gate_status_message)
    end subroutine clear_gate

    ! Factory functions
    function create_validation_engine(strict_enforcement, audit_trail_path) result(engine)
        logical, intent(in), optional :: strict_enforcement
        character(len=*), intent(in), optional :: audit_trail_path
        type(automated_validation_engine_t) :: engine

        if (present(strict_enforcement) .and. present(audit_trail_path)) then
            call engine%initialize_engine(audit_trail_path, strict_enforcement)
        else if (present(audit_trail_path)) then
            call engine%initialize_engine(audit_trail_path)
        else if (present(strict_enforcement)) then
            call engine%initialize_engine(strict_enforcement=strict_enforcement)
        else
            call engine%initialize_engine()
        end if
    end function create_validation_engine

    function create_verification_rule(claim_type, required_evidence_types, requires_manual_review, rule_description) result(rule)
        integer, intent(in) :: claim_type
        integer, intent(in) :: required_evidence_types(:)
        logical, intent(in), optional :: requires_manual_review
        character(len=*), intent(in), optional :: rule_description
        type(verification_rule_t) :: rule

        integer :: i, evidence_count

        rule%claim_type = claim_type
        
        evidence_count = min(size(required_evidence_types), 6)
        rule%required_evidence_count = evidence_count
        
        do i = 1, evidence_count
            rule%required_evidence_types(i) = required_evidence_types(i)
        end do

        if (present(requires_manual_review)) then
            rule%requires_manual_review = requires_manual_review
        end if

        if (present(rule_description)) then
            rule%rule_description = trim(rule_description)
        end if
    end function create_verification_rule

    function create_completion_gate(strict_enforcement) result(gate)
        logical, intent(in), optional :: strict_enforcement
        type(completion_gate_controller_t) :: gate

        if (present(strict_enforcement)) then
            call gate%initialize_gate(strict_enforcement)
        else
            call gate%initialize_gate()
        end if
    end function create_completion_gate

    ! Initialize default verification rules
    subroutine initialize_default_rules(engine)
        type(automated_validation_engine_t), intent(inout) :: engine

        call engine%add_default_rules()
    end subroutine initialize_default_rules

end module completion_verification_framework