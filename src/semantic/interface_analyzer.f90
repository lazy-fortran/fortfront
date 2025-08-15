module interface_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_data, only: declaration_node
    implicit none
    private

    public :: interface_analyzer_t, interface_signature_t, parameter_info_t
    public :: interface_comparison_result_t, parameters_compatible

    ! Parameter information
    type :: parameter_info_t
        character(:), allocatable :: name
        character(:), allocatable :: type_spec
        character(:), allocatable :: intent  ! in, out, inout
        logical :: is_optional = .false.
        integer :: source_location = 0
    end type

    ! Interface signature
    type :: interface_signature_t
        character(:), allocatable :: name
        character(:), allocatable :: return_type
        type(parameter_info_t), allocatable :: parameters(:)
        logical :: is_function = .false.
        logical :: is_pure = .false.
        logical :: is_elemental = .false.
        logical :: is_recursive = .false.
        integer :: source_location = 0
        integer :: parameter_count = 0
    end type

    ! Interface comparison result
    type :: interface_comparison_result_t
        type(interface_signature_t), allocatable :: signatures(:)
        character(:), allocatable :: mismatched_procedures(:)
        integer, allocatable :: mismatch_locations(:)
        integer :: signature_count = 0
        integer :: mismatch_count = 0
    end type

    ! Interface analyzer plugin
    type, extends(semantic_analyzer_t) :: interface_analyzer_t
        type(interface_comparison_result_t) :: result
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_interfaces
        procedure :: get_results => get_interface_results
        procedure :: get_name => get_interface_analyzer_name
        procedure :: assign => assign_interface_analyzer
        
        ! Analysis methods for fluff rules
        procedure :: extract_interface_signature
        procedure :: compare_interfaces
        procedure :: find_interface_mismatches
        procedure :: check_parameter_consistency
        procedure :: validate_procedure_attributes
    end type

contains

    subroutine analyze_interfaces(this, shared_context, arena, node_index)
        class(interface_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Extract interface signatures from procedures
        call extract_all_signatures(this%result, arena, node_index)
        
        ! Find interface mismatches
        call find_signature_mismatches(this%result)
        
        this%analysis_complete = .true.
    end subroutine

    function get_interface_results(this) result(results)
        class(interface_analyzer_t), intent(in) :: this
        class(*), allocatable :: results
        
        ! Return the interface analysis result
        allocate(interface_comparison_result_t :: results)
        select type(results)
        type is (interface_comparison_result_t)
            results = this%result
        end select
    end function

    function get_interface_analyzer_name(this) result(name)
        class(interface_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "interface_analyzer"
    end function

    subroutine assign_interface_analyzer(lhs, rhs)
        use semantic_analyzer_base, only: semantic_analyzer_t
        class(interface_analyzer_t), intent(inout) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (interface_analyzer_t)
            ! Deep copy the result
            lhs%result = rhs%result
            lhs%analysis_complete = rhs%analysis_complete
        class default
            error stop "Type mismatch in interface_analyzer assignment"
        end select
    end subroutine

    ! Analysis methods for fluff rules
    function extract_interface_signature(this, proc_node_index, arena) result(signature)
        class(interface_analyzer_t), intent(in) :: this
        integer, intent(in) :: proc_node_index
        type(ast_arena_t), intent(in) :: arena
        type(interface_signature_t) :: signature
        
        ! Extract signature from function or subroutine node
        call extract_signature_from_node(signature, arena, proc_node_index)
    end function

    function compare_interfaces(this, sig1, sig2) result(are_compatible)
        class(interface_analyzer_t), intent(in) :: this
        type(interface_signature_t), intent(in) :: sig1, sig2
        logical :: are_compatible
        
        integer :: i
        
        are_compatible = .false.
        
        ! Check basic compatibility
        if (sig1%name /= sig2%name) return
        if (sig1%is_function .neqv. sig2%is_function) return
        if (sig1%parameter_count /= sig2%parameter_count) return
        
        ! Check return types for functions
        if (sig1%is_function) then
            if (sig1%return_type /= sig2%return_type) return
        end if
        
        ! Check parameter compatibility
        if (allocated(sig1%parameters) .and. allocated(sig2%parameters)) then
            do i = 1, sig1%parameter_count
                if (.not. parameters_compatible(sig1%parameters(i), sig2%parameters(i))) then
                    return
                end if
            end do
        end if
        
        are_compatible = .true.
    end function

    function find_interface_mismatches(this) result(mismatches)
        class(interface_analyzer_t), intent(in) :: this
        character(:), allocatable :: mismatches(:)
        
        if (.not. this%analysis_complete) then
            allocate(character(0) :: mismatches(0))
            return
        end if
        
        mismatches = this%result%mismatched_procedures
    end function

    function check_parameter_consistency(this, proc_name) result(consistent)
        class(interface_analyzer_t), intent(in) :: this
        character(*), intent(in) :: proc_name
        logical :: consistent
        
        integer :: i, j
        
        if (.not. this%analysis_complete) then
            consistent = .true.
            return
        end if
        
        consistent = .true.
        ! Check if this procedure appears in mismatch list
        if (allocated(this%result%mismatched_procedures)) then
            do i = 1, size(this%result%mismatched_procedures)
                if (this%result%mismatched_procedures(i) == proc_name) then
                    consistent = .false.
                    exit
                end if
            end do
        end if
    end function

    function validate_procedure_attributes(this, proc_name) result(valid)
        class(interface_analyzer_t), intent(in) :: this
        character(*), intent(in) :: proc_name
        logical :: valid
        
        integer :: i
        type(interface_signature_t) :: signature
        
        if (.not. this%analysis_complete) then
            valid = .true.
            return
        end if
        
        valid = .true.
        ! Find procedure signature
        do i = 1, this%result%signature_count
            if (this%result%signatures(i)%name == proc_name) then
                signature = this%result%signatures(i)
                
                ! Check attribute consistency
                ! (e.g., PURE procedures shouldn't have side effects)
                if (signature%is_pure .and. signature%is_elemental) then
                    ! This combination might need validation
                end if
                
                exit
            end if
        end do
    end function

    ! Helper subroutines
    subroutine extract_all_signatures(result, arena, root_index)
        type(interface_comparison_result_t), intent(inout) :: result
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        integer :: i, signature_count
        type(interface_signature_t), allocatable :: temp_signatures(:)
        
        ! Count procedures in arena
        signature_count = 0
        do i = 1, arena%size
            if (is_procedure_node_type(arena, i)) then
                signature_count = signature_count + 1
            end if
        end do
        
        if (signature_count == 0) then
            result%signature_count = 0
            return
        end if
        
        ! Extract signatures
        allocate(temp_signatures(signature_count))
        signature_count = 0
        
        do i = 1, arena%size
            if (is_procedure_node_type(arena, i)) then
                signature_count = signature_count + 1
                call extract_signature_from_node(temp_signatures(signature_count), arena, i)
            end if
        end do
        
        ! Store results
        allocate(result%signatures(signature_count))
        result%signatures(1:signature_count) = temp_signatures(1:signature_count)
        result%signature_count = signature_count
    end subroutine

    subroutine extract_signature_from_node(signature, arena, node_index)
        use ast_nodes_data, only: parameter_declaration_node, INTENT_IN, INTENT_OUT, &
                                  INTENT_INOUT, INTENT_NONE
        type(interface_signature_t), intent(out) :: signature
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Initialize signature
        signature%source_location = node_index
        
        ! Extract complete signature information from AST nodes
        select type (node => arena%entries(node_index)%node)
        class is (function_def_node)
            signature%is_function = .true.
            if (allocated(node%name)) then
                signature%name = node%name
            else
                signature%name = "unnamed_function"
            end if
            
            ! Extract return type
            if (allocated(node%return_type)) then
                signature%return_type = node%return_type
            else
                signature%return_type = "unknown"
            end if
            
            ! Extract parameters from param_indices
            if (allocated(node%param_indices)) then
                signature%parameter_count = size(node%param_indices)
                if (signature%parameter_count > 0) then
                    allocate(signature%parameters(signature%parameter_count))
                    call extract_parameters_from_indices(signature%parameters, &
                                                        arena, node%param_indices)
                end if
            else
                signature%parameter_count = 0
            end if
            
        class is (subroutine_def_node)
            signature%is_function = .false.
            if (allocated(node%name)) then
                signature%name = node%name
            else
                signature%name = "unnamed_subroutine"
            end if
            
            ! Extract parameters from param_indices
            if (allocated(node%param_indices)) then
                signature%parameter_count = size(node%param_indices)
                if (signature%parameter_count > 0) then
                    allocate(signature%parameters(signature%parameter_count))
                    call extract_parameters_from_indices(signature%parameters, &
                                                        arena, node%param_indices)
                end if
            else
                signature%parameter_count = 0
            end if
            
        class default
            signature%name = "unknown_procedure"
            signature%is_function = .false.
            signature%parameter_count = 0
        end select
    end subroutine

    ! Extract parameter details from parameter declaration nodes
    subroutine extract_parameters_from_indices(parameters, arena, param_indices)
        use ast_nodes_data, only: parameter_declaration_node, declaration_node, &
                                  INTENT_IN, INTENT_OUT, INTENT_INOUT, INTENT_NONE
        type(parameter_info_t), intent(out) :: parameters(:)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        
        integer :: i, param_idx
        
        do i = 1, size(param_indices)
            param_idx = param_indices(i)
            
            if (param_idx > 0 .and. param_idx <= arena%size) then
                if (allocated(arena%entries(param_idx)%node)) then
                    call extract_single_parameter(parameters(i), arena, param_idx)
                end if
            end if
        end do
    end subroutine

    ! Extract information from a single parameter node
    subroutine extract_single_parameter(param_info, arena, node_index)
        use ast_nodes_data, only: parameter_declaration_node, declaration_node, &
                                  INTENT_IN, INTENT_OUT, INTENT_INOUT, INTENT_NONE
        type(parameter_info_t), intent(out) :: param_info
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Initialize parameter info
        param_info%source_location = node_index
        param_info%is_optional = .false.
        
        select type (node => arena%entries(node_index)%node)
        class is (parameter_declaration_node)
            ! Extract from parameter_declaration_node
            if (allocated(node%name)) then
                param_info%name = node%name
            else
                param_info%name = "unknown_param"
            end if
            
            if (allocated(node%type_name)) then
                if (node%has_kind .and. node%kind_value > 0) then
                    ! Create type string with kind
                    block
                        character(len=100) :: temp_str
                        write(temp_str, '(A,"(kind=",I0,")")') &
                            trim(node%type_name), node%kind_value
                        param_info%type_spec = trim(temp_str)
                    end block
                else
                    param_info%type_spec = node%type_name
                end if
            else
                param_info%type_spec = "unknown"
            end if
            
            ! Extract intent
            select case (node%intent_type)
            case (INTENT_IN)
                param_info%intent = "in"
            case (INTENT_OUT)
                param_info%intent = "out"
            case (INTENT_INOUT)
                param_info%intent = "inout"
            case default
                param_info%intent = "none"
            end select
            
            param_info%is_optional = node%is_optional
            
        class is (declaration_node)
            ! Extract from declaration_node (for older parsing)
            if (allocated(node%var_name)) then
                param_info%name = node%var_name
            else
                param_info%name = "unknown_param"
            end if
            
            if (allocated(node%type_name)) then
                if (node%has_kind .and. node%kind_value > 0) then
                    block
                        character(len=100) :: temp_str
                        write(temp_str, '(A,"(kind=",I0,")")') &
                            trim(node%type_name), node%kind_value
                        param_info%type_spec = trim(temp_str)
                    end block
                else
                    param_info%type_spec = node%type_name
                end if
            else
                param_info%type_spec = "unknown"
            end if
            
            ! Extract intent from string
            if (allocated(node%intent)) then
                param_info%intent = node%intent
            else
                param_info%intent = "none"
            end if
            
            param_info%is_optional = node%is_optional
            
        class default
            ! Fallback for unknown node types
            param_info%name = "unknown_param"
            param_info%type_spec = "unknown"
            param_info%intent = "none"
            param_info%is_optional = .false.
        end select
    end subroutine

    subroutine find_signature_mismatches(result)
        type(interface_comparison_result_t), intent(inout) :: result
        
        integer :: i, j, mismatch_count
        character(:), allocatable :: temp_mismatches(:)
        integer, allocatable :: temp_locations(:)
        
        if (result%signature_count <= 1) then
            result%mismatch_count = 0
            return
        end if
        
        ! Look for procedures with same name but different signatures
        mismatch_count = 0
        allocate(character(len=256) :: temp_mismatches(result%signature_count))
        allocate(temp_locations(result%signature_count))
        
        do i = 1, result%signature_count - 1
            do j = i + 1, result%signature_count
                if (result%signatures(i)%name == result%signatures(j)%name) then
                    ! Same name - check if signatures match
                    if (.not. signatures_match(result%signatures(i), result%signatures(j))) then
                        mismatch_count = mismatch_count + 1
                        temp_mismatches(mismatch_count) = result%signatures(i)%name
                        temp_locations(mismatch_count) = result%signatures(i)%source_location
                    end if
                end if
            end do
        end do
        
        if (mismatch_count > 0) then
            allocate(character(len=256) :: result%mismatched_procedures(mismatch_count))
            allocate(result%mismatch_locations(mismatch_count))
            result%mismatched_procedures(1:mismatch_count) = temp_mismatches(1:mismatch_count)
            result%mismatch_locations(1:mismatch_count) = temp_locations(1:mismatch_count)
        else
            allocate(character(0) :: result%mismatched_procedures(0))
            allocate(result%mismatch_locations(0))
        end if
        
        result%mismatch_count = mismatch_count
    end subroutine

    ! Helper functions
    function is_procedure_node_type(arena, node_index) result(is_proc)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_proc
        
        is_proc = .false.
        if (node_index <= 0 .or. node_index > arena%size) return
        
        select type (node => arena%entries(node_index)%node)
        class is (function_def_node)
            is_proc = .true.
        class is (subroutine_def_node)
            is_proc = .true.
        end select
    end function

    function parameters_compatible(param1, param2) result(compatible)
        type(parameter_info_t), intent(in) :: param1, param2
        logical :: compatible
        
        compatible = .true.
        
        ! Check type compatibility
        if (allocated(param1%type_spec) .and. allocated(param2%type_spec)) then
            if (param1%type_spec /= param2%type_spec) then
                compatible = .false.
                return
            end if
        end if
        
        ! Check intent compatibility
        if (allocated(param1%intent) .and. allocated(param2%intent)) then
            if (param1%intent /= param2%intent) then
                compatible = .false.
                return
            end if
        end if
        
        ! Check optional compatibility
        if (param1%is_optional .neqv. param2%is_optional) then
            compatible = .false.
        end if
    end function

    function signatures_match(sig1, sig2) result(match)
        type(interface_signature_t), intent(in) :: sig1, sig2
        logical :: match
        
        integer :: i
        
        match = .false.
        
        ! Basic checks
        if (sig1%is_function .neqv. sig2%is_function) return
        if (sig1%parameter_count /= sig2%parameter_count) return
        
        ! Check return types for functions
        if (sig1%is_function) then
            if (allocated(sig1%return_type) .and. allocated(sig2%return_type)) then
                if (sig1%return_type /= sig2%return_type) return
            end if
        end if
        
        ! Check all parameters
        if (allocated(sig1%parameters) .and. allocated(sig2%parameters)) then
            do i = 1, sig1%parameter_count
                if (.not. parameters_compatible(sig1%parameters(i), sig2%parameters(i))) then
                    return
                end if
            end do
        end if
        
        match = .true.
    end function

end module interface_analyzer