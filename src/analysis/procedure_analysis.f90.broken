module procedure_analysis
    ! Procedure and parameter analysis functionality
    ! Provides APIs for analyzing procedures, parameters, and usage contexts
    
    use ast_core, only: ast_arena_t, identifier_node, assignment_node, function_def_node, &
                       subroutine_def_node, call_or_subscript_node, interface_block_node, &
                       get_procedure_name, get_procedure_params, get_procedure_body, &
                       procedure_has_return_type, get_procedure_return_type
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, INTENT_IN, &
                             INTENT_OUT, INTENT_INOUT
    use fortfront_types, only: function_signature_t
    
    implicit none
    private
    
    ! Public procedure analysis functions
    public :: count_procedure_parameters, get_parameter_intent, &
              get_parameter_type, is_parameter_optional, &
              get_procedure_signature, get_procedure_references, &
              is_procedure_used_in_generic, get_parameter_usage_context
    
contains

    ! Count parameters in a procedure
    function count_procedure_parameters(arena, node_index) result(param_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: param_count
        integer, allocatable :: param_indices(:)
        
        param_count = 0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                param_indices = get_procedure_params(arena, node_index)
                if (allocated(param_indices)) then
                    param_count = size(param_indices)
                end if
            end if
        end if
    end function count_procedure_parameters

    ! Get intent of a parameter in a procedure
    function get_parameter_intent(arena, node_index, param_index) result(intent_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, param_index
        character(len=:), allocatable :: intent_str
        integer, allocatable :: param_indices(:)
        integer :: param_node_idx
        
        intent_str = ""
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                param_indices = get_procedure_params(arena, node_index)
                if (allocated(param_indices) .and. param_index > 0 .and. &
                    param_index <= size(param_indices)) then
                    param_node_idx = param_indices(param_index)
                    
                    if (param_node_idx > 0 .and. param_node_idx <= arena%size) then
                        if (allocated(arena%entries(param_node_idx)%node)) then
                            select type (node => arena%entries(param_node_idx)%node)
                            type is (declaration_node)
                                intent_str = intent_type_to_string(node%intent_type)
                            end select
                        end if
                    end if
                end if
            end if
        end if
        
        if (.not. allocated(intent_str)) then
            intent_str = "none"
        end if
    end function get_parameter_intent

    ! Get type of a parameter in a procedure
    function get_parameter_type(arena, node_index, param_index) result(type_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, param_index
        character(len=:), allocatable :: type_str
        integer, allocatable :: param_indices(:)
        integer :: param_node_idx
        
        type_str = ""
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                param_indices = get_procedure_params(arena, node_index)
                if (allocated(param_indices) .and. param_index > 0 .and. &
                    param_index <= size(param_indices)) then
                    param_node_idx = param_indices(param_index)
                    
                    if (param_node_idx > 0 .and. param_node_idx <= arena%size) then
                        if (allocated(arena%entries(param_node_idx)%node)) then
                            select type (node => arena%entries(param_node_idx)%node)
                            type is (declaration_node)
                                if (allocated(node%type_spec)) then
                                    type_str = node%type_spec
                                end if
                            end select
                        end if
                    end if
                end if
            end if
        end if
        
        if (.not. allocated(type_str)) then
            type_str = "unknown"
        end if
    end function get_parameter_type

    ! Check if a parameter is optional
    function is_parameter_optional(arena, node_index, param_index) result(is_optional)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, param_index
        logical :: is_optional
        integer, allocatable :: param_indices(:)
        integer :: param_node_idx
        
        is_optional = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                param_indices = get_procedure_params(arena, node_index)
                if (allocated(param_indices) .and. param_index > 0 .and. &
                    param_index <= size(param_indices)) then
                    param_node_idx = param_indices(param_index)
                    
                    if (param_node_idx > 0 .and. param_node_idx <= arena%size) then
                        if (allocated(arena%entries(param_node_idx)%node)) then
                            select type (node => arena%entries(param_node_idx)%node)
                            type is (declaration_node)
                                is_optional = node%optional_attr
                            end select
                        end if
                    end if
                end if
            end if
        end if
    end function is_parameter_optional

    ! Get complete signature of a procedure
    function get_procedure_signature(arena, node_index) result(signature)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(function_signature_t) :: signature
        integer, allocatable :: param_indices(:)
        integer :: i, param_count
        
        ! Initialize signature
        signature%name = ""
        signature%is_intrinsic = .false.
        signature%return_type = ""
        
        if (node_index <= 0 .or. node_index > arena%size) then
            allocate(character(len=16) :: signature%parameter_names(0))
            allocate(character(len=16) :: signature%parameter_types(0))
            allocate(character(len=8) :: signature%parameter_intents(0))
            allocate(signature%parameter_optional(0))
            return
        end if
        
        if (.not. allocated(arena%entries(node_index)%node)) then
            allocate(character(len=16) :: signature%parameter_names(0))
            allocate(character(len=16) :: signature%parameter_types(0))
            allocate(character(len=8) :: signature%parameter_intents(0))
            allocate(signature%parameter_optional(0))
            return
        end if
        
        ! Get procedure name and parameters
        signature%name = get_procedure_name(arena, node_index)
        param_indices = get_procedure_params(arena, node_index)
        
        if (allocated(param_indices)) then
            param_count = size(param_indices)
        else
            param_count = 0
        end if
        
        ! Allocate parameter arrays
        allocate(character(len=64) :: signature%parameter_names(param_count))
        allocate(character(len=32) :: signature%parameter_types(param_count))
        allocate(character(len=8) :: signature%parameter_intents(param_count))
        allocate(signature%parameter_optional(param_count))
        
        ! Fill parameter information
        do i = 1, param_count
            signature%parameter_names(i) = get_param_name(arena, param_indices(i))
            signature%parameter_types(i) = get_parameter_type(arena, node_index, i)
            signature%parameter_intents(i) = get_parameter_intent(arena, node_index, i)
            signature%parameter_optional(i) = is_parameter_optional(arena, node_index, i)
        end do
        
        ! Get return type for functions
        if (procedure_has_return_type(arena, node_index)) then
            signature%return_type = get_procedure_return_type(arena, node_index)
        else
            signature%return_type = "void"
        end if
    end function get_procedure_signature

    ! Get all references to a procedure in the arena
    function get_procedure_references(arena, proc_name) result(reference_indices)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: proc_name
        integer, allocatable :: reference_indices(:)
        integer :: i, ref_count
        integer, allocatable :: temp_indices(:)
        
        ! First pass: count references
        ref_count = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name) .and. node%name == proc_name) then
                        ref_count = ref_count + 1
                    end if
                end select
            end if
        end do
        
        if (ref_count == 0) then
            allocate(reference_indices(0))
            return
        end if
        
        ! Second pass: collect references
        allocate(temp_indices(ref_count))
        ref_count = 0
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name) .and. node%name == proc_name) then
                        ref_count = ref_count + 1
                        temp_indices(ref_count) = i
                    end if
                end select
            end if
        end do
        
        ! Move to result
        call move_alloc(temp_indices, reference_indices)
    end function get_procedure_references

    ! Check if procedure is used in a generic interface
    function is_procedure_used_in_generic(arena, proc_name) result(is_used)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: proc_name
        logical :: is_used
        integer :: i
        
        is_used = .false.
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (interface_block_node)
                    if (interface_contains_procedure(node, proc_name)) then
                        is_used = .true.
                        return
                    end if
                end select
            end if
        end do
    end function is_procedure_used_in_generic

    ! Get usage context information for a parameter
    function get_parameter_usage_context(arena, param_name) result(context_info)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: param_name
        character(len=:), allocatable :: context_info
        
        integer :: i, usage_count, definition_count
        character(len=256) :: temp_info
        
        usage_count = 0
        definition_count = 0
        
        ! Count parameter usages and definitions
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            
            select type (node => arena%entries(i)%node)
            type is (identifier_node)
                if (allocated(node%name) .and. node%name == param_name) then
                    usage_count = usage_count + 1
                end if
            type is (assignment_node)
                ! Check if parameter is being assigned to (definition)
                if (node%target_index > 0 .and. node%target_index <= arena%size) then
                    if (allocated(arena%entries(node%target_index)%node)) then
                        select type (target => arena%entries(node%target_index)%node)
                        type is (identifier_node)
                            if (allocated(target%name) .and. target%name == param_name) then
                                definition_count = definition_count + 1
                            end if
                        end select
                    end if
                end if
            end select
        end do
        
        write(temp_info, '(A,I0,A,I0,A)') "Parameter '", usage_count, "' usages, '", definition_count, "' definitions"
        context_info = trim(temp_info)
    end function get_parameter_usage_context

    ! Helper functions
    
    function get_param_name(arena, param_node_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_node_index
        character(len=:), allocatable :: name
        
        name = ""
        
        if (param_node_index > 0 .and. param_node_index <= arena%size) then
            if (allocated(arena%entries(param_node_index)%node)) then
                select type (node => arena%entries(param_node_index)%node)
                type is (declaration_node)
                    if (allocated(node%var_names) .and. size(node%var_names) > 0) then
                        if (allocated(node%var_names(1))) then
                            name = node%var_names(1)
                        end if
                    end if
                type is (identifier_node)
                    if (allocated(node%name)) then
                        name = node%name
                    end if
                end select
            end if
        end if
        
        if (.not. allocated(name)) then
            name = "unnamed"
        end if
    end function get_param_name

    function interface_contains_procedure(interface_node, proc_name) result(contains)
        type(interface_block_node), intent(in) :: interface_node
        character(len=*), intent(in) :: proc_name
        logical :: contains
        integer :: i
        
        contains = .false.
        
        if (allocated(interface_node%procedure_names)) then
            do i = 1, size(interface_node%procedure_names)
                if (allocated(interface_node%procedure_names(i))) then
                    if (interface_node%procedure_names(i) == proc_name) then
                        contains = .true.
                        return
                    end if
                end if
            end do
        end if
    end function interface_contains_procedure

end module procedure_analysis