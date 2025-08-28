module ast_node_accessors
    ! Type-safe accessor functions for AST nodes
    ! Provides safe access to node fields without exposing internal structure
    
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                       binary_op_node, function_def_node, identifier_node, &
                       literal_node, array_literal_node, call_or_subscript_node, &
                       subroutine_def_node, subroutine_call_node, declaration_node, &
                       parameter_declaration_node, interface_block_node, &
                       LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_ARRAY, LITERAL_COMPLEX, &
                       get_procedure_name, get_procedure_params, get_procedure_body, &
                       procedure_has_return_type, get_procedure_return_type
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, INTENT_IN, &
                             INTENT_OUT, INTENT_INOUT
    use type_system_unified, only: mono_type_t
    
    implicit none
    private
    
    ! Public accessor functions
    public :: get_assignment_indices, get_binary_op_info, get_identifier_name, &
              get_literal_value, get_call_info, get_array_literal_info, &
              get_program_info, get_declaration_info, get_parameter_declaration_info, &
              get_declaration_details, get_parameter_declaration_details
    
contains

    ! Assignment node accessors
    function get_assignment_indices(arena, node_index, target_index, &
                                     value_index, operator) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: target_index, value_index
        character(len=:), allocatable, intent(out) :: operator
        logical :: found
        
        found = .false.
        target_index = 0
        value_index = 0
        ! operator is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (assignment_node)
                    target_index = node%target_index
                    value_index = node%value_index
                    if (allocated(node%operator)) then
                        operator = node%operator
                    else
                        operator = "="
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_assignment_indices

    ! Binary operation node accessors
    function get_binary_op_info(arena, node_index, left_index, right_index, &
                                operator) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: left_index, right_index
        character(len=:), allocatable, intent(out) :: operator
        logical :: found
        
        found = .false.
        left_index = 0
        right_index = 0
        ! operator is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (binary_op_node)
                    left_index = node%left_index
                    right_index = node%right_index
                    if (allocated(node%operator)) then
                        operator = node%operator
                    else
                        operator = "UNKNOWN"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_binary_op_info

    ! Identifier node accessors
    function get_identifier_name(arena, node_index, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        logical :: found
        
        found = .false.
        ! name is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (identifier_node)
                    if (allocated(node%name)) then
                        name = node%name
                        found = .true.
                    end if
                end select
            end if
        end if
    end function get_identifier_name

    ! Literal node accessors
    function get_literal_value(arena, node_index, value, literal_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out) :: literal_type
        logical :: found
        
        found = .false.
        literal_type = 0
        ! value is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (literal_node)
                    if (allocated(node%value)) then
                        value = node%value
                        literal_type = node%literal_type
                        found = .true.
                    end if
                end select
            end if
        end if
    end function get_literal_value

    ! Call/subscript node accessors
    function get_call_info(arena, node_index, name, arg_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: arg_indices(:)
        logical :: found
        
        found = .false.
        ! name and arg_indices are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name)) then
                        name = node%name
                        if (allocated(node%arg_indices)) then
                            allocate(arg_indices(size(node%arg_indices)))
                            arg_indices = node%arg_indices
                        else
                            allocate(arg_indices(0))
                        end if
                        found = .true.
                    end if
                end select
            end if
        end if
    end function get_call_info

    ! Array literal accessors
    function get_array_literal_info(arena, node_index, element_indices, &
                                    literal_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: element_indices(:)
        integer, intent(out) :: literal_type
        logical :: found
        
        found = .false.
        literal_type = 0
        ! element_indices is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (array_literal_node)
                    literal_type = node%literal_type
                    if (allocated(node%element_indices)) then
                        allocate(element_indices(size(node%element_indices)))
                        element_indices = node%element_indices
                    else
                        allocate(element_indices(0))
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_array_literal_info

    ! Program node accessors
    function get_program_info(arena, node_index, name, body_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        logical :: found
        
        found = .false.
        ! name and body_indices are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (program_node)
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = "unnamed"
                    end if
                    if (allocated(node%body_indices)) then
                        allocate(body_indices(size(node%body_indices)))
                        body_indices = node%body_indices
                    else
                        allocate(body_indices(0))
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_program_info

    ! Declaration node accessors
    function get_declaration_info(arena, node_index, var_names, type_spec, &
                                  intent_type, allocatable_attr, optional_attr, &
                                  pointer_attr) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_spec
        integer, intent(out) :: intent_type
        logical, intent(out) :: allocatable_attr, optional_attr, pointer_attr
        logical :: found
        integer :: i
        
        found = .false.
        intent_type = INTENT_NONE
        allocatable_attr = .false.
        optional_attr = .false.
        pointer_attr = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (declaration_node)
                    if (allocated(node%var_names)) then
                        allocate(character(len=64) :: var_names(size(node%var_names)))
                        do i = 1, size(node%var_names)
                            if (allocated(node%var_names(i))) then
                                var_names(i) = node%var_names(i)
                            else
                                var_names(i) = ""
                            end if
                        end do
                    else
                        allocate(character(len=64) :: var_names(0))
                    end if
                    
                    if (allocated(node%type_spec)) then
                        type_spec = node%type_spec
                    else
                        type_spec = ""
                    end if
                    
                    intent_type = node%intent_type
                    allocatable_attr = node%allocatable_attr
                    optional_attr = node%optional_attr
                    pointer_attr = node%pointer_attr
                    found = .true.
                end select
            end if
        end if
    end function get_declaration_info

    ! Detailed declaration information with all attributes
    function get_declaration_details(arena, node_index, var_names, type_name, kind_value, has_kind, &
                                    array_spec, is_array, attributes) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_name
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind, is_array
        character(len=:), allocatable, intent(out) :: array_spec
        character(len=:), allocatable, intent(out) :: attributes(:)
        logical :: found
        integer :: i
        
        found = .false.
        kind_value = 0
        has_kind = .false.
        is_array = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (declaration_node)
                    ! Get variable names
                    if (allocated(node%var_names)) then
                        allocate(character(len=64) :: var_names(size(node%var_names)))
                        do i = 1, size(node%var_names)
                            if (allocated(node%var_names(i))) then
                                var_names(i) = node%var_names(i)
                            else
                                var_names(i) = ""
                            end if
                        end do
                    else
                        allocate(character(len=64) :: var_names(0))
                    end if
                    
                    ! Get type information
                    if (allocated(node%type_spec)) then
                        type_name = node%type_spec
                    else
                        type_name = ""
                    end if
                    
                    ! Get kind information
                    if (allocated(node%kind_spec)) then
                        read(node%kind_spec, *, iostat=i) kind_value
                        if (i == 0) then
                            has_kind = .true.
                        end if
                    end if
                    
                    ! Get array information
                    if (allocated(node%array_spec)) then
                        array_spec = node%array_spec
                        is_array = .true.
                    else
                        array_spec = ""
                    end if
                    
                    ! Build attributes list
                    call build_declaration_attributes(node, attributes)
                    
                    found = .true.
                end select
            end if
        end if
    end function get_declaration_details

    ! Parameter declaration accessors
    function get_parameter_declaration_info(arena, node_index, var_names, values, type_spec) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: values(:)
        character(len=:), allocatable, intent(out) :: type_spec
        logical :: found
        integer :: i
        
        found = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (parameter_declaration_node)
                    ! Get variable names
                    if (allocated(node%var_names)) then
                        allocate(character(len=64) :: var_names(size(node%var_names)))
                        do i = 1, size(node%var_names)
                            if (allocated(node%var_names(i))) then
                                var_names(i) = node%var_names(i)
                            else
                                var_names(i) = ""
                            end if
                        end do
                    else
                        allocate(character(len=64) :: var_names(0))
                    end if
                    
                    ! Get values
                    if (allocated(node%values)) then
                        allocate(character(len=64) :: values(size(node%values)))
                        do i = 1, size(node%values)
                            if (allocated(node%values(i))) then
                                values(i) = node%values(i)
                            else
                                values(i) = ""
                            end if
                        end do
                    else
                        allocate(character(len=64) :: values(0))
                    end if
                    
                    ! Get type specification
                    if (allocated(node%type_spec)) then
                        type_spec = node%type_spec
                    else
                        type_spec = ""
                    end if
                    
                    found = .true.
                end select
            end if
        end if
    end function get_parameter_declaration_info

    ! Detailed parameter declaration information
    function get_parameter_declaration_details(arena, node_index, name, type_name, kind_value, has_kind, &
                                              value, is_array, array_spec, attributes) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name, type_name, value, array_spec
        character(len=:), allocatable, intent(out) :: attributes(:)
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind, is_array
        logical :: found
        integer :: i
        
        found = .false.
        kind_value = 0
        has_kind = .false.
        is_array = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (parameter_declaration_node)
                    ! Get first variable name (assuming single parameter)
                    if (allocated(node%var_names) .and. size(node%var_names) > 0) then
                        if (allocated(node%var_names(1))) then
                            name = node%var_names(1)
                        else
                            name = ""
                        end if
                    else
                        name = ""
                    end if
                    
                    ! Get type information
                    if (allocated(node%type_spec)) then
                        type_name = node%type_spec
                    else
                        type_name = ""
                    end if
                    
                    ! Get value
                    if (allocated(node%values) .and. size(node%values) > 0) then
                        if (allocated(node%values(1))) then
                            value = node%values(1)
                        else
                            value = ""
                        end if
                    else
                        value = ""
                    end if
                    
                    ! Get kind information
                    if (allocated(node%kind_spec)) then
                        read(node%kind_spec, *, iostat=i) kind_value
                        if (i == 0) then
                            has_kind = .true.
                        end if
                    end if
                    
                    ! Get array information
                    if (allocated(node%array_spec)) then
                        array_spec = node%array_spec
                        is_array = .true.
                    else
                        array_spec = ""
                    end if
                    
                    ! Build attributes - parameter declarations have minimal attributes
                    allocate(character(len=16) :: attributes(1))
                    attributes(1) = "parameter"
                    
                    found = .true.
                end select
            end if
        end if
    end function get_parameter_declaration_details

    ! Helper function to convert integer to string
    function int_to_str(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str
        character(len=12) :: temp
        write(temp, '(I0)') int_val
        str = trim(temp)
    end function int_to_str

    ! Helper subroutine to build declaration attributes
    subroutine build_declaration_attributes(node, attributes)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: attributes(:)
        integer :: attr_count, idx
        
        ! Count attributes
        attr_count = 0
        if (node%intent_type /= INTENT_NONE) attr_count = attr_count + 1
        if (node%allocatable_attr) attr_count = attr_count + 1
        if (node%optional_attr) attr_count = attr_count + 1
        if (node%pointer_attr) attr_count = attr_count + 1
        if (allocated(node%array_spec)) attr_count = attr_count + 1
        if (allocated(node%kind_spec)) attr_count = attr_count + 1
        
        if (attr_count == 0) then
            allocate(character(len=16) :: attributes(0))
            return
        end if
        
        allocate(character(len=16) :: attributes(attr_count))
        
        idx = 1
        if (node%intent_type /= INTENT_NONE) then
            attributes(idx) = intent_type_to_string(node%intent_type)
            idx = idx + 1
        end if
        
        if (node%allocatable_attr) then
            attributes(idx) = "allocatable"
            idx = idx + 1
        end if
        
        if (node%optional_attr) then
            attributes(idx) = "optional"
            idx = idx + 1
        end if
        
        if (node%pointer_attr) then
            attributes(idx) = "pointer"
            idx = idx + 1
        end if
        
        if (allocated(node%array_spec)) then
            attributes(idx) = "dimension"
            idx = idx + 1
        end if
        
        if (allocated(node%kind_spec)) then
            attributes(idx) = "kind"
        end if
    end subroutine build_declaration_attributes

end module ast_node_accessors