module ast_factory
    use ast_core
    implicit none
    private

    ! Public interface for creating AST nodes in stack-based system
    public :: push_program, push_assignment, push_pointer_assignment, push_binary_op
   public :: push_call_or_subscript, push_subroutine_call, push_identifier, &
             push_literal, push_array_literal
    public :: push_derived_type, push_declaration, push_multi_declaration, &
              push_parameter_declaration
    public :: push_if, push_do_loop, push_do_while, push_forall, push_select_case
    public :: push_case_block, push_case_range, push_case_default, &
              push_select_case_with_default
    public :: push_use_statement, push_include_statement, push_print_statement, &
              push_write_statement, push_read_statement, push_read_statement_with_err, &
              push_read_statement_with_end, push_read_statement_with_all_specifiers, &
              push_write_statement_with_iostat, push_write_statement_with_format, &
              push_write_statement_with_runtime_format
    public :: push_function_def, push_subroutine_def, push_interface_block, push_module
    public :: push_stop, push_return
    public :: push_cycle, push_exit
    public :: push_where, push_where_construct, push_where_construct_with_elsewhere
    public :: push_type_constructor, push_component_access
    public :: push_complex_literal
    public :: push_allocate, push_deallocate
    public :: push_array_section
    public :: build_ast_from_nodes

contains

    ! Create program node and add to stack
    function push_program(arena, name, body_indices, line, column) result(prog_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        integer :: prog_index
        type(program_node) :: prog

        prog = create_program(name, body_indices, line, column)
        call arena%push(prog, "program", 0)
        prog_index = arena%size
    end function push_program

    ! Create assignment node and add to stack
    function push_assignment(arena, target_index, value_index, line, column, &
                            parent_index) result(assign_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: assign_index
        type(assignment_node) :: assign

        assign = create_assignment(target_index, value_index, line, column)
        call arena%push(assign, "assignment", parent_index)
        assign_index = arena%size
    end function push_assignment

    ! Create pointer assignment node and add to stack
    function push_pointer_assignment(arena, pointer_index, target_index, &
                                     line, column, parent_index) &
            result(assign_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: pointer_index
        integer, intent(in) :: target_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: assign_index
        type(pointer_assignment_node) :: ptr_assign

        ptr_assign = create_pointer_assignment(pointer_index, target_index, &
                                               line, column)
        call arena%push(ptr_assign, "pointer_assignment", parent_index)
        assign_index = arena%size
    end function push_pointer_assignment

    ! Create binary operation node and add to stack
    function push_binary_op(arena, left_index, right_index, operator, &
                           line, column, parent_index) result(binop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column, parent_index
        integer :: binop_index
        type(binary_op_node) :: binop

        binop = create_binary_op(left_index, right_index, operator, line, column)
        call arena%push(binop, "binary_op", parent_index)
        binop_index = arena%size
    end function push_binary_op

    ! Create call_or_subscript node and add to stack
    function push_call_or_subscript(arena, name, arg_indices, line, column, &
                                   parent_index) result(call_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(call_or_subscript_node) :: call_node

        call_node = create_call_or_subscript(name, arg_indices, line, column)
        call arena%push(call_node, "call_or_subscript", parent_index)
        call_index = arena%size
    end function push_call_or_subscript

    ! Create subroutine call node and add to stack
    function push_subroutine_call(arena, name, arg_indices, line, column, &
                                 parent_index) result(call_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(subroutine_call_node) :: call_node

        call_node = create_subroutine_call(name, arg_indices, line, column)
        call arena%push(call_node, "subroutine_call", parent_index)
        call_index = arena%size
    end function push_subroutine_call

    ! Create identifier node and add to stack
    function push_identifier(arena, name, line, column, parent_index) result(id_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column, parent_index
        integer :: id_index
        type(identifier_node) :: id

        id = create_identifier(name, line, column)
        call arena%push(id, "identifier", parent_index)
        id_index = arena%size
    end function push_identifier

    ! Create literal node and add to stack
 function push_literal(arena, value, kind, line, column, parent_index) result(lit_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column, parent_index
        integer :: lit_index
        type(literal_node) :: lit

        lit = create_literal(value, kind, line, column)
        call arena%push(lit, "literal", parent_index)
        lit_index = arena%size
    end function push_literal

    ! Create array literal node and add to stack
    function push_array_literal(arena, element_indices, line, column, &
                               parent_index) result(array_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: element_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: array_index
        type(array_literal_node) :: array_lit
        array_lit = create_array_literal(element_indices, line, column)
        call arena%push(array_lit, "array_literal", parent_index)
        array_index = arena%size

    end function push_array_literal

    ! Create derived type node and add to stack
    function push_derived_type(arena, name, component_indices, param_indices, &
                               line, column, parent_index) result(type_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: component_indices(:)
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: type_index
        type(derived_type_node) :: dtype

        ! Create derived type with index-based components
        dtype%name = name

        if (present(component_indices)) then
            if (size(component_indices) > 0) then
                allocate (dtype%component_indices, source=component_indices)
            end if
        end if

        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                dtype%has_parameters = .true.
                allocate (dtype%param_indices, source=param_indices)
            end if
        end if

        if (present(line)) dtype%line = line
        if (present(column)) dtype%column = column

        call arena%push(dtype, "derived_type", parent_index)
        type_index = arena%size

    end function push_derived_type

    ! Create declaration node and add to stack
  function push_declaration(arena, type_name, var_name, kind_value, &
       dimension_indices, &
       initializer_index, is_allocatable, is_pointer, intent_value, line, column, &
       parent_index) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name, var_name
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer  
        character(len=*), intent(in), optional :: intent_value
        integer, intent(in), optional :: line, column, parent_index
        integer :: decl_index
        type(declaration_node) :: decl

        ! Create declaration with index-based fields
        decl%type_name = type_name
        decl%var_name = var_name

        if (present(kind_value)) then
            decl%kind_value = kind_value
            decl%has_kind = .true.
        else
            decl%kind_value = 0
            decl%has_kind = .false.
        end if

        if (present(initializer_index)) then
            decl%initializer_index = initializer_index
            decl%has_initializer = .true.
        else
            decl%initializer_index = 0
            decl%has_initializer = .false.
        end if

        if (present(dimension_indices)) then
            decl%is_array = .true.
            allocate (decl%dimension_indices, source=dimension_indices)
        else
            decl%is_array = .false.
        end if

        if (present(is_allocatable)) then
            decl%is_allocatable = is_allocatable
        else
            decl%is_allocatable = .false.
        end if

        if (present(is_pointer)) then
            decl%is_pointer = is_pointer
        else
            decl%is_pointer = .false.
        end if

        if (present(intent_value)) then
            decl%intent = intent_value
            decl%has_intent = .true.
        else
            decl%has_intent = .false.
        end if

        if (present(line)) decl%line = line
        if (present(column)) decl%column = column

        call arena%push(decl, "declaration", parent_index)
        decl_index = arena%size

    end function push_declaration

    ! Create multi-variable declaration node and add to stack
    function push_multi_declaration(arena, type_name, var_names, kind_value, &
           dimension_indices, &
           initializer_index, is_allocatable, is_pointer, intent_value, &
           line, column, parent_index) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_names(:)
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        character(len=*), intent(in), optional :: intent_value
        integer, intent(in), optional :: line, column, parent_index
        integer :: decl_index
        type(declaration_node) :: decl
        integer :: i

        ! Create multi-variable declaration
        decl%type_name = type_name
        decl%is_multi_declaration = .true.
        
        ! Allocate and copy variable names
        allocate(character(len=100) :: decl%var_names(size(var_names)))
        do i = 1, size(var_names)
            decl%var_names(i) = trim(var_names(i))
        end do

        if (present(kind_value)) then
            decl%kind_value = kind_value
            decl%has_kind = .true.
        else
            decl%kind_value = 0
            decl%has_kind = .false.
        end if

        if (present(initializer_index)) then
            decl%initializer_index = initializer_index
            decl%has_initializer = .true.
        else
            decl%initializer_index = 0
            decl%has_initializer = .false.
        end if

        if (present(dimension_indices)) then
            decl%is_array = .true.
            allocate (decl%dimension_indices, source=dimension_indices)
        else
            decl%is_array = .false.
        end if

        if (present(is_allocatable)) then
            decl%is_allocatable = is_allocatable
        else
            decl%is_allocatable = .false.
        end if

        if (present(is_pointer)) then
            decl%is_pointer = is_pointer
        else
            decl%is_pointer = .false.
        end if

        if (present(intent_value)) then
            decl%intent = intent_value
            decl%has_intent = .true.
        else
            decl%has_intent = .false.
        end if

        if (present(line)) decl%line = line
        if (present(column)) decl%column = column

        call arena%push(decl, "multi_declaration", parent_index)
        decl_index = arena%size

    end function push_multi_declaration

    ! Create parameter declaration node and add to stack
 function push_parameter_declaration(arena, name, type_name, kind_value, intent_value, &
                      dimension_indices, line, column, parent_index) result(param_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name, type_name
        integer, intent(in), optional :: kind_value, intent_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: param_index
        type(parameter_declaration_node) :: param

        param%name = name
        param%type_name = type_name

        if (present(kind_value) .and. kind_value > 0) then
            param%kind_value = kind_value
        else
            param%kind_value = 0
        end if

        if (present(intent_value)) then
            select case (intent_value)
            case (1)
                param%intent = "in"
            case (2)
                param%intent = "out"
            case (3)
                param%intent = "inout"
            case default
                param%intent = ""
            end select
        else
            param%intent = ""
        end if

        ! Handle array dimensions
        if (present(dimension_indices)) then
            if (size(dimension_indices) > 0) then
                param%is_array = .true.
                allocate (param%dimension_indices, source=dimension_indices)
            else
                param%is_array = .false.
            end if
        else
            param%is_array = .false.
        end if

        if (present(line)) param%line = line
        if (present(column)) param%column = column

        call arena%push(param, "parameter_declaration", parent_index)
        param_index = arena%size

    end function push_parameter_declaration

    ! Create if statement node and add to stack
    function push_if(arena, condition_index, then_body_indices, elseif_indices, &
                    else_body_indices, &
                     line, column, parent_index) result(if_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        integer, intent(in), optional :: elseif_indices(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: if_index
        type(if_node) :: if_stmt
        integer :: i

        ! Set condition index
        if (condition_index > 0 .and. condition_index <= arena%size) then
            if_stmt%condition_index = condition_index
        end if

        ! Set then body indices
        if (present(then_body_indices)) then
            if (size(then_body_indices) > 0) then
                if_stmt%then_body_indices = then_body_indices
            end if
        end if

        ! Set else body indices
        if (present(else_body_indices)) then
            if (size(else_body_indices) > 0) then
                if_stmt%else_body_indices = else_body_indices
            end if
        end if

        ! Handle elseif blocks
        if (present(elseif_indices)) then
            if (size(elseif_indices) > 0) then
                ! For now, treat elseif_indices as pairs: &
                ! condition, body, condition, body, ...
                ! Each pair becomes one elseif_wrapper
                if (mod(size(elseif_indices), 2) == 0) then
                    allocate (if_stmt%elseif_blocks(size(elseif_indices)/2))
                    do i = 1, size(elseif_indices)/2
                      if_stmt%elseif_blocks(i)%condition_index = elseif_indices(2*i - 1)
                        if_stmt%elseif_blocks(i)%body_indices = [elseif_indices(2*i)]
                    end do
                end if
            end if
        end if

        if (present(line)) if_stmt%line = line
        if (present(column)) if_stmt%column = column

        call arena%push(if_stmt, "if_statement", parent_index)
        if_index = arena%size

    end function push_if

    ! Create do loop node and add to stack
    function push_do_loop(arena, var_name, start_index, end_index, step_index, &
                         body_indices, &
                          loop_label, line, column, parent_index) result(loop_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index
        integer, intent(in), optional :: body_indices(:)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: loop_index
        type(do_loop_node) :: loop_node
        integer :: i

        loop_node%var_name = var_name
        if (present(loop_label)) loop_node%label = loop_label

        ! Set start and end expression indices
        if (start_index > 0 .and. start_index <= arena%size) then
            loop_node%start_expr_index = start_index
        end if

        if (end_index > 0 .and. end_index <= arena%size) then
            loop_node%end_expr_index = end_index
        end if

        ! Set optional step expression index
        if (present(step_index)) then
            if (step_index > 0) then
                loop_node%step_expr_index = step_index
            end if
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                loop_node%body_indices = body_indices
            end if
        end if

        if (present(line)) loop_node%line = line
        if (present(column)) loop_node%column = column

        call arena%push(loop_node, "do_loop", parent_index)
        loop_index = arena%size

    end function push_do_loop

    ! Create do while loop node and add to stack
    function push_do_while(arena, condition_index, body_indices, line, column, &
                          parent_index) result(while_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: while_index
        type(do_while_node) :: while_node
        integer :: i

        ! Set condition index
        if (condition_index > 0 .and. condition_index <= arena%size) then
            while_node%condition_index = condition_index
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                while_node%body_indices = body_indices
            end if
        end if

        if (present(line)) while_node%line = line
        if (present(column)) while_node%column = column

        call arena%push(while_node, "do_while", parent_index)
        while_index = arena%size

    end function push_do_while

    ! Create forall construct node and add to stack
    function push_forall(arena, index_var, start_index, end_index, step_index, &
              mask_index, body_indices, line, column, parent_index) result(forall_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: index_var
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index, mask_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: forall_index
        type(forall_node) :: forall_stmt
        integer :: i

        forall_stmt%index_var = index_var

        ! Set start and end expression indices
        if (start_index > 0 .and. start_index <= arena%size) then
            forall_stmt%start_index = start_index
        end if

        if (end_index > 0 .and. end_index <= arena%size) then
            forall_stmt%end_index = end_index
        end if

        ! Set optional step expression index
        if (present(step_index)) then
            if (step_index > 0) then
                forall_stmt%step_index = step_index
            end if
        end if

        ! Set optional mask expression index
        if (present(mask_index)) then
            if (mask_index > 0) then
                forall_stmt%mask_index = mask_index
            end if
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                forall_stmt%body_indices = body_indices
            end if
        end if

        if (present(line)) forall_stmt%line = line
        if (present(column)) forall_stmt%column = column

        call arena%push(forall_stmt, "forall", parent_index)
        forall_index = arena%size

    end function push_forall

    ! Create select case node and add to stack
    function push_select_case(arena, selector_index, case_indices, line, &
                             column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index
        type(select_case_node) :: select_node

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set case indices
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                select_node%case_indices = case_indices
            end if
        end if

        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        call arena%push(select_node, "select_case", parent_index)
        select_index = arena%size

    end function push_select_case

    ! Create select case node with default case and add to stack
    function push_select_case_with_default(arena, selector_index, case_indices, &
                                           default_index, &
                                        line, column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in) :: default_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index
        type(select_case_node) :: select_node

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set case indices
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                select_node%case_indices = case_indices
            end if
        end if

        ! Set default case index
        if (default_index > 0 .and. default_index <= arena%size) then
            select_node%default_index = default_index
        end if

        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        call arena%push(select_node, "select_case", parent_index)
        select_index = arena%size

    end function push_select_case_with_default

    ! Create case block node and add to stack
    function push_case_block(arena, value_indices, body_indices, line, column, &
                            parent_index) result(case_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: value_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: case_index
        type(case_block_node) :: case_node

        ! Set case values
        if (size(value_indices) > 0) then
            case_node%value_indices = value_indices
        end if

        ! Set case body
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                case_node%body_indices = body_indices
            end if
        end if

        if (present(line)) case_node%line = line
        if (present(column)) case_node%column = column

        call arena%push(case_node, "case_block", parent_index)
        case_index = arena%size

    end function push_case_block

    ! Create case range node and add to stack
    function push_case_range(arena, start_value, end_value, line, column, &
                            parent_index) result(range_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: start_value, end_value
        integer, intent(in), optional :: line, column, parent_index
        integer :: range_index
        type(case_range_node) :: range_node

        range_node%start_value = start_value
        range_node%end_value = end_value

        if (present(line)) range_node%line = line
        if (present(column)) range_node%column = column

        call arena%push(range_node, "case_range", parent_index)
        range_index = arena%size

    end function push_case_range

    ! Create case default node and add to stack
    function push_case_default(arena, body_indices, line, column, &
                              parent_index) result(default_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: default_index
        type(case_default_node) :: default_node

        ! Set default case body
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                default_node%body_indices = body_indices
            end if
        end if

        if (present(line)) default_node%line = line
        if (present(column)) default_node%column = column

        call arena%push(default_node, "case_default", parent_index)
        default_index = arena%size

    end function push_case_default

    ! Build AST from individual nodes (helper function)
    subroutine build_ast_from_nodes(arena, node_specs, indices)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: node_specs(:)  ! Array of "type:name" specs
        integer, intent(out) :: indices(:)  ! Output indices
        integer :: i

        do i = 1, size(node_specs)
            block
                character(len=:), allocatable :: spec
                integer :: colon_pos
                character(len=:), allocatable :: node_type, node_name

                spec = trim(node_specs(i))
                colon_pos = index(spec, ':')

                if (colon_pos > 0) then
                    node_type = spec(1:colon_pos - 1)
                    node_name = spec(colon_pos + 1:)

                    select case (trim(node_type))
                    case ('identifier')
                        indices(i) = push_identifier(arena, node_name, i, 1)
                    case ('literal_int')
                      indices(i) = push_literal(arena, node_name, LITERAL_INTEGER, i, 1)
                    case ('literal_real')
                        indices(i) = push_literal(arena, node_name, LITERAL_REAL, i, 1)
                    case ('literal_string')
                       indices(i) = push_literal(arena, node_name, LITERAL_STRING, i, 1)
                    case default
                        indices(i) = push_identifier(arena, node_name, i, 1)
                    end select
                else
                    indices(i) = push_identifier(arena, spec, i, 1)
                end if
            end block
        end do
    end subroutine build_ast_from_nodes

    ! Create use statement node and add to stack
    function push_use_statement(arena, module_name, only_list, rename_list, &
                                has_only, line, column, parent_index) result(use_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column, parent_index
        integer :: use_index
        type(use_statement_node) :: use_stmt

        use_stmt = create_use_statement(module_name, only_list, rename_list, &
                                        has_only, line, column)
        call arena%push(use_stmt, "use_statement", parent_index)
        use_index = arena%size

    end function push_use_statement

    ! Create include statement node and add to stack
    function push_include_statement(arena, filename, line, column, &
                                   parent_index) result(include_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column, parent_index
        integer :: include_index
        type(include_statement_node) :: include_stmt

        include_stmt = create_include_statement(filename, line, column)
        call arena%push(include_stmt, "include_statement", parent_index)
        include_index = arena%size

    end function push_include_statement

    ! Create print statement node and add to stack
    function push_print_statement(arena, format_spec, arg_indices, line, &
                                 column, parent_index) result(print_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: format_spec
        integer, intent(in), optional :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: print_index
        type(print_statement_node) :: print_stmt

        print_stmt%format_spec = format_spec
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                print_stmt%expression_indices = arg_indices
            end if
        end if
        if (present(line)) print_stmt%line = line
        if (present(column)) print_stmt%column = column

        call arena%push(print_stmt, "print_statement", parent_index)
        print_index = arena%size

    end function push_print_statement

    function push_write_statement(arena, unit_spec, arg_indices, format_spec, &
                                 line, column, parent_index) result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: arg_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                write_stmt%arg_indices = arg_indices
            end if
        end if
        if (present(format_spec)) write_stmt%format_spec = format_spec
        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size

    end function push_write_statement

    function push_read_statement(arena, unit_spec, var_indices, format_spec, &
                                line, column, parent_index) result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: var_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        if (present(var_indices)) then
            if (size(var_indices) > 0) then
                read_stmt%var_indices = var_indices
            end if
        end if
        if (present(format_spec)) read_stmt%format_spec = format_spec
        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size

    end function push_read_statement

    ! Create allocate statement node and add to stack
    function push_allocate(arena, var_indices, shape_indices, stat_var_index, &
                           errmsg_var_index, source_expr_index, mold_expr_index, &
                           line, column, parent_index) result(alloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: var_indices(:)
        integer, intent(in), optional :: shape_indices(:)
        integer, intent(in), optional :: stat_var_index
        integer, intent(in), optional :: errmsg_var_index
        integer, intent(in), optional :: source_expr_index
        integer, intent(in), optional :: mold_expr_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: alloc_index
        type(allocate_statement_node) :: alloc_stmt

        if (size(var_indices) > 0) then
            alloc_stmt%var_indices = var_indices
        end if

        if (present(shape_indices)) then
            if (size(shape_indices) > 0) then
                alloc_stmt%shape_indices = shape_indices
            end if
        end if

        if (present(stat_var_index)) alloc_stmt%stat_var_index = stat_var_index
        if (present(errmsg_var_index)) alloc_stmt%errmsg_var_index = errmsg_var_index
        if (present(source_expr_index)) alloc_stmt%source_expr_index = source_expr_index
        if (present(mold_expr_index)) alloc_stmt%mold_expr_index = mold_expr_index
        if (present(line)) alloc_stmt%line = line
        if (present(column)) alloc_stmt%column = column

        call arena%push(alloc_stmt, "allocate_statement", parent_index)
        alloc_index = arena%size

    end function push_allocate

    ! Create deallocate statement node and add to stack
    function push_deallocate(arena, var_indices, stat_var_index, errmsg_var_index, &
                             line, column, parent_index) result(dealloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: var_indices(:)
        integer, intent(in), optional :: stat_var_index
        integer, intent(in), optional :: errmsg_var_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: dealloc_index
        type(deallocate_statement_node) :: dealloc_stmt

        if (size(var_indices) > 0) then
            dealloc_stmt%var_indices = var_indices
        end if

        if (present(stat_var_index)) dealloc_stmt%stat_var_index = stat_var_index
        if (present(errmsg_var_index)) dealloc_stmt%errmsg_var_index = errmsg_var_index
        if (present(line)) dealloc_stmt%line = line
        if (present(column)) dealloc_stmt%column = column

        call arena%push(dealloc_stmt, "deallocate_statement", parent_index)
        dealloc_index = arena%size

    end function push_deallocate

    ! Create function definition node and add to stack
    function push_function_def(arena, name, param_indices, return_type, body_indices, &
                               line, column, parent_index) result(func_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in), optional :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: func_index
        type(function_def_node) :: func_def

        func_def = create_function_def(name, param_indices, return_type, &
                                       body_indices, line, column)
        call arena%push(func_def, "function_def", parent_index)
        func_index = arena%size

    end function push_function_def

    ! Create subroutine definition node and add to stack
    function push_subroutine_def(arena, name, param_indices, body_indices, &
                                 line, column, parent_index) result(sub_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: sub_index
        type(subroutine_def_node) :: sub_def

        sub_def = create_subroutine_def(name, param_indices, body_indices, line, column)
        call arena%push(sub_def, "subroutine_def", parent_index)
        sub_index = arena%size

    end function push_subroutine_def

    ! Create interface block node and add to stack
    function push_interface_block(arena, interface_name, procedure_indices, &
                                  line, column, parent_index) result(interface_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: interface_name
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: interface_index
        type(interface_block_node) :: interface_block

        interface_block = create_interface_block(interface_name, "interface", &
                          procedure_indices=procedure_indices, line=line, column=column)
        call arena%push(interface_block, "interface_block", parent_index)
        interface_index = arena%size

    end function push_interface_block

    ! Create module node and add to stack
    function push_module(arena, name, body_indices, line, column, &
                        parent_index) result(module_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: module_index
        type(module_node) :: mod_node

        mod_node = create_module(name, declaration_indices=body_indices, &
                                line=line, column=column)

        call arena%push(mod_node, "module_node", parent_index)
        module_index = arena%size

    end function push_module

    ! Create STOP statement node and add to stack
    function push_stop(arena, stop_code_index, stop_message, line, column, &
                      parent_index) result(stop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: stop_code_index
        character(len=*), intent(in), optional :: stop_message
        integer, intent(in), optional :: line, column, parent_index
        integer :: stop_index
        type(stop_node) :: stop_stmt

        stop_stmt = create_stop(stop_code_index=stop_code_index, &
                                stop_message=stop_message, &
                                line=line, column=column)

        call arena%push(stop_stmt, "stop_node", parent_index)
        stop_index = arena%size

    end function push_stop

    ! Create RETURN statement node and add to stack
    function push_return(arena, line, column, parent_index) result(return_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: return_index
        type(return_node) :: return_stmt

        return_stmt = create_return(line=line, column=column)

        call arena%push(return_stmt, "return_node", parent_index)
        return_index = arena%size

    end function push_return

    ! Create CYCLE statement node and add to stack
  function push_cycle(arena, loop_label, line, column, parent_index) result(cycle_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: cycle_index
        type(cycle_node) :: cycle_stmt

        cycle_stmt = create_cycle(loop_label=loop_label, line=line, column=column)

        call arena%push(cycle_stmt, "cycle_node", parent_index)
        cycle_index = arena%size

    end function push_cycle

    ! Create EXIT statement node and add to stack
    function push_exit(arena, loop_label, line, column, parent_index) result(exit_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: exit_index
        type(exit_node) :: exit_stmt

        exit_stmt = create_exit(loop_label=loop_label, line=line, column=column)

        call arena%push(exit_stmt, "exit_node", parent_index)
        exit_index = arena%size

    end function push_exit

    ! Create WHERE construct node and add to stack
    function push_where(arena, mask_expr_index, where_body_indices, &
                       elsewhere_body_indices, &
                        line, column, parent_index) result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index
        type(where_node) :: where_stmt

        where_stmt = create_where(mask_expr_index=mask_expr_index, &
                                  where_body_indices=where_body_indices, &
                                  elsewhere_body_indices=elsewhere_body_indices, &
                                  line=line, column=column)

        call arena%push(where_stmt, "where_node", parent_index)
        where_index = arena%size

    end function push_where

    ! Create WHERE construct node and add to stack (simplified interface)
    function push_where_construct(arena, mask_expr_index, where_body_indices, &
                                  line, column, parent_index) result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index

        where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                 line=line, column=column, parent_index=parent_index)
    end function push_where_construct

    ! Create WHERE construct with ELSEWHERE and add to stack
    function push_where_construct_with_elsewhere(arena, mask_expr_index, &
                                                 where_body_indices, &
                                   elsewhere_body_indices, line, column, parent_index) &
        result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index

        where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                 elsewhere_body_indices, line, column, parent_index)
    end function push_where_construct_with_elsewhere

    ! Create type constructor node and add to stack
    function push_type_constructor(arena, type_name, arg_indices, line, &
                                  column, parent_index) result(constructor_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        integer, intent(in), optional :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: constructor_index
        type(call_or_subscript_node) :: constructor_node

        ! Type constructors are treated as special function calls
        constructor_node%name = type_name
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                constructor_node%arg_indices = arg_indices
            end if
        end if

        if (present(line)) constructor_node%line = line
        if (present(column)) constructor_node%column = column

        call arena%push(constructor_node, "type_constructor", parent_index)
        constructor_index = arena%size

    end function push_type_constructor

    ! Create component access node and add to stack
    function push_component_access(arena, object_index, component_name, &
                                  line, column, parent_index) result(access_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: object_index
        character(len=*), intent(in) :: component_name
        integer, intent(in), optional :: line, column, parent_index
        integer :: access_index
        type(binary_op_node) :: access_node

        ! Component access is treated as a special binary operation: object % component
        access_node%left_index = object_index
        ! Create a temporary identifier for the component name
        access_node%right_index = push_identifier(arena, component_name, &
                                                 parent_index=parent_index)
        access_node%operator = "%"

        if (present(line)) access_node%line = line
        if (present(column)) access_node%column = column

        call arena%push(access_node, "component_access", parent_index)
        access_index = arena%size

    end function push_component_access

    ! Create complex literal node and add to stack
    function push_complex_literal(arena, real_index, imag_index, line, column, &
                                 parent_index) result(complex_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: real_index, imag_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: complex_index
        type(complex_literal_node) :: complex_node

        complex_node%real_index = real_index
        complex_node%imag_index = imag_index

        if (present(line)) complex_node%line = line
        if (present(column)) complex_node%column = column

        call arena%push(complex_node, "complex_literal", parent_index)
        complex_index = arena%size

    end function push_complex_literal

    ! Extended I/O statement functions with iostat/err/end specifiers
 function push_write_statement_with_iostat(arena, unit_spec, arg_indices, format_spec, &
                             iostat_var, line, column, parent_index) result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: arg_indices(:), iostat_var
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        write_stmt%format_spec = format_spec
        write_stmt%arg_indices = arg_indices
        write_stmt%iostat_var_index = iostat_var

        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size

    end function push_write_statement_with_iostat

    function push_read_statement_with_err(arena, unit_spec, var_indices, format_spec, &
                               err_label, line, column, parent_index) result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: var_indices(:), err_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        read_stmt%format_spec = format_spec
        read_stmt%var_indices = var_indices
        read_stmt%err_label_index = err_label

        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size

    end function push_read_statement_with_err

    function push_read_statement_with_end(arena, unit_spec, var_indices, format_spec, &
                               end_label, line, column, parent_index) result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: var_indices(:), end_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        read_stmt%format_spec = format_spec
        read_stmt%var_indices = var_indices
        read_stmt%end_label_index = end_label

        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size

    end function push_read_statement_with_end

    function push_read_statement_with_all_specifiers(arena, unit_spec, &
                                                     var_indices, format_spec, &
                                                     iostat_var, err_label, end_label, &
                                          line, column, parent_index) result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: var_indices(:), iostat_var, err_label, end_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        read_stmt%format_spec = format_spec
        read_stmt%var_indices = var_indices
        read_stmt%iostat_var_index = iostat_var
        read_stmt%err_label_index = err_label
        read_stmt%end_label_index = end_label

        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size

    end function push_read_statement_with_all_specifiers

    ! Format descriptor support functions
 function push_write_statement_with_format(arena, unit_spec, arg_indices, format_spec, &
                                         line, column, parent_index) result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        write_stmt%format_spec = format_spec
        write_stmt%arg_indices = arg_indices
        write_stmt%is_formatted = .true.

        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size

    end function push_write_statement_with_format

    function push_write_statement_with_runtime_format(arena, unit_spec, &
                                                      arg_indices, format_var, &
                                         line, column, parent_index) result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in) :: arg_indices(:), format_var
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        write_stmt%arg_indices = arg_indices
        write_stmt%format_expr_index = format_var
        write_stmt%is_formatted = .true.

        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size

    end function push_write_statement_with_runtime_format

    function push_array_section(arena, array_name, start_idx, end_idx, &
                               line, column, parent_index) result(section_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: array_name
        integer, intent(in) :: start_idx, end_idx
        integer, intent(in), optional :: line, column, parent_index
        integer :: section_index
        type(call_or_subscript_node) :: section
        integer :: start_literal_idx, end_literal_idx
        character(len=20) :: start_str, end_str

        ! Convert indices to strings
        write (start_str, '(I0)') start_idx
        write (end_str, '(I0)') end_idx

        ! Create start and end index literals
 start_literal_idx = push_literal(arena, trim(start_str), LITERAL_INTEGER, line, column)
     end_literal_idx = push_literal(arena, trim(end_str), LITERAL_INTEGER, line, column)

        ! Create subscript node with array section range
        section%name = array_name
        allocate (section%arg_indices(2))
        section%arg_indices(1) = start_literal_idx
        section%arg_indices(2) = end_literal_idx

        if (present(line)) section%line = line
        if (present(column)) section%column = column

        call arena%push(section, "call_or_subscript", parent_index)
        section_index = arena%size
    end function push_array_section

end module ast_factory
