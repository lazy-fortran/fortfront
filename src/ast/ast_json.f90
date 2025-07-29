module ast_json
    use json_module
    use ast_types
    implicit none
    private

    ! Make all JSON serialization methods public
    public :: program_to_json, assignment_to_json, binary_op_to_json
    public :: function_def_to_json, subroutine_def_to_json, subroutine_call_to_json
    public :: call_or_subscript_to_json, identifier_to_json, literal_to_json
    public :: use_statement_to_json, include_statement_to_json, print_statement_to_json
    public :: declaration_to_json, do_loop_to_json, do_while_to_json, if_to_json
    public :: select_case_to_json, interface_block_to_json, module_to_json, derived_type_to_json

contains

    subroutine program_to_json(this, json, parent)
        class(program_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'program')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)

        if (allocated(this%body)) then
            do i = 1, size(this%body)
                call this%body(i)%node%to_json(json, body_array)
            end do
        end if

        call json%add(parent, obj)
    end subroutine program_to_json

    subroutine assignment_to_json(this, json, parent)
        class(assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'assignment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'inferred_type', this%type_was_inferred)
        if (allocated(this%inferred_type_name)) then
            call json%add(obj, 'inferred_type_name', this%inferred_type_name)
        end if

        block
            type(json_value), pointer :: target_obj, value_obj
            call json%create_object(target_obj, 'target')
            call this%target%to_json(json, target_obj)
            call json%add(obj, target_obj)

            call json%create_object(value_obj, 'value')
            call this%value%to_json(json, value_obj)
            call json%add(obj, value_obj)
        end block

        call json%add(parent, obj)
    end subroutine assignment_to_json

    subroutine binary_op_to_json(this, json, parent)
        class(binary_op_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'binary_op')
        call json%add(obj, 'operator', this%operator)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        block
            type(json_value), pointer :: left_obj, right_obj
            call json%create_object(left_obj, 'left')
            call this%left%to_json(json, left_obj)
            call json%add(obj, left_obj)

            call json%create_object(right_obj, 'right')
            call this%right%to_json(json, right_obj)
            call json%add(obj, right_obj)
        end block

        call json%add(parent, obj)
    end subroutine binary_op_to_json

    subroutine function_def_to_json(this, json, parent)
        class(function_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'function_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(params_array, 'params')
        call json%add(obj, params_array)
        do i = 1, size(this%params)
            call this%params(i)%node%to_json(json, params_array)
        end do

        block
            type(json_value), pointer :: return_type_obj
            call json%create_object(return_type_obj, 'return_type')
            call this%return_type%to_json(json, return_type_obj)
            call json%add(obj, return_type_obj)
        end block

        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)
        do i = 1, size(this%body)
            call this%body(i)%node%to_json(json, body_array)
        end do

        call json%add(parent, obj)
    end subroutine function_def_to_json

    subroutine subroutine_def_to_json(this, json, parent)
        class(subroutine_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(params_array, 'params')
        call json%add(obj, params_array)
        do i = 1, size(this%params)
            call this%params(i)%node%to_json(json, params_array)
        end do

        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)
        do i = 1, size(this%body)
            call this%body(i)%node%to_json(json, body_array)
        end do

        call json%add(parent, obj)
    end subroutine subroutine_def_to_json

    subroutine subroutine_call_to_json(this, json, parent)
        class(subroutine_call_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_call')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                call json%add(args_array, '', this%arg_indices(i))
            end do
        end if

        call json%add(parent, obj)
    end subroutine subroutine_call_to_json

    subroutine call_or_subscript_to_json(this, json, parent)
        class(call_or_subscript_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'call_or_subscript')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                call json%add(args_array, '', this%arg_indices(i))
            end do
        end if

        call json%add(parent, obj)
    end subroutine call_or_subscript_to_json

    subroutine identifier_to_json(this, json, parent)
        class(identifier_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'identifier')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%add(parent, obj)
    end subroutine identifier_to_json

    subroutine literal_to_json(this, json, parent)
        class(literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        character(len=:), allocatable :: kind_name

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'literal')
        call json%add(obj, 'value', this%value)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        select case (this%literal_kind)
        case (LITERAL_INTEGER)
            kind_name = 'integer'
        case (LITERAL_REAL)
            kind_name = 'real'
        case (LITERAL_STRING)
            kind_name = 'string'
        case (LITERAL_LOGICAL)
            kind_name = 'logical'
        case default
            kind_name = 'unknown'
        end select
        call json%add(obj, 'kind', kind_name)

        call json%add(parent, obj)
    end subroutine literal_to_json

    subroutine use_statement_to_json(this, json, parent)
        class(use_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, only_array, rename_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'use_statement')
        call json%add(obj, 'module_name', this%module_name)
        call json%add(obj, 'has_only', this%has_only)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%only_list)) then
            call json%create_array(only_array, 'only_list')
            call json%add(obj, only_array)
            do i = 1, size(this%only_list)
                call json%add(only_array, '', this%only_list(i)%s)
            end do
        end if

        if (allocated(this%rename_list)) then
            call json%create_array(rename_array, 'rename_list')
            call json%add(obj, rename_array)
            do i = 1, size(this%rename_list)
                call json%add(rename_array, '', this%rename_list(i)%s)
            end do
        end if

        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine include_statement_to_json(this, json, parent)
        class(include_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'include_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'filename', this%filename)

        call json%add(parent, obj)
    end subroutine include_statement_to_json

    subroutine print_statement_to_json(this, json, parent)
        class(print_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'print_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%args)) then
            do i = 1, size(this%args)
                call this%args(i)%node%to_json(json, args_array)
            end do
        end if

        call json%add(parent, obj)
    end subroutine print_statement_to_json

    subroutine declaration_to_json(this, json, parent)
        class(declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'declaration')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'type_name', this%type_name)
        call json%add(obj, 'var_name', this%var_name)

        if (this%has_kind) then
            call json%add(obj, 'kind_value', this%kind_value)
        end if

        if (this%is_array) then
            call json%add(obj, 'is_array', .true.)
        end if

        if (this%is_allocatable) then
            call json%add(obj, 'is_allocatable', .true.)
        end if

        if (this%is_pointer) then
            call json%add(obj, 'is_pointer', .true.)
        end if

        if (this%is_array) then

            if (allocated(this%dimensions)) then
                block
                    type(json_value), pointer :: dims_array
                    call json%create_array(dims_array, 'dimensions')
                    do i = 1, size(this%dimensions)
                        if (allocated(this%dimensions(i)%node)) then
                            call this%dimensions(i)%node%to_json(json, dims_array)
                        end if
                    end do
                    call json%add(obj, dims_array)
                end block
            end if
        end if

        if (allocated(this%initializer)) then
            block
                type(json_value), pointer :: init_obj
                call json%create_object(init_obj, 'initializer')
                call this%initializer%to_json(json, init_obj)
                call json%add(obj, init_obj)
            end block
        end if

        call json%add(parent, obj)
    end subroutine declaration_to_json

    subroutine do_loop_to_json(this, json, parent)
        class(do_loop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'do_loop')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'var_name', this%var_name)
        call json%add(parent, obj)
    end subroutine do_loop_to_json

    subroutine do_while_to_json(this, json, parent)
        class(do_while_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'do_while')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine do_while_to_json

    subroutine if_to_json(this, json, parent)
        class(if_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
      type(json_value), pointer :: obj, then_array, else_array, elseif_array, elseif_obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'if_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add condition
        if (allocated(this%condition)) then
            block
                type(json_value), pointer :: cond_obj
                call json%create_object(cond_obj, 'condition')
                call this%condition%to_json(json, cond_obj)
                call json%add(obj, cond_obj)
            end block
        end if

        ! Add then body
        if (allocated(this%then_body)) then
            call json%create_array(then_array, 'then_body')
            call json%add(obj, then_array)
            do i = 1, size(this%then_body)
                if (allocated(this%then_body(i)%node)) then
                    call this%then_body(i)%node%to_json(json, then_array)
                end if
            end do
        end if

        ! Add elseif blocks
        if (allocated(this%elseif_blocks)) then
            call json%create_array(elseif_array, 'elseif_blocks')
            call json%add(obj, elseif_array)
            do i = 1, size(this%elseif_blocks)
                call json%create_object(elseif_obj, '')
                ! Add elseif condition
                if (allocated(this%elseif_blocks(i)%condition)) then
                    block
                        type(json_value), pointer :: cond_obj
                        call json%create_object(cond_obj, 'condition')
                        call this%elseif_blocks(i)%condition%to_json(json, cond_obj)
                        call json%add(elseif_obj, cond_obj)
                    end block
                end if
                ! Add elseif body
                if (allocated(this%elseif_blocks(i)%body)) then
                    block
                        type(json_value), pointer :: body_array
                        integer :: j
                        call json%create_array(body_array, 'body')
                        call json%add(elseif_obj, body_array)
                        do j = 1, size(this%elseif_blocks(i)%body)
                            if (allocated(this%elseif_blocks(i)%body(j)%node)) then
                       call this%elseif_blocks(i)%body(j)%node%to_json(json, body_array)
                            end if
                        end do
                    end block
                end if
                call json%add(elseif_array, elseif_obj)
            end do
        end if

        ! Add else body
        if (allocated(this%else_body)) then
            call json%create_array(else_array, 'else_body')
            call json%add(obj, else_array)
            do i = 1, size(this%else_body)
                if (allocated(this%else_body(i)%node)) then
                    call this%else_body(i)%node%to_json(json, else_array)
                end if
            end do
        end if

        call json%add(parent, obj)
    end subroutine if_to_json

    subroutine select_case_to_json(this, json, parent)
        class(select_case_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'select_case')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'selector_index', this%selector_index)
        if (allocated(this%case_indices)) then
            call json%add(obj, 'case_indices', this%case_indices)
        end if
        call json%add(obj, 'default_index', this%default_index)
        call json%add(parent, obj)
    end subroutine select_case_to_json

    subroutine interface_block_to_json(this, json, parent)
        class(interface_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'interface_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'kind', this%kind)

        if (allocated(this%name)) then
            call json%add(obj, 'name', this%name)
        end if

        if (allocated(this%operator)) then
            call json%add(obj, 'operator', this%operator)
        end if

        if (allocated(this%procedures)) then
            block
                type(json_value), pointer :: procedures_array
                call json%create_array(procedures_array, 'procedures')
                do i = 1, size(this%procedures)
                    if (allocated(this%procedures(i)%node)) then
                        call this%procedures(i)%node%to_json(json, procedures_array)
                    end if
                end do
                call json%add(obj, procedures_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine interface_block_to_json

    subroutine module_to_json(this, json, parent)
        class(module_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'module')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'has_contains', this%has_contains)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add declarations array
        if (allocated(this%declarations)) then
            block
                type(json_value), pointer :: declarations_array
                type(json_value), pointer :: child_obj
                integer :: i
                call json%create_array(declarations_array, 'declarations')
                do i = 1, size(this%declarations)
                    call this%declarations(i)%node%to_json(json, declarations_array)
                end do
                call json%add(obj, declarations_array)
            end block
        end if

        ! Add procedures array
        if (allocated(this%procedures)) then
            block
                type(json_value), pointer :: procedures_array
                type(json_value), pointer :: child_obj
                integer :: i
                call json%create_array(procedures_array, 'procedures')
                do i = 1, size(this%procedures)
                    call this%procedures(i)%node%to_json(json, procedures_array)
                end do
                call json%add(obj, procedures_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine module_to_json

    subroutine derived_type_to_json(this, json, parent)
        class(derived_type_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'derived_type')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (this%has_parameters) then
            call json%add(obj, 'has_parameters', .true.)
            if (allocated(this%parameters)) then
                block
                    type(json_value), pointer :: params_array
                    call json%create_array(params_array, 'parameters')
                    do i = 1, size(this%parameters)
                        if (allocated(this%parameters(i)%node)) then
                            call this%parameters(i)%node%to_json(json, params_array)
                        end if
                    end do
                    call json%add(obj, params_array)
                end block
            end if
        end if

        if (allocated(this%components)) then
            block
                type(json_value), pointer :: components_array
                call json%create_array(components_array, 'components')
                do i = 1, size(this%components)
                    if (allocated(this%components(i)%node)) then
                        call this%components(i)%node%to_json(json, components_array)
                    end if
                end do
                call json%add(obj, components_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine derived_type_to_json

end module ast_json
