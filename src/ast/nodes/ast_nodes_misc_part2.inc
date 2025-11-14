
    subroutine namelist_statement_assign(lhs, rhs)
        class(namelist_statement_node), intent(inout) :: lhs
        class(namelist_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        if (allocated(rhs%group_name)) lhs%group_name = rhs%group_name

        if (allocated(rhs%variable_names)) then
            lhs%variable_names = rhs%variable_names
        end if
    end subroutine namelist_statement_assign

    ! DATA statement implementations
    subroutine data_statement_accept(this, visitor)
        class(data_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine data_statement_accept

    subroutine data_statement_to_json(this, json, parent)
        class(data_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'data_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine data_statement_to_json

    subroutine data_statement_assign(lhs, rhs)
        class(data_statement_node), intent(inout) :: lhs
        class(data_statement_node), intent(in) :: rhs
        integer, allocatable :: tmp_object_indices(:)
        integer, allocatable :: tmp_value_indices(:)

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        if (allocated(rhs%object_indices)) then
            lhs%object_indices = rhs%object_indices
        else
            if (allocated(lhs%object_indices)) then
                call move_alloc(lhs%object_indices, tmp_object_indices)
            end if
        end if

        if (allocated(rhs%value_indices)) then
            lhs%value_indices = rhs%value_indices
        else
            if (allocated(lhs%value_indices)) then
                call move_alloc(lhs%value_indices, tmp_value_indices)
            end if
        end if
    end subroutine data_statement_assign

    ! Include statement implementations
    subroutine include_statement_accept(this, visitor)
        class(include_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine include_statement_accept

    subroutine include_statement_to_json(this, json, parent)
        class(include_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'include_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%filename)) call json%add(obj, 'filename', this%filename)
        call json%add(parent, obj)
    end subroutine include_statement_to_json

    subroutine include_statement_assign(lhs, rhs)
        class(include_statement_node), intent(inout) :: lhs
        class(include_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        if (allocated(rhs%filename)) lhs%filename = rhs%filename
    end subroutine include_statement_assign

    ! Import statement implementations
    subroutine import_statement_accept(this, visitor)
        class(import_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine import_statement_accept

    subroutine import_statement_to_json(this, json, parent)
        class(import_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'import_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'has_double_colon', this%has_double_colon)
        call json%add(obj, 'has_list', this%has_list)
        call json%add(obj, 'is_all', this%is_all)
        call json%add(obj, 'is_none', this%is_none)
        if (allocated(this%import_list)) then
            do i = 1, size(this%import_list)
                if (allocated(this%import_list(i)%s)) then
                    call json%add(obj, 'name_'//trim(adjustl(int_to_string(i))), &
                                  this%import_list(i)%s)
                end if
            end do
        end if
        call json%add(parent, obj)
    end subroutine import_statement_to_json

    subroutine import_statement_assign(lhs, rhs)
        class(import_statement_node), intent(inout) :: lhs
        class(import_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%has_double_colon = rhs%has_double_colon
        lhs%has_list = rhs%has_list
        lhs%is_all = rhs%is_all
        lhs%is_none = rhs%is_none

        if (allocated(rhs%import_list)) then
            lhs%import_list = rhs%import_list
        end if
    end subroutine import_statement_assign

    subroutine statement_function_accept(this, visitor)
        class(statement_function_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Implementation handled by visitor dispatchers
    end subroutine statement_function_accept

    subroutine statement_function_to_json(this, json, parent)
        class(statement_function_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'statement_function')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'name', this%name)
        if (allocated(this%arg_names)) then
            do i = 1, size(this%arg_names)
                call json%add(obj, 'arg_'//trim(adjustl(int_to_string(i))), &
                              this%arg_names(i))
            end do
        end if
        call json%add(obj, 'body_expr_index', this%body_expr_index)
        call json%add(parent, obj)
    end subroutine statement_function_to_json

    subroutine statement_function_assign(lhs, rhs)
        class(statement_function_node), intent(inout) :: lhs
        class(statement_function_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%arg_names)) lhs%arg_names = rhs%arg_names
        lhs%body_expr_index = rhs%body_expr_index
        if (allocated(rhs%stmt_label)) lhs%stmt_label = rhs%stmt_label
    end subroutine statement_function_assign

    ! Contains node implementations
    subroutine contains_accept(this, visitor)
        class(contains_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine contains_accept

    subroutine contains_to_json(this, json, parent)
        class(contains_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'contains')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine contains_to_json

    subroutine contains_assign(lhs, rhs)
        class(contains_node), intent(inout) :: lhs
        class(contains_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine contains_assign

    ! End statement node implementations
    subroutine end_statement_accept(this, visitor)
        class(end_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine end_statement_accept

    subroutine end_statement_to_json(this, json, parent)
        class(end_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'end_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine end_statement_to_json

    subroutine end_statement_assign(lhs, rhs)
        class(end_statement_node), intent(inout) :: lhs
        class(end_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine end_statement_assign

    ! Interface block implementations
    subroutine interface_block_accept(this, visitor)
        class(interface_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine interface_block_accept

    subroutine interface_block_to_json(this, json, parent)
        class(interface_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'interface_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%name)) call json%add(obj, 'name', this%name)
        if (allocated(this%kind)) call json%add(obj, 'kind', this%kind)
        if (allocated(this%operator)) call json%add(obj, 'operator', this%operator)
        call json%add(obj, 'is_abstract', this%is_abstract)
        call json%add(parent, obj)
    end subroutine interface_block_to_json

    subroutine interface_block_assign(lhs, rhs)
        class(interface_block_node), intent(inout) :: lhs
        class(interface_block_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%kind)) lhs%kind = rhs%kind
        if (allocated(rhs%operator)) lhs%operator = rhs%operator
        lhs%is_abstract = rhs%is_abstract
        if (allocated(rhs%procedure_indices)) then
            lhs%procedure_indices = rhs%procedure_indices
        end if
    end subroutine interface_block_assign

    ! Module procedure implementations
    subroutine module_procedure_accept(this, visitor)
        class(module_procedure_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine module_procedure_accept

    subroutine module_procedure_to_json(this, json, parent)
        class(module_procedure_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        type(json_value), pointer :: arr
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'module_procedure')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%procedure_names)) then
            call json%create_array(arr, 'procedure_names')
            do i = 1, size(this%procedure_names)
                if (allocated(this%procedure_names(i)%s)) then
                    call json%add(arr, '', trim(this%procedure_names(i)%s))
                else
                    call json%add(arr, '', '')
                end if
            end do
            call json%add(obj, arr)
        end if
        call json%add(obj, 'has_module_prefix', this%has_module_prefix)
        call json%add(obj, 'has_double_colon', this%has_double_colon)
        call json%add(parent, obj)
    end subroutine module_procedure_to_json

    subroutine module_procedure_assign(lhs, rhs)
        class(module_procedure_node), intent(inout) :: lhs
        class(module_procedure_node), intent(in) :: rhs
        integer :: n
        type(string_t), allocatable :: tmp(:)

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%has_module_prefix = rhs%has_module_prefix
        lhs%has_double_colon = rhs%has_double_colon

        if (allocated(rhs%procedure_names)) then
            n = size(rhs%procedure_names)
            allocate (tmp(n))
            tmp = rhs%procedure_names
            call move_alloc(tmp, lhs%procedure_names)
        else
            if (allocated(lhs%procedure_names)) then
                call move_alloc(lhs%procedure_names, tmp)
            end if
        end if
    end subroutine module_procedure_assign

    ! Comment node methods
    subroutine comment_accept(this, visitor)
        class(comment_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! For now, do nothing since we don't have a visitor framework in place
    end subroutine comment_accept

    subroutine comment_to_json(this, json, parent)
        class(comment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'comment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%text)) call json%add(obj, 'text', this%text)
        call json%add(parent, obj)
    end subroutine comment_to_json

    subroutine comment_assign(lhs, rhs)
        class(comment_node), intent(inout) :: lhs
        class(comment_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy comment text
        if (allocated(rhs%text)) lhs%text = rhs%text
    end subroutine comment_assign

    ! Directive node methods
    subroutine directive_accept(this, visitor)
        class(directive_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! No-op placeholder for future visitor hooks
    end subroutine directive_accept

    subroutine directive_to_json(this, json, parent)
        class(directive_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'directive')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%text)) call json%add(obj, 'text', this%text)
        call json%add(obj, 'is_openmp', this%is_openmp)
        call json%add(obj, 'is_openacc', this%is_openacc)
        call json%add(parent, obj)
    end subroutine directive_to_json

    subroutine directive_assign(lhs, rhs)
        class(directive_node), intent(inout) :: lhs
        class(directive_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%is_openmp = rhs%is_openmp
        lhs%is_openacc = rhs%is_openacc

        if (allocated(rhs%text)) lhs%text = rhs%text
    end subroutine directive_assign

    ! Blank line node methods
    subroutine blank_line_accept(this, visitor)
        class(blank_line_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! For now, do nothing since we don't have a visitor framework in place
    end subroutine blank_line_accept

    subroutine blank_line_to_json(this, json, parent)
        class(blank_line_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'blank_line')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'count', this%count)
        call json%add(parent, obj)
    end subroutine blank_line_to_json

    subroutine blank_line_assign(lhs, rhs)
        class(blank_line_node), intent(inout) :: lhs
        class(blank_line_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy blank line count
        lhs%count = rhs%count
    end subroutine blank_line_assign

    ! Implicit statement implementations
    subroutine implicit_statement_accept(this, visitor)
        class(implicit_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! For now, do nothing since we don't have a visitor framework in place
    end subroutine implicit_statement_accept

    subroutine implicit_statement_to_json(this, json, parent)
        class(implicit_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'implicit_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'is_none', this%is_none)

        if (.not. this%is_none) then
            ! Add type specification as simple fields
            if (allocated(this%type_spec%type_name)) then
                call json%add(obj, 'type_name', this%type_spec%type_name)
            end if
            call json%add(obj, 'has_kind', this%type_spec%has_kind)
            if (this%type_spec%has_kind) then
                call json%add(obj, 'kind_value', this%type_spec%kind_value)
            end if
            call json%add(obj, 'has_length', this%type_spec%has_length)
            if (this%type_spec%has_length) then
                call json%add(obj, 'length_value', this%type_spec%length_value)
            end if

            ! For now, just add letter count
            ! Full letter spec serialization can be added later
            if (allocated(this%letter_specs)) then
                call json%add(obj, 'letter_specs_count', size(this%letter_specs))
            else
                call json%add(obj, 'letter_specs_count', 0)
            end if
        end if

        call json%add(parent, obj)
    end subroutine implicit_statement_to_json

    subroutine implicit_statement_assign(lhs, rhs)
        class(implicit_statement_node), intent(inout) :: lhs
        class(implicit_statement_node), intent(in) :: rhs
        character(len=:), allocatable :: tmp_none_spec
        character(len=:), allocatable :: tmp_type_name
        type(implicit_letter_spec_t), allocatable :: tmp_letter_specs(:)

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        ! Copy implicit statement specific fields
        lhs%is_none = rhs%is_none

        ! Copy none specification
        if (allocated(rhs%none_spec)) then
            lhs%none_spec = rhs%none_spec
        else
            if (allocated(lhs%none_spec)) then
                call move_alloc(lhs%none_spec, tmp_none_spec)
            end if
        end if

        ! Copy type specification
        if (allocated(rhs%type_spec%type_name)) then
            lhs%type_spec%type_name = rhs%type_spec%type_name
        else
            if (allocated(lhs%type_spec%type_name)) then
                call move_alloc(lhs%type_spec%type_name, tmp_type_name)
            end if
        end if
        lhs%type_spec%has_kind = rhs%type_spec%has_kind
        lhs%type_spec%kind_value = rhs%type_spec%kind_value
        lhs%type_spec%has_length = rhs%type_spec%has_length
        lhs%type_spec%length_value = rhs%type_spec%length_value

        ! Copy letter specifications
        if (allocated(rhs%letter_specs)) then
            lhs%letter_specs = rhs%letter_specs
        else
            if (allocated(lhs%letter_specs)) then
                call move_alloc(lhs%letter_specs, tmp_letter_specs)
            end if
        end if
    end subroutine implicit_statement_assign
