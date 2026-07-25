module semantic_bind_c_validation
    ! Validates BIND(C) interoperability constraints per ISO/IEC 1539-1:2004
    ! Section 15.3.2. A BIND(C) derived type must not contain allocatable,
    ! pointer, or procedure components. A BIND(C) procedure must not have
    ! assumed-shape or assumed-size array dummy arguments or procedure dummy
    ! arguments. Non-interoperable constructs are reported as semantic errors.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, derived_type_node, &
        parameter_declaration_node
    use ast_nodes_bounds, only: array_bounds_node, range_expression_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use scope_manager, only: binding_label_table_t
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    public :: validate_bind_c_derived_type, validate_bind_c_procedure
    public :: validate_global_binding_labels
    public :: has_bind_c_attribute

contains

    ! Reject duplicate global BIND(C) binding labels (Fortran 2018 C1553:
    ! a binding label shall not be the same as the binding label of any
    ! other global entity). The binding label namespace is flat across the
    ! whole compilation unit, so the whole arena is scanned once.
    subroutine validate_global_binding_labels(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(binding_label_table_t) :: labels
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (function_def_node)
                call register_procedure_label(labels, node%bind_c_clause, &
                    node%name, node%line, node%column, errors)
                type is (subroutine_def_node)
                call register_procedure_label(labels, node%bind_c_clause, &
                    node%name, node%line, node%column, errors)
                type is (declaration_node)
                call register_declaration_labels(labels, node, errors)
            end select
        end do
    end subroutine validate_global_binding_labels

    ! Register the binding label of a BIND(C) procedure definition or
    ! interface body. A procedure without BIND(C) owns no binding label.
    subroutine register_procedure_label(labels, clause, name, line, column, errors)
        type(binding_label_table_t), intent(inout) :: labels
        character(len=:), allocatable, intent(in) :: clause
        character(len=*), intent(in) :: name
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors

        if (.not. allocated(clause)) return
        if (len_trim(clause) == 0) return
        if (len_trim(name) == 0) return
        call register_label(labels, binding_label_from_clause(clause, name), &
            name, line, column, errors)
    end subroutine register_procedure_label

    ! Register the binding label of a BIND(C) variable declaration. An
    ! explicit NAME= applies to a single entity; a multi-entity BIND(C)
    ! declaration gives every entity its own default label.
    subroutine register_declaration_labels(labels, decl, errors)
        type(binding_label_table_t), intent(inout) :: labels
        type(declaration_node), intent(in) :: decl
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        if (.not. decl%is_bind_c) return

        if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
            do i = 1, size(decl%var_names)
                call register_label(labels, to_lower(trim(decl%var_names(i))), &
                    trim(decl%var_names(i)), decl%line, decl%column, errors)
            end do
            return
        end if

        if (.not. allocated(decl%var_name)) return
        if (allocated(decl%bind_name)) then
            call register_label(labels, strip_quotes(decl%bind_name), &
                decl%var_name, decl%line, decl%column, errors)
        else
            call register_label(labels, to_lower(decl%var_name), decl%var_name, &
                decl%line, decl%column, errors)
        end if
    end subroutine register_declaration_labels

    subroutine register_label(labels, label, entity, line, column, errors)
        type(binding_label_table_t), intent(inout) :: labels
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: entity
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: other_entity
        integer :: other_line, other_column
        logical :: has_collision

        if (len_trim(label) == 0) return
        call labels%register(label, entity, line, column, has_collision, &
            other_entity, other_line, other_column)
        if (.not. has_collision) return

        call errors%add_error( &
            message='BIND(C) binding label "'//trim(label)//'" of "'// &
            trim(entity)//'" collides with global entity "'// &
            trim(other_entity)//'"', &
            code=ERROR_SEMANTIC, &
            component='semantic_bind_c_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column)//'; first use at line '// &
            int_to_string(other_line), &
            suggestion='give each global BIND(C) entity a distinct '// &
            'NAME= binding label', line=line, column=column, &
            end_line=line, end_column=column + 1)
    end subroutine register_label

    ! Extract the binding label from a verbatim BIND(C) clause. Without an
    ! explicit NAME= the label is the lower-cased entity name. An explicit
    ! empty NAME= means the entity has no binding label at all.
    function binding_label_from_clause(clause, name) result(label)
        character(len=*), intent(in) :: clause
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: label
        character(len=:), allocatable :: lowered
        integer :: name_pos, eq_pos, close_pos

        lowered = to_lower(clause)
        name_pos = index(lowered, 'name')
        if (name_pos == 0) then
            label = to_lower(trim(name))
            return
        end if

        eq_pos = index(lowered(name_pos:), '=')
        if (eq_pos == 0) then
            label = to_lower(trim(name))
            return
        end if
        eq_pos = name_pos + eq_pos

        close_pos = index(clause(eq_pos:), ')')
        if (close_pos == 0) then
            label = strip_quotes(clause(eq_pos:))
        else
            label = strip_quotes(clause(eq_pos:eq_pos + close_pos - 2))
        end if
    end function binding_label_from_clause

    ! Remove surrounding blanks and one layer of string quotes.
    function strip_quotes(text) result(stripped)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: stripped
        integer :: first, last

        stripped = ''
        first = 1
        last = len(text)
        do while (first <= last)
            if (text(first:first) /= ' ') exit
            first = first + 1
        end do
        do while (last >= first)
            if (text(last:last) /= ' ') exit
            last = last - 1
        end do
        if (last < first) return

        if (last > first) then
            if (text(first:first) == '"' .or. text(first:first) == "'") then
                if (text(last:last) == text(first:first)) then
                    first = first + 1
                    last = last - 1
                end if
            end if
        end if
        if (last < first) return
        stripped = text(first:last)
    end function strip_quotes

    ! Returns .true. when an attribute clause carries a BIND(C) attribute.
    ! The clause is taken verbatim from source, so the match is case
    ! insensitive on the leading keyword.
    function has_bind_c_attribute(attribute_clause) result(is_bind_c)
        character(len=:), allocatable, intent(in) :: attribute_clause
        logical :: is_bind_c

        is_bind_c = .false.
        if (.not. allocated(attribute_clause)) return
        is_bind_c = index(to_lower(attribute_clause), 'bind') > 0
    end function has_bind_c_attribute

    ! Validate a derived type when its header carries BIND(C). Reject any
    ! component that is allocatable, pointer, or a procedure component.
    subroutine validate_bind_c_derived_type(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        integer :: i, comp_index

        if (.not. node%has_attributes) return
        if (.not. has_bind_c_attribute(node%attribute_clause)) return
        if (.not. allocated(node%component_indices)) return

        do i = 1, size(node%component_indices)
            comp_index = node%component_indices(i)
            if (comp_index <= 0) cycle
            if (.not. arena%has_node_at(comp_index)) cycle
            call check_bind_c_component(arena, comp_index, node%name, errors)
        end do
    end subroutine validate_bind_c_derived_type

    ! Inspect a single component of a BIND(C) derived type.
    subroutine check_bind_c_component(arena, comp_index, type_name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: comp_index
        character(len=*), intent(in) :: type_name
        type(error_collection_t), intent(inout) :: errors

        select type (comp => arena%entries(comp_index)%node)
            type is (declaration_node)
            if (comp%is_allocatable) then
                call report_component(errors, type_name, comp%var_name, &
                    'allocatable', comp%line, comp%column)
            else if (is_procedure_declaration(comp)) then
                call report_component(errors, type_name, comp%var_name, &
                    'procedure', comp%line, comp%column)
            else if (comp%is_pointer) then
                call report_component(errors, type_name, comp%var_name, &
                    'pointer', comp%line, comp%column)
            end if
        end select
    end subroutine check_bind_c_component

    ! Validate a BIND(C) procedure's dummy arguments. Reject assumed-shape
    ! and assumed-size array dummies and procedure dummy arguments. Dummy
    ! attributes live on the body declarations, matched to the dummy names
    ! carried by the procedure's parameter placeholders.
    subroutine validate_bind_c_procedure(arena, param_indices, body_indices, &
            bind_c_clause, proc_name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=:), allocatable, intent(in) :: bind_c_clause
        character(len=*), intent(in) :: proc_name
        type(error_collection_t), intent(inout) :: errors
        integer :: i, decl_index

        if (.not. allocated(bind_c_clause)) return
        if (len_trim(bind_c_clause) == 0) return
        if (.not. allocated(body_indices)) return

        do i = 1, size(body_indices)
            decl_index = body_indices(i)
            if (decl_index <= 0) cycle
            if (.not. arena%has_node_at(decl_index)) cycle
            call check_bind_c_dummy(arena, decl_index, param_indices, &
                proc_name, errors)
        end do
    end subroutine validate_bind_c_procedure

    ! Inspect a body declaration of a BIND(C) procedure. Only declarations
    ! that name a dummy argument are checked.
    subroutine check_bind_c_dummy(arena, decl_index, param_indices, &
            proc_name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        integer, allocatable, intent(in) :: param_indices(:)
        character(len=*), intent(in) :: proc_name
        type(error_collection_t), intent(inout) :: errors

        select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (.not. allocated(decl%var_name)) return
            if (.not. is_dummy_name(arena, param_indices, decl%var_name)) return
            if (is_procedure_declaration(decl)) then
                call report_dummy(errors, proc_name, decl%var_name, &
                    'a procedure dummy argument', &
                    decl%line, decl%column)
            else if (has_assumed_size_dummy(arena, decl)) then
                call report_dummy(errors, proc_name, decl%var_name, &
                    'an assumed-size array dummy argument', &
                    decl%line, decl%column)
            else if (has_assumed_shape_dummy(arena, decl)) then
                call report_dummy(errors, proc_name, decl%var_name, &
                    'an assumed-shape array dummy argument', &
                    decl%line, decl%column)
            else if (is_non_interoperable_character(decl)) then
                call report_dummy(errors, proc_name, decl%var_name, &
                    'a character dummy argument of length other than 1', &
                    decl%line, decl%column)
            end if
        end select
    end subroutine check_bind_c_dummy

    ! True when a name matches one of the procedure's dummy argument names.
    function is_dummy_name(arena, param_indices, name) result(is_dummy)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        character(len=*), intent(in) :: name
        logical :: is_dummy
        integer :: i, param_index

        is_dummy = .false.
        if (.not. allocated(param_indices)) return

        do i = 1, size(param_indices)
            param_index = param_indices(i)
            if (param_index <= 0) cycle
            if (.not. arena%has_node_at(param_index)) cycle
            select type (param => arena%entries(param_index)%node)
                type is (parameter_declaration_node)
                if (allocated(param%name)) then
                    if (trim(param%name) == trim(name)) is_dummy = .true.
                end if
                type is (declaration_node)
                if (allocated(param%var_name)) then
                    if (trim(param%var_name) == trim(name)) is_dummy = .true.
                end if
            end select
        end do
    end function is_dummy_name

    ! A procedure component or dummy is a declaration whose type name begins
    ! with the procedure keyword (e.g. "procedure(real)").
    function is_procedure_declaration(decl) result(is_proc)
        type(declaration_node), intent(in) :: decl
        logical :: is_proc

        is_proc = .false.
        if (.not. allocated(decl%type_name)) return
        if (len(decl%type_name) < 9) return
        is_proc = to_lower(decl%type_name(1:9)) == 'procedure'
    end function is_procedure_declaration

    ! True when a dummy is of type character with a length that is known at
    ! this layer to differ from 1 (F2018 18.3.1: an interoperable character
    ! entity has character length 1, and an assumed-length character dummy
    ! is not interoperable). Only assumed length and integer-literal lengths
    ! are judged; a length given by a named constant or other expression is
    ! left alone because its value is not resolved here.
    function is_non_interoperable_character(decl) result(is_bad)
        type(declaration_node), intent(in) :: decl
        logical :: is_bad
        character(len=:), allocatable :: length_text
        integer :: value, stat

        is_bad = .false.
        if (.not. allocated(decl%type_name)) return
        if (base_type_keyword(decl%type_name) /= 'character') return
        if (.not. decl%has_character_length) return
        if (.not. allocated(decl%character_length_expr)) return

        length_text = character_length_value(decl%character_length_expr)
        if (len(length_text) == 0) return
        if (length_text == '*') then
            is_bad = .true.
            return
        end if
        if (verify(length_text, '0123456789') /= 0) return
        read (length_text, *, iostat=stat) value
        if (stat /= 0) return
        is_bad = value /= 1
    end function is_non_interoperable_character

    ! The declared type name keeps its parameter list, e.g. "character(len=4)".
    function base_type_keyword(type_name) result(keyword)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: keyword
        integer :: paren

        paren = index(type_name, '(')
        if (paren > 0) then
            keyword = to_lower(trim(adjustl(type_name(1:paren - 1))))
        else
            keyword = to_lower(trim(adjustl(type_name)))
        end if
    end function base_type_keyword

    ! The length expression may arrive as "4" or as "len=4".
    function character_length_value(length_expr) result(value_text)
        character(len=*), intent(in) :: length_expr
        character(len=:), allocatable :: value_text
        integer :: eq_pos

        value_text = trim(adjustl(length_expr))
        eq_pos = index(to_lower(value_text), 'len=')
        if (eq_pos == 1) value_text = trim(adjustl(value_text(5:)))
    end function character_length_value

    ! True when any dimension of an array declaration is assumed size (*).
    function has_assumed_size_dummy(arena, decl) result(is_assumed_size)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl
        logical :: is_assumed_size
        integer :: i, dim_index

        is_assumed_size = .false.
        if (.not. decl%is_array) return
        if (.not. allocated(decl%dimension_indices)) return

        do i = 1, size(decl%dimension_indices)
            dim_index = decl%dimension_indices(i)
            if (dim_index <= 0) cycle
            if (.not. arena%has_node_at(dim_index)) cycle
            select type (dim => arena%entries(dim_index)%node)
                type is (array_bounds_node)
                if (dim%is_assumed_size) is_assumed_size = .true.
            end select
        end do
    end function has_assumed_size_dummy

    ! True when any dimension of an array declaration is assumed shape (:),
    ! represented as a range with both bounds implicit.
    function has_assumed_shape_dummy(arena, decl) result(is_assumed_shape)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl
        logical :: is_assumed_shape
        integer :: i, dim_index

        is_assumed_shape = .false.
        if (.not. decl%is_array) return
        if (decl%is_allocatable) return
        if (decl%is_pointer) return
        if (.not. allocated(decl%dimension_indices)) return

        do i = 1, size(decl%dimension_indices)
            dim_index = decl%dimension_indices(i)
            if (dim_index <= 0) cycle
            if (.not. arena%has_node_at(dim_index)) cycle
            select type (dim => arena%entries(dim_index)%node)
                type is (range_expression_node)
                if (dim%start_index <= 0 .and. dim%end_index <= 0) &
                    is_assumed_shape = .true.
            end select
        end do
    end function has_assumed_shape_dummy

    subroutine report_component(errors, type_name, comp_name, kind, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: type_name, comp_name, kind
        integer, intent(in) :: line, column

        call errors%add_error( &
            message='BIND(C) derived type "'//trim(type_name)// &
            '" must not have '//kind//' component "'//trim(comp_name)//'"', &
            code=ERROR_SEMANTIC, &
            component='semantic_bind_c_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='remove the '//kind//' attribute or drop BIND(C) '// &
            'from the type', line=line, column=column, end_line=line, &
            end_column=column + 1)
    end subroutine report_component

    subroutine report_dummy(errors, proc_name, dummy_name, kind, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: proc_name, dummy_name, kind
        integer, intent(in) :: line, column

        call errors%add_error( &
            message='BIND(C) procedure "'//trim(proc_name)// &
            '" must not have '//kind//' "'//trim(dummy_name)//'"', &
            code=ERROR_SEMANTIC, &
            component='semantic_bind_c_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='use an explicit-shape or assumed-rank dummy, or '// &
            'drop BIND(C) from the procedure', line=line, column=column, &
            end_line=line, end_column=column + 1)
    end subroutine report_dummy

end module semantic_bind_c_validation
