module frontend_compiler_type_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node, string_t, LITERAL_INTEGER, LITERAL_REAL, &
        LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
        call_or_subscript_node, component_access_node, assignment_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
        derived_type_node
    use ast_nodes_misc, only: complex_literal_node, use_statement_node
    use ast_nodes_procedure, only: function_def_node
    use frontend_compiler_resolution, only: declaration_binding_t, &
        resolve_name_at_node, resolve_identifier_binding
    use string_utils_mod, only: to_lower
    use type_system_unified, only: mono_type_t, TINT, TREAL, TCHAR, &
        TLOGICAL, TARRAY, TCOMPLEX, TDOUBLE, TDERIVED
    implicit none
    private

    type, public :: resolved_type_query_t
        logical :: found = .false.
        integer :: type_kind = 0
        integer :: kind_value = 0
        integer :: storage_size_bits = 0
        integer :: rank = -1
        character(len=:), allocatable :: derived_type_name
        character(len=:), allocatable :: diagnostic
    end type resolved_type_query_t

    public :: annotate_resolved_expression_types
    public :: query_resolved_type

contains

    subroutine annotate_resolved_expression_types(arena)
        type(ast_arena_t), intent(inout) :: arena
        logical, allocatable :: visiting(:)
        integer :: i

        if (arena%size <= 0) return
        allocate (visiting(arena%size), source=.false.)

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            call clear_resolved_metadata(arena%entries(i)%node)
        end do

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            call resolve_node_metadata(arena, i, visiting)
        end do
    end subroutine annotate_resolved_expression_types

    function query_resolved_type(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(resolved_type_query_t) :: query

        query%derived_type_name = ""
        query%diagnostic = ""

        if (.not. arena%has_node_at(node_index)) then
            query%diagnostic = "node index does not reference an AST node"
            return
        end if

        associate (node => arena%entries(node_index)%node)
            query%type_kind = node%resolved_type_kind
            query%kind_value = node%resolved_kind_value
            query%storage_size_bits = node%resolved_storage_bits
            query%rank = node%resolved_rank
            query%derived_type_name = trim(node%resolved_derived_type_name)
            query%found = node%resolved_type_found
        end associate

        if (.not. query%found) then
            query%diagnostic = &
                "exact semantic type is unavailable for this AST node"
        end if
    end function query_resolved_type

    subroutine clear_resolved_metadata(node)
        class(ast_node), intent(inout) :: node

        node%resolved_type_found = .false.
        node%resolved_type_kind = 0
        node%resolved_kind_value = 0
        node%resolved_storage_bits = 0
        node%resolved_rank = -1
        node%resolved_derived_type_name = ""
    end subroutine clear_resolved_metadata

    recursive subroutine resolve_node_metadata(arena, node_index, visiting)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        logical, intent(inout) :: visiting(:)

        if (node_index <= 0) return
        if (node_index > arena%size) return
        if (.not. arena%has_node_at(node_index)) return
        if (arena%entries(node_index)%node%resolved_type_found) return
        if (visiting(node_index)) return

        visiting(node_index) = .true.
        select type (node => arena%entries(node_index)%node)
            type is (literal_node)
            call resolve_literal(arena, node_index, node)
            type is (complex_literal_node)
            call resolve_complex_literal(arena, node, visiting)
            type is (declaration_node)
            call resolve_declaration(arena, node_index, node, visiting)
            type is (parameter_declaration_node)
            call resolve_parameter_declaration(arena, node_index, node)
            type is (identifier_node)
            call resolve_identifier(arena, node_index, node, visiting)
            type is (binary_op_node)
            call resolve_binary_operation(arena, node, visiting)
            type is (call_or_subscript_node)
            call resolve_call_or_subscript(arena, node_index, node, visiting)
            type is (component_access_node)
            call resolve_component_access(arena, node_index, node, visiting)
            type is (function_def_node)
            call resolve_function_result(arena, node_index, node, visiting)
        class default
            call resolve_from_inferred_type(node)
        end select
        visiting(node_index) = .false.
    end subroutine resolve_node_metadata

    subroutine resolve_literal(arena, node_index, node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(literal_node), intent(inout) :: node
        character(len=:), allocatable :: selector
        character(len=:), allocatable :: raw_value
        logical :: has_selector
        integer :: kind_value

        raw_value = ""
        if (allocated(node%value)) raw_value = node%value
        select case (node%literal_kind)
        case (LITERAL_INTEGER)
            call literal_selector(raw_value, selector, has_selector)
            if (has_selector) then
                kind_value = resolve_kind_selector(arena, node_index, selector)
            else
                kind_value = 4
            end if
            call set_intrinsic_metadata(node, TINT, kind_value, 0)
        case (LITERAL_REAL)
            call literal_selector(raw_value, selector, has_selector)
            if (has_selector) then
                kind_value = resolve_kind_selector(arena, node_index, selector)
            else if (has_double_exponent(raw_value)) then
                kind_value = 8
            else
                kind_value = 4
            end if
            call set_intrinsic_metadata(node, TREAL, kind_value, 0)
        case (LITERAL_STRING)
            call set_intrinsic_metadata(node, TCHAR, 1, 0)
        case (LITERAL_LOGICAL)
            call literal_selector(raw_value, selector, has_selector)
            if (has_selector) then
                kind_value = resolve_kind_selector(arena, node_index, selector)
            else
                kind_value = 4
            end if
            call set_intrinsic_metadata(node, TLOGICAL, kind_value, 0)
        end select
    end subroutine resolve_literal

    subroutine resolve_complex_literal(arena, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        type(complex_literal_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        integer :: real_kind, imag_kind

        real_kind = 0
        imag_kind = 0
        if (node%real_index > 0) then
            call resolve_node_metadata(arena, node%real_index, visiting)
            if (arena%has_node_at(node%real_index)) then
                real_kind = arena%entries(node%real_index)%node%resolved_kind_value
            end if
        end if
        if (node%imag_index > 0) then
            call resolve_node_metadata(arena, node%imag_index, visiting)
            if (arena%has_node_at(node%imag_index)) then
                imag_kind = arena%entries(node%imag_index)%node%resolved_kind_value
            end if
        end if
        call set_intrinsic_metadata(node, TCOMPLEX, max(real_kind, imag_kind), 0)
    end subroutine resolve_complex_literal

    subroutine resolve_declaration(arena, node_index, node, visiting)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(declaration_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        integer :: rank
        integer :: selected_kind

        if (.not. allocated(node%type_name)) then
            call resolve_from_inferred_type(node)
            return
        end if
        rank = 0
        if (node%is_array) then
            if (allocated(node%dimension_indices)) rank = size(node%dimension_indices)
        end if
        call resolve_type_spec(arena, node_index, node%type_name, node%has_kind, &
            node%kind_value, rank, node)
        if (.not. node%resolved_type_found .and. node%kind_selector_index > 0) then
            selected_kind = 0
            call evaluate_integer_constant(arena, node%kind_selector_index, &
                selected_kind, visiting)
            if (selected_kind > 0) then
                call set_intrinsic_metadata(node, &
                    type_kind_from_spec(compact_lower(node%type_name)), &
                    selected_kind, rank)
            end if
        end if
    end subroutine resolve_declaration

    subroutine resolve_parameter_declaration(arena, node_index, node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(parameter_declaration_node), intent(inout) :: node
        integer :: rank

        if (.not. allocated(node%type_name)) then
            call resolve_from_inferred_type(node)
            return
        end if
        rank = 0
        if (node%is_array) then
            if (allocated(node%dimension_indices)) rank = size(node%dimension_indices)
        end if
        call resolve_type_spec(arena, node_index, node%type_name, node%has_kind, &
            node%kind_value, rank, node)
    end subroutine resolve_parameter_declaration

    subroutine resolve_identifier(arena, node_index, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(identifier_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg

        call resolve_identifier_binding(arena, node_index, binding, error_msg)
        if (binding%found) then
            call resolve_node_metadata(arena, binding%declaration_node_index, visiting)
            call copy_metadata_from_index(arena, binding%declaration_node_index, node)
            if (node%resolved_type_found) return
        end if
        call resolve_from_inferred_type(node)
    end subroutine resolve_identifier

    subroutine resolve_binary_operation(arena, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        integer :: left_type, right_type, left_kind, right_kind
        integer :: result_type, result_kind, result_rank
        character(len=:), allocatable :: operator

        left_type = 0
        right_type = 0
        left_kind = 0
        right_kind = 0
        result_rank = 0
        if (node%left_index > 0) then
            call resolve_node_metadata(arena, node%left_index, visiting)
            call get_metadata(arena, node%left_index, left_type, left_kind, &
                result_rank)
        end if
        if (node%right_index > 0) then
            call resolve_node_metadata(arena, node%right_index, visiting)
            call get_metadata(arena, node%right_index, right_type, right_kind, &
                result_rank)
        end if

        operator = ""
        if (allocated(node%operator)) operator = to_lower(trim(node%operator))
        if (is_relational_operator(operator)) then
            call set_intrinsic_metadata(node, TLOGICAL, 4, result_rank)
            return
        end if
        if (is_logical_operator(operator)) then
            result_kind = max(left_kind, right_kind)
            if (result_kind <= 0) result_kind = 4
            call set_intrinsic_metadata(node, TLOGICAL, result_kind, result_rank)
            return
        end if

        if (node%left_index <= 0) then
            if (node%right_index > 0) then
                call copy_metadata_from_index(arena, node%right_index, node)
            end if
            return
        end if
        if (node%right_index <= 0) then
            call copy_metadata_from_index(arena, node%left_index, node)
            return
        end if

        if (operator == "**") then
            if (left_type == TREAL .or. left_type == TCOMPLEX) then
                call set_intrinsic_metadata(node, left_type, left_kind, result_rank)
                return
            end if
        end if

        result_type = numeric_result_type(left_type, right_type)
        result_kind = numeric_result_kind(result_type, left_type, left_kind, &
            right_type, right_kind)
        if (result_type > 0 .and. result_kind > 0) then
            call set_intrinsic_metadata(node, result_type, result_kind, result_rank)
        else
            call resolve_from_inferred_type(node)
        end if
    end subroutine resolve_binary_operation

    subroutine resolve_call_or_subscript(arena, node_index, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(call_or_subscript_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg

        if (.not. allocated(node%name)) then
            call resolve_from_inferred_type(node)
            return
        end if

        call resolve_name_at_node(arena, node_index, node%name, binding, error_msg)
        if (binding%found) then
            call resolve_node_metadata(arena, binding%declaration_node_index, visiting)
            call copy_metadata_from_index(arena, binding%declaration_node_index, node)
            if (node%resolved_type_found) return
        end if

        if (node%is_intrinsic) then
            call resolve_intrinsic_call(arena, node, visiting)
            if (node%resolved_type_found) return
        end if
        call resolve_from_inferred_type(node)
    end subroutine resolve_call_or_subscript

    subroutine resolve_intrinsic_call(arena, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        character(len=:), allocatable :: name
        integer :: kind_value, result_type, result_rank

        name = to_lower(trim(node%name))
        result_type = inferred_base_kind(node%inferred_type, result_rank)
        result_type = normalize_type_kind(result_type)

        select case (name)
        case ("real", "int", "logical", "cmplx")
            kind_value = intrinsic_kind_argument(arena, node, name)
            if (kind_value <= 0 .and. name == "real") then
                call real_result_kind_from_argument(arena, node, visiting, &
                    kind_value)
            end if
            if (kind_value <= 0) kind_value = 4
            select case (name)
            case ("real")
                result_type = TREAL
            case ("int")
                result_type = TINT
            case ("logical")
                result_type = TLOGICAL
            case ("cmplx")
                result_type = TCOMPLEX
            end select
            call set_intrinsic_metadata(node, result_type, kind_value, result_rank)
        case ("kind", "selected_int_kind", "selected_real_kind")
            call set_intrinsic_metadata(node, TINT, 4, 0)
        case ("aimag", "abs")
            call resolve_unary_same_kind_intrinsic(arena, node, visiting, name, &
                result_rank)
        case default
            if (.not. allocated(node%arg_indices)) return
            if (size(node%arg_indices) <= 0) return
            call resolve_node_metadata(arena, node%arg_indices(1), visiting)
            if (.not. arena%has_node_at(node%arg_indices(1))) return
            kind_value = &
                arena%entries(node%arg_indices(1))%node%resolved_kind_value
            if (result_type > 0 .and. kind_value > 0) then
                call set_intrinsic_metadata(node, result_type, kind_value, &
                    result_rank)
            end if
        end select
    end subroutine resolve_intrinsic_call

    subroutine resolve_unary_same_kind_intrinsic(arena, node, visiting, name, &
            result_rank)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        character(len=*), intent(in) :: name
        integer, intent(in) :: result_rank
        integer :: argument_type, argument_kind, result_type

        call first_intrinsic_argument_metadata(arena, node, visiting, &
            argument_type, argument_kind)
        if (argument_kind <= 0) return

        select case (name)
        case ("aimag")
            if (argument_type /= TCOMPLEX) return
            call set_intrinsic_metadata(node, TREAL, argument_kind, result_rank)
        case ("abs")
            result_type = argument_type
            if (argument_type == TCOMPLEX) result_type = TREAL
            select case (result_type)
            case (TINT, TREAL)
                call set_intrinsic_metadata(node, result_type, argument_kind, &
                    result_rank)
            end select
        end select
    end subroutine resolve_unary_same_kind_intrinsic

    subroutine resolve_component_access(arena, node_index, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(component_access_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: derived_name
        integer :: derived_index

        if (node%base_expr_index <= 0) return
        if (.not. allocated(node%component_name)) return
        call resolve_node_metadata(arena, node%base_expr_index, visiting)
        if (.not. arena%has_node_at(node%base_expr_index)) return
        derived_name = trim(arena%entries(node%base_expr_index)%node% &
            resolved_derived_type_name)
        if (len_trim(derived_name) == 0) return

        call resolve_name_at_node(arena, node_index, derived_name, binding, error_msg)
        if (.not. binding%found) return
        derived_index = binding%declaration_node_index
        call resolve_component_in_type(arena, derived_index, node%component_name, &
            node, visiting)
    end subroutine resolve_component_access

    recursive subroutine resolve_component_in_type(arena, derived_index, &
            component_name, target, visiting)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: derived_index
        character(len=*), intent(in) :: component_name
        class(ast_node), intent(inout) :: target
        logical, intent(inout) :: visiting(:)
        integer :: i, component_index

        if (.not. arena%has_node_at(derived_index)) return
        select type (derived => arena%entries(derived_index)%node)
            type is (derived_type_node)
            if (.not. allocated(derived%component_indices)) return
            do i = 1, size(derived%component_indices)
                component_index = derived%component_indices(i)
                if (.not. declaration_has_name(arena, component_index, &
                    component_name)) cycle
                call resolve_node_metadata(arena, component_index, visiting)
                call copy_metadata_from_index(arena, component_index, target)
                return
            end do
        end select
    end subroutine resolve_component_in_type

    subroutine resolve_function_result(arena, node_index, node, visiting)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(function_def_node), intent(inout) :: node
        logical, intent(inout) :: visiting(:)
        character(len=:), allocatable :: result_name
        integer :: i, body_index

        if (allocated(node%return_type)) then
            if (len_trim(node%return_type) > 0) then
                call resolve_type_spec(arena, node_index, node%return_type, &
                    .false., 0, 0, node)
                if (node%resolved_type_found) return
            end if
        end if

        result_name = ""
        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        else if (allocated(node%name)) then
            result_name = trim(node%name)
        end if
        if (len_trim(result_name) == 0) return
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            body_index = node%body_indices(i)
            if (.not. declaration_has_name(arena, body_index, result_name)) cycle
            call resolve_node_metadata(arena, body_index, visiting)
            call copy_metadata_from_index(arena, body_index, node)
            return
        end do
    end subroutine resolve_function_result

    subroutine resolve_type_spec(arena, node_index, type_name, has_kind, &
            explicit_kind, rank, node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: type_name
        logical, intent(in) :: has_kind
        integer, intent(in) :: explicit_kind
        integer, intent(in) :: rank
        class(ast_node), intent(inout) :: node
        character(len=:), allocatable :: lowered, selector, derived_name
        integer :: type_kind, kind_value
        logical :: selector_found

        lowered = compact_lower(type_name)
        type_kind = type_kind_from_spec(lowered)
        if (type_kind == TDERIVED) then
            derived_name = derived_name_from_spec(lowered)
            call set_derived_metadata(node, derived_name, rank)
            return
        end if
        if (type_kind <= 0) return

        kind_value = 0
        if (has_kind .and. explicit_kind > 0) then
            kind_value = explicit_kind
        else
            call selector_from_type_spec(lowered, type_kind, selector, &
                selector_found)
            if (selector_found) then
                kind_value = resolve_kind_selector(arena, node_index, selector)
            else
                kind_value = default_kind_for_type(type_kind)
            end if
        end if
        call set_intrinsic_metadata(node, type_kind, kind_value, rank)
    end subroutine resolve_type_spec

    subroutine resolve_from_inferred_type(node)
        class(ast_node), intent(inout) :: node
        integer :: rank, raw_type_kind, type_kind, kind_value

        raw_type_kind = inferred_base_kind(node%inferred_type, rank)
        type_kind = normalize_type_kind(raw_type_kind)
        if (type_kind <= 0) return
        if (raw_type_kind == TDOUBLE) then
            kind_value = 8
        else
            kind_value = default_kind_for_type(type_kind)
        end if
        call set_intrinsic_metadata(node, type_kind, kind_value, rank)
    end subroutine resolve_from_inferred_type

    subroutine set_intrinsic_metadata(node, type_kind, kind_value, rank)
        class(ast_node), intent(inout) :: node
        integer, intent(in) :: type_kind, kind_value, rank

        node%resolved_type_kind = normalize_type_kind(type_kind)
        node%resolved_kind_value = kind_value
        node%resolved_rank = rank
        node%resolved_storage_bits = storage_bits_for_type( &
            node%resolved_type_kind, kind_value)
        node%resolved_derived_type_name = ""
        node%resolved_type_found = node%resolved_type_kind > 0
        if (node%resolved_type_kind /= TDERIVED) then
            node%resolved_type_found = node%resolved_type_found .and. &
                kind_value > 0
        end if
    end subroutine set_intrinsic_metadata

    subroutine set_derived_metadata(node, derived_name, rank)
        class(ast_node), intent(inout) :: node
        character(len=*), intent(in) :: derived_name
        integer, intent(in) :: rank

        node%resolved_type_kind = TDERIVED
        node%resolved_kind_value = 0
        node%resolved_storage_bits = 0
        node%resolved_rank = rank
        node%resolved_derived_type_name = trim(derived_name)
        node%resolved_type_found = len_trim(derived_name) > 0
    end subroutine set_derived_metadata

    subroutine copy_metadata_from_index(arena, source_index, target)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: source_index
        class(ast_node), intent(inout) :: target

        if (.not. arena%has_node_at(source_index)) return
        associate (source => arena%entries(source_index)%node)
            target%resolved_type_found = source%resolved_type_found
            target%resolved_type_kind = source%resolved_type_kind
            target%resolved_kind_value = source%resolved_kind_value
            target%resolved_storage_bits = source%resolved_storage_bits
            target%resolved_rank = source%resolved_rank
            target%resolved_derived_type_name = &
                source%resolved_derived_type_name
        end associate
    end subroutine copy_metadata_from_index

    subroutine get_metadata(arena, node_index, type_kind, kind_value, rank)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: type_kind, kind_value
        integer, intent(inout) :: rank

        type_kind = 0
        kind_value = 0
        if (.not. arena%has_node_at(node_index)) return
        type_kind = arena%entries(node_index)%node%resolved_type_kind
        kind_value = arena%entries(node_index)%node%resolved_kind_value
        rank = max(rank, arena%entries(node_index)%node%resolved_rank)
    end subroutine get_metadata

    integer function inferred_base_kind(typ, rank) result(type_kind)
        type(mono_type_t), intent(in) :: typ
        integer, intent(out) :: rank
        type(mono_type_t) :: current

        rank = 0
        current = typ
        do while (current%kind == TARRAY)
            rank = rank + 1
            if (current%get_args_count() <= 0) then
                type_kind = 0
                return
            end if
            current = current%get_arg(1)
        end do
        type_kind = current%kind
    end function inferred_base_kind

    integer pure function normalize_type_kind(type_kind) result(normalized)
        integer, intent(in) :: type_kind

        if (type_kind == TDOUBLE) then
            normalized = TREAL
        else
            normalized = type_kind
        end if
    end function normalize_type_kind

    integer pure function default_kind_for_type(type_kind) result(kind_value)
        integer, intent(in) :: type_kind

        select case (normalize_type_kind(type_kind))
        case (TCHAR)
            kind_value = 1
        case (TINT, TLOGICAL, TCOMPLEX)
            kind_value = 4
        case (TREAL)
            ! Lazy dialect: kind-less real is real64 (8 bytes)
            kind_value = 8
        case default
            kind_value = 0
        end select
    end function default_kind_for_type

    integer pure function storage_bits_for_type(type_kind, kind_value) result(bits)
        integer, intent(in) :: type_kind, kind_value

        bits = 0
        if (kind_value <= 0) return
        select case (normalize_type_kind(type_kind))
        case (TINT, TLOGICAL, TCHAR)
            bits = 8 * kind_value
        case (TREAL)
            select case (kind_value)
            case (1, 2, 4, 8)
                bits = 8 * kind_value
            case (10, 16)
                bits = 128
            end select
        case (TCOMPLEX)
            select case (kind_value)
            case (1, 2, 4, 8)
                bits = 16 * kind_value
            case (10, 16)
                bits = 256
            end select
        end select
    end function storage_bits_for_type

    integer pure function type_kind_from_spec(type_spec) result(type_kind)
        character(len=*), intent(in) :: type_spec

        type_kind = 0
        if (index(type_spec, "doubleprecision") == 1) then
            type_kind = TREAL
        else if (index(type_spec, "integer") == 1) then
            type_kind = TINT
        else if (index(type_spec, "real") == 1) then
            type_kind = TREAL
        else if (index(type_spec, "logical") == 1) then
            type_kind = TLOGICAL
        else if (index(type_spec, "complex") == 1) then
            type_kind = TCOMPLEX
        else if (index(type_spec, "character") == 1) then
            type_kind = TCHAR
        else if (index(type_spec, "type(") == 1 .or. &
                index(type_spec, "class(") == 1) then
            type_kind = TDERIVED
        end if
    end function type_kind_from_spec

    subroutine selector_from_type_spec(type_spec, type_kind, selector, found)
        character(len=*), intent(in) :: type_spec
        integer, intent(in) :: type_kind
        character(len=:), allocatable, intent(out) :: selector
        logical, intent(out) :: found
        integer :: open_pos, close_pos, kind_pos, comma_pos
        character(len=:), allocatable :: contents

        selector = ""
        found = .false.
        if (index(type_spec, "doubleprecision") == 1) then
            selector = "8"
            found = .true.
            return
        end if

        open_pos = index(type_spec, "(")
        close_pos = index(type_spec, ")", back=.true.)
        if (open_pos <= 0) return
        if (close_pos <= open_pos) return
        contents = type_spec(open_pos + 1:close_pos - 1)

        if (type_kind == TCHAR) then
            kind_pos = index(contents, "kind=")
            if (kind_pos <= 0) return
            contents = contents(kind_pos + len("kind="):)
        else
            kind_pos = index(contents, "kind=")
            if (kind_pos > 0) contents = contents(kind_pos + len("kind="):)
        end if
        comma_pos = index(contents, ",")
        if (comma_pos > 0) contents = contents(:comma_pos - 1)
        selector = trim(contents)
        found = len_trim(selector) > 0
    end subroutine selector_from_type_spec

    function derived_name_from_spec(type_spec) result(name)
        character(len=*), intent(in) :: type_spec
        character(len=:), allocatable :: name
        integer :: open_pos, close_pos, comma_pos

        name = ""
        open_pos = index(type_spec, "(")
        close_pos = index(type_spec, ")", back=.true.)
        if (open_pos <= 0) return
        if (close_pos <= open_pos) return
        name = type_spec(open_pos + 1:close_pos - 1)
        comma_pos = index(name, ",")
        if (comma_pos > 0) name = name(:comma_pos - 1)
        name = trim(name)
    end function derived_name_from_spec

    subroutine literal_selector(raw_value, selector, found)
        character(len=*), intent(in) :: raw_value
        character(len=:), allocatable, intent(out) :: selector
        logical, intent(out) :: found
        character(len=:), allocatable :: lowered
        integer :: underscore_pos

        selector = ""
        found = .false.
        lowered = compact_lower(raw_value)
        underscore_pos = index(lowered, "_", back=.true.)
        if (underscore_pos <= 0) return
        if (underscore_pos >= len(lowered)) return
        selector = lowered(underscore_pos + 1:)
        found = len_trim(selector) > 0
    end subroutine literal_selector

    logical function has_double_exponent(raw_value) result(has_double)
        character(len=*), intent(in) :: raw_value
        character(len=:), allocatable :: lowered
        integer :: underscore_pos

        lowered = compact_lower(raw_value)
        underscore_pos = index(lowered, "_", back=.true.)
        if (underscore_pos > 0) lowered = lowered(:underscore_pos - 1)
        has_double = index(lowered, "d") > 0
    end function has_double_exponent

    integer function resolve_kind_selector(arena, reference_index, selector) &
            result(kind_value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_index
        character(len=*), intent(in) :: selector
        character(len=:), allocatable :: lowered
        integer :: ios
        logical, allocatable :: visiting(:)

        kind_value = 0
        lowered = compact_lower(selector)

        read (lowered, *, iostat=ios) kind_value
        if (ios == 0) return
        kind_value = 0

        ! A constant declared in scope outranks the conventional spelling so an
        ! explicitly shadowed selector keeps its declared value.
        if (arena%size > 0) then
            allocate (visiting(arena%size), source=.false.)
            call resolve_named_integer_constant(arena, reference_index, lowered, &
                kind_value, visiting)
            if (kind_value > 0) return
        end if

        if (resolve_iso_c_binding_kind(arena, reference_index, lowered, &
            kind_value)) return

        kind_value = conventional_kind_selector(lowered)
    end function resolve_kind_selector

    ! Kind names carried by the iso_fortran_env rename idiom, which leaves no
    ! declaration in the arena to resolve against.
    integer pure function conventional_kind_selector(selector) result(kind_value)
        character(len=*), intent(in) :: selector

        select case (selector)
        case ("int8")
            kind_value = 1
        case ("int16")
            kind_value = 2
        case ("int32", "real32", "sp")
            kind_value = 4
        case ("int64", "real64", "double", "doubleprecision", "dp")
            kind_value = 8
        case ("real128", "qp")
            kind_value = 16
        case default
            kind_value = 0
        end select
    end function conventional_kind_selector

    logical function resolve_iso_c_binding_kind(arena, reference_index, selector, &
            kind_value) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_index
        character(len=*), intent(in) :: selector
        integer, intent(out) :: kind_value
        integer :: current, child, i
        logical :: imported
        character(len=:), allocatable :: canonical_selector, remote_selector

        found = .false.
        kind_value = 0
        imported = .false.
        canonical_selector = compact_lower(selector)
        current = reference_index
        do while (current > 0)
            if (.not. arena%has_node_at(current)) return
            if (allocated(arena%entries(current)%child_indices)) then
                do i = 1, arena%entries(current)%child_count
                    child = arena%entries(current)%child_indices(i)
                    if (.not. arena%has_node_at(child)) cycle
                    select type (use_node => arena%entries(child)%node)
                        type is (use_statement_node)
                        if (.not. allocated(use_node%module_name)) cycle
                        if (compact_lower(use_node%module_name) /= &
                            'iso_c_binding') cycle
                        if (.not. use_node%has_only) then
                            imported = .true.
                        else if (allocated(use_node%only_list)) then
                            imported = use_list_contains(use_node%only_list, selector)
                        end if
                        if (.not. imported .and. allocated(use_node%rename_list)) then
                            remote_selector = ''
                            imported = use_rename_contains(use_node%rename_list, &
                                selector, remote_selector)
                            if (imported) canonical_selector = remote_selector
                        end if
                        if (imported) exit
                    end select
                end do
            end if
            if (imported) exit
            current = arena%entries(current)%parent_index
        end do
        if (.not. imported) return

        select case (compact_lower(canonical_selector))
        case ('c_signed_char', 'c_int8_t', 'c_int_least8_t', &
                'c_int_fast8_t', 'c_bool', 'c_char')
            kind_value = 1
        case ('c_short', 'c_int16_t', 'c_int_least16_t', 'c_int_fast16_t')
            kind_value = 2
        case ('c_int', 'c_int32_t', 'c_int_least32_t', 'c_int_fast32_t', &
                'c_float', 'c_float32')
            kind_value = 4
        case ('c_long', 'c_long_long', 'c_size_t', 'c_intptr_t', &
                'c_ptrdiff_t', 'c_intmax_t', 'c_int64_t', 'c_int_least64_t', &
                'c_int_fast64_t', 'c_double', 'c_float64')
            kind_value = 8
        case ('c_long_double', 'c_float128')
            kind_value = 16
        case default
            return
        end select
        found = .true.
    end function resolve_iso_c_binding_kind

    logical function use_list_contains(list, selector) result(found)
        type(string_t), allocatable, intent(in) :: list(:)
        character(len=*), intent(in) :: selector
        integer :: i

        found = .false.
        do i = 1, size(list)
            if (.not. allocated(list(i)%s)) cycle
            if (compact_lower(list(i)%s) == compact_lower(selector)) then
                found = .true.
                return
            end if
        end do
    end function use_list_contains

    logical function use_rename_contains(list, selector, remote) result(found)
        type(string_t), allocatable, intent(in) :: list(:)
        character(len=*), intent(in) :: selector
        character(len=:), allocatable, intent(out) :: remote
        integer :: i

        found = .false.
        remote = ''
        i = 1
        do while (i + 1 <= size(list))
            if (allocated(list(i)%s)) then
                if (compact_lower(list(i)%s) == compact_lower(selector)) then
                    if (allocated(list(i + 1)%s)) remote = list(i + 1)%s
                    found = .true.
                    return
                end if
            end if
            i = i + 2
        end do
    end function use_rename_contains

    recursive subroutine resolve_named_integer_constant(arena, reference_index, &
            name, value, visiting)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_index
        character(len=*), intent(in) :: name
        integer, intent(out) :: value
        logical, intent(inout) :: visiting(:)
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: declaration_index

        value = 0
        call resolve_name_at_node(arena, reference_index, name, binding, error_msg)
        if (.not. binding%found) return
        declaration_index = binding%declaration_node_index
        if (.not. arena%has_node_at(declaration_index)) return
        if (visiting(declaration_index)) return
        visiting(declaration_index) = .true.
        select type (decl => arena%entries(declaration_index)%node)
            type is (declaration_node)
            if (decl%has_initializer) then
                call evaluate_integer_constant(arena, decl%initializer_index, value, &
                    visiting)
            else if (decl%is_constant) then
                value = decl%constant_integer
            end if
        class default
            if (decl%is_constant) value = decl%constant_integer
        end select
        visiting(declaration_index) = .false.
    end subroutine resolve_named_integer_constant

    recursive subroutine evaluate_integer_constant(arena, expr_index, value, &
            visiting)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        integer, intent(out) :: value
        logical, intent(inout) :: visiting(:)
        integer :: lhs, rhs, ios
        character(len=:), allocatable :: raw, name, operator

        value = 0
        if (.not. arena%has_node_at(expr_index)) return
        if (visiting(expr_index)) return
        visiting(expr_index) = .true.
        select type (expr => arena%entries(expr_index)%node)
            type is (literal_node)
            if (.not. allocated(expr%value)) then
                visiting(expr_index) = .false.
                return
            end if
            raw = trim(expr%value)
            if (index(raw, "_") > 0) raw = raw(:index(raw, "_") - 1)
            read (raw, *, iostat=ios) value
            if (ios /= 0) value = 0
            type is (identifier_node)
            if (allocated(expr%name)) then
                name = expr%name
                visiting(expr_index) = .false.
                call resolve_named_integer_constant(arena, expr_index, name, value, &
                    visiting)
                return
            end if
            type is (binary_op_node)
            lhs = 0
            rhs = 0
            if (expr%left_index > 0) then
                call evaluate_integer_constant(arena, expr%left_index, lhs, visiting)
            end if
            if (expr%right_index > 0) then
                call evaluate_integer_constant(arena, expr%right_index, rhs, visiting)
            end if
            operator = ""
            if (allocated(expr%operator)) operator = trim(expr%operator)
            select case (operator)
            case ("+")
                value = lhs + rhs
            case ("-")
                if (expr%left_index <= 0) then
                    value = -rhs
                else
                    value = lhs - rhs
                end if
            case ("*")
                value = lhs * rhs
            case ("/")
                if (rhs /= 0) value = lhs / rhs
            case ("**")
                if (rhs >= 0) value = lhs**rhs
            end select
            type is (call_or_subscript_node)
            if (.not. allocated(expr%name)) then
                visiting(expr_index) = .false.
                return
            end if
            select case (compact_lower(expr%name))
            case ("selected_real_kind")
                call evaluate_selected_real_kind(arena, expr, value, visiting)
            case ("selected_int_kind")
                call evaluate_selected_int_kind(arena, expr, value, visiting)
            end select
        class default
            if (expr%is_constant) value = expr%constant_integer
        end select
        visiting(expr_index) = .false.
    end subroutine evaluate_integer_constant

    subroutine evaluate_selected_real_kind(arena, node, value, visiting)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(inout) :: visiting(:)
        integer :: precision, range_value

        value = 0
        precision = 0
        range_value = 0
        if (.not. allocated(node%arg_indices)) return
        if (size(node%arg_indices) >= 1) then
            call evaluate_integer_constant(arena, node%arg_indices(1), &
                precision, visiting)
        end if
        if (size(node%arg_indices) >= 2) then
            call evaluate_integer_constant(arena, node%arg_indices(2), &
                range_value, visiting)
        end if

        if (precision <= 6 .and. range_value <= 37) then
            value = 4
        else if (precision <= 15 .and. range_value <= 307) then
            value = 8
        else if (precision <= 33 .and. range_value <= 4931) then
            value = 16
        else
            value = -1
        end if
    end subroutine evaluate_selected_real_kind

    subroutine evaluate_selected_int_kind(arena, node, value, visiting)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(out) :: value
        logical, intent(inout) :: visiting(:)
        integer :: range_value

        value = 0
        range_value = 0
        if (.not. allocated(node%arg_indices)) return
        if (size(node%arg_indices) < 1) return
        call evaluate_integer_constant(arena, node%arg_indices(1), range_value, visiting)
        if (range_value <= 2) then
            value = 1
        else if (range_value <= 4) then
            value = 2
        else if (range_value <= 9) then
            value = 4
        else if (range_value <= 18) then
            value = 8
        else
            value = -1
        end if
    end subroutine evaluate_selected_int_kind

    subroutine real_result_kind_from_argument(arena, node, visiting, kind_value)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: node
        logical, intent(inout) :: visiting(:)
        integer, intent(out) :: kind_value
        integer :: argument_type, argument_kind

        kind_value = 0
        call first_intrinsic_argument_metadata(arena, node, visiting, &
            argument_type, argument_kind)
        select case (argument_type)
        case (TREAL, TCOMPLEX)
            kind_value = argument_kind
        end select
    end subroutine real_result_kind_from_argument

    subroutine first_intrinsic_argument_metadata(arena, node, visiting, &
            type_kind, kind_value)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: node
        logical, intent(inout) :: visiting(:)
        integer, intent(out) :: type_kind, kind_value
        integer :: argument_index

        type_kind = 0
        kind_value = 0
        if (.not. allocated(node%arg_indices)) return
        if (size(node%arg_indices) <= 0) return
        argument_index = node%arg_indices(1)
        if (.not. arena%has_node_at(argument_index)) return
        call resolve_node_metadata(arena, argument_index, visiting)
        if (.not. arena%has_node_at(argument_index)) return
        type_kind = normalize_type_kind( &
            arena%entries(argument_index)%node%resolved_type_kind)
        kind_value = arena%entries(argument_index)%node%resolved_kind_value
    end subroutine first_intrinsic_argument_metadata

    integer function intrinsic_kind_argument(arena, node, intrinsic_name) &
            result(value)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        character(len=*), intent(in) :: intrinsic_name
        logical, allocatable :: visiting(:)
        integer :: argument_index, argument_position

        value = 0
        if (.not. allocated(node%arg_indices)) return
        argument_position = kind_argument_position(arena, node, intrinsic_name)
        if (argument_position <= 0) return
        if (argument_position > size(node%arg_indices)) return
        if (arena%size <= 0) return
        argument_index = node%arg_indices(argument_position)
        if (.not. arena%has_node_at(argument_index)) return
        select type (argument => arena%entries(argument_index)%node)
            type is (assignment_node)
            argument_index = argument%value_index
        end select
        if (.not. arena%has_node_at(argument_index)) return
        allocate (visiting(arena%size), source=.false.)
        call evaluate_integer_constant(arena, argument_index, value, visiting)
    end function intrinsic_kind_argument

    integer function kind_argument_position(arena, node, intrinsic_name) &
            result(position)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        character(len=*), intent(in) :: intrinsic_name
        integer :: second_argument

        position = 0
        if (.not. allocated(node%arg_indices)) return
        select case (intrinsic_name)
        case ("cmplx")
            if (size(node%arg_indices) >= 3) then
                position = 3
                return
            end if
            if (size(node%arg_indices) /= 2) return
            second_argument = node%arg_indices(2)
            if (.not. arena%has_node_at(second_argument)) return
            select type (argument => arena%entries(second_argument)%node)
                type is (assignment_node)
                position = 2
            end select
        case default
            if (size(node%arg_indices) >= 2) position = 2
        end select
    end function kind_argument_position

    integer pure function numeric_result_type(left_type, right_type) result(kind_id)
        integer, intent(in) :: left_type, right_type

        kind_id = 0
        if (left_type == TCOMPLEX .or. right_type == TCOMPLEX) then
            kind_id = TCOMPLEX
        else if (left_type == TREAL .or. right_type == TREAL) then
            kind_id = TREAL
        else if (left_type == TINT .and. right_type == TINT) then
            kind_id = TINT
        end if
    end function numeric_result_type

    integer pure function numeric_result_kind(result_type, left_type, left_kind, &
            right_type, right_kind) result(kind_value)
        integer, intent(in) :: result_type, left_type, left_kind
        integer, intent(in) :: right_type, right_kind

        kind_value = 0
        select case (result_type)
        case (TINT)
            kind_value = max(left_kind, right_kind)
        case (TREAL)
            if (left_type == TREAL) kind_value = max(kind_value, left_kind)
            if (right_type == TREAL) kind_value = max(kind_value, right_kind)
        case (TCOMPLEX)
            if (left_type == TCOMPLEX .or. left_type == TREAL) then
                kind_value = max(kind_value, left_kind)
            end if
            if (right_type == TCOMPLEX .or. right_type == TREAL) then
                kind_value = max(kind_value, right_kind)
            end if
        end select
    end function numeric_result_kind

    logical pure function is_relational_operator(operator) result(is_relational)
        character(len=*), intent(in) :: operator

        select case (operator)
        case ("==", "/=", "<", "<=", ">", ">=", ".eq.", ".ne.", &
                ".lt.", ".le.", ".gt.", ".ge.")
            is_relational = .true.
        case default
            is_relational = .false.
        end select
    end function is_relational_operator

    logical pure function is_logical_operator(operator) result(is_logical)
        character(len=*), intent(in) :: operator

        select case (operator)
        case (".and.", ".or.", ".not.", ".eqv.", ".neqv.")
            is_logical = .true.
        case default
            is_logical = .false.
        end select
    end function is_logical_operator

    logical function declaration_has_name(arena, node_index, name) result(has_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name
        integer :: i

        has_name = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (decl => arena%entries(node_index)%node)
            type is (declaration_node)
            if (allocated(decl%var_name)) then
                if (compact_lower(decl%var_name) == compact_lower(name)) then
                    has_name = .true.
                    return
                end if
            end if
            if (.not. allocated(decl%var_names)) return
            do i = 1, size(decl%var_names)
                if (compact_lower(decl%var_names(i)) /= compact_lower(name)) cycle
                has_name = .true.
                return
            end do
            type is (parameter_declaration_node)
            if (allocated(decl%name)) then
                has_name = compact_lower(decl%name) == compact_lower(name)
            end if
        end select
    end function declaration_has_name

    function compact_lower(text) result(compact)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: compact
        character(len=:), allocatable :: lowered
        integer :: i, count

        lowered = to_lower(trim(adjustl(text)))
        count = 0
        do i = 1, len(lowered)
            if (lowered(i:i) == " ") cycle
            if (lowered(i:i) == achar(9)) cycle
            count = count + 1
        end do
        allocate (character(len=count) :: compact)
        count = 0
        do i = 1, len(lowered)
            if (lowered(i:i) == " ") cycle
            if (lowered(i:i) == achar(9)) cycle
            count = count + 1
            compact(count:count) = lowered(i:i)
        end do
    end function compact_lower

end module frontend_compiler_type_queries
