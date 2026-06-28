module semantic_binary_operations
    ! Binary operation inference logic extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: mono_type_t, create_mono_type, TCHAR, &
        TLOGICAL
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
        identifier_node, literal_node
    use ast_base, only: LITERAL_STRING
    use lexer_core, only: to_lower
    implicit none
    private

    public :: infer_string_concatenation
    public :: infer_comparison_operation
    public :: infer_logical_operation

contains

    ! Calculate string concatenation result type
    function infer_string_concatenation(arena, left_index, right_index, &
            left_typ, right_typ) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: left_index, right_index
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ
        integer :: left_size, right_size, total_size
        logical :: can_calculate_size
        integer :: inferred_left
        integer :: inferred_right

        ! Initialize sizes as unknown
        left_size = -1
        right_size = -1
        can_calculate_size = .false.

        ! Attempt to determine sizes from AST structure first
        if (left_typ%kind == TCHAR) then
            inferred_left = estimate_character_length(arena, left_index)
            if (inferred_left >= 0) then
                left_size = inferred_left
            else if (left_typ%size > 0 .and. &
                    .not. left_typ%alloc_info%needs_allocatable_string) then
                left_size = left_typ%size
            end if
        end if

        if (right_typ%kind == TCHAR) then
            inferred_right = estimate_character_length(arena, right_index)
            if (inferred_right >= 0) then
                right_size = inferred_right
            else if (right_typ%size > 0 .and. &
                    .not. right_typ%alloc_info%needs_allocatable_string) then
                right_size = right_typ%size
            end if
        end if

        ! Treat zero-length results as known lengths when explicitly inferred
        if (left_size < 0 .and. left_typ%kind == TCHAR .and. &
            left_typ%size == 0 .and. &
            .not. left_typ%alloc_info%needs_allocatable_string) then
            left_size = 0
        end if

        if (right_size < 0 .and. right_typ%kind == TCHAR .and. &
            right_typ%size == 0 .and. &
            .not. right_typ%alloc_info%needs_allocatable_string) then
            right_size = 0
        end if

        ! If we can determine both sizes, calculate total
        if (left_size >= 0 .and. right_size >= 0 .and. left_typ%kind == TCHAR &
            .and. right_typ%kind == TCHAR) then
            total_size = left_size + right_size
            can_calculate_size = .true.
        end if

        ! Create appropriate character type
        if (can_calculate_size) then
            typ = create_mono_type(TCHAR, char_size=total_size)
            typ%alloc_info%needs_allocatable_string = .false.
        else
            typ = create_mono_type(TCHAR)
            typ%alloc_info%needs_allocatable_string = .true.
        end if
    end function infer_string_concatenation

    recursive function estimate_character_length(arena, expr_index) result(len_out)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        integer :: len_out
        integer :: left_len
        integer :: right_len
        logical :: needs_alloc
        character(len=:), allocatable :: lowered_name

        len_out = -1
        if (expr_index <= 0) return
        if (expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
            type is (identifier_node)
            if (node%inferred_type%kind == TCHAR .and. &
                node%inferred_type%size >= 0 .and. &
                .not. node%inferred_type%alloc_info%needs_allocatable_string) then
                len_out = node%inferred_type%size
            end if
            type is (literal_node)
            if (node%literal_kind == LITERAL_STRING) then
                len_out = literal_string_length(node)
                if (len_out < 0) then
                    if (node%inferred_type%kind == TCHAR) then
                        if (node%inferred_type%size >= 0) then
                            needs_alloc = node%inferred_type%alloc_info% &
                                needs_allocatable_string
                            if (.not. needs_alloc) then
                                len_out = node%inferred_type%size
                            end if
                        end if
                    end if
                end if
            end if
            type is (call_or_subscript_node)
            if (allocated(node%name)) then
                lowered_name = to_lower(trim(node%name))
                select case (lowered_name)
                case ("trim", "adjustl", "adjustr")
                    if (allocated(node%arg_indices)) then
                        if (size(node%arg_indices) >= 1) then
                            len_out = estimate_character_length(arena, &
                                node%arg_indices(1))
                        end if
                    end if
                end select
            end if
            type is (binary_op_node)
            if (allocated(node%operator)) then
                if (trim(node%operator) == "//") then
                    left_len = estimate_character_length(arena, node%left_index)
                    right_len = estimate_character_length(arena, node%right_index)
                    if (left_len >= 0 .and. right_len >= 0) then
                        len_out = left_len + right_len
                    end if
                end if
            end if
        end select
    end function estimate_character_length

    integer function literal_string_length(literal) result(len_value)
        type(literal_node), intent(in) :: literal
        character(len=:), allocatable :: trimmed_value
        character(len=:), allocatable :: inner_value
        character(len=1) :: quote_char
        integer :: trimmed_length
        integer :: i

        len_value = -1
        if (.not. allocated(literal%value)) return

        trimmed_value = adjustl(trim(literal%value))
        trimmed_length = len_trim(trimmed_value)
        if (trimmed_length <= 0) then
            len_value = 0
            return
        end if

        quote_char = trimmed_value(1:1)
        if (quote_char /= '"' .and. quote_char /= "'") then
            len_value = trimmed_length
            return
        end if

        if (trimmed_length == 1) then
            len_value = 0
            return
        end if

        if (trimmed_value(trimmed_length:trimmed_length) /= quote_char) then
            len_value = trimmed_length - 1
            if (len_value < 0) len_value = 0
            return
        end if

        if (trimmed_length == 2) then
            len_value = 0
            return
        end if

        inner_value = trimmed_value(2:trimmed_length - 1)
        len_value = len(inner_value)

        i = 1
        do while (i <= len(inner_value) - 1)
            if (inner_value(i:i) == quote_char .and. &
                inner_value(i + 1:i + 1) == quote_char) then
                len_value = len_value - 1
                i = i + 2
            else
                i = i + 1
            end if
        end do
    end function literal_string_length

    ! Infer comparison operation result type
    function infer_comparison_operation(left_typ, right_typ) result(typ)
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ

        ! Comparison operators always return logical
        typ = create_mono_type(TLOGICAL)
    end function infer_comparison_operation

    ! Infer logical operation result type
    function infer_logical_operation() result(typ)
        type(mono_type_t) :: typ

        ! Logical operators always return logical
        typ = create_mono_type(TLOGICAL)
    end function infer_logical_operation

end module semantic_binary_operations
