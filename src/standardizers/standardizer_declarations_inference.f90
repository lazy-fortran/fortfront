module standardizer_declarations_inference
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
        identifier_node, literal_node
    use ast_base, only: LITERAL_INTEGER, LITERAL_STRING
    use lexer_core, only: to_lower
    use intrinsic_registry, only: get_intrinsic_signature, is_intrinsic_function
    use type_system_unified, only: TINT
    implicit none
    private

    public :: handle_string_concatenation
    public :: infer_type_from_binary_operation
    public :: get_string_length_from_node
    public :: infer_type_from_intrinsic_call
    public :: build_character_type_from_length
    public :: merge_character_type_lengths
    public :: is_integer_expression

contains

    function handle_string_concatenation(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type

        var_type = ""
        if (expr_index > 0 .and. expr_index <= arena%size) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                    type is (binary_op_node)
                    if (node%operator == "//") then
                        var_type = "character(len=:), allocatable"
                    end if
                end select
            end if
        end if
    end function handle_string_concatenation

    function infer_type_from_binary_operation(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type

        if (is_integer_expression(arena, expr_index)) then
            var_type = "integer"
        else
            var_type = "real"
        end if
    end function infer_type_from_binary_operation

    logical function is_integer_expression(arena, idx) result(is_int)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, allocatable :: node_stack(:)
        integer :: top, cap
        integer :: current
        logical :: ok

        is_int = .false.
        if (.not. arena%has_node_at(idx)) return

        cap = 16
        allocate (node_stack(cap))
        top = 0

        call push(idx)
        is_int = .true.

        loop_nodes: do while (top > 0)
        current = node_stack(top)
        top = top - 1

        if (current <= 0 .or. current > arena%size) then
            is_int = .false.
            exit loop_nodes
        end if
        if (.not. allocated(arena%entries(current)%node)) then
            is_int = .false.
            exit loop_nodes
        end if

        select type (node => arena%entries(current)%node)
            type is (literal_node)
            if (node%literal_kind /= LITERAL_INTEGER) then
                is_int = .false.
                exit loop_nodes
            end if
            type is (identifier_node)
            if (node%inferred_type%kind > 0) then
                if (node%inferred_type%kind /= TINT) then
                    is_int = .false.
                    exit loop_nodes
                end if
            end if
            type is (binary_op_node)
            if (trim(node%operator) == "/") then
                is_int = .false.
                exit loop_nodes
            end if
            ok = push_if_valid(node%left_index)
            if (.not. ok) then
                is_int = .false.
                exit loop_nodes
            end if
            ok = push_if_valid(node%right_index)
            if (.not. ok) then
                is_int = .false.
                exit loop_nodes
            end if
        class default
            is_int = .false.
            exit loop_nodes
        end select
    end do loop_nodes

    if (allocated(node_stack)) deallocate (node_stack)

contains

    subroutine push(index)
        integer, intent(in) :: index
        if (top >= size(node_stack)) then
            call grow_stack()
        end if
        top = top + 1
        node_stack(top) = index
    end subroutine push

    logical function push_if_valid(index) result(success)
        integer, intent(in) :: index
        success = .false.
        if (.not. arena%has_node_at(index)) return
        call push(index)
        success = .true.
    end function push_if_valid

    subroutine grow_stack()
        integer, allocatable :: tmp(:)
        allocate (tmp(cap * 2))
        tmp(1:cap) = node_stack(1:cap)
        call move_alloc(tmp, node_stack)
        cap = cap * 2
    end subroutine grow_stack

end function is_integer_expression

recursive function get_string_length_from_node(arena, node_index) &
        result(length)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: node_index
    integer :: length
    integer :: left_len
    integer :: right_len

    length = -1
    if (node_index <= 0) return
    if (node_index > arena%size) return
    if (.not. allocated(arena%entries(node_index)%node)) return

    select type (node => arena%entries(node_index)%node)
        type is (literal_node)
        if (node%literal_kind == LITERAL_STRING) then
            if (allocated(node%value)) then
                length = compute_string_literal_length(node%value)
            else
                length = 0
            end if
        end if
        type is (binary_op_node)
        if (allocated(node%operator)) then
            if (trim(node%operator) == "//") then
                left_len = get_string_length_from_node(arena, &
                    node%left_index)
                right_len = get_string_length_from_node(arena, &
                    node%right_index)
                if (left_len >= 0 .and. right_len >= 0) then
                    length = left_len + right_len
                end if
            end if
        end if
    class default
        length = -1
    end select
end function get_string_length_from_node

subroutine infer_type_from_intrinsic_call(arena, value_index, var_type)
    type(ast_arena_t), intent(in) :: arena
    integer, intent(in) :: value_index
    character(len=64), intent(inout) :: var_type
    character(len=:), allocatable :: intrinsic_sig

    if (len_trim(var_type) > 0) return
    if (value_index <= 0) return
    if (value_index > arena%size) return
    if (.not. allocated(arena%entries(value_index)%node)) return

    select type (val_node => arena%entries(value_index)%node)
        type is (call_or_subscript_node)
        if (.not. is_intrinsic_function(val_node%name)) return
        intrinsic_sig = get_intrinsic_signature(val_node%name)
        if (len_trim(intrinsic_sig) == 0) return

        if (index(intrinsic_sig, "real(") == 1) then
            var_type = "real"
        else if (index(intrinsic_sig, "integer(") == 1) then
            var_type = "integer"
        else if (index(intrinsic_sig, "unsigned_integer(") == 1) then
            var_type = "integer, unsigned"
        else if (index(intrinsic_sig, "logical(") == 1) then
            var_type = "logical"
        else if (index(intrinsic_sig, "character(") == 1) then
            var_type = "character(len=:), allocatable"
        else
            var_type = "real"
        end if
    end select
end subroutine infer_type_from_intrinsic_call

pure function build_character_type_from_length(len_value) result(type_str)
    integer, intent(in) :: len_value
    character(len=64) :: type_str
    character(len=32) :: buffer

    type_str = ""
    if (len_value < 0) return

    write (buffer, '(i0)') len_value
    type_str = "character(len=" // trim(buffer) // ")"
end function build_character_type_from_length

pure function merge_character_type_lengths(existing, incoming) result(result_type)
    character(len=*), intent(in) :: existing
    character(len=*), intent(in) :: incoming
    character(len=64) :: result_type
    integer :: existing_len
    integer :: incoming_len

    if (is_deferred_character_length(existing) .or. &
        is_deferred_character_length(incoming)) then
        result_type = "character(len=:), allocatable"
        return
    end if

    existing_len = extract_character_length(existing)
    incoming_len = extract_character_length(incoming)

    if (incoming_len < 0) then
        result_type = trim(existing)
    else if (existing_len < 0) then
        result_type = build_character_type_from_length(incoming_len)
    else
        result_type = build_character_type_from_length( &
            max(existing_len, incoming_len))
    end if
end function merge_character_type_lengths

pure integer function extract_character_length(type_str) result(len_value)
    character(len=*), intent(in) :: type_str
    character(len=:), allocatable :: lowered
    integer :: len_pos
    integer :: close_pos
    integer :: ios

    len_value = -1
    lowered = to_lower(adjustl(trim(type_str)))
    if (len(lowered) == 0) return
    if (index(lowered, "character") /= 1) return

    len_pos = index(lowered, "len=")
    if (len_pos <= 0) return
    if (len_pos + 4 > len(lowered)) return

    if (lowered(len_pos + 4:len_pos + 4) == ':' .or. &
        lowered(len_pos + 4:len_pos + 4) == '*') then
        len_value = -1
        return
    end if

    close_pos = index(lowered(len_pos:), ")")
    if (close_pos <= 0) return
    if (len_pos + close_pos - 2 < len_pos + 4) return

    read (lowered(len_pos + 4:len_pos + close_pos - 2), *, iostat=ios) len_value
    if (ios /= 0) len_value = -1
end function extract_character_length

pure logical function is_deferred_character_length(type_str) result(is_deferred)
    character(len=*), intent(in) :: type_str
    character(len=:), allocatable :: lowered
    integer :: len_pos

    lowered = to_lower(adjustl(trim(type_str)))
    len_pos = index(lowered, "len=")
    if (len_pos <= 0) then
        is_deferred = .false.
        return
    end if

    if (len_pos + 4 > len(lowered)) then
        is_deferred = .false.
        return
    end if

    is_deferred = (lowered(len_pos + 4:len_pos + 4) == ':' .or. &
        lowered(len_pos + 4:len_pos + 4) == '*')
end function is_deferred_character_length

pure integer function compute_string_literal_length(literal) result(len_value)
    character(len=*), intent(in) :: literal
    character(len=:), allocatable :: trimmed_literal
    character(len=:), allocatable :: inner
    character(len=1) :: quote_char
    integer :: trimmed_len
    integer :: i

    trimmed_literal = adjustl(trim(literal))
    trimmed_len = len_trim(trimmed_literal)
    if (trimmed_len < 2) then
        len_value = trimmed_len
        return
    end if

    quote_char = trimmed_literal(1:1)
    if (quote_char /= '"' .and. quote_char /= "'") then
        len_value = trimmed_len
        return
    end if

    if (trimmed_literal(trimmed_len:trimmed_len) /= quote_char) then
        len_value = trimmed_len
        return
    end if

    if (trimmed_len == 2) then
        len_value = 0
        return
    end if

    inner = trimmed_literal(2:trimmed_len - 1)
    len_value = len(inner)

    i = 1
    do while (i <= len(inner) - 1)
        if (inner(i:i) == quote_char .and. inner(i + 1:i + 1) == quote_char) then
            len_value = len_value - 1
            i = i + 2
        else
            i = i + 1
        end if
    end do
end function compute_string_literal_length

end module standardizer_declarations_inference
