module parser_token_views_module
    use lexer_core, only: token_t, to_lower
    use parser_state_module, only: parser_state_t
    implicit none
    private

    type :: token_view_t
        character(len=:), allocatable :: text(:)
        character(len=:), allocatable :: lower(:)
        integer, allocatable :: kind(:)
        integer, allocatable :: line(:)
        integer, allocatable :: column(:)
        integer :: base_index = 1
        integer :: count = 0
    end type token_view_t

    public :: token_view_t
    public :: build_token_view
    public :: view_peek_token
    public :: view_lower_token
    public :: view_consume_token
    public :: view_lookahead_token

contains

    subroutine build_token_view(view, parser)
        type(token_view_t), intent(inout) :: view
        type(parser_state_t), intent(in) :: parser
        integer :: start_idx
        integer :: end_idx
        integer :: count
        integer :: max_len

        call initialize_token_view_state(view, parser, start_idx, end_idx, count)
        if (count == 0) return

        max_len = compute_max_token_length(parser, start_idx, end_idx)
        call prepare_token_view_arrays(view, count, max_len)
        call populate_token_view(view, parser, start_idx, count)
        view%base_index = start_idx
        view%count = count
    end subroutine build_token_view

    subroutine initialize_token_view_state(view, parser, start_idx, end_idx, count)
        type(token_view_t), intent(inout) :: view
        type(parser_state_t), intent(in) :: parser
        integer, intent(out) :: start_idx
        integer, intent(out) :: end_idx
        integer, intent(out) :: count

        if (.not. associated(parser%tokens)) then
            view%count = 0
            view%base_index = parser%current_token
            call release_token_view_arrays(view)
            count = 0
            start_idx = 0
            end_idx = -1
            return
        end if

        start_idx = max(parser%current_token, 1)
        end_idx = size(parser%tokens)
        count = end_idx - start_idx + 1

        if (count <= 0) then
            count = 0
            start_idx = parser%current_token
            end_idx = parser%current_token - 1
            view%count = 0
            view%base_index = parser%current_token
            call release_token_view_arrays(view)
        end if
    end subroutine initialize_token_view_state

    integer function compute_max_token_length(parser, start_idx, end_idx) &
        result(max_len)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: start_idx
        integer, intent(in) :: end_idx
        integer :: idx

        max_len = 0
        do idx = start_idx, end_idx
            if (associated(parser%tokens)) then
                max_len = max(max_len, len_trim(parser%tokens(idx)%text))
            end if
        end do
    end function compute_max_token_length

    subroutine prepare_token_view_arrays(view, count, max_len)
        type(token_view_t), intent(inout) :: view
        integer, intent(in) :: count
        integer, intent(in) :: max_len

        if (count <= 0) then
            call release_token_view_arrays(view)
            return
        end if

        if (allocated(view%text)) then
            if (size(view%text) /= count .or. len(view%text) < max_len) then
                call release_token_view_arrays(view)
            end if
        end if

        if (.not. allocated(view%text)) then
            allocate (character(len=max_len) :: view%text(count))
            allocate (character(len=max_len) :: view%lower(count))
            allocate (view%kind(count))
            allocate (view%line(count))
            allocate (view%column(count))
        end if
    end subroutine prepare_token_view_arrays

    subroutine populate_token_view(view, parser, start_idx, count)
        type(token_view_t), intent(inout) :: view
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: start_idx
        integer, intent(in) :: count
        integer :: idx
        integer :: target_idx

        do idx = 1, count
            target_idx = start_idx + idx - 1
            view%text(idx) = parser%tokens(target_idx)%text
            view%lower(idx) = to_lower(parser%tokens(target_idx)%text)
            view%kind(idx) = parser%tokens(target_idx)%kind
            view%line(idx) = parser%tokens(target_idx)%line
            view%column(idx) = parser%tokens(target_idx)%column
        end do
    end subroutine populate_token_view

    subroutine release_token_view_arrays(view)
        type(token_view_t), intent(inout) :: view

        if (allocated(view%text)) then
            block
                character(len=:), allocatable :: temp(:)
                call move_alloc(view%text, temp)
            end block
        end if

        if (allocated(view%lower)) then
            block
                character(len=:), allocatable :: temp(:)
                call move_alloc(view%lower, temp)
            end block
        end if

        if (allocated(view%kind)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(view%kind, temp)
            end block
        end if

        if (allocated(view%line)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(view%line, temp)
            end block
        end if

        if (allocated(view%column)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(view%column, temp)
            end block
        end if
    end subroutine release_token_view_arrays

    function view_peek_token(view, parser) result(token)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(in) :: parser
        type(token_t) :: token
        integer :: idx

        idx = parser%current_token - view%base_index + 1
        if (idx < 1 .or. idx > view%count) then
            token = parser%peek()
            return
        end if

        ! view%text is a fixed-width slot, right-padded to the longest token;
        ! trim the padding so callers see the original token text (matching the
        ! trimming already done by view_lower_token).
        token%text = trim(view%text(idx))
        token%kind = view%kind(idx)
        token%line = view%line(idx)
        token%column = view%column(idx)
    end function view_peek_token

    function view_lower_token(view, parser, offset) result(lowered)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(in) :: parser
        integer, intent(in), optional :: offset
        character(len=:), allocatable :: lowered
        integer :: idx
        integer :: off

        off = 0
        if (present(offset)) off = offset
        idx = parser%current_token - view%base_index + 1 + off

        if (idx < 1 .or. idx > view%count) then
            block
                type(token_t) :: fallback_token
                fallback_token = view_peek_token(view, parser)
                lowered = to_lower(fallback_token%text)
            end block
            return
        end if

        lowered = trim(view%lower(idx))
    end function view_lower_token

    function view_consume_token(view, parser) result(token)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        token = view_peek_token(view, parser)
        if (.not. parser%is_at_end()) then
            block
                type(token_t) :: discarded_token
                discarded_token = parser%consume()
            end block
        end if
    end function view_consume_token

    function view_lookahead_token(view, parser, offset) result(token)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: offset
        type(token_t) :: token
        integer :: idx

        idx = parser%current_token - view%base_index + 1 + offset
        if (idx < 1 .or. idx > view%count) then
            token = parser%peek()
            return
        end if

        token%text = trim(view%text(idx))
        token%kind = view%kind(idx)
        token%line = view%line(idx)
        token%column = view%column(idx)
    end function view_lookahead_token

end module parser_token_views_module
