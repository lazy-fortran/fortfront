module parser_state_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF
    use error_reporting, only: error_collection_t
    use arena_memory, only: arena_t, arena_handle_t, arena_stats_t, create_arena, is_valid_handle, null_handle
    implicit none
    private

    ! Parser state type for tracking position in token stream
    type, public :: parser_state_t
        ! Arena-based token storage infrastructure (transitional)
        type(arena_t) :: token_arena
        type(arena_handle_t) :: tokens_handle
        integer :: token_count = 0
        integer :: current_token = 1
        type(error_collection_t) :: errors
        integer :: generation = 1  ! For arena safety
        ! Compatibility: cached token array for backward compatibility
        type(token_t), allocatable :: tokens(:)  ! Will be populated on demand
    contains
        procedure :: peek => parser_peek
        procedure :: consume => parser_consume
        procedure :: is_at_end => parser_is_at_end
        procedure :: match => parser_match
        procedure :: expect => parser_expect
        procedure :: error => parser_add_error
        procedure :: has_errors => parser_has_errors
        procedure :: get_error_messages => parser_get_error_messages
        procedure :: deep_copy => parser_state_deep_copy
        procedure :: assign => parser_state_assign
        procedure :: get_token_at_index => parser_get_token_at_index
        procedure :: get_all_tokens => parser_get_all_tokens
        procedure :: get_token_range => parser_get_token_range
        procedure :: get_token_count => parser_get_token_count
        procedure :: ensure_tokens_cached => parser_ensure_tokens_cached
        procedure :: uses_arena_storage => parser_uses_arena_storage
        procedure :: get_memory_stats => parser_get_memory_stats
        procedure :: cleanup => parser_cleanup
        generic :: assignment(=) => assign
    end type parser_state_t

    ! Public constructors
    public :: create_parser_state, create_parser_state_with_arena

contains

    ! Create parser state from tokens (backward compatibility)
    function create_parser_state(tokens) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(parser_state_t) :: state

        state = create_parser_state_with_arena(tokens)
    end function create_parser_state

    ! Create parser state with arena-based token storage
    function create_parser_state_with_arena(tokens) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(parser_state_t) :: state
        integer(1), allocatable :: token_buffer(:)
        logical :: status
        integer :: token_size

        ! Initialize arena for token storage
        state%token_arena = create_arena(chunk_size=8192)
        state%token_count = size(tokens)
        state%current_token = 1
        state%generation = 1

        ! Store tokens in compatibility array (primary storage)
        if (state%token_count > 0) then
            allocate(state%tokens(size(tokens)))
            state%tokens = tokens
            
            ! Also store in arena infrastructure for future migration
            token_size = storage_size(tokens(1)) / 8 * size(tokens)
            state%tokens_handle = state%token_arena%allocate(token_size)
            
            if (is_valid_handle(state%tokens_handle)) then
                ! Convert tokens to byte buffer for arena storage
                token_buffer = transfer(tokens, token_buffer)
                call state%token_arena%set_data(state%tokens_handle, token_buffer, status)
                if (.not. status) then
                    ! Arena storage failed, use null handle
                    state%tokens_handle = null_handle()
                end if
            else
                state%tokens_handle = null_handle()
            end if
        else
            state%tokens_handle = null_handle()
        end if
    end function create_parser_state_with_arena

    ! Peek at current token without consuming it
    function parser_peek(this) result(current_token)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current_token

        if (this%current_token <= this%token_count) then
            current_token = this%get_token_at_index(this%current_token)
        else
            ! Return EOF token
            current_token%kind = TK_EOF
            current_token%text = ""
            current_token%line = 1
            current_token%column = 1
        end if
    end function parser_peek

    ! Consume current token and advance
    function parser_consume(this) result(consumed_token)
        class(parser_state_t), intent(inout) :: this
        type(token_t) :: consumed_token

        consumed_token = this%peek()
        if (.not. this%is_at_end()) then
            this%current_token = this%current_token + 1
        end if
    end function parser_consume

    ! Check if we're at the end of tokens
    logical function parser_is_at_end(this)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current

        current = this%peek()
        parser_is_at_end = (current%kind == TK_EOF)
    end function parser_is_at_end

    ! Check if current token matches expected kind and consume if so
    logical function parser_match(this, expected_kind)
        class(parser_state_t), intent(inout) :: this
        integer, intent(in) :: expected_kind
        type(token_t) :: current, consumed

        current = this%peek()
        if (current%kind == expected_kind) then
            consumed = this%consume()
            parser_match = .true.
        else
            parser_match = .false.
        end if
    end function parser_match

    ! Expect a specific token kind, add error if not found
    logical function parser_expect(this, expected_kind, error_message)
        class(parser_state_t), intent(inout) :: this
        integer, intent(in) :: expected_kind
        character(len=*), intent(in), optional :: error_message
        type(token_t) :: current
        character(len=:), allocatable :: msg

        current = this%peek()
        if (current%kind == expected_kind) then
            current = this%consume()
            parser_expect = .true.
        else
            parser_expect = .false.
            if (present(error_message)) then
                msg = error_message
            else
                msg = "Unexpected token"
            end if
            call this%errors%add_error_with_token(msg, current)
        end if
    end function parser_expect

    ! Add error with current token context
    subroutine parser_add_error(this, message, suggestion)
        class(parser_state_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: suggestion
        type(token_t) :: current

        current = this%peek()
        call this%errors%add_error_with_token(message, current, suggestion=suggestion)
    end subroutine parser_add_error

    ! Check if parser has any errors
    logical function parser_has_errors(this)
        class(parser_state_t), intent(in) :: this
        parser_has_errors = this%errors%has_errors()
    end function parser_has_errors

    ! Get formatted error messages
    function parser_get_error_messages(this) result(messages)
        class(parser_state_t), intent(in) :: this
        character(len=:), allocatable :: messages
        messages = this%errors%format_messages()
    end function parser_get_error_messages

    ! Get token at specific index (backward compatibility - uses cached array)
    function parser_get_token_at_index(this, index) result(token)
        class(parser_state_t), intent(in) :: this
        integer, intent(in) :: index
        type(token_t) :: token

        ! Initialize to EOF in case of failure
        token%kind = TK_EOF
        token%text = ""
        token%line = 1
        token%column = 1

        ! Bounds check
        if (index < 1 .or. index > this%token_count) return
        
        ! Use compatibility array (primary storage for now)
        if (allocated(this%tokens) .and. index <= size(this%tokens)) then
            token = this%tokens(index)
            return
        end if
        
        ! Fallback: Could implement arena extraction here in the future
        ! For now, return EOF to prevent crashes
    end function parser_get_token_at_index

    ! Get all tokens as traditional array (for compatibility)
    function parser_get_all_tokens(this) result(tokens)
        class(parser_state_t), intent(in) :: this
        type(token_t), allocatable :: tokens(:)

        ! Use primary storage (compatibility array)
        if (allocated(this%tokens)) then
            allocate(tokens(size(this%tokens)))
            tokens = this%tokens
        else
            ! Empty token array
            allocate(tokens(0))
        end if
    end function parser_get_all_tokens

    ! Get token range (for slicing operations like parser%tokens(start:end))
    function parser_get_token_range(this, start_idx, end_idx) result(tokens)
        class(parser_state_t), intent(in) :: this
        integer, intent(in) :: start_idx
        integer, intent(in), optional :: end_idx
        type(token_t), allocatable :: tokens(:)
        type(token_t), allocatable :: all_tokens(:)
        integer :: actual_end, range_size

        ! Get all tokens
        all_tokens = this%get_all_tokens()
        
        if (size(all_tokens) == 0) then
            allocate(tokens(0))
            return
        end if

        ! Determine actual end index
        if (present(end_idx)) then
            actual_end = min(end_idx, size(all_tokens))
        else
            actual_end = size(all_tokens)
        end if

        ! Validate range
        if (start_idx < 1 .or. start_idx > actual_end) then
            allocate(tokens(0))
            return
        end if

        ! Extract range
        range_size = actual_end - start_idx + 1
        allocate(tokens(range_size))
        tokens = all_tokens(start_idx:actual_end)
    end function parser_get_token_range

    ! Get token count (replacement for size(parser%tokens))
    function parser_get_token_count(this) result(count)
        class(parser_state_t), intent(in) :: this
        integer :: count
        count = this%token_count
    end function parser_get_token_count

    ! Ensure tokens array is cached for compatibility access
    subroutine parser_ensure_tokens_cached(this)
        class(parser_state_t), intent(inout) :: this

        if (.not. allocated(this%tokens)) then
            this%tokens = this%get_all_tokens()
        end if
    end subroutine parser_ensure_tokens_cached

    ! Check if parser uses arena storage
    logical function parser_uses_arena_storage(this)
        class(parser_state_t), intent(in) :: this
        
        ! Defensive check - for now, return true if we have any arena handle
        parser_uses_arena_storage = is_valid_handle(this%tokens_handle)
    end function parser_uses_arena_storage

    ! Get memory statistics for performance measurement
    function parser_get_memory_stats(this) result(stats)
        class(parser_state_t), intent(in) :: this
        type(arena_stats_t) :: stats
        
        if (this%uses_arena_storage()) then
            stats = this%token_arena%get_stats()
        else
            ! Traditional storage estimation based on token count
            stats%total_allocated = this%token_count * 128  ! Rough token_t size estimate
            stats%total_capacity = stats%total_allocated
            stats%chunk_count = 1  ! Single allocatable array
            stats%current_generation = this%generation
            stats%utilization = 1.0  ! Allocatable arrays are fully utilized
        end if
    end function parser_get_memory_stats

    ! Cleanup arena resources
    subroutine parser_cleanup(this)
        class(parser_state_t), intent(inout) :: this
        
        ! Arena cleanup is automatic, but we can reset state
        this%tokens_handle = null_handle()
        this%token_count = 0
        this%current_token = 1
        this%generation = this%generation + 1
    end subroutine parser_cleanup

    ! Deep copy parser state
    function parser_state_deep_copy(this) result(copy)
        class(parser_state_t), intent(in) :: this
        type(parser_state_t) :: copy
        integer(1), allocatable :: token_buffer(:)
        logical :: status
        integer :: token_size

        copy%current_token = this%current_token
        copy%token_count = this%token_count
        copy%generation = this%generation + 1  ! New generation for safety

        ! Copy tokens array (primary storage)
        if (allocated(this%tokens)) then
            allocate(copy%tokens(size(this%tokens)))
            copy%tokens = this%tokens
        end if
        
        ! Create new arena for copy
        copy%token_arena = create_arena(chunk_size=8192)
        copy%tokens_handle = null_handle()  ! Safe default
        
        ! Copy arena data if valid and we have tokens
        if (is_valid_handle(this%tokens_handle) .and. allocated(copy%tokens) .and. size(copy%tokens) > 0) then
            ! Store tokens in new arena
            token_size = storage_size(copy%tokens(1)) / 8 * size(copy%tokens)
            copy%tokens_handle = copy%token_arena%allocate(token_size)
            
            if (is_valid_handle(copy%tokens_handle)) then
                token_buffer = transfer(copy%tokens, token_buffer)
                call copy%token_arena%set_data(copy%tokens_handle, token_buffer, status)
                if (.not. status) then
                    copy%tokens_handle = null_handle()
                end if
            end if
        end if
    end function parser_state_deep_copy

    ! Assignment operator for parser_state_t (deep copy)
    subroutine parser_state_assign(lhs, rhs)
        class(parser_state_t), intent(out) :: lhs
        type(parser_state_t), intent(in) :: rhs
        integer(1), allocatable :: token_buffer(:)
        logical :: status
        integer :: token_size

        ! Copy scalar fields
        lhs%current_token = rhs%current_token
        lhs%token_count = rhs%token_count
        lhs%generation = rhs%generation
        
        ! Copy tokens array (primary storage)
        if (allocated(rhs%tokens)) then
            allocate(lhs%tokens(size(rhs%tokens)))
            lhs%tokens = rhs%tokens
        end if
        
        ! Initialize arena safely
        lhs%token_arena = create_arena(chunk_size=8192)
        lhs%tokens_handle = null_handle()  ! Safe default
        
        ! Store tokens in arena if we have them
        if (allocated(lhs%tokens) .and. size(lhs%tokens) > 0) then
            token_size = storage_size(lhs%tokens(1)) / 8 * size(lhs%tokens)
            lhs%tokens_handle = lhs%token_arena%allocate(token_size)
            
            if (is_valid_handle(lhs%tokens_handle)) then
                token_buffer = transfer(lhs%tokens, token_buffer)
                call lhs%token_arena%set_data(lhs%tokens_handle, token_buffer, status)
                if (.not. status) then
                    lhs%tokens_handle = null_handle()
                end if
            end if
        end if
    end subroutine parser_state_assign

end module parser_state_module
