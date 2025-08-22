module parser_state_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF
    use error_reporting, only: error_collection_t
    use arena_memory, only: arena_t, arena_handle_t, arena_stats_t, &
                           create_arena, destroy_arena, null_handle
    implicit none
    private

    ! Parser state type for tracking position in token stream
    ! Supports both traditional allocatable storage and arena-based storage
    type, public :: parser_state_t
        ! Token storage - either allocatable array or arena-based
        type(token_t), allocatable :: tokens(:)
        
        ! Arena integration for memory-efficient token storage
        type(arena_t) :: token_arena
        type(arena_handle_t) :: tokens_handle  ! Single handle for entire token array
        logical :: use_arena = .false.
        
        ! Parser position and error tracking
        integer :: current_token = 1
        integer :: generation = 1  ! Generation for lifecycle tracking
        type(error_collection_t) :: errors
    contains
        procedure :: peek => parser_peek
        procedure :: consume => parser_consume
        procedure :: is_at_end => parser_is_at_end
        procedure :: match => parser_match
        procedure :: expect => parser_expect
        procedure :: error => parser_add_error
        procedure :: has_errors => parser_has_errors
        procedure :: get_error_messages => parser_get_error_messages
        
        ! Arena-specific methods
        procedure :: uses_arena_storage => parser_uses_arena_storage
        procedure :: get_memory_stats => parser_get_memory_stats
        procedure :: cleanup => parser_cleanup
        procedure :: get_token_at_index => parser_get_token_at_index
        procedure :: get_token_count => parser_get_token_count
        
        ! Assignment operator
        procedure :: assign => parser_state_assign
        generic :: assignment(=) => assign
    end type parser_state_t

    ! Public constructors
    public :: create_parser_state
    public :: create_parser_state_with_arena

contains

    ! Create parser state from tokens (traditional allocatable storage)
    function create_parser_state(tokens) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(parser_state_t) :: state

        ! Simple allocatable array storage
        if (size(tokens) > 0) then
            allocate(state%tokens(size(tokens)))
            state%tokens = tokens
        end if
        state%current_token = 1
        state%use_arena = .false.
        state%generation = 1
        state%tokens_handle = null_handle()  ! Initialize to null handle
    end function create_parser_state

    ! Create parser state with arena-based token storage
    function create_parser_state_with_arena(tokens, arena) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(arena_t), intent(in), optional :: arena
        type(parser_state_t) :: state
        integer :: total_size
        integer(1), allocatable :: token_buffer(:)
        logical :: status

        if (present(arena)) then
            ! Use provided arena for token storage
            state%token_arena = arena
            state%use_arena = .true.
        else
            ! Create new arena for token storage
            ! Use chunk size optimized for typical token arrays
            state%token_arena = create_arena(chunk_size=65536)
            state%use_arena = .true.
        end if

        ! Store entire token array in arena as a single allocation
        if (size(tokens) > 0) then
            ! Calculate total size for entire token array
            total_size = size(tokens) * (storage_size(tokens(1)) / 8)
            
            ! Allocate single arena handle for entire token array
            state%tokens_handle = state%token_arena%allocate(total_size)
            
            ! Store token array data in arena
            allocate(token_buffer(total_size))
            token_buffer = transfer(tokens, token_buffer)
            call state%token_arena%set_data(state%tokens_handle, &
                                           token_buffer, status)
            deallocate(token_buffer)
            
            ! Also keep a copy in allocatable array for now
            ! (transitional approach for compatibility)
            allocate(state%tokens(size(tokens)))
            state%tokens = tokens
        else
            state%tokens_handle = null_handle()
        end if
        
        state%current_token = 1
        state%generation = 1
    end function create_parser_state_with_arena

    ! Peek at current token without consuming it
    function parser_peek(this) result(current_token)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current_token

        if (allocated(this%tokens) .and. this%current_token >= 1 .and. this%current_token <= size(this%tokens)) then
            current_token = this%tokens(this%current_token)
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


    ! Check if parser state uses arena storage
    logical function parser_uses_arena_storage(this)
        class(parser_state_t), intent(in) :: this
        parser_uses_arena_storage = this%use_arena
    end function parser_uses_arena_storage

    ! Get memory statistics from parser state
    function parser_get_memory_stats(this) result(stats)
        class(parser_state_t), intent(in) :: this
        type(arena_stats_t) :: stats
        
        if (this%use_arena) then
            stats = this%token_arena%get_stats()
        else
            ! Traditional allocatable storage stats
            stats%total_allocated = 0
            stats%total_capacity = 0
            if (allocated(this%tokens)) then
                stats%total_allocated = size(this%tokens) * &
                                       (storage_size(this%tokens(1)) / 8)
                stats%total_capacity = stats%total_allocated
            end if
            stats%chunk_count = 0
            stats%current_generation = this%generation
            stats%utilization = 1.0
        end if
    end function parser_get_memory_stats

    ! Clean up parser state and advance generation
    subroutine parser_cleanup(this)
        class(parser_state_t), intent(inout) :: this
        
        ! Advance generation to invalidate references
        this%generation = this%generation + 1
        
        if (this%use_arena) then
            ! Reset arena to reclaim all memory
            call this%token_arena%reset()
            ! Invalidate the handle
            this%tokens_handle = null_handle()
        end if
        
        ! Clear tokens
        if (allocated(this%tokens)) then
            deallocate(this%tokens)
        end if
        
        ! Reset position
        this%current_token = 1
    end subroutine parser_cleanup

    ! Get token at specific index
    function parser_get_token_at_index(this, index) result(token)
        class(parser_state_t), intent(in) :: this
        integer, intent(in) :: index
        type(token_t) :: token
        
        if (allocated(this%tokens) .and. index >= 1 .and. &
            index <= size(this%tokens)) then
            token = this%tokens(index)
        else
            ! Return EOF token for out-of-bounds access
            token%kind = TK_EOF
            token%text = ""
            token%line = 1
            token%column = 1
        end if
    end function parser_get_token_at_index

    ! Get total token count
    function parser_get_token_count(this) result(count)
        class(parser_state_t), intent(in) :: this
        integer :: count
        
        if (allocated(this%tokens)) then
            count = size(this%tokens)
        else
            count = 0
        end if
    end function parser_get_token_count

    ! Assignment operator for parser_state_t (deep copy)
    subroutine parser_state_assign(lhs, rhs)
        class(parser_state_t), intent(out) :: lhs
        type(parser_state_t), intent(in) :: rhs

        ! Copy scalar fields
        lhs%current_token = rhs%current_token
        lhs%generation = rhs%generation
        lhs%use_arena = rhs%use_arena
        lhs%errors = rhs%errors
        
        ! Copy arena if used
        if (rhs%use_arena) then
            lhs%token_arena = rhs%token_arena
            lhs%tokens_handle = rhs%tokens_handle
        end if
        
        ! Copy tokens array
        if (allocated(rhs%tokens)) then
            allocate(lhs%tokens(size(rhs%tokens)))
            lhs%tokens = rhs%tokens
        end if
    end subroutine parser_state_assign

end module parser_state_module
