module token_pool_helpers
    ! Helper module for working with token pools and handles
    ! Provides compatibility layer and utility functions
    use token_text_pool_mod
    use lexer_token_types, only: token_t, trivia_token_t
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: create_token_with_text
    public :: get_token_text
    public :: get_token_lower
    public :: sync_token_legacy_text
    public :: create_trivia_with_text
    public :: get_trivia_text

contains

    ! Create a token with text from a pool
    function create_token_with_text(pool, kind, text, line, column) result(tok)
        type(token_text_pool_t), intent(inout) :: pool
        integer, intent(in) :: kind
        character(len=*), intent(in) :: text
        integer, intent(in) :: line
        integer, intent(in) :: column
        type(token_t) :: tok

        tok%kind = kind
        tok%line = line
        tok%column = column
        tok%text_handle = pool%intern(text)
        ! Also set legacy text for compatibility
        tok%text = text
    end function create_token_with_text

    ! Get token text from pool
    function get_token_text(pool, tok) result(text)
        type(token_text_pool_t), intent(in) :: pool
        type(token_t), intent(in) :: tok
        character(len=:), allocatable :: text

        ! First try to use legacy text if available
        if (allocated(tok%text)) then
            text = tok%text
        else
            ! Otherwise get from pool
            text = pool%get_original(tok%text_handle)
        end if
    end function get_token_text

    ! Get lowercase token text from pool (cached)
    function get_token_lower(pool, tok) result(text)
        type(token_text_pool_t), intent(inout) :: pool
        type(token_t), intent(in) :: tok
        character(len=:), allocatable :: text

        ! First try legacy text
        if (allocated(tok%text)) then
            text = to_lower(tok%text)
        else
            ! Otherwise get cached lowercase from pool
            text = pool%get_lower(tok%text_handle)
        end if
    end function get_token_lower

    ! Sync legacy text field from handle (for migration)
    subroutine sync_token_legacy_text(pool, tok)
        type(token_text_pool_t), intent(in) :: pool
        type(token_t), intent(inout) :: tok

        if (.not. allocated(tok%text)) then
            tok%text = pool%get_original(tok%text_handle)
        end if
    end subroutine sync_token_legacy_text

    ! Create trivia with text from a pool
    function create_trivia_with_text(pool, kind, text, line, column) result(triv)
        type(token_text_pool_t), intent(inout) :: pool
        integer, intent(in) :: kind
        character(len=*), intent(in) :: text
        integer, intent(in) :: line
        integer, intent(in) :: column
        type(trivia_token_t) :: triv

        triv%kind = kind
        triv%line = line
        triv%column = column
        triv%text_handle = pool%intern(text)
    end function create_trivia_with_text

    ! Get trivia text from pool
    function get_trivia_text(pool, triv) result(text)
        type(token_text_pool_t), intent(in) :: pool
        type(trivia_token_t), intent(in) :: triv
        character(len=:), allocatable :: text

        text = pool%get_original(triv%text_handle)
    end function get_trivia_text

end module token_pool_helpers
