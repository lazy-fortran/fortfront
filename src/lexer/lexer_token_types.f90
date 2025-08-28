module lexer_token_types
    use error_handling
    implicit none
    private

    ! Token types
    integer, parameter, public :: TK_EOF = 0
    integer, parameter, public :: TK_IDENTIFIER = 1
    integer, parameter, public :: TK_NUMBER = 2
    integer, parameter, public :: TK_STRING = 3
    integer, parameter, public :: TK_OPERATOR = 4
    integer, parameter, public :: TK_KEYWORD = 5
    integer, parameter, public :: TK_NEWLINE = 6
    integer, parameter, public :: TK_COMMENT = 7
    integer, parameter, public :: TK_WHITESPACE = 8    ! New: for CST trivia
    integer, parameter, public :: TK_UNKNOWN = 99
    
    ! Backward compatibility aliases
    integer, parameter, public :: TOKEN_WHITESPACE = TK_WHITESPACE
    integer, parameter, public :: TOKEN_COMMENT = TK_COMMENT
    integer, parameter, public :: TOKEN_NEWLINE = TK_NEWLINE

    ! Trivia token type (simpler, non-recursive)
    type, public :: trivia_token_t
        integer :: kind = TK_UNKNOWN
        character(len=:), allocatable :: text
        integer :: line = 1
        integer :: column = 1
    contains
        procedure :: assign => trivia_token_assign
        generic :: assignment(=) => assign
    end type trivia_token_t

    ! Token structure
    type, public :: token_t
        integer :: kind = TK_UNKNOWN
        character(len=:), allocatable :: text
        integer :: line = 1
        integer :: column = 1
        ! CST trivia storage (optional) - using separate type to avoid recursion
        type(trivia_token_t), allocatable :: leading_trivia(:)
        type(trivia_token_t), allocatable :: trailing_trivia(:)
    contains
        procedure :: assign => token_assign
        procedure :: deep_copy => token_deep_copy
        generic :: assignment(=) => assign
    end type token_t

    ! Lexer options for configurable behavior
    type, public :: lexer_options_t
        logical :: preserve_whitespace = .false.
        logical :: preserve_comments = .false.
        logical :: include_trivia = .false.
        logical :: case_sensitive = .false.
    end type lexer_options_t

    ! Tokenization result type
    type, public :: tokenize_result_t
        type(token_t), allocatable :: tokens(:)
        type(result_t) :: result
        integer :: token_count = 0
        logical :: success = .false.
    end type tokenize_result_t

    ! Scanning result type for safe operations
    type, public :: scan_result_t
        integer :: chars_consumed = 0
        type(result_t) :: result
        logical :: success = .false.
    end type scan_result_t

    ! Public utilities
    public :: token_type_name

contains

    ! Assignment operator for trivia tokens
    subroutine trivia_token_assign(this, other)
        class(trivia_token_t), intent(out) :: this
        type(trivia_token_t), intent(in) :: other
        
        this%kind = other%kind
        this%line = other%line
        this%column = other%column
        
        if (allocated(other%text)) then
            this%text = other%text
        end if
    end subroutine trivia_token_assign

    ! Assignment operator for tokens with deep copy
    subroutine token_assign(this, other)
        class(token_t), intent(out) :: this
        type(token_t), intent(in) :: other
        integer :: i
        
        this%kind = other%kind
        this%line = other%line
        this%column = other%column
        
        if (allocated(other%text)) then
            this%text = other%text
        end if
        
        ! Copy leading trivia
        if (allocated(other%leading_trivia)) then
            allocate(this%leading_trivia(size(other%leading_trivia)))
            do i = 1, size(other%leading_trivia)
                this%leading_trivia(i) = other%leading_trivia(i)
            end do
        end if
        
        ! Copy trailing trivia
        if (allocated(other%trailing_trivia)) then
            allocate(this%trailing_trivia(size(other%trailing_trivia)))
            do i = 1, size(other%trailing_trivia)
                this%trailing_trivia(i) = other%trailing_trivia(i)
            end do
        end if
    end subroutine token_assign

    ! Deep copy function for tokens
    function token_deep_copy(this) result(copy)
        class(token_t), intent(in) :: this
        type(token_t) :: copy
        
        copy = this
    end function token_deep_copy

    ! Get token type name for debugging
    function token_type_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=:), allocatable :: name
        
        select case (kind)
        case (TK_EOF)
            name = "EOF"
        case (TK_IDENTIFIER)
            name = "IDENTIFIER"
        case (TK_NUMBER)
            name = "NUMBER"
        case (TK_STRING)
            name = "STRING"
        case (TK_OPERATOR)
            name = "OPERATOR"
        case (TK_KEYWORD)
            name = "KEYWORD"
        case (TK_NEWLINE)
            name = "NEWLINE"
        case (TK_COMMENT)
            name = "COMMENT"
        case (TK_WHITESPACE)
            name = "WHITESPACE"
        case default
            name = "UNKNOWN"
        end select
    end function token_type_name

end module lexer_token_types