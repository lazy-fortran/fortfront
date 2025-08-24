module cst_nodes
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: cst_node_t, trivia_t
    public :: CST_PROGRAM, CST_SUBROUTINE, CST_FUNCTION
    public :: CST_DECLARATION, CST_ASSIGNMENT, CST_CALL
    public :: CST_IDENTIFIER, CST_LITERAL, CST_OPERATOR
    public :: CST_COMMENT, CST_WHITESPACE, CST_NEWLINE

    ! CST Node Kind Constants
    integer, parameter :: CST_PROGRAM = 1
    integer, parameter :: CST_SUBROUTINE = 2
    integer, parameter :: CST_FUNCTION = 3
    integer, parameter :: CST_DECLARATION = 4
    integer, parameter :: CST_ASSIGNMENT = 5
    integer, parameter :: CST_CALL = 6
    integer, parameter :: CST_IDENTIFIER = 7
    integer, parameter :: CST_LITERAL = 8
    integer, parameter :: CST_OPERATOR = 9
    integer, parameter :: CST_COMMENT = 10
    integer, parameter :: CST_WHITESPACE = 11
    integer, parameter :: CST_NEWLINE = 12

    ! Trivia type for comments, whitespace, and newlines
    type :: trivia_t
        integer :: kind                         ! WHITESPACE, COMMENT, NEWLINE
        character(len=:), allocatable :: text   ! Trivia content
        integer :: start_pos                    ! Start position in source
        integer :: end_pos                      ! End position in source
    contains
        procedure :: assign_trivia
        generic :: assignment(=) => assign_trivia
    end type trivia_t

    ! Basic CST node type - Enhanced per DESIGN.md specification
    type :: cst_node_t
        integer :: kind                                      ! Node type (CST_* constants)
        integer(int64) :: uid                                ! Stable identifier
        integer :: start_pos                                 ! Start position
        integer :: end_pos                                   ! End position
        
        ! Arena handles for tree structure
        integer :: ast_link                                  ! Index to corresponding AST node
        integer, allocatable :: children(:)                 ! Child CST node indices
        type(trivia_t), allocatable :: leading_trivia(:)    ! Leading trivia
        type(trivia_t), allocatable :: trailing_trivia(:)   ! Trailing trivia
        
        ! Direct text for terminals (identifiers, literals, keywords)
        character(len=:), allocatable :: text               ! Content for terminal nodes
    contains
        procedure :: assign_cst_node
        generic :: assignment(=) => assign_cst_node
    end type cst_node_t

contains

    ! Deep copy assignment for trivia_t
    subroutine assign_trivia(this, other)
        class(trivia_t), intent(inout) :: this
        type(trivia_t), intent(in) :: other
        
        this%kind = other%kind
        this%start_pos = other%start_pos
        this%end_pos = other%end_pos
        
        if (allocated(other%text)) then
            this%text = other%text
        else
            if (allocated(this%text)) deallocate(this%text)
        end if
    end subroutine assign_trivia

    ! Deep copy assignment for cst_node_t
    subroutine assign_cst_node(this, other)
        class(cst_node_t), intent(inout) :: this
        type(cst_node_t), intent(in) :: other
        
        integer :: i
        
        this%kind = other%kind
        this%uid = other%uid
        this%start_pos = other%start_pos
        this%end_pos = other%end_pos
        this%ast_link = other%ast_link
        
        ! Deep copy children indices
        if (allocated(other%children)) then
            if (allocated(this%children)) deallocate(this%children)
            allocate(this%children(size(other%children)))
            this%children = other%children
        else
            if (allocated(this%children)) deallocate(this%children)
        end if
        
        ! Deep copy leading trivia
        if (allocated(other%leading_trivia)) then
            if (allocated(this%leading_trivia)) deallocate(this%leading_trivia)
            allocate(this%leading_trivia(size(other%leading_trivia)))
            do i = 1, size(other%leading_trivia)
                this%leading_trivia(i) = other%leading_trivia(i)
            end do
        else
            if (allocated(this%leading_trivia)) deallocate(this%leading_trivia)
        end if
        
        ! Deep copy trailing trivia
        if (allocated(other%trailing_trivia)) then
            if (allocated(this%trailing_trivia)) deallocate(this%trailing_trivia)
            allocate(this%trailing_trivia(size(other%trailing_trivia)))
            do i = 1, size(other%trailing_trivia)
                this%trailing_trivia(i) = other%trailing_trivia(i)
            end do
        else
            if (allocated(this%trailing_trivia)) deallocate(this%trailing_trivia)
        end if
        
        ! Deep copy text
        if (allocated(other%text)) then
            this%text = other%text
        else
            if (allocated(this%text)) deallocate(this%text)
        end if
    end subroutine assign_cst_node

end module cst_nodes
