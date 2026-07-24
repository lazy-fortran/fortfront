module cst_nodes
    implicit none
    private

    public :: trivia_t
    public :: CST_COMMENT, CST_WHITESPACE, CST_NEWLINE

    ! Trivia kinds. The numbering is part of the public API: fluff compares
    ! against these values through the fortfront facade.
    integer, parameter :: CST_COMMENT = 10
    integer, parameter :: CST_WHITESPACE = 11
    integer, parameter :: CST_NEWLINE = 12

    ! One run of comment, whitespace, or newline text recovered from the token
    ! stream and attached to an AST node. This is lexical trivia, not a node of
    ! a concrete syntax tree: nothing in this repository builds such a tree.
    type :: trivia_t
        integer :: kind ! CST_COMMENT/WHITESPACE/NEWLINE
        character(len=:), allocatable :: text ! Trivia content
        integer :: start_pos ! Start position in source
        integer :: end_pos ! End position in source
    contains
        procedure :: assign_trivia
        generic :: assignment(=) => assign_trivia
    end type trivia_t

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
            if (allocated(this%text)) deallocate (this%text)
        end if
    end subroutine assign_trivia

end module cst_nodes
