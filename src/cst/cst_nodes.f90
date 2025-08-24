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
    end type trivia_t

    ! Basic CST node type
    type :: cst_node_t
        integer :: kind                                      ! Node type
        integer(int64) :: uid                                ! Stable identifier
        integer :: start_pos                                 ! Start position
        integer :: end_pos                                   ! End position
        type(trivia_t), allocatable :: leading_trivia(:)    ! Leading trivia
        type(trivia_t), allocatable :: trailing_trivia(:)   ! Trailing trivia
    end type cst_node_t

end module cst_nodes