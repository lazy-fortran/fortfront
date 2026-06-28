module ast_nodes_misc
    use ast_base, only: ast_node, string_t, &
        ast_visitor_base_t
    implicit none
    private

    ! Miscellaneous AST nodes

    ! Comment node
    type, extends(ast_node), public :: comment_node
        character(len=:), allocatable :: text
    contains

        include 'ast_nodes_misc_part1.inc'
        include 'ast_nodes_misc_part2.inc'

    end module ast_nodes_misc
