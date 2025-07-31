module ast_types
    ! This module now simply re-exports the stack-based AST types from ast_core
    ! All AST types are now defined in ast_core with stack-based storage
    use ast_core, only: ast_node, program_node, assignment_node, binary_op_node, &
          call_or_subscript_node, subroutine_call_node, identifier_node, literal_node, &
                        ast_arena_t, ast_arena_stats_t, create_ast_arena, &
                       LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL, &
                create_program, create_assignment, create_binary_op, &
     create_call_or_subscript, create_subroutine_call, create_identifier, create_literal
    implicit none

    ! Re-export everything from ast_core
    public :: ast_node, program_node, assignment_node, binary_op_node
   public :: call_or_subscript_node, subroutine_call_node, identifier_node, literal_node
    public :: ast_arena_t, ast_arena_stats_t
    public :: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    public :: create_ast_arena, create_program, create_assignment, create_binary_op
    public :: create_call_or_subscript, create_subroutine_call, create_identifier, create_literal

end module ast_types
