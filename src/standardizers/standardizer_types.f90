module standardizer_types
    ! Type inference and utilities module
    ! Handles type analysis, expression type detection, and type string generation

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_bounds, only: array_slice_node
    use ast_nodes_loops
    use ast_nodes_misc, only: complex_literal_node
    use type_system_unified
    use type_string_utils, only: mono_type_to_string
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result, &
        ERROR_TYPE_SYSTEM
    use debug_trace, only: trace_enter, trace_leave, trace_is_enabled
    use standardizer_declarations_state, only: &
        get_shared_type_standardization => get_standardizer_type_standardization
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    ! Constants
    integer, parameter :: INVALID_INTEGER = -999999

    ! Result type for string operations
    type, public :: string_result_t
        type(result_t) :: result
        character(len=:), allocatable :: value ! Valid only if result%success = .true.
    contains

        include 'standardizer_types_part1.inc'
        include 'standardizer_types_part2.inc'

    end module standardizer_types
