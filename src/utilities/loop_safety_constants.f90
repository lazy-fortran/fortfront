module loop_safety_constants
    ! Maximum iteration limits to prevent unbounded loops from hanging
    ! These constants provide safety bounds for all loops in the codebase
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    ! User input processing (lexer, parser)
    integer(int32), parameter, public :: MAX_TOKEN_ITERATIONS = 10000000_int32
    integer(int32), parameter, public :: MAX_STRING_SCAN_ITERATIONS = 100000_int32
    integer(int32), parameter, public :: MAX_PARSE_ITERATIONS = 1000000_int32
    integer(int32), parameter, public :: MAX_STATEMENT_ITERATIONS = 100000_int32

    ! String processing (codegen, utilities)
    integer(int32), parameter, public :: MAX_STRING_REPLACE_ITERATIONS = 100000_int32
    integer(int32), parameter, public :: MAX_LINE_PROCESSING = 100000_int32
    integer(int32), parameter, public :: MAX_CHAR_SCAN = 100000_int32

    ! Data structure operations
    integer(int32), parameter, public :: MAX_CAPACITY_DOUBLINGS = 50_int32
    integer(int32), parameter, public :: MAX_HASH_PROBE = 10000_int32
    integer(int32), parameter, public :: MAX_STACK_OPERATIONS = 100000_int32

    ! Graph/tree traversal
    integer(int32), parameter, public :: MAX_TREE_NODES = 1000000_int32
    integer(int32), parameter, public :: MAX_GRAPH_NODES = 100000_int32
    integer(int32), parameter, public :: MAX_NESTING_DEPTH = 1000_int32

    ! Recursive function depth limits
    integer(int32), parameter, public :: MAX_RECURSION_DEPTH = 500_int32

end module loop_safety_constants
