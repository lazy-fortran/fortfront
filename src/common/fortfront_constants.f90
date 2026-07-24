module fortfront_constants
    implicit none
    private

    public :: MAX_PROGRAM_VARIABLES
    public :: MAX_PROC_NAME_LEN
    public :: INITIAL_SYMBOL_TABLE_CAPACITY
    public :: MAX_SUBST_SIZE
    public :: MAX_ENV_SIZE
    public :: MAX_EXPR_RECURSION_DEPTH
    public :: AST_ARENA_GROWTH_MINIMUM
    public :: MAX_DIAGNOSTIC_MESSAGE_LEN
    public :: MAX_TYPE_SPEC_BUFFER_LEN
    public :: MAX_PARSE_ERROR_LEN
    public :: MAX_ENV_VALUE_LEN
    public :: MAX_FRONTEND_ERROR_LEN
    public :: MAX_AST_STRING_DATA_LEN
    public :: MAX_ANALYZER_STATUS_LEN
    public :: MAX_DIMENSION_BUFFER_LEN
    public :: BASIC_ARENA_POLYMORPHIC_ITEM_SIZE
    public :: MAX_TRACE_FILE_PATH_LEN
    public :: MAX_DEBUG_TRACE_FILE_NAME_LEN
    public :: MAX_TEST_SEARCH_LINE_LEN
    public :: MAX_EXAMPLE_PATH_LEN

    integer, parameter :: MAX_PROGRAM_VARIABLES = 256
    integer, parameter :: MAX_PROC_NAME_LEN = 256
    integer, parameter :: INITIAL_SYMBOL_TABLE_CAPACITY = 256
    integer, parameter :: MAX_SUBST_SIZE = 512
    integer, parameter :: MAX_ENV_SIZE = 4096
    integer, parameter :: MAX_EXPR_RECURSION_DEPTH = 256
    integer, parameter :: AST_ARENA_GROWTH_MINIMUM = 1024
    integer, parameter :: MAX_DIAGNOSTIC_MESSAGE_LEN = 256
    integer, parameter :: MAX_TYPE_SPEC_BUFFER_LEN = 1024
    integer, parameter :: MAX_PARSE_ERROR_LEN = 512
    integer, parameter :: MAX_ENV_VALUE_LEN = 256
    integer, parameter :: MAX_FRONTEND_ERROR_LEN = 256
    integer, parameter :: MAX_AST_STRING_DATA_LEN = 256
    integer, parameter :: MAX_ANALYZER_STATUS_LEN = 256
    integer, parameter :: MAX_DIMENSION_BUFFER_LEN = 512
    integer, parameter :: BASIC_ARENA_POLYMORPHIC_ITEM_SIZE = 256
    integer, parameter :: MAX_TRACE_FILE_PATH_LEN = 512
    integer, parameter :: MAX_DEBUG_TRACE_FILE_NAME_LEN = 256
    integer, parameter :: MAX_TEST_SEARCH_LINE_LEN = 512
    integer, parameter :: MAX_EXAMPLE_PATH_LEN = 256
end module fortfront_constants
