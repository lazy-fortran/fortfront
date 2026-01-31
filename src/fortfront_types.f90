module fortfront_types
    ! fortfront Types and Constants - Type definitions and constants
    ! This module contains all type definitions and constants used throughout fortfront:
    ! - Symbol table and scope API types
    ! - Node type constants
    ! - Source location and diagnostic types
    ! - Function signature types

    use type_system_unified, only: mono_type_t, TVAR

    implicit none
    public

    ! ===== SYMBOL TABLE AND SCOPE API TYPES =====

    ! Symbol information for semantic queries
    type :: symbol_info_t
        character(len=:), allocatable :: name
        type(mono_type_t) :: type_info
        integer :: definition_line = 0
        integer :: definition_column = 0
        logical :: is_used = .false.
        logical :: is_parameter = .false.
        logical :: is_defined = .false.
        integer :: scope_level = 0
        character(len=:), allocatable :: intent
    end type symbol_info_t

    ! Symbol reference information for cross-reference analysis
    type :: symbol_reference_t
        integer :: node_index = 0  ! Where symbol is referenced
        integer :: scope_level = 0  ! Scope level of reference
        logical :: is_definition = .false.  ! True if this is the declaration
        logical :: is_assignment = .false.  ! True if symbol is being assigned
    end type symbol_reference_t

    ! Scope information type
    type :: scope_info_t
        integer :: level = 0
        integer :: scope_type = 0  ! SCOPE_GLOBAL, SCOPE_MODULE, etc.
        character(len=:), allocatable :: name
        integer :: symbol_count = 0
    end type scope_info_t

    ! Node type constants for type queries
    integer, parameter :: NODE_PROGRAM = 1
    integer, parameter :: NODE_FUNCTION_DEF = 2
    integer, parameter :: NODE_ASSIGNMENT = 3
    integer, parameter :: NODE_BINARY_OP = 4
    integer, parameter :: NODE_IDENTIFIER = 5
    integer, parameter :: NODE_LITERAL = 6
    integer, parameter :: NODE_ARRAY_LITERAL = 7
    integer, parameter :: NODE_CALL_OR_SUBSCRIPT = 8
    integer, parameter :: NODE_SUBROUTINE_DEF = 9
    integer, parameter :: NODE_SUBROUTINE_CALL = 10
    integer, parameter :: NODE_DECLARATION = 11
    integer, parameter :: NODE_PARAMETER_DECLARATION = 12
    integer, parameter :: NODE_IF = 13
    integer, parameter :: NODE_DO_LOOP = 14
    integer, parameter :: NODE_DO_WHILE = 15
    integer, parameter :: NODE_SELECT_CASE = 16
    integer, parameter :: NODE_CASE_BLOCK = 17
    integer, parameter :: NODE_MODULE = 18
    integer, parameter :: NODE_USE_STATEMENT = 19
    integer, parameter :: NODE_PRINT_STATEMENT = 20
    integer, parameter :: NODE_WRITE_STATEMENT = 21
    integer, parameter :: NODE_READ_STATEMENT = 22
    integer, parameter :: NODE_ALLOCATE_STATEMENT = 23
    integer, parameter :: NODE_DEALLOCATE_STATEMENT = 24
    integer, parameter :: NODE_STOP = 25
    integer, parameter :: NODE_RETURN = 26
    integer, parameter :: NODE_GOTO = 27
    integer, parameter :: NODE_ERROR_STOP = 28
    integer, parameter :: NODE_CYCLE = 29
    integer, parameter :: NODE_EXIT = 30
    integer, parameter :: NODE_WHERE = 31
    integer, parameter :: NODE_INTERFACE_BLOCK = 32
    integer, parameter :: NODE_DERIVED_TYPE = 33
    integer, parameter :: NODE_POINTER_ASSIGNMENT = 34
    integer, parameter :: NODE_FORALL = 35
    integer, parameter :: NODE_CASE_RANGE = 36
    integer, parameter :: NODE_CASE_DEFAULT = 37
    integer, parameter :: NODE_COMPLEX_LITERAL = 38
    integer, parameter :: NODE_INCLUDE_STATEMENT = 39
    integer, parameter :: NODE_CONTAINS = 40
    integer, parameter :: NODE_FORMAT_DESCRIPTOR = 41
    integer, parameter :: NODE_COMMENT = 42
    integer, parameter :: NODE_IMPLICIT_STATEMENT = 43
    integer, parameter :: NODE_FORMAT_STATEMENT = 44
    integer, parameter :: NODE_CONTINUE = 45
    integer, parameter :: NODE_PAUSE = 46
    integer, parameter :: NODE_NULLIFY = 47
    integer, parameter :: NODE_DIRECTIVE = 48
    integer, parameter :: NODE_ARRAY_BOUNDS = 50
    integer, parameter :: NODE_ARRAY_SLICE = 51
    integer, parameter :: NODE_RANGE_EXPRESSION = 52
    integer, parameter :: NODE_ARRAY_OPERATION = 53
    integer, parameter :: NODE_SUBMODULE = 54
    integer, parameter :: NODE_BLOCK_DATA = 55
    integer, parameter :: NODE_TYPE_BINDING = 56
    integer, parameter :: NODE_MIXED_CONSTRUCT = 57
    integer, parameter :: NODE_WHERE_STMT = 58
    integer, parameter :: NODE_SELECT_TYPE = 59
    integer, parameter :: NODE_ENTRY = 60
    integer, parameter :: NODE_TYPE_GUARD_BLOCK = 61
    integer, parameter :: NODE_SELECT_RANK = 62
    integer, parameter :: NODE_RANK_BLOCK = 63
    integer, parameter :: NODE_ASSOCIATE = 64
    integer, parameter :: NODE_BLOCK_CONSTRUCT = 65
    integer, parameter :: NODE_IO_IMPLIED_DO = 66
    integer, parameter :: NODE_OPEN_STATEMENT = 67
    integer, parameter :: NODE_CLOSE_STATEMENT = 68
    integer, parameter :: NODE_INQUIRE_STATEMENT = 69
    integer, parameter :: NODE_BACKSPACE_STATEMENT = 70
    integer, parameter :: NODE_REWIND_STATEMENT = 71
    integer, parameter :: NODE_ENDFILE_STATEMENT = 72
    integer, parameter :: NODE_COMPONENT_ACCESS = 73
    integer, parameter :: NODE_RANGE_SUBSCRIPT = 74
    integer, parameter :: NODE_BLANK_LINE = 75
    integer, parameter :: NODE_END_STATEMENT = 76
    integer, parameter :: NODE_INTRINSIC_STATEMENT = 77
    integer, parameter :: NODE_VISIBILITY_STATEMENT = 78
    integer, parameter :: NODE_NAMELIST_STATEMENT = 79
    integer, parameter :: NODE_DATA_STATEMENT = 80
    integer, parameter :: NODE_IMPORT_STATEMENT = 81
    integer, parameter :: NODE_STATEMENT_FUNCTION = 82
    integer, parameter :: NODE_MODULE_PROCEDURE = 83
    integer, parameter :: NODE_UNKNOWN = 99

    ! Source location tracking
    type :: source_location_t
        integer :: line = 1
        integer :: column = 1
        integer :: byte_offset = 0  ! For efficient text manipulation
    end type source_location_t

    type :: source_range_t
        type(source_location_t) :: start
        type(source_location_t) :: end
    end type source_range_t

    ! Diagnostic information
    integer, parameter :: DIAGNOSTIC_ERROR = 1
    integer, parameter :: DIAGNOSTIC_WARNING = 2
    integer, parameter :: DIAGNOSTIC_INFO = 3
    integer, parameter :: DIAGNOSTIC_HINT = 4

    ! Type information
    type :: type_info_t
        integer :: base_type = TVAR  ! TINT, TREAL, etc.
        integer :: bit_width = 32  ! 32, 64, etc.
        logical :: is_signed = .true.  ! For integers
        integer :: array_rank = 0  ! 0 for scalars
        integer, allocatable :: array_dims(:)  ! Shape if known
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        character(len=:), allocatable :: derived_type_name
    end type type_info_t

    type :: diagnostic_t
        integer :: severity = DIAGNOSTIC_INFO
        character(len=:), allocatable :: message
        type(source_range_t) :: location
        character(len=:), allocatable :: code  ! Error code (e.g., "F001")
        character(len=:), allocatable :: category  ! Error category
    end type diagnostic_t

    ! Function signature type
    type :: function_signature_t
        type(type_info_t), allocatable :: param_types(:)
        type(type_info_t) :: return_type
        logical :: is_elemental = .false.
        logical :: is_pure = .false.
    end type function_signature_t

end module fortfront_types
