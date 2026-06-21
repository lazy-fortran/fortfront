module type_system_unified
    ! Unified type system using arena-based storage
    ! Provides compatibility layer for legacy API while using efficient arena storage

    use, intrinsic :: iso_fortran_env, only: error_unit
    use type_system_arena
    use error_handling, only: &
        success_result, ERROR_MEMORY
    use fortfront_constants, only: MAX_ENV_SIZE
    use identifier_table, only: identifier_table_t, identifier_id_kind, &
                                identifier_table_intern, identifier_table_find
    implicit none
    private

    ! Re-export arena types and constants
    public :: type_arena_t, mono_handle_t, poly_handle_t, args_handle_t
    public :: arena_mono_type_t, arena_poly_type_t
    public :: create_type_arena, destroy_type_arena

    ! Type kind constants (compatible with legacy system)
    integer, parameter, public :: TVAR = 1 ! Type variable
    integer, parameter, public :: TINT = 2 ! Integer type
    integer, parameter, public :: TREAL = 3 ! Real type
    integer, parameter, public :: TCHAR = 4 ! Character type
    integer, parameter, public :: TLOGICAL = 5 ! Logical type
    integer, parameter, public :: TFUN = 6 ! Function type
    integer, parameter, public :: TARRAY = 7 ! Array type
    integer, parameter, public :: TCOMPLEX = 8 ! Complex type
    integer, parameter, public :: TDOUBLE = 9 ! Double precision type
    integer, parameter, public :: TDERIVED = 10 ! Derived/user-defined type

    ! Compatibility layer types (lightweight wrappers around arena handles)
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: allocation_info_t

    type :: type_var_t
        integer :: id = 0
        character(len=64) :: name = "" ! Fixed size to avoid allocatable issues
    contains
        procedure :: assign => type_var_assign
        generic :: assignment(=) => assign
    end type type_var_t

    type :: allocation_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type allocation_info_t

    ! Arena-backed types providing legacy API
    type :: mono_type_t
        type(mono_handle_t) :: handle
        type(type_arena_t), pointer :: arena => null() ! Reference to parent arena
        ! Cached values for compatibility (updated lazily)
        integer :: kind = 0
        type(type_var_t) :: var
        integer :: size = 0
        logical :: is_unsigned = .false.
        type(allocation_info_t) :: alloc_info
    contains
        procedure :: to_string => mono_type_to_string
        procedure :: assign => mono_type_assign
        procedure :: get_kind => mono_type_get_kind
        procedure :: get_size => mono_type_get_size
        procedure :: get_alloc_info => mono_type_get_alloc_info
        procedure :: sync_from_arena => mono_type_sync_from_arena
        procedure :: has_args => mono_type_has_args
        procedure :: get_arg => mono_type_get_arg
        procedure :: get_args_count => mono_type_get_args_count
        generic :: assignment(=) => assign
    end type mono_type_t

    type :: poly_type_t
        type(poly_handle_t) :: handle
        type(type_arena_t), pointer :: arena => null()
        ! Cached mono type for compatibility - simplified version without arena linkage
        integer :: mono_kind = 0
        integer :: mono_size = 0
        type(type_var_t) :: mono_var
        type(allocation_info_t) :: mono_alloc_info
        logical :: mono_is_unsigned = .false.
        logical :: mono_synced = .false.
    contains
        procedure :: assign => poly_type_assign
        procedure :: sync_mono => poly_type_sync_mono
        procedure :: get_mono => poly_type_get_mono
        generic :: assignment(=) => assign
    end type poly_type_t

    ! Maximum sizes for fixed arrays (GCC 15.2.1 compatibility)
    ! Increased for large-scale processing (100K+ lines) - Issue #1046
    ! Increase environment capacity to better handle larger inputs (Issue #1046)

    type :: substitution_t
        integer :: count = 0
        integer :: capacity = 0
        type(type_var_t), allocatable :: vars(:)
        type(mono_type_t), allocatable :: types(:)
    contains
        procedure :: add => substitution_add
        procedure :: apply => substitution_apply
        procedure :: assign => substitution_assign
        procedure :: ensure_capacity => substitution_ensure_capacity
        generic :: assignment(=) => assign
    end type substitution_t

    type :: type_env_t
        integer :: count = 0
        integer :: capacity = 0
        integer(identifier_id_kind), allocatable :: name_ids(:)
        type(poly_type_t), allocatable :: schemes(:)
        type(identifier_table_t), pointer :: identifiers => null()
        logical :: capacity_exceeded_reported = .false.
        logical :: is_fixed = .false.
    contains
        procedure :: extend => type_env_extend
        procedure :: assign => type_env_assign
        procedure :: ensure_capacity => type_env_ensure_capacity
        generic :: assignment(=) => assign
    end type type_env_t

    ! Global arena for type operations - initialized by create functions
    type(type_arena_t), target, save :: global_arena
    logical, save :: arena_initialized = .false.

    ! Public API functions (compatibility with legacy system)
    public :: create_type_var, create_mono_type, create_poly_type, create_fun_type
    public :: get_poly_forall_vars
    public :: compose_substitutions, occurs_check, free_type_vars
    public :: reset_type_system

    ! Compatibility wrapper functions for type_checker
    public :: type_has_args, type_get_arg, type_get_args_count

    ! Additional compatibility functions for semantic analyzer
    public :: type_args_allocated, type_args_size, type_args_element

contains

    include 'type_system_unified_part1.inc'
    include 'type_system_unified_part2.inc'

end module type_system_unified
