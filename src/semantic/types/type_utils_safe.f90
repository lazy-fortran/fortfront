module type_utils_safe
    ! Safe type utilities without self-referential allocatable patterns
    ! Provides minimal interface needed by type_system_bridge
    use type_constants
    implicit none
    private

    ! Public safe interfaces
    public :: safe_type_var_t, safe_mono_type_t, safe_poly_type_t
    public :: create_safe_type_var, create_safe_mono_type

    ! Type variable - safe version with fixed-size name
    type :: safe_type_var_t
        integer :: id
        character(len=64) :: name = ""  ! Fixed size to avoid allocatable
    end type safe_type_var_t

    ! Safe monomorphic type - uses interface access patterns to avoid 
    ! direct self-referential allocatables
    type :: safe_mono_type_t
        integer :: kind = TINT
        type(safe_type_var_t) :: var
        integer :: size = 0
        logical :: has_args = .false.
        integer :: args_count = 0
        
        ! Allocation info fields directly embedded
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
    end type safe_mono_type_t

    ! Safe polymorphic type - simplified structure
    type :: safe_poly_type_t
        type(safe_mono_type_t) :: mono
        logical :: has_forall = .false.
        integer :: forall_count = 0
    end type safe_poly_type_t

contains

    ! Create a safe type variable
    function create_safe_type_var(id, name) result(var)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(safe_type_var_t) :: var
        
        var%id = id
        if (present(name)) then
            var%name = name
        else
            var%name = ""
        end if
    end function create_safe_type_var

    ! Create a safe monomorphic type
    function create_safe_mono_type(kind) result(mono)
        integer, intent(in) :: kind
        type(safe_mono_type_t) :: mono
        
        mono%kind = kind
        mono%has_args = .false.
        mono%args_count = 0
    end function create_safe_mono_type

end module type_utils_safe