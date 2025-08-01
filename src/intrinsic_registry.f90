module intrinsic_registry
    implicit none
    private

    ! Public interfaces
    public :: is_intrinsic_function
    public :: get_intrinsic_signature
    public :: get_intrinsic_info
    public :: initialize_intrinsic_registry

    ! Intrinsic function signature type
    type, public :: intrinsic_signature_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: return_type
        character(len=:), allocatable :: arg_types
        character(len=:), allocatable :: description
    end type intrinsic_signature_t

    ! Registry storage
    type(intrinsic_signature_t), allocatable :: intrinsic_functions(:)
    logical :: registry_initialized = .false.

contains

    ! Combined function to get intrinsic information efficiently
    subroutine get_intrinsic_info(name, is_intrinsic, signature)
        character(len=*), intent(in) :: name
        logical, intent(out) :: is_intrinsic
        character(len=:), allocatable, intent(out) :: signature
        integer :: i

        if (.not. registry_initialized) call initialize_intrinsic_registry()

        is_intrinsic = .false.
        if (allocated(signature)) deallocate(signature)
        if (.not. allocated(intrinsic_functions)) return

        do i = 1, size(intrinsic_functions)
            if (trim(to_lower(intrinsic_functions(i)%name)) == trim(to_lower(name))) then
                is_intrinsic = .true.
                signature = intrinsic_functions(i)%return_type // "(" // &
                           intrinsic_functions(i)%arg_types // ")"
                return
            end if
        end do
    end subroutine get_intrinsic_info

    ! Check if a function name corresponds to an intrinsic function
    function is_intrinsic_function(name) result(is_intrinsic)
        character(len=*), intent(in) :: name
        logical :: is_intrinsic
        character(len=:), allocatable :: signature

        call get_intrinsic_info(name, is_intrinsic, signature)
    end function is_intrinsic_function

    ! Get signature information for an intrinsic function
    function get_intrinsic_signature(name) result(signature)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: signature
        logical :: is_intrinsic

        call get_intrinsic_info(name, is_intrinsic, signature)
        if (.not. is_intrinsic) signature = ""
    end function get_intrinsic_signature

    ! Initialize the intrinsic function registry
    subroutine initialize_intrinsic_registry()
        integer, parameter :: NUM_INTRINSICS = 30
        integer :: i

        if (registry_initialized) return

        ! Allocate exact space for intrinsic functions
        allocate(intrinsic_functions(NUM_INTRINSICS))
        i = 0

        ! Mathematical functions
        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="sin", return_type="real", arg_types="real", &
            description="Sine function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="cos", return_type="real", arg_types="real", &
            description="Cosine function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="tan", return_type="real", arg_types="real", &
            description="Tangent function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="sqrt", return_type="real", arg_types="real", &
            description="Square root function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="exp", return_type="real", arg_types="real", &
            description="Exponential function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="log", return_type="real", arg_types="real", &
            description="Natural logarithm function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="abs", return_type="real", arg_types="real", &
            description="Absolute value function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="asin", return_type="real", arg_types="real", &
            description="Arc sine function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="acos", return_type="real", arg_types="real", &
            description="Arc cosine function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="atan", return_type="real", arg_types="real", &
            description="Arc tangent function")

        ! Type conversion functions
        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="int", return_type="integer", arg_types="real", &
            description="Convert to integer")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="real", return_type="real", arg_types="integer", &
            description="Convert to real")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="nint", return_type="integer", arg_types="real", &
            description="Nearest integer")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="floor", return_type="integer", arg_types="real", &
            description="Floor function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="ceiling", return_type="integer", arg_types="real", &
            description="Ceiling function")

        ! Array functions
        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="size", return_type="integer", arg_types="array", &
            description="Array size")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="sum", return_type="numeric", arg_types="array", &
            description="Array sum")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="shape", return_type="integer_array", arg_types="array", &
            description="Array shape")

        ! String functions
        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="len", return_type="integer", arg_types="character", &
            description="String length")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="len_trim", return_type="integer", arg_types="character", &
            description="Trimmed string length")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="trim", return_type="character", arg_types="character", &
            description="Remove trailing blanks")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="adjustl", return_type="character", arg_types="character", &
            description="Adjust left")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="adjustr", return_type="character", arg_types="character", &
            description="Adjust right")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="index", return_type="integer", arg_types="character,character", &
            description="Substring position")

        ! Variadic functions
        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="min", return_type="numeric", arg_types="numeric,numeric,...", &
            description="Minimum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="max", return_type="numeric", arg_types="numeric,numeric,...", &
            description="Maximum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="mod", return_type="numeric", arg_types="numeric,numeric", &
            description="Modulo function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="modulo", return_type="numeric", arg_types="numeric,numeric", &
            description="Modulo function")

        ! Inquiry functions
        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="present", return_type="logical", arg_types="any", &
            description="Check if optional argument is present")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
            name="precision", return_type="integer", arg_types="real", &
            description="Decimal precision")

        ! Validate we used all allocated slots
        if (i /= NUM_INTRINSICS) then
            error stop "Intrinsic function count mismatch in initialization"
        end if

        registry_initialized = .true.
    end subroutine initialize_intrinsic_registry

    ! Helper function to convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                lower_str(i:i) = achar(ascii_val + 32)  ! Convert to lowercase
            end if
        end do
    end function to_lower

end module intrinsic_registry