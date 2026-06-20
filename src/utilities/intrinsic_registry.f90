module intrinsic_registry
    use, intrinsic :: iso_fortran_env, only: error_unit
    use string_utils_mod, only: to_lower
    use ieee_intrinsic_module, only: is_ieee_function, is_ieee_subroutine, &
                                     ieee_function_return_type
    implicit none
    private

    ! Public interfaces
    public :: is_intrinsic_function
    public :: is_intrinsic_subroutine
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
    integer, parameter :: MAX_INTRINSIC_SUBROUTINE_NAME_LEN = 32
    integer, parameter :: NUM_INTRINSIC_SUBROUTINES = 43
    character(len=MAX_INTRINSIC_SUBROUTINE_NAME_LEN), parameter :: &
        intrinsic_subroutine_names(NUM_INTRINSIC_SUBROUTINES) = &
        [character(len=MAX_INTRINSIC_SUBROUTINE_NAME_LEN) :: &
         "c_f_pointer", "c_f_procpointer", &
         "cpu_time", "date_and_time", &
         "execute_command_line", "get_command", &
         "get_command_argument", &
         "get_environment_variable", "move_alloc", &
         "random_init", "random_number", &
         "random_seed", "system_clock", &
         "sync_all", "sync_images", &
         "sync_memory", "lock", "unlock", &
         "event_post", "event_wait", &
         "event_query", "form_team", &
         "change_team", "end_team", &
         "co_broadcast", "co_sum", &
         "co_max", "co_min", "co_reduce", &
         "co_allgather", "co_gather", &
         "co_scatter", "atomic_define", &
         "atomic_ref", "atomic_cas", &
         "atomic_add", "atomic_and", &
         "atomic_or", "atomic_xor", &
         "atomic_fetch_add", &
         "atomic_fetch_and", &
         "atomic_fetch_or", &
         "atomic_fetch_xor"]

contains

    ! Combined function to get intrinsic information efficiently
    subroutine get_intrinsic_info(name, is_intrinsic, signature)
        character(len=*), intent(in) :: name
        logical, intent(out) :: is_intrinsic
        character(len=:), allocatable, intent(out) :: signature
        integer :: i
        character(len=:), allocatable :: lowered_name

        if (.not. registry_initialized) call initialize_intrinsic_registry()

        is_intrinsic = .false.
        if (allocated(signature)) deallocate (signature)
        if (len_trim(name) == 0) return
        lowered_name = to_lower(trim(name))

        if (is_ieee_function(lowered_name)) then
            is_intrinsic = .true.
            signature = ieee_function_return_type(lowered_name)//"(real)"
            return
        end if

        if (.not. allocated(intrinsic_functions)) return

        do i = 1, size(intrinsic_functions)
            if (intrinsic_functions(i)%name == lowered_name) then
                is_intrinsic = .true.
                signature = intrinsic_functions(i)%return_type//"("// &
                            intrinsic_functions(i)%arg_types//")"
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

    function is_intrinsic_subroutine(name) result(is_intrinsic)
        character(len=*), intent(in) :: name
        logical :: is_intrinsic
        integer :: i

        is_intrinsic = .false.
        if (len_trim(name) == 0) return

        if (is_ieee_subroutine(name)) then
            is_intrinsic = .true.
            return
        end if

        if (len_trim(name) > MAX_INTRINSIC_SUBROUTINE_NAME_LEN) return

        do i = 1, size(intrinsic_subroutine_names)
            if (equals_case_insensitive_trimmed(name, &
                                                intrinsic_subroutine_names(i))) then
                is_intrinsic = .true.
                return
            end if
        end do
    end function is_intrinsic_subroutine

    pure logical function equals_case_insensitive_trimmed(text, literal) &
        result(is_equal)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: literal

        integer :: i
        integer :: left_len, right_len
        integer :: left_code
        integer :: literal_code

        is_equal = .false.
        left_len = len_trim(text)
        right_len = len_trim(literal)
        if (left_len /= right_len) return
        if (left_len == 0) then
            is_equal = .true.
            return
        end if

        do i = 1, left_len
            left_code = iachar(text(i:i))
            if (left_code >= iachar('A') .and. left_code <= iachar('Z')) then
                left_code = left_code + 32
            end if

            literal_code = iachar(literal(i:i))
            if (literal_code /= left_code) return
        end do

        is_equal = .true.
    end function equals_case_insensitive_trimmed

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
        integer, parameter :: NUM_INTRINSICS = 61
        integer :: i

        if (registry_initialized) return

        allocate (intrinsic_functions(NUM_INTRINSICS))
        i = 0

        call register_mathematical_intrinsics(i)
        call register_type_conversion_intrinsics(i)
        call register_array_intrinsics(i)
        call register_string_intrinsics(i)
        call register_variadic_intrinsics(i)
        call register_inquiry_intrinsics(i)
        call register_c_interop_intrinsics(i)

        if (i /= NUM_INTRINSICS) then
            write (error_unit, '(A)') &
                "ERROR [intrinsic_registry]: Intrinsic function count mismatch"
            write (error_unit, '(A)') "in initialization - registry may be incomplete"
        end if

        registry_initialized = .true.
    end subroutine initialize_intrinsic_registry

    subroutine register_mathematical_intrinsics(i)
        integer, intent(inout) :: i

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

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="sign", return_type="real", &
                                 arg_types="real,real", &
                                 description="Sign transfer function")
    end subroutine register_mathematical_intrinsics

    subroutine register_type_conversion_intrinsics(i)
        integer, intent(inout) :: i

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="int", return_type="integer", arg_types="real", &
                                 description="Convert to integer")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="uint", return_type="unsigned_integer", &
                                 arg_types="integer", &
                                 description="Convert to unsigned integer")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="real", return_type="real", &
                                 arg_types="integer", &
                                 description="Convert to real")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="nint", return_type="integer", &
                                 arg_types="real", &
                                 description="Nearest integer")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="floor", return_type="integer", &
                                 arg_types="real", &
                                 description="Floor function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="ceiling", return_type="integer", &
                                 arg_types="real", &
                                 description="Ceiling function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="cmplx", return_type="complex", &
                                 arg_types="numeric,numeric", &
                                 description="Convert to complex")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="aimag", return_type="real", &
                                 arg_types="complex", &
                                 description="Imaginary part of complex")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="wrap_add", return_type="unsigned_integer", &
                                 arg_types="unsigned_integer,unsigned_integer", &
                                 description="Wraparound unsigned addition")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="wrap_sub", return_type="unsigned_integer", &
                                 arg_types="unsigned_integer,unsigned_integer", &
                                 description="Wraparound unsigned subtraction")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="wrap_mul", return_type="unsigned_integer", &
                                 arg_types="unsigned_integer,unsigned_integer", &
                                 description="Wraparound unsigned multiplication")
    end subroutine register_type_conversion_intrinsics

    subroutine register_array_intrinsics(i)
        integer, intent(inout) :: i

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="size", return_type="integer", &
                                 arg_types="array", &
                                 description="Array size")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="sum", return_type="numeric", &
                                 arg_types="array", &
                                 description="Array sum")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="maxval", return_type="numeric", &
                                 arg_types="array", &
                                 description="Array maximum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="minval", return_type="numeric", &
                                 arg_types="array", &
                                 description="Array minimum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="shape", return_type="array", &
                                 arg_types="array", &
                                 description="Array shape")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="reshape", return_type="array", &
                                 arg_types="array,integer_array", &
                                 description="Reshape array to new dimensions")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="dot_product", return_type="numeric", &
                                 arg_types="array,array", &
                                 description="Dot product of two vectors")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="matmul", return_type="array", &
                                 arg_types="array,array", &
                                 description="Matrix multiplication")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="transpose", return_type="array", &
                                 arg_types="array", &
                                 description="Matrix transpose")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="product", return_type="numeric", &
                                 arg_types="array", &
                                 description="Product of array elements")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="any", return_type="logical", &
                                 arg_types="logical_array", &
                                 description="True if any element is true")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="all", return_type="logical", &
                                 arg_types="logical_array", &
                                 description="True if all elements are true")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="count", return_type="integer", &
                                 arg_types="logical_array", &
                                 description="Number of true elements in array")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="minloc", return_type="array", &
                                 arg_types="array", &
                                 description="Location of minimum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="maxloc", return_type="array", &
                                 arg_types="array", &
                                 description="Location of maximum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="pack", return_type="array", &
                                 arg_types="array,logical_array", &
                                 description="Pack array into vector")
    end subroutine register_array_intrinsics

    subroutine register_string_intrinsics(i)
        integer, intent(inout) :: i

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="len", return_type="integer", &
                                 arg_types="character", &
                                 description="String length")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="len_trim", return_type="integer", &
                                 arg_types="character", &
                                 description="Trimmed string length")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="trim", return_type="character", &
                                 arg_types="character", &
                                 description="Remove trailing blanks")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="adjustl", return_type="character", &
                                 arg_types="character", &
                                 description="Adjust left")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="adjustr", return_type="character", &
                                 arg_types="character", &
                                 description="Adjust right")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="index", return_type="integer", &
                                 arg_types="character,character", &
                                 description="Substring position")
    end subroutine register_string_intrinsics

    subroutine register_variadic_intrinsics(i)
        integer, intent(inout) :: i

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="min", return_type="numeric", &
                                 arg_types="numeric,numeric,...", &
                                 description="Minimum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="max", return_type="numeric", &
                                 arg_types="numeric,numeric,...", &
                                 description="Maximum value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="mod", return_type="numeric", &
                                 arg_types="numeric,numeric", &
                                 description="Modulo function")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="modulo", return_type="numeric", &
                                 arg_types="numeric,numeric", &
                                 description="Modulo function")
    end subroutine register_variadic_intrinsics

    subroutine register_inquiry_intrinsics(i)
        integer, intent(inout) :: i

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="present", return_type="logical", &
                                 arg_types="any", &
                                 description="Check if optional argument is present")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="precision", return_type="integer", &
                                 arg_types="real", &
                                 description="Decimal precision")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="allocated", return_type="logical", &
                                 arg_types="allocatable", &
                                 description="Check if allocatable variable is "// &
                                 "allocated")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="huge", return_type="numeric", &
                                 arg_types="numeric", &
                                 description="Largest number of the kind")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="epsilon", return_type="real", &
                                 arg_types="real", &
                                 description="Machine epsilon")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="kind", return_type="integer", &
                                 arg_types="any", &
                                 description="Kind type parameter value")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="selected_int_kind", return_type="integer", &
                                 arg_types="integer", &
                                 description="Integer kind for range")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="lbound", return_type="array", &
                                 arg_types="array", &
                                 description="Lower bounds of array")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="ubound", return_type="array", &
                                 arg_types="array", &
                                 description="Upper bounds of array")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="null", return_type="pointer", &
                                 arg_types="", &
                                 description="Disassociated pointer or unallocated "// &
                                 "allocatable")
    end subroutine register_inquiry_intrinsics

    subroutine register_c_interop_intrinsics(i)
        integer, intent(inout) :: i

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="c_loc", return_type="c_ptr", &
                                 arg_types="any", &
                                 description="C address of a target argument")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="c_funloc", return_type="c_funptr", &
                                 arg_types="any", &
                                 description="C address of a procedure argument")

        i = i + 1
        intrinsic_functions(i) = intrinsic_signature_t( &
                                 name="c_associated", return_type="logical", &
                                 arg_types="c_ptr", &
                                 description="C pointer association status")
    end subroutine register_c_interop_intrinsics

end module intrinsic_registry
