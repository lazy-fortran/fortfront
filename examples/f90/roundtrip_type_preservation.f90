! Comprehensive test for Type Preservation round-trip
! Tests: integer types (default, kind-specified), real types (default, kind-specified),
!        double precision, complex, logical, character (fixed-length, allocatable),
!        derived types

program roundtrip_type_preservation
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none

    ! Integer types
    integer :: int_default
    integer(kind=int8) :: int8_val
    integer(kind=int16) :: int16_val
    integer(kind=int32) :: int32_val
    integer(kind=int64) :: int64_val

    ! Real types
    real :: real_default
    real(kind=real32) :: real32_val
    real(kind=real64) :: real64_val

    ! Double precision
    double precision :: dbl_val

    ! Complex types
    complex :: complex_default
    complex(kind=real64) :: complex64_val

    ! Logical types
    logical :: log_val
    logical :: log_array(3)

    ! Character types
    character(len=10) :: char_fixed
    character(len=50) :: char_long
    character(len=:), allocatable :: char_alloc

    ! Derived type
    type :: person_t
        character(len=30) :: name
        integer :: age
        real :: height
        logical :: is_active
    end type person_t

    type(person_t) :: person1, person2

    ! Initialize integer types
    int_default = 42
    int8_val = 127
    int16_val = 32000
    int32_val = 2000000000
    int64_val = 9000000000_int64

    ! Initialize real types
    real_default = 3.14
    real32_val = 2.718_real32
    real64_val = 1.414213562_real64

    ! Initialize double precision
    dbl_val = 2.718281828d0

    ! Initialize complex types
    complex_default = (1.0, 2.0)
    complex64_val = (3.0_real64, 4.0_real64)

    ! Initialize logical types
    log_val = .true.
    log_array = [.true., .false., .true.]

    ! Initialize character types
    char_fixed = 'Hello'
    char_long = 'This is a longer string for testing'
    char_alloc = 'Allocatable string'

    ! Initialize derived types
    person1%name = 'Alice'
    person1%age = 30
    person1%height = 165.5
    person1%is_active = .true.

    person2%name = 'Bob'
    person2%age = 25
    person2%height = 180.0
    person2%is_active = .false.

    ! Test functions with different type parameters
    call test_integer_types(int32_val, int64_val)
    call test_real_types(real32_val, real64_val)
    call test_complex_types(complex_default)
    call test_derived_type(person1)

    print *, 'Integer values:', int_default, int32_val
    print *, 'Real values:', real_default, real64_val
    print *, 'Double precision:', dbl_val
    print *, 'Complex:', complex_default
    print *, 'Logical:', log_val
    print *, 'Character:', trim(char_fixed)
    print *, 'Person:', trim(person1%name), person1%age

contains

    subroutine test_integer_types(i32, i64)
        integer(kind=int32), intent(in) :: i32
        integer(kind=int64), intent(in) :: i64
        print *, 'Testing integers:', i32, i64
    end subroutine test_integer_types

    subroutine test_real_types(r32, r64)
        real(kind=real32), intent(in) :: r32
        real(kind=real64), intent(in) :: r64
        print *, 'Testing reals:', r32, r64
    end subroutine test_real_types

    subroutine test_complex_types(c)
        complex, intent(in) :: c
        print *, 'Testing complex:', c
    end subroutine test_complex_types

    subroutine test_derived_type(p)
        type(person_t), intent(in) :: p
        print *, 'Testing person:', trim(p%name), p%age
    end subroutine test_derived_type

    ! Function returning double precision
    double precision function compute_double(x)
        double precision, intent(in) :: x
        compute_double = x * 2.0d0
    end function compute_double

    ! Function with character result
    function get_greeting(name) result(greeting)
        character(len=*), intent(in) :: name
        character(len=50) :: greeting
        greeting = 'Hello, ' // trim(name) // '!'
    end function get_greeting

end program roundtrip_type_preservation
