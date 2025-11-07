! Comprehensive test for Declaration Attributes round-trip
! Tests: dimension() attribute, parameter attribute, save attribute, target attribute,
!        allocatable, pointer attributes, optional attribute

module roundtrip_declaration_attributes
    implicit none

    ! Module-level variables with SAVE attribute (implicit in modules)
    real, save :: module_counter = 0.0
    integer, save :: call_count = 0

    ! Parameter declarations
    integer, parameter :: MAX_SIZE = 100
    real, parameter :: PI = 3.141592653589793
    real, parameter :: E = 2.718281828459045

    ! Module arrays with dimension attribute
    real, dimension(10) :: module_array
    integer, dimension(5, 5) :: matrix

contains

    ! Subroutine with optional parameters
    subroutine process_optional(required, optional1, optional2)
        real, intent(in) :: required
        real, intent(in), optional :: optional1
        integer, intent(in), optional :: optional2

        if (present(optional1)) then
            print *, 'Optional1 present:', optional1
        end if

        if (present(optional2)) then
            print *, 'Optional2 present:', optional2
        end if

        print *, 'Required:', required
    end subroutine process_optional

    ! Subroutine demonstrating TARGET and POINTER attributes
    subroutine pointer_example()
        real, target :: target_var
        real, target :: target_array(10)
        real, pointer :: ptr_scalar
        real, pointer :: ptr_array(:)
        integer :: i

        target_var = 42.0
        do i = 1, 10
            target_array(i) = real(i)
        end do

        ! Associate pointers with targets
        ptr_scalar => target_var
        ptr_array => target_array

        print *, 'Pointer scalar:', ptr_scalar
        print *, 'Pointer array:', ptr_array(1:3)
    end subroutine pointer_example

    ! Function with allocatable local variable
    function create_matrix(rows, cols, init_val) result(mat)
        integer, intent(in) :: rows, cols
        real, intent(in) :: init_val
        real, allocatable :: mat(:, :)
        integer :: i, j

        allocate (mat(rows, cols))

        do i = 1, rows
            do j = 1, cols
                mat(i, j) = init_val
            end do
        end do
    end function create_matrix

    ! Subroutine with SAVE attribute in local variables
    subroutine counter_with_save()
        integer, save :: local_counter = 0

        local_counter = local_counter + 1
        print *, 'Local counter (with SAVE):', local_counter
    end subroutine counter_with_save

end module roundtrip_declaration_attributes

program test_declaration_attributes
    use roundtrip_declaration_attributes
    implicit none

    ! Local variables with dimension attribute
    real, dimension(20) :: local_array
    integer, dimension(3, 4) :: local_matrix

    ! Allocatable arrays
    real, allocatable :: alloc_1d(:)
    real, allocatable :: alloc_2d(:, :)

    ! Pointer variables
    real, pointer :: ptr_var
    real, pointer :: ptr_arr(:)

    ! Target variables
    real, target :: target_val
    real, target, dimension(5) :: target_arr

    ! Parameter usage
    real :: circle_area
    integer :: i

    ! Using parameters
    circle_area = PI * 5.0**2
    print *, 'Using PI parameter:', PI
    print *, 'Using E parameter:', E
    print *, 'MAX_SIZE parameter:', MAX_SIZE

    ! Initialize arrays with dimension attribute
    do i = 1, 20
        local_array(i) = real(i)
    end do

    ! Allocate arrays
    allocate (alloc_1d(10))
    allocate (alloc_2d(5, 5))

    alloc_1d = 1.0
    alloc_2d = 0.0

    ! Use target and pointer
    target_val = 99.0
    target_arr = [1.0, 2.0, 3.0, 4.0, 5.0]

    ptr_var => target_val
    ptr_arr => target_arr

    print *, 'Pointer to scalar:', ptr_var
    print *, 'Pointer to array:', ptr_arr(1:3)

    ! Test optional parameters
    call process_optional(1.0)
    call process_optional(1.0, optional1=2.0)
    call process_optional(1.0, optional1=2.0, optional2=3)

    ! Test pointer example
    call pointer_example()

    ! Test SAVE attribute
    call counter_with_save()
    call counter_with_save()
    call counter_with_save()

    ! Cleanup
    deallocate (alloc_1d)
    deallocate (alloc_2d)

    print *, 'Program completed'

contains

    ! Local subroutine with dimension declarations
    subroutine local_sub()
        real, dimension(10), save :: persistent_array
        integer, dimension(5, 5) :: temp_matrix
        integer :: i, j

        do i = 1, 5
            do j = 1, 5
                temp_matrix(i, j) = i * j
            end do
        end do

        print *, 'Temp matrix diagonal:', [(temp_matrix(i, i), i=1, 5)]
    end subroutine local_sub

end program test_declaration_attributes
