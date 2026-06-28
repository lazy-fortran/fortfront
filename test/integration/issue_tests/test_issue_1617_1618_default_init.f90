program test_issue_1617_1618_default_init
    use transformation_api, only: transform_lazy_fortran_string

    call test_integer_real_init()
    call test_pointer_null_init()
    call test_logical_init()
    call test_mixed_init_no_init()
    call test_multiple_types()
    print *, ""
    print *, "All default initialization tests passed"

contains

    subroutine test_integer_real_init()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: Superclass" // new_line('A') // &
            "  integer :: stop_at = -1" // new_line('A') // &
            "  real :: x = 0.0" // new_line('A') // &
            "end type Superclass"

        print *, ""
        print *, "=== Test 1: Integer and real default initialization (issue #1617) ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "stop_at = -1") == 0) then
            print *, "FAIL: Integer initialization not preserved"
            error stop 1
        end if

        if (index(output_code, "x = 0.0") == 0) then
            print *, "FAIL: Real initialization not preserved"
            error stop 1
        end if

        print *, "PASS: Integer and real initialization preserved"
    end subroutine test_integer_real_init

    subroutine test_pointer_null_init()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: Array" // new_line('A') // &
            "  real, pointer :: values(:) => null()" // new_line('A') // &
            "  integer :: size = 0" // new_line('A') // &
            "end type Array"

        print *, ""
        print *, "=== Test 2: Pointer null initialization (issue #1618) ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "=> null()") == 0) then
            print *, "FAIL: Pointer null initialization not preserved"
            error stop 1
        end if

        if (index(output_code, "size = 0") == 0) then
            print *, "FAIL: Integer initialization not preserved"
            error stop 1
        end if

        print *, "PASS: Pointer null initialization preserved"
    end subroutine test_pointer_null_init

    subroutine test_logical_init()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: Config" // new_line('A') // &
            "  logical :: enabled = .true." // new_line('A') // &
            "  logical :: verbose = .false." // new_line('A') // &
            "end type Config"

        print *, ""
        print *, "=== Test 3: Logical default initialization ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "enabled = .true.") == 0) then
            print *, "FAIL: Logical true initialization not preserved"
            error stop 1
        end if

        if (index(output_code, "verbose = .false.") == 0) then
            print *, "FAIL: Logical false initialization not preserved"
            error stop 1
        end if

        print *, "PASS: Logical initialization preserved"
    end subroutine test_logical_init

    subroutine test_mixed_init_no_init()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: Mixed" // new_line('A') // &
            "  integer :: initialized = 42" // new_line('A') // &
            "  integer :: uninitialized" // new_line('A') // &
            "  real :: value = 3.14" // new_line('A') // &
            "end type Mixed"

        print *, ""
        print *, "=== Test 4: Mixed initialized and uninitialized fields ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "initialized = 42") == 0) then
            print *, "FAIL: Initialized field not preserved"
            error stop 1
        end if

        if (index(output_code, "uninitialized") == 0) then
            print *, "FAIL: Uninitialized field missing"
            error stop 1
        end if

        if (index(output_code, "value = 3.14") == 0) then
            print *, "FAIL: Real initialization not preserved"
            error stop 1
        end if

        print *, "PASS: Mixed fields preserved correctly"
    end subroutine test_mixed_init_no_init

    subroutine test_multiple_types()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: Container" // new_line('A') // &
            "  real, pointer :: ptr(:) => null()" // new_line('A') // &
            "  real :: scale = 1.0" // new_line('A') // &
            "  logical :: active = .true." // new_line('A') // &
            "end type Container"

        print *, ""
        print *, "=== Test 5: Pointer array with multiple initialized fields ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "ptr(:) => null()") == 0) then
            print *, "FAIL: Pointer array initialization not preserved"
            error stop 1
        end if

        if (index(output_code, "scale = 1.0") == 0) then
            print *, "FAIL: Real initialization not preserved"
            error stop 1
        end if

        if (index(output_code, "active = .true.") == 0) then
            print *, "FAIL: Logical initialization not preserved"
            error stop 1
        end if

        print *, "PASS: Pointer array and field initialization preserved"
    end subroutine test_multiple_types

end program test_issue_1617_1618_default_init
