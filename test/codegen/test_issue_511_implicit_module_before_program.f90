! Test for Issue #511 - Allow implicit module above explicit program
! The derived type before the explicit program should be wrapped in a module
! and automatically imported into the program

program test_issue_511_implicit_module_before_program
    use frontend
    implicit none

    character(len=:), allocatable :: input, expected, actual, error_msg

    ! Test 1: Single type declaration before explicit program
    call run_test("Single type declaration before explicit program")
    
    input = &
        "type :: a"//new_line('A')// &
        "    integer :: t"//new_line('A')// &
        "end type :: a"//new_line('A')// &
        ""//new_line('A')// &
        "program pro"//new_line('A')// &
        "    type(a) :: testy"//new_line('A')// &
        "    testy%t = 3"//new_line('A')// &
        "end program pro"
    
    expected = &
        "module implicit_module"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    type :: a"//new_line('A')// &
        "        integer :: t"//new_line('A')// &
        "    end type a"//new_line('A')// &
        "end module implicit_module"//new_line('A')// &
        ""//new_line('A')// &
        "program pro"//new_line('A')// &
        "    use implicit_module, only: a"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    type(a) :: testy"//new_line('A')// &
        ""//new_line('A')// &
        "    testy%t = 3"//new_line('A')// &
        "end program pro"
    
    call compile_and_check(input, expected)

    ! Test 2: Multiple type declarations before explicit program
    call run_test("Multiple types before explicit program")
    
    input = &
        "type :: point"//new_line('A')// &
        "    real :: x, y"//new_line('A')// &
        "end type point"//new_line('A')// &
        ""//new_line('A')// &
        "type :: circle"//new_line('A')// &
        "    type(point) :: center"//new_line('A')// &
        "    real :: radius"//new_line('A')// &
        "end type circle"//new_line('A')// &
        ""//new_line('A')// &
        "program test_geom"//new_line('A')// &
        "    type(circle) :: c"//new_line('A')// &
        "    c%radius = 5.0"//new_line('A')// &
        "    c%center%x = 0.0"//new_line('A')// &
        "    c%center%y = 0.0"//new_line('A')// &
        "end program test_geom"
    
    expected = &
        "module implicit_module"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    type :: point"//new_line('A')// &
        "        real :: x, y"//new_line('A')// &
        "    end type point"//new_line('A')// &
        ""//new_line('A')// &
        "    type :: circle"//new_line('A')// &
        "        type(point) :: center"//new_line('A')// &
        "        real :: radius"//new_line('A')// &
        "    end type circle"//new_line('A')// &
        "end module implicit_module"//new_line('A')// &
        ""//new_line('A')// &
        "program test_geom"//new_line('A')// &
        "    use implicit_module, only: point, circle"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    type(circle) :: c"//new_line('A')// &
        ""//new_line('A')// &
        "    c%radius = 5.0"//new_line('A')// &
        "    c%center%x = 0.0"//new_line('A')// &
        "    c%center%y = 0.0"//new_line('A')// &
        "end program test_geom"

    call compile_and_check(input, expected)

    ! Test 3: Interface block before explicit program
    call run_test("Interface block before explicit program")
    
    input = &
        "interface"//new_line('A')// &
        "    function add_int(a, b) result(c)"//new_line('A')// &
        "        integer, intent(in) :: a, b"//new_line('A')// &
        "        integer :: c"//new_line('A')// &
        "    end function add_int"//new_line('A')// &
        "end interface"//new_line('A')// &
        ""//new_line('A')// &
        "program test_interface"//new_line('A')// &
        "    integer :: result"//new_line('A')// &
        "    result = add_int(2, 3)"//new_line('A')// &
        "end program test_interface"
    
    expected = &
        "module implicit_module"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    interface"//new_line('A')// &
        "        function add_int(a, b) result(c)"//new_line('A')// &
        "            integer, intent(in) :: a, b"//new_line('A')// &
        "            integer :: c"//new_line('A')// &
        "        end function add_int"//new_line('A')// &
        "    end interface"//new_line('A')// &
        "end module implicit_module"//new_line('A')// &
        ""//new_line('A')// &
        "program test_interface"//new_line('A')// &
        "    use implicit_module"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    integer :: result"//new_line('A')// &
        ""//new_line('A')// &
        "    result = add_int(2, 3)"//new_line('A')// &
        "end program test_interface"

    call compile_and_check(input, expected)

    ! Test 4: Mixed declarations before explicit program
    call run_test("Mixed declarations before explicit program")
    
    input = &
        "integer, parameter :: MAX_SIZE = 100"//new_line('A')// &
        ""//new_line('A')// &
        "type :: config"//new_line('A')// &
        "    integer :: size"//new_line('A')// &
        "    logical :: enabled"//new_line('A')// &
        "end type config"//new_line('A')// &
        ""//new_line('A')// &
        "program test_config"//new_line('A')// &
        "    type(config) :: cfg"//new_line('A')// &
        "    cfg%size = MAX_SIZE"//new_line('A')// &
        "    cfg%enabled = .true."//new_line('A')// &
        "end program test_config"
    
    expected = &
        "module implicit_module"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    integer, parameter :: MAX_SIZE = 100"//new_line('A')// &
        ""//new_line('A')// &
        "    type :: config"//new_line('A')// &
        "        integer :: size"//new_line('A')// &
        "        logical :: enabled"//new_line('A')// &
        "    end type config"//new_line('A')// &
        "end module implicit_module"//new_line('A')// &
        ""//new_line('A')// &
        "program test_config"//new_line('A')// &
        "    use implicit_module, only: MAX_SIZE, config"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    type(config) :: cfg"//new_line('A')// &
        ""//new_line('A')// &
        "    cfg%size = MAX_SIZE"//new_line('A')// &
        "    cfg%enabled = .true."//new_line('A')// &
        "end program test_config"

    call compile_and_check(input, expected)

    ! Test 5: No declarations before explicit program (should not create module)
    call run_test("No declarations before explicit program")
    
    input = &
        "program simple"//new_line('A')// &
        "    integer :: i"//new_line('A')// &
        "    i = 42"//new_line('A')// &
        "end program simple"
    
    expected = &
        "program simple"//new_line('A')// &
        "    implicit none"//new_line('A')// &
        ""//new_line('A')// &
        "    integer :: i"//new_line('A')// &
        ""//new_line('A')// &
        "    i = 42"//new_line('A')// &
        "end program simple"

    call compile_and_check(input, expected)

contains

    subroutine compile_and_check(input, expected)
        character(len=*), intent(in) :: input, expected
        character(len=:), allocatable :: actual_output
        character(len=:), allocatable :: error_msg_local
        
        ! Transform the source
        call transform_lazy_fortran_string(input, actual_output, error_msg_local)
        
        if (len_trim(error_msg_local) > 0) then
            print *, "Compilation failed: "//trim(error_msg_local)
            stop 1
        end if
        
        ! Check output
        if (trim(actual_output) /= trim(expected)) then
            print *, "Expected:"
            call print_with_line_numbers(expected)
            print *, ""
            print *, "Actual:"
            call print_with_line_numbers(actual_output)
            stop 1
        else
            print *, "  PASS"
        end if
    end subroutine compile_and_check
    
    subroutine run_test(test_name)
        character(len=*), intent(in) :: test_name
        print *, ""
        print *, "Test: ", trim(test_name)
    end subroutine run_test
    
    subroutine print_with_line_numbers(code)
        character(len=*), intent(in) :: code
        integer :: i, line_num, start_pos
        character(len=:), allocatable :: line
        
        line_num = 1
        start_pos = 1
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                if (i >= start_pos) then
                    line = code(start_pos:i-1)
                    write(*, '(I3,A,A)') line_num, ': ', line
                else
                    write(*, '(I3,A)') line_num, ': '
                end if
                line_num = line_num + 1
                start_pos = i + 1
            end if
        end do
        
        ! Print last line if file doesn't end with newline
        if (start_pos <= len(code)) then
            line = code(start_pos:len(code))
            write(*, '(I3,A,A)') line_num, ': ', line
        end if
    end subroutine print_with_line_numbers

end program test_issue_511_implicit_module_before_program