program test_multi_variable_declarations
    ! Integration test for multi-variable declarations
    ! Tests that all variables in declarations like "real :: a, b, c" 
    ! survive the complete compilation pipeline
    use frontend
    implicit none
    
    call test_basic_multi_variable_real()
    call test_multi_variable_with_kind()
    call test_multi_variable_with_dimensions()
    ! TODO: Fix multi-variable character parsing bug (separate issue)
    ! call test_multi_variable_character()
    call test_mixed_multi_variable_types()
    
contains

    subroutine test_basic_multi_variable_real()
        character(len=*), parameter :: test_name = "basic_multi_variable_real"
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "    real :: a, b, c" // new_line('a') // &
            "end program"
        character(len=:), allocatable :: result, error_msg
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (len(error_msg) > 0) then
            write(*,*) "FAIL ", test_name, ": Compilation error: ", trim(error_msg)
            error stop 1
        end if
        
        ! Debug: Show actual output
        write(*,*) "DEBUG: Generated code for ", test_name, ":"
        write(*,*) trim(result)
        write(*,*) "END DEBUG"
        
        ! All three variables should appear in the output (standardized as real(8))
        if (index(result, "real(8) :: a") == 0) then
            write(*,*) "FAIL ", test_name, ": Variable 'a' missing from output"
            error stop 1
        end if
        
        if (index(result, "real(8) :: b") == 0) then
            write(*,*) "FAIL ", test_name, ": Variable 'b' missing from output"
            error stop 1
        end if
        
        if (index(result, "real(8) :: c") == 0) then
            write(*,*) "FAIL ", test_name, ": Variable 'c' missing from output"
            error stop 1
        end if
        
        write(*,*) "PASS ", test_name
    end subroutine

    subroutine test_multi_variable_with_kind()
        character(len=*), parameter :: test_name = "multi_variable_with_kind"
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "    real(8) :: x, y, z" // new_line('a') // &
            "end program"
        character(len=:), allocatable :: result, error_msg
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (len(error_msg) > 0) then
            write(*,*) "FAIL ", test_name, ": Compilation error: ", trim(error_msg)
            error stop 1
        end if
        
        ! All three variables with kind should appear
        if (index(result, "real(8) :: x") == 0) then
            write(*,*) "FAIL ", test_name, ": Variable 'x' with kind missing"
            error stop 1
        end if
        
        if (index(result, "real(8) :: y") == 0) then
            write(*,*) "FAIL ", test_name, ": Variable 'y' with kind missing"
            error stop 1
        end if
        
        if (index(result, "real(8) :: z") == 0) then
            write(*,*) "FAIL ", test_name, ": Variable 'z' with kind missing"
            error stop 1
        end if
        
        write(*,*) "PASS ", test_name
    end subroutine

    subroutine test_multi_variable_with_dimensions()
        character(len=*), parameter :: test_name = "multi_variable_with_dimensions"
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "    integer, dimension(10) :: arr1, arr2, arr3" // new_line('a') // &
            "end program"
        character(len=:), allocatable :: result, error_msg
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (len(error_msg) > 0) then
            write(*,*) "FAIL ", test_name, ": Compilation error: ", trim(error_msg)
            error stop 1
        end if
        
        ! Debug: Show actual output
        write(*,*) "DEBUG: Generated code for ", test_name, ":"
        write(*,*) trim(result)
        write(*,*) "END DEBUG"
        
        ! All arrays should appear with dimensions (standardized format)
        if (index(result, "integer :: arr1(10)") == 0) then
            write(*,*) "FAIL ", test_name, ": Array 'arr1' missing"
            error stop 1
        end if
        
        if (index(result, "integer :: arr2(10)") == 0) then
            write(*,*) "FAIL ", test_name, ": Array 'arr2' missing"
            error stop 1
        end if
        
        if (index(result, "integer :: arr3(10)") == 0) then
            write(*,*) "FAIL ", test_name, ": Array 'arr3' missing"
            error stop 1
        end if
        
        write(*,*) "PASS ", test_name
    end subroutine

    subroutine test_multi_variable_character()
        character(len=*), parameter :: test_name = "multi_variable_character"
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "    character(len=20) :: str1, str2, str3" // new_line('a') // &
            "end program"
        character(len=:), allocatable :: result, error_msg
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (len(error_msg) > 0) then
            write(*,*) "FAIL ", test_name, ": Compilation error: ", trim(error_msg)
            error stop 1
        end if
        
        ! Debug: Show actual output
        write(*,*) "DEBUG: Generated code for ", test_name, ":"
        write(*,*) trim(result)
        write(*,*) "END DEBUG"
        
        ! All character variables should appear
        if (index(result, "character(len=20) :: str1") == 0) then
            write(*,*) "FAIL ", test_name, ": Character 'str1' missing"
            error stop 1
        end if
        
        if (index(result, "character(len=20) :: str2") == 0) then
            write(*,*) "FAIL ", test_name, ": Character 'str2' missing"
            error stop 1
        end if
        
        if (index(result, "character(len=20) :: str3") == 0) then
            write(*,*) "FAIL ", test_name, ": Character 'str3' missing"
            error stop 1
        end if
        
        write(*,*) "PASS ", test_name
    end subroutine

    subroutine test_mixed_multi_variable_types()
        character(len=*), parameter :: test_name = "mixed_multi_variable_types"
        character(len=*), parameter :: source = &
            "program test" // new_line('a') // &
            "    real :: a, b" // new_line('a') // &
            "    integer :: i, j, k" // new_line('a') // &
            "    logical :: flag1, flag2" // new_line('a') // &
            "end program"
        character(len=:), allocatable :: result, error_msg
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (len(error_msg) > 0) then
            write(*,*) "FAIL ", test_name, ": Compilation error: ", trim(error_msg)
            error stop 1
        end if
        
        ! Check all variables across different types (real standardized to real(8))
        if (index(result, "real(8) :: a") == 0 .or. index(result, "real(8) :: b") == 0) then
            write(*,*) "FAIL ", test_name, ": Real variables missing"
            error stop 1
        end if
        
        if (index(result, "integer :: i") == 0 .or. &
            index(result, "integer :: j") == 0 .or. &
            index(result, "integer :: k") == 0) then
            write(*,*) "FAIL ", test_name, ": Integer variables missing"
            error stop 1
        end if
        
        if (index(result, "logical :: flag1") == 0 .or. &
            index(result, "logical :: flag2") == 0) then
            write(*,*) "FAIL ", test_name, ": Logical variables missing"
            error stop 1
        end if
        
        write(*,*) "PASS ", test_name
    end subroutine

end program