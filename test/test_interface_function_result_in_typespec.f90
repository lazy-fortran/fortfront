program test_interface_function_result_in_typespec
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    logical :: test_passed

    test_passed = .true.

    call test_function_result_in_character_length()

    if (test_passed) then
        print *, "test_interface_function_result_in_typespec PASSED"
    else
        print *, "test_interface_function_result_in_typespec FAILED"
        error stop 1
    end if

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character :: temp

        open (newunit=unit, file=filepath, status='old', action='read', &
              access='stream', iostat=stat)
        if (stat /= 0) then
            content = ''
            return
        end if

        inquire (unit=unit, size=file_size)
        if (file_size <= 0) then
            content = ''
            close (unit)
            return
        end if

        allocate (character(len=file_size) :: content)
        read (unit, iostat=stat) content
        close (unit)

        if (stat /= 0) content = ''
    end subroutine read_example

    subroutine test_function_result_in_character_length()
        call read_example('examples/f90/issue_2413_interface_function_result_valid.f90', &
                         source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: Errors during transformation:', trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'character(len=compute_length(n))') == 0) then
            print *, 'FAIL: Function call in character length not preserved'
            print *, 'Output:', output
            test_passed = .false.
            return
        end if

        if (index(output, 'interface') == 0) then
            print *, 'FAIL: Interface block not preserved'
            test_passed = .false.
            return
        end if

        print *, 'PASS: Interface function result in character length'
    end subroutine test_function_result_in_character_length

end program test_interface_function_result_in_typespec
