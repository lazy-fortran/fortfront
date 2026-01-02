program test_issue_2497_include_line_standalone
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors

    call read_example('examples/f90/issue_2497_include_line_standalone.f90', source)

    call transform_lazy_fortran_string(source, output, errors)

    if (len_trim(errors) > 0) then
        print *, "Unexpected errors:", trim(errors)
        error stop 1
    end if

    if (index(output, "include") == 0) then
        print *, "ERROR: INCLUDE line was dropped during round-trip"
        error stop 1
    end if

    if (index(output, "'other_file.f90'") == 0) then
        print *, "ERROR: INCLUDE filename was not preserved"
        error stop 1
    end if

    print *, "PASS: Standalone INCLUDE line preserved correctly"


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_2497_include_line_standalone
