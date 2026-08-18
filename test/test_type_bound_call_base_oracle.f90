program test_type_bound_call_base_oracle
    use test_command_helpers, only: test_executable_path, test_remove_file
    use transformation_api, only: compilation_options_t, compile_source
    implicit none

    character(len=*), parameter :: input_path = &
        "examples/f90/type_bound_call_base_oracle.f90"
    character(len=*), parameter :: output_path = &
        "build/type_bound_call_base_oracle.f90"
    character(len=:), allocatable :: executable_path
    character(len=512) :: error_msg
    type(compilation_options_t) :: options
    integer :: cmdstat, exitstat

    executable_path = test_executable_path('type_bound_call_base_oracle')

    options%output_file = output_path
    call compile_source(input_path, options, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL type-bound source transform: ", trim(error_msg)
        error stop 1
    end if

    call execute_command_line("gfortran "//output_path//" -o "// &
        executable_path, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
    if (cmdstat /= 0 .or. exitstat /= 0) then
        print *, "FAIL emitted type-bound call did not compile"
        error stop 2
    end if

    call execute_command_line(executable_path, wait=.true., &
        exitstat=exitstat, cmdstat=cmdstat)
    if (cmdstat /= 0 .or. exitstat /= 0) then
        print *, "FAIL emitted type-bound call changed behavior"
        error stop 3
    end if
    call test_remove_file(executable_path)

    print *, "test_type_bound_call_base_oracle: all cases passed"
end program test_type_bound_call_base_oracle
