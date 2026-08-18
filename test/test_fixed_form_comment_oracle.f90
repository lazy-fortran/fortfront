program test_fixed_form_comment_oracle
    use test_command_helpers, only: test_executable_path
    use transformation_api, only: compilation_options_t, compile_source
    use frontend_core, only: normalize_fixed_form_source_text
    implicit none

    character(len=*), parameter :: input_path = &
        "examples/f90/fixed_form_comment_no_continuation.f"
    character(len=*), parameter :: output_path = &
        "build/fixed_form_comment_no_continuation.f90"
    character(len=:), allocatable :: executable_path
    character(len=512) :: error_msg
    character(len=:), allocatable :: free_form
    type(compilation_options_t) :: options
    integer :: cmdstat, exitstat

    executable_path = test_executable_path('fixed_form_comment_no_continuation')

    free_form = "! ordinary free-form comment"//new_line('a')// &
        "      program untouched"//new_line('a')// &
        "      end program untouched"
    call normalize_fixed_form_source_text(free_form)
    if (index(free_form, "! ordinary free-form comment") /= 1) then
        print *, "FAIL fixed-form normalizer rewrote free-form comments"
        error stop 1
    end if

    options%output_file = output_path
    call compile_source(input_path, options, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL fixed-form parse: ", trim(error_msg)
        error stop 1
    end if

    call execute_command_line("gfortran "//output_path//" -o "// &
        executable_path, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
    if (cmdstat /= 0 .or. exitstat /= 0) then
        print *, "FAIL generated fixed-form program did not compile"
        error stop 1
    end if

    call execute_command_line(executable_path, wait=.true., &
        exitstat=exitstat, cmdstat=cmdstat)
    if (cmdstat /= 0 .or. exitstat /= 0) then
        print *, "FAIL generated fixed-form program changed behavior"
        error stop 1
    end if

    print *, "test_fixed_form_comment_oracle: all cases passed"
end program test_fixed_form_comment_oracle
