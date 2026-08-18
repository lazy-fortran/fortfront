program test_fixed_form_implicit_dimension_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: compilation_options_t, compile_source
    implicit none

    character(len=*), parameter :: input_path = &
        "examples/f90/fixed_form_implicit_dimension.f"
    character(len=*), parameter :: output_path = &
        "build/fixed_form_implicit_dimension.f90"
    character(len=:), allocatable :: source_executable, output_executable
    character(len=512) :: error_msg
    type(compilation_options_t) :: options

    source_executable = test_executable_path('fixed_form_implicit_dimension_source')
    output_executable = test_executable_path('fixed_form_implicit_dimension_output')

    call compile_and_run(input_path, source_executable, &
        "reference fixed-form program")

    options%output_file = output_path
    call compile_source(input_path, options, error_msg)
    if (len_trim(error_msg) > 0) then
        call fail("transformation failed: "//trim(error_msg))
    end if

    call compile_and_run(output_path, output_executable, &
        "generated free-form program")
    print *, "PASS: implicit DIMENSION dummy preserved"

contains

    include 'common/test_command_helpers.inc'

    subroutine compile_and_run(source_path, executable_path, description)
        character(len=*), intent(in) :: source_path, executable_path, description
        integer :: cmdstat, exitstat

        call execute_command_line("gfortran "//source_path//" -o "// &
            executable_path, wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            call fail(trim(description)//" did not compile")
        end if

        call execute_command_line(executable_path, wait=.true., &
            exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            call fail(trim(description)//" changed behavior")
        end if
    end subroutine compile_and_run

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') "FAIL: "//trim(message)
        error stop 1
    end subroutine fail

end program test_fixed_form_implicit_dimension_oracle
