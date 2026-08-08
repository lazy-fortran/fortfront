program test_keyword_named_subroutine_oracle
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD, OPERATING_MODE_INFER
    implicit none

    character(len=*), parameter :: input_path = &
        'examples/f90/issue_tapenade_v290_keyword_subroutine.f90'
    character(len=*), parameter :: output_path = &
        'build/issue_tapenade_v290_keyword_subroutine.f90'
    character(len=:), allocatable :: source, output
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: context
    integer :: unit
    integer :: cmdstat, exitstat

    call compile_with_gfortran(input_path, 'reference source')

    call read_example(input_path, source)
    context%input_mode = INPUT_MODE_STANDARD
    context%operating_mode = OPERATING_MODE_INFER
    call transform_with_context(source, output, error_msg, context)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: FortFront rejected v290 source: '// &
            trim(error_msg)
        error stop 1
    end if

    open (newunit=unit, file=output_path, status='replace', access='stream', &
        form='unformatted', action='write')
    write (unit) output
    close (unit)

    call compile_with_gfortran(output_path, 'FortFront output')
    print *, 'PASS: keyword-named subroutine and explicit interface compile'

contains

    include 'common/read_example.inc'

    subroutine compile_with_gfortran(source_path, description)
        character(len=*), intent(in) :: source_path, description
        character(len=:), allocatable :: object_path

        object_path = trim(source_path)//'.o'
        call execute_command_line('gfortran -std=f2018 -c '//trim(source_path)// &
            ' -o '//trim(object_path), wait=.true., &
            exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            write (error_unit, '(A)') 'FAIL: '//trim(description)// &
                ' did not compile with gfortran'
            error stop 2
        end if
    end subroutine compile_with_gfortran

end program test_keyword_named_subroutine_oracle
