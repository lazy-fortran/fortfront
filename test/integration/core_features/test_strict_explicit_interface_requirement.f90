program test_strict_explicit_interface_requirement
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_context_t, transform_with_context, &
                                  INPUT_MODE_STANDARD, OPERATING_MODE_INFER, &
                                  OPERATING_MODE_STRICT
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .false.

    call read_example( &
        'examples/f90/issue_2592_missing_explicit_interface_subroutine.f90', &
        source)
    ctx%operating_mode = OPERATING_MODE_INFER
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_contains(error_msg, 'No explicit interface for procedure')
    call assert_contains(error_msg, "external_sub")

    call read_example( &
        'examples/f90/issue_2592_missing_explicit_interface_function.f90', &
        source)
    ctx%operating_mode = OPERATING_MODE_INFER
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_contains(error_msg, 'No explicit interface for procedure')
    call assert_contains(error_msg, "external_func")

    call read_example( &
        'examples/f90/issue_2592_internal_procedure_explicit_interface_ok.f90', &
        source)
    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    call read_example( &
        'examples/f90/issue_2592_interface_block_explicit_interface_ok.f90', &
        source)
    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    call read_example( &
        'examples/f90/issue_2639_intrinsic_subroutine_calls_ok.f90', &
        source)
    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    call read_example('examples/f90/call_graph_module_program_scopes.f90', source)
    ctx%operating_mode = OPERATING_MODE_STRICT
    call transform_with_context(source, output, error_msg, ctx)
    call assert_no_error(error_msg)

    print *, 'PASS: strict mode requires explicit interfaces'

contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

    subroutine assert_no_error(msg)
        character(len=:), allocatable, intent(in) :: msg

        if (.not. allocated(msg)) return
        if (len_trim(msg) == 0) return
        write (error_unit, '(A)') 'FAIL: unexpected error message:'
        write (error_unit, '(A)') trim(msg)
        error stop 1
    end subroutine assert_no_error

    subroutine assert_contains(msg, needle)
        character(len=:), allocatable, intent(in) :: msg
        character(len=*), intent(in) :: needle

        if (.not. allocated(msg)) then
            write (error_unit, '(A)') 'FAIL: expected error message'
            error stop 1
        end if
        if (index(msg, needle) == 0) then
            write (error_unit, '(A)') 'FAIL: expected message to contain: ' // &
                trim(needle)
            write (error_unit, '(A)') 'Got:'
            write (error_unit, '(A)') trim(msg)
            error stop 1
        end if
    end subroutine assert_contains

end program test_strict_explicit_interface_requirement
