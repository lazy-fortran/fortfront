program test_issue_2286_openacc_directives
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_parallel_loop, has_end_parallel

    call read_example('examples/f90/issue_2286_openacc_internal.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2286_openacc_internal'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_parallel_loop = index(output_code, '!$acc parallel loop') > 0
    has_end_parallel = index(output_code, '!$acc end parallel loop') > 0

    if (.not. has_parallel_loop) then
        write (error_unit, '(A)') 'FAIL: missing !$acc parallel loop directive'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_end_parallel) then
        write (error_unit, '(A)') 'FAIL: missing !$acc end parallel loop directive'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: OpenACC directives preserved inside internal procedures'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2286_openacc_directives
