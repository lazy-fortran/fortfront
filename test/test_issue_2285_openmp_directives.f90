program test_issue_2285_openmp_directives
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_parallel_do, has_end_parallel

    call read_example('examples/f90/issue_2285_openmp_directives.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2285_openmp_directives'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_parallel_do = index(output_code, '!$omp parallel do') > 0
    has_end_parallel = index(output_code, '!$omp end parallel do') > 0

    if (.not. has_parallel_do) then
        write (error_unit, '(A)') 'FAIL: missing !$omp parallel do directive'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_end_parallel) then
        write (error_unit, '(A)') 'FAIL: missing !$omp end parallel do directive'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: OpenMP directives preserved'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2285_openmp_directives
