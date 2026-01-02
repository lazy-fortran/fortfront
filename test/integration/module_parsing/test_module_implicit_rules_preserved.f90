program test_module_implicit_rules_preserved
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_context_t, transform_with_context
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    integer :: module_pos

    call read_example('examples/f90/issue_2390_module_implicit_rules.f90', &
                      source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2390_module_implicit_rules'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    module_pos = index(output_code, 'module issue_2390_module_implicit_rules')
    if (module_pos == 0) then
        write (error_unit, '(A)') 'FAIL: module unit missing in round-trip output'
        error stop 1
    end if

    if (index(output_code, 'implicit integer (a-z)') == 0) then
        write (error_unit, '(A)') 'FAIL: implicit typing rule missing in output'
        error stop 1
    end if

    if (index(output_code, 'implicit none') > 0) then
        write (error_unit, '(A)') 'FAIL: unexpected implicit none injected'
        error stop 1
    end if

    if (index(output_code, 'val = 7') == 0) then
        write (error_unit, '(A)') 'FAIL: module body was not preserved'
        error stop 1
    end if

    print *, 'PASS: module implicit rules preserved without injection'


contains


    include '../../common/read_example.inc'
end program test_module_implicit_rules_preserved
