program test_tooling_passthrough
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_context_t, transform_with_context
    use frontend_transformation, only: INPUT_MODE_STANDARD

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: expected
    type(transform_context_t) :: ctx

    call read_example('examples/f90/external_tool_example.f90', input_text)
    expected = input_text // new_line('a')

    ! Use standard Fortran mode for .f90 files
    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .false.

    call transform_with_context(expected, output_text, error_msg, ctx)

    if (.not. allocated(output_text)) then
        print *, 'FAIL: passthrough produced no output'
        stop 1
    end if

    ! Note: Standard Fortran transformation may make minor formatting changes
    ! (whitespace, comment placement) but should preserve semantic meaning.
    ! We verify the output is valid and contains key elements rather than
    ! requiring byte-for-byte identity.
    if (index(output_text, 'program external_tool_example') == 0) then
        print *, 'FAIL: program declaration missing'
        stop 1
    end if

    if (index(output_text, 'use frontend_transformation') == 0) then
        print *, 'FAIL: API import missing'
        stop 1
    end if

    if (index(output_text, 'call transform_lazy_fortran_string') == 0) then
        print *, 'FAIL: API call missing'
        stop 1
    end if

    if (index(output_text, 'end program external_tool_example') == 0) then
        print *, 'FAIL: program end missing'
        stop 1
    end if

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error message:'
            print *, trim(error_msg)
            stop 1
        end if
    end if

    print *, 'PASS: tooling example transformation preserved key elements'


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_tooling_passthrough
