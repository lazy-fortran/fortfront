program test_issue_2349_data_trailing_commas
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    type(transform_context_t) :: ctx

    ctx%input_mode = INPUT_MODE_STANDARD

    ! Test 1: Trailing comma in value list (accepted as extension)
    ! Non-standard trailing commas are tolerated by many compilers. We parse
    ! them and emit compliant Fortran without trailing commas.
    call read_example('examples/f90/issue_2349_data_trailing_value_comma.f90', &
                      source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) == 0, "Trailing comma in value list "// &
                "should be accepted as extension: "//trim(error_msg))
    call assert(index(output, "data arr/1, 2, 3 /") > 0, "Output should "// &
                "contain DATA statement without trailing comma")

    ! Test 2: Trailing comma in object list (accepted as extension)
    call read_example('examples/f90/issue_2349_data_trailing_object_comma.f90', &
                      source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) == 0, "Trailing comma in object list "// &
                "should be accepted as extension: "//trim(error_msg))
    call assert(index(output, "data a, b, c/1, 2, 3 /") > 0, "Output should "// &
                "contain DATA statement without trailing comma")

    ! Test 3: BOZ X prefix (already works, verify no regression)
    call read_example('examples/f90/issue_2349_data_boz_x_prefix.f90', source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) == 0, "BOZ X prefix should parse: "// &
                error_msg)
    call assert(index(output, "data") > 0, "Should contain data statement")

    ! Test 4: BOZ postfix (already works, verify no regression)
    call read_example('examples/f90/issue_2349_data_boz_postfix.f90', source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) == 0, "BOZ postfix should parse: "// &
                error_msg)
    call assert(index(output, "data") > 0, "Should contain data statement")

    ! Test 5: Implied DO (already works, verify no regression)
    call read_example('examples/f90/issue_2349_data_implied_do.f90', source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) == 0, "Implied DO should parse: "// &
                error_msg)
    call assert(index(output, "data") > 0, "Should contain data statement")

    print *, "test_issue_2349_data_trailing_commas PASSED"

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'


    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, "ASSERTION FAILED: ", message
            error stop 1
        end if
    end subroutine assert

end program test_issue_2349_data_trailing_commas
