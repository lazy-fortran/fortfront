program test_issue_2349_data_trailing_commas
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    type(transform_context_t) :: ctx

    ctx%input_mode = INPUT_MODE_STANDARD

    ! Test 1: Trailing comma in value list (non-compliant)
    call read_example('examples/f90/issue_2349_data_trailing_value_comma.f90', &
                      source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) > 0, "Trailing comma in value list "// &
                "must be rejected (ISO/IEC 1539-1:2018 8.6.7, R838)")
    call assert(index(error_msg, "Trailing comma") > 0, "Error should "// &
                "mention trailing comma")

    ! Test 2: Trailing comma in object list (non-compliant)
    call read_example('examples/f90/issue_2349_data_trailing_object_comma.f90', &
                      source)
    call transform_with_context(source, output, error_msg, ctx)
    call assert(len_trim(error_msg) > 0, "Trailing comma in object list "// &
                "must be rejected (ISO/IEC 1539-1:2018 8.6.7, R838)")
    call assert(index(error_msg, "Trailing comma") > 0, "Error should "// &
                "mention trailing comma")

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

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, read_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', action='read', &
              access='stream')
        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=read_size) buffer
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, "ASSERTION FAILED: ", message
            error stop 1
        end if
    end subroutine assert

end program test_issue_2349_data_trailing_commas
