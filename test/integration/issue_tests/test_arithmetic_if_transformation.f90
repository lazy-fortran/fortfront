program test_arithmetic_if_transformation
    ! Integration test: validate arithmetic IF transforms to block IF/ELSEIF/ELSE
    !
    ! ISO/IEC 1539-1:2018 Compliance:
    ! - Annex B.3 item 1: Arithmetic IF is deleted from Fortran 2008
    ! - Section 11.1.8: IF construct and statement (defines standard block IF)
    !
    ! This test validates that fortfront transforms arithmetic IF statements
    ! into ISO 2018 standard-conforming block IF constructs:
    !   IF (expr) label1, label2, label3
    ! becomes:
    !   IF (expr < 0) THEN
    !       GO TO label1
    !   ELSE IF (expr == 0) THEN
    !       GO TO label2
    !   ELSE
    !       GO TO label3
    !   END IF
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: source, output, lowered
    character(len=:), allocatable :: error_msg
    integer :: passed, total

    passed = 0
    total = 0

    print *, "=== Arithmetic IF Transformation Integration Tests ==="
    print *, ""
    print *, "ISO/IEC 1539-1:2018 compliance validation"
    print *, "Arithmetic IF (B.3 deleted feature) -> block IF (section 11.1.8)"
    print *, ""

    ! Test 1: Basic arithmetic IF transformation
    call read_example('examples/f90/issue_2074_arithmetic_if_not_supported.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)
    call check_no_error("basic transformation", error_msg)
    lowered = to_lower(output)

    total = total + 1
    if (index(lowered, "if (x < 0) then") > 0) then
        passed = passed + 1
        print *, "  PASS: negative branch IF condition"
    else
        write (error_unit, '(A)') "  FAIL: missing negative branch condition"
    end if

    total = total + 1
    if (index(lowered, "go to 10") > 0) then
        passed = passed + 1
        print *, "  PASS: negative branch GOTO"
    else
        write (error_unit, '(A)') "  FAIL: missing negative branch goto"
    end if

    total = total + 1
    if (index(lowered, "else if (x == 0) then") > 0) then
        passed = passed + 1
        print *, "  PASS: zero branch ELSEIF condition"
    else
        write (error_unit, '(A)') "  FAIL: missing zero branch elseif condition"
    end if

    total = total + 1
    if (index(lowered, "go to 20") > 0) then
        passed = passed + 1
        print *, "  PASS: zero branch GOTO"
    else
        write (error_unit, '(A)') "  FAIL: missing zero branch goto"
    end if

    total = total + 1
    if (index(lowered, "go to 30") > 0) then
        passed = passed + 1
        print *, "  PASS: positive branch GOTO (in ELSE)"
    else
        write (error_unit, '(A)') "  FAIL: missing positive branch goto"
    end if

    total = total + 1
    if (index(lowered, "end if") > 0) then
        passed = passed + 1
        print *, "  PASS: END IF present"
    else
        write (error_unit, '(A)') "  FAIL: missing END IF"
    end if

    ! Test 2: Verify arithmetic IF syntax NOT in output
    total = total + 1
    if (index(lowered, "if (x) 10") == 0 .and. &
        index(lowered, ") 10, 20, 30") == 0 .and. &
        index(lowered, ") 10,20,30") == 0) then
        passed = passed + 1
        print *, "  PASS: arithmetic IF syntax removed"
    else
        write (error_unit, '(A)') "  FAIL: arithmetic IF syntax still present"
    end if

    ! Test 3: Whitespace variant
    call read_example('examples/f90/arithmetic_if_whitespace_labels.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)
    call check_no_error("whitespace variant", error_msg)
    lowered = to_lower(output)

    total = total + 1
    if (index(lowered, "if (x < 0) then") > 0 .and. &
        index(lowered, "go to 10") > 0 .and. &
        index(lowered, "go to 20") > 0 .and. &
        index(lowered, "go to 30") > 0) then
        passed = passed + 1
        print *, "  PASS: whitespace labels transformed correctly"
    else
        write (error_unit, '(A)') "  FAIL: whitespace labels transformation failed"
    end if

    ! Test 4: Comment variant
    call read_example('examples/f90/arithmetic_if_inline_comment.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)
    call check_no_error("comment variant", error_msg)
    lowered = to_lower(output)

    total = total + 1
    if (index(lowered, "if (y < 0) then") > 0 .and. &
        index(lowered, "go to 100") > 0 .and. &
        index(lowered, "go to 200") > 0 .and. &
        index(lowered, "go to 300") > 0) then
        passed = passed + 1
        print *, "  PASS: inline comment preserved, IF transformed"
    else
        write (error_unit, '(A)') "  FAIL: inline comment variant failed"
    end if

    ! Test 5: Complex expression variant
    call read_example('examples/f90/arithmetic_if_expression_complex.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)
    call check_no_error("complex expression", error_msg)
    lowered = to_lower(output)

    total = total + 1
    if (index(lowered, "< 0) then") > 0 .and. &
        index(lowered, "go to 10") > 0 .and. &
        index(lowered, "go to 20") > 0 .and. &
        index(lowered, "go to 30") > 0) then
        passed = passed + 1
        print *, "  PASS: complex expression transformed"
    else
        write (error_unit, '(A)') "  FAIL: complex expression transformation failed"
    end if

    print *, ""
    print *, "=== Results: ", passed, " / ", total, " passed ==="

    if (passed == total) then
        print *, "PASS: All arithmetic IF transformation tests passed"
    else
        write (error_unit, '(A)') "FAIL: Some arithmetic IF transformation tests failed"
        error stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'

    subroutine check_no_error(test_name, err)
        character(len=*), intent(in) :: test_name
        character(len=:), allocatable, intent(in) :: err

        if (allocated(err)) then
            if (len_trim(err) > 0) then
                write (error_unit, '(A,A,A)') "FAIL: ", trim(test_name), " error"
                write (error_unit, '(A)') trim(err)
                error stop 1
            end if
        end if
    end subroutine check_no_error


end program test_arithmetic_if_transformation
