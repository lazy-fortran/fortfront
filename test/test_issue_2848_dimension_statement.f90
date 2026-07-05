program test_issue_2848_dimension_statement
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/f90/issue_2848_dimension_statement.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: unexpected error:", trim(error_msg)
        error stop 1
    end if

    ! The DIMENSION statement should create a proper array declaration
    ! Check that 'dimension' is NOT declared as a variable
    if (index(output, 'dimension') > 0) then
        ! 'dimension' should only appear in the dimension attribute, not as a var name
        if (index(output, ':: dimension') > 0 .or. &
            index(output, 'real') > 0 .and. index(output, 'dimension') > 0) then
            ! Check more carefully - dimension should appear as attribute, not variable
            block
                character(len=:), allocatable :: lower_output
                integer :: dim_pos, decl_pos
                lower_output = tolower(output)
                dim_pos = index(lower_output, 'dimension(')
                decl_pos = index(lower_output, ':: dimension')
                if (decl_pos > 0 .and. (dim_pos == 0 .or. decl_pos < dim_pos)) then
                    print *, "FAIL: 'dimension' parsed as variable instead of statement"
                    print *, "Output:"
                    print *, trim(output)
                    error stop 1
                end if
            end block
        end if
    end if

    ! Check that array 'a' has a proper declaration with dimension
    if (index(output, 'a(5)') <= 0 .and. index(output, 'a (5)') <= 0) then
        print *, "FAIL: array 'a' dimension not preserved"
        print *, "Output:"
        print *, trim(output)
        error stop 1
    end if

    ! Check that 'a' is declared as an array, not as allocatable with unknown shape
    if (index(output, 'a(:)') > 0) then
        print *, &
            "FAIL: array a should have explicit dimension, not allocatable shape"
        print *, "Output:"
        print *, trim(output)
        error stop 1
    end if

    print *, "PASS: DIMENSION statement creates proper array declaration"

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_num, ios
        integer :: file_size

        open (newunit=unit_num, file=filepath, status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: could not open example file:", trim(filepath)
            error stop 1
        end if

        inquire (unit=unit_num, size=file_size)
        allocate (character(len=file_size) :: content)

        read (unit_num, '(A)') content
        close (unit_num)
    end subroutine read_example

    function tolower(s) result(lower)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: lower
        integer :: i, code_val

        lower = s
        do i = 1, len(s)
            code_val = ichar(lower(i:i))
            if (code_val >= ichar('A') .and. code_val <= ichar('Z')) then
                lower(i:i) = char(code_val + ichar('a') - ichar('A'))
            end if
        end do
    end function tolower

end program test_issue_2848_dimension_statement
