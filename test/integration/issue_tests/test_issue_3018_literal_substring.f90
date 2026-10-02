program test_issue_3018_literal_substring
    ! #3018: a string-literal base with a constant substring range must not
    ! lose its range at parse time. 'abcdef'(2:4) folds to the literal
    ! 'bcd'; before the fix the postfix was dropped and the whole literal
    ! was lowered - a silent miscompile (ffc printed 'abcdef', gfortran
    ! printed 'bcd').
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: lowered, error_msg
    integer :: passed, total

    passed = 0
    total = 0

    call check_fold("'abcdef'(2:4)", "'bcd'", "'abcdef'(")
    call check_fold("'hello'(2:4)", "'ell'", "'hello'(")
    call check_fold('"dqtest"(2:3)', '"qt"', '"dqtest"(')
    call check_fold("'xyzw'(4:4)", "'w'", "'xyzw'(")

    ! Out-of-range bounds are NOT folded (gfortran rejects them at compile
    ! time); the slice must survive to be refused loudly downstream.
    total = total + 1
    call transform_lazy_fortran_string( &
        "program p"//new_line('a')// &
        "  print *, 'abcdef'(3:10)"//new_line('a')// &
        "end program p"//new_line('a'), lowered, error_msg)
    if (index(lowered, "'bcd'") > 0) then
        print *, 'FAIL: out-of-range literal range was folded wrongly'
    else
        passed = passed + 1
    end if

    print '(a,i0,a,i0)', 'test_issue_3018: ', passed, '/', total
    if (passed /= total) stop 1

contains

    subroutine check_fold(src_frag, expect, forbid)
        character(len=*), intent(in) :: src_frag, expect, forbid
        character(len=:), allocatable :: text
        text = "program p"//new_line('a')// &
            "  print *, "//src_frag//new_line('a')// &
            "end program p"//new_line('a')
        total = total + 1
        call transform_lazy_fortran_string(text, lowered, error_msg)
        if (index(lowered, expect) == 0) then
            print '(a,a)', 'FAIL missing fold: ', expect
        else if (index(lowered, forbid) > 0) then
            print '(a,a)', 'FAIL unfolded base survives: ', forbid
        else
            passed = passed + 1
        end if
    end subroutine check_fold
end program test_issue_3018_literal_substring
