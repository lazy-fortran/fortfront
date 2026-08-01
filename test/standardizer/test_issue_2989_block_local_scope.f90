program test_issue_2989_block_local_scope
    ! fortfront #2989: a variable declared inside a BLOCK construct leaked a
    ! declaration to the enclosing program scope, and the leaked declaration
    ! carried the fallback type rather than the declared one -- an
    ! `integer :: k` inside the block reappeared as `real :: k` outside it,
    ! and a `character(len=3) :: tag` reappeared as `real :: tag`.
    !
    ! Entities declared in a BLOCK specification part are local to the
    ! construct (F2018 11.1.4). A name that exists at the outer scope with a
    ! type the source never wrote is a wrong-code path: anything referring to
    ! it binds to the invented declaration.
    !
    ! Oracle: examples/f90/block_local_scope.f90 is accepted by
    ! "gfortran -fsyntax-only" and prints "10 7". The emitted program must
    ! declare, at program scope, exactly the names the source declares there.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    integer :: failures
    character(len=:), allocatable :: source, output, errors

    failures = 0

    call read_example('examples/f90/block_local_scope.f90', source)
    call transform_lazy_fortran_string(source, output, errors)
    if (allocated(errors)) then
        if (len_trim(errors) > 0) then
            write (error_unit, '(a)') 'FAIL: transform reported: '//trim(errors)
            error stop 1
        end if
    end if

    ! The BLOCK locals keep their own declarations, inside the block.
    call expect_present(output, 'integer :: k', failures)
    call expect_present(output, 'real(dp) :: shadowed', failures)
    call expect_present(output, 'character(len=3) :: tag', failures)

    ! None of them may acquire a second, invented declaration carrying the
    ! fallback type. `k` and `tag` are declared only inside their blocks, so
    ! they must not appear with any other type anywhere.
    call expect_absent(output, 'real :: k', failures)
    call expect_absent(output, 'real(dp) :: k', failures)
    call expect_absent(output, 'real :: tag', failures)
    call expect_absent(output, 'real(dp) :: tag', failures)

    ! The outer `shadowed` is an integer and stays one. Exactly two
    ! declarations of the name may exist: the program-scope integer and the
    ! BLOCK-local real that shadows it -- never a third, invented one.
    call expect_present(output, 'integer :: shadowed', failures)
    call expect_occurrences(output, ':: shadowed', 2, failures)

    if (failures > 0) then
        write (error_unit, '(a,i0,a)') 'FAIL: ', failures, ' block-scope checks'
        write (error_unit, '(a)') '--- emitted ---'
        write (error_unit, '(a)') output
        error stop 1
    end if
    print *, 'PASS: BLOCK-local declarations stay local'

contains

    include '../common/read_example.inc'

    subroutine expect_present(haystack, needle, failures)
        character(len=*), intent(in) :: haystack, needle
        integer, intent(inout) :: failures

        if (index(haystack, needle) == 0) then
            write (error_unit, '(a)') 'FAIL: emitted code lost "'//needle//'"'
            failures = failures + 1
        end if
    end subroutine expect_present

    subroutine expect_absent(haystack, needle, failures)
        character(len=*), intent(in) :: haystack, needle
        integer, intent(inout) :: failures

        if (index(haystack, needle) /= 0) then
            write (error_unit, '(a)') 'FAIL: BLOCK-local name leaked as "'// &
                needle//'"'
            failures = failures + 1
        end if
    end subroutine expect_absent

    subroutine expect_occurrences(haystack, needle, wanted, failures)
        character(len=*), intent(in) :: haystack, needle
        integer, intent(in) :: wanted
        integer, intent(inout) :: failures
        integer :: pos, hit, found

        found = 0
        pos = 1
        do
            if (pos > len(haystack)) exit
            hit = index(haystack(pos:), needle)
            if (hit == 0) exit
            found = found + 1
            pos = pos + hit + len(needle) - 1
        end do
        if (found /= wanted) then
            write (error_unit, '(a,i0,a,i0)') 'FAIL: occurrences of "'//needle// &
                '": expected ', wanted, ', found ', found
            failures = failures + 1
        end if
    end subroutine expect_occurrences

end program test_issue_2989_block_local_scope
