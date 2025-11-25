! Issue #2498: Operator precedence bugs
! Tests ISO/IEC 1539-1:2018 Table 10.1 operator precedence
!
! Case 1: .not. should have lower precedence than comparison operators
! .not. x == x should parse as .not. (x == x), NOT as (.not. x) == x
!
! Case 2: Old-style comparison operators (.ne., .eq., etc.)
! should have same precedence as modern comparison operators (==, /=, etc.)
program test_operator_precedence
    implicit none
    logical :: result1, result2, result3
    real :: x
    integer :: a, b

    ! Case 1: .not. precedence - should bind looser than ==
    x = 1.0
    result1 = .not. x == x
    ! Expected: .not. (x == x) = .not. .true. = .false.

    ! Case 2: Old-style comparisons should have PREC_COMPARISON
    a = 2
    b = 1
    result2 = a .ne. 2 .and. b .ne. 1
    ! Expected: (a .ne. 2) .and. (b .ne. 1) = .false. .and. .false. = .false.

    ! Case 3: Mix of old and new style
    result3 = a == 2 .and. b .ne. 1
    ! Expected: (a == 2) .and. (b .ne. 1) = .true. .and. .false. = .false.

    print *, "Case 1 (.not. precedence):", result1
    print *, "Case 2 (old-style comparison):", result2
    print *, "Case 3 (mixed comparison):", result3
end program test_operator_precedence
