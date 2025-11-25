! Test case for issue #2493: Character array constructor string literals
! must NOT be padded with spaces during roundtrip transformation.
!
! Per ISO/IEC 1539-1:2018 Section 7.8, character array constructors require
! all elements to have the same length-type-parameter. The compiler enforces
! this requirement, not the source code transformation tool.
!
! Strings must be emitted exactly as written in the source code.

program issue_2493_char_array_constructor
    implicit none
    character(len=1), dimension(2) :: arr

    ! Single character strings - must NOT be padded
    arr = (/'1', '2'/)
    print *, arr

    ! Array in expression context - must NOT be padded
    write(*, '(2A3)') 'X' // (/'1', '2'/) // 'Y'

    ! Modern array syntax - must NOT be padded
    print *, ['a', 'b', 'c']
end program issue_2493_char_array_constructor
