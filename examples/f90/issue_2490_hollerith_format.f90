! Test case for issue #2490: FORMAT statements with Hollerith constants
! Hollerith constants use the syntax nHchars where n is the count and
! the next n characters are taken literally, including special characters
! like ! that would otherwise start a comment.
program main
    implicit none
    character(len=72) :: c
    write(c, 8000)
    8000 format (36(2H !!))
end program
