! Valid IMPLICIT letter-spec lists that must keep compiling: a range and
! several implicit-specs separated by commas in one statement.
program implicit_letter_spec_forms
    implicit integer (a-h), real (i-n)
    integer :: apple
    real :: index

    apple = 1
    index = 2.0
    print *, apple, index
end program implicit_letter_spec_forms
