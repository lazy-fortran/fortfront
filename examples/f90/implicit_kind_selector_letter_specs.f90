! Valid IMPLICIT statements whose declaration-type-spec carries a kind or
! length selector before the letter-spec list. The selector parentheses are not
! a letter-spec list and must not be validated as one.
program implicit_kind_selector_letter_specs
    implicit real(kind=8) (o-t)
    implicit character(len=4) (u)

    omega = 3.0d0
    udder = 'abcd'
    print *, omega, udder
end program implicit_kind_selector_letter_specs
