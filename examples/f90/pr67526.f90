! Negative fixture for issue #2897, mirrored on gfortran.dg/pr67526.f90.
! Every substring reference is left unterminated, so each initializer is an
! error in SUBSTRING and must be rejected.
program p
    character :: c1 = 'abc'(:
    character :: c2 = 'abc'(3:
    character :: c3 = 'abc'(:1
    character :: c4 = 'abc'(2:2
end program p
