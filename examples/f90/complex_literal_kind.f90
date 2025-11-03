! Complex literal with kind parameter should retain both parts
program complex_literal_kind
    implicit none
    complex(kind=8) :: w = (1.0d0, 2.0d0)
    print *, w
end program complex_literal_kind
