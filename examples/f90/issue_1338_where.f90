! Minimal reproducer for issue #1338: WHERE construct body incomplete
program test_where
    implicit none
    real, dimension(5) :: values, modified

    values = (/1.0, -2.0, 3.0, -4.0, 5.0/)

    where (values > 0.0)
        modified = values * 2.0
    elsewhere
        modified = 0.0
    end where

    print *, modified
end program test_where
