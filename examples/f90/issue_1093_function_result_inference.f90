! Regression reproducer for issue 1093: function result inference without result()
program p
contains

    function incr(x)
        integer :: x

        incr = x + 1
    end function incr
end program p
