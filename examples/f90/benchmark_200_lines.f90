program benchmark
    implicit none
    integer :: result
    integer :: x, y, z

contains

    integer function bench_func_1(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_1 = acc
    end function bench_func_1

    integer function bench_func_2(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_2 = acc
    end function bench_func_2

    integer function bench_func_3(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_3 = acc
    end function bench_func_3

    integer function bench_func_4(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_4 = acc
    end function bench_func_4

    integer function bench_func_5(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_5 = acc
    end function bench_func_5

    integer function bench_func_6(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_6 = acc
    end function bench_func_6

    integer function bench_func_7(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_7 = acc
    end function bench_func_7

    integer function bench_func_8(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_8 = acc
    end function bench_func_8

    integer function bench_func_9(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_9 = acc
    end function bench_func_9

    integer function bench_func_10(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_10 = acc
    end function bench_func_10

    integer function bench_func_11(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_11 = acc
    end function bench_func_11

    integer function bench_func_12(a, b)
        integer, intent(in) :: a, b
        integer :: temp
        integer :: acc
        integer :: k

        acc = 0
        temp = a + b
        do k = 1, 10
            acc = acc + temp * k
        end do
        bench_func_12 = acc
    end function bench_func_12


    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
    ! padding line
end program benchmark
