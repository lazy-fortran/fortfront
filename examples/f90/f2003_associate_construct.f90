! Example demonstrating associate construct within procedure bodies
! ISO/IEC 1539-1:2018 Section 11.1.3 - ASSOCIATE construct
module associate_construct_demo_mod
    implicit none

    type :: point_t
        real :: x
        real :: y
    end type point_t

contains

    subroutine process_point(pt, magnitude)
        type(point_t), intent(in) :: pt
        real, intent(out) :: magnitude

        ! Associate construct with component access
        associate (px => pt%x, py => pt%y)
            magnitude = sqrt(px**2 + py**2)
            print *, 'Point coordinates:', px, py
        end associate

    end subroutine process_point

    subroutine compute_scaled(val, result_val)
        real, intent(in) :: val
        real, intent(out) :: result_val

        ! Associate with expression
        associate (scaled_val => val * 2.0)
            result_val = scaled_val
        end associate

    end subroutine compute_scaled

end module associate_construct_demo_mod
