program issue_2837_interface_type_return
    use iso_c_binding
    implicit none

    type, bind(C) :: point_t
        real(c_float) :: x, y
    end type

    type, bind(C) :: rect_t
        type(point_t) :: origin
        real(c_float) :: width, height
    end type

    interface
        type(point_t) function make_point(x, y) bind(C)
            import :: c_float, point_t
            real(c_float), value :: x, y
        end function

        type(rect_t) function make_rect(ox, oy, w, h) bind(C)
            import :: c_float, rect_t
            real(c_float), value :: ox, oy, w, h
        end function
    end interface
end program issue_2837_interface_type_return
