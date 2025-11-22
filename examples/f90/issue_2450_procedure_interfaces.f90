program issue_2450_procedure_interfaces
    implicit none
    real, external :: external_scale
    procedure(real), pointer :: proc_ptr

    proc_ptr => internal_double
    print *, proc_ptr(5.0)

    proc_ptr => get_proc()
    print *, proc_ptr(2.0)
    print *, external_scale(3.0)

contains

    real function internal_double(x)
        real, intent(in) :: x
        internal_double = x * 2.0
    end function internal_double

    function apply_proc(fn, value) result(res)
        procedure(real) :: fn
        real, intent(in) :: value
        real :: res

        res = fn(value)
    end function apply_proc

    function get_proc() result(p)
        procedure(real), pointer :: p
        p => internal_double
    end function get_proc
end program issue_2450_procedure_interfaces

real function external_scale(y)
    real, intent(in) :: y
    external_scale = y * 3.0
end function external_scale
