program test_nullify_if
    implicit none
    integer, pointer :: ptr
    logical :: reset

    reset = .true.
    if (reset) then
        nullify (ptr)
    end if
end program test_nullify_if
