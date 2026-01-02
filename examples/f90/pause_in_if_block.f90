program test_pause_if
    implicit none
    logical :: debug_mode

    debug_mode = .true.
    if (debug_mode) then
        pause 'Debug pause'
    end if
end program test_pause_if
