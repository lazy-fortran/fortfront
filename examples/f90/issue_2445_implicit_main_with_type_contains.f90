type :: config_t
    integer :: value
end type config_t

type(config_t) :: cfg

cfg%value = 42
print *, cfg%value

contains

    subroutine helper()
        implicit none
        print *, 'Helper called'
    end subroutine helper
    end
