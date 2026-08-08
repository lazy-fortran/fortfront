module select_type_arm_facts
    implicit none

    type :: base_t
        integer :: value
    end type base_t

    type, extends(base_t) :: child_t
    end type child_t

    type :: unrelated_t
    end type unrelated_t

contains

    subroutine inspect(selector, any_value)
        class(base_t), intent(in) :: selector
        class(*), intent(in) :: any_value

        select type (selector)
            type is (child_t)
            continue
        class is (base_t)
            continue
        class is (unrelated_t)
            continue
        class default
            continue
        end select

        select type (any_value)
            type is (integer)
            continue
        class default
            continue
        end select

        select type (any_value)
            type is (missing_t)
            continue
        class default
            continue
        end select
    end subroutine inspect

end module select_type_arm_facts
