subroutine select_rank_facts(values, pointers, polymorphic)
    real, intent(inout) :: values(..)
    real, pointer, intent(in) :: pointers(..)
    class(*), intent(in) :: polymorphic

    select rank (values)
        rank (0)
        values = 1.0
        rank (1)
        values = 2.0
        rank default
        continue
    end select

    select rank (pointers)
        rank (1)
        continue
        rank (*)
        continue
    end select

    select type (polymorphic)
    class default
        continue
    end select
end subroutine select_rank_facts
