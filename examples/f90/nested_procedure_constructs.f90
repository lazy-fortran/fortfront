module nested_procedure_constructs
contains
    subroutine check(value)
        integer, intent(in) :: value
        integer :: i
        type :: holder
            class(*), allocatable :: node
        end type holder
        type(holder) :: arena
        type(holder), allocatable :: entries(:)
        class(*), allocatable :: obj
        character(len=:), allocatable :: current_type
        allocate (integer :: obj)
        allocate (entries(1))
        select type (node => entries(1)%node)
            type is (integer)
            if (value < 0 .and. &
                allocated(entries)) then
                if (value == -1) then
                    return
                end if
            end if
            type_names: block
                integer :: block_value
                block_value = 1
                value = value
            end block type_names
            type is (holder)
            if (value > 0) then
                return
            end if
        class default
            return
        end select
        deallocate (obj)
        current_type = "child"
        flush (1)
        i = 1
        do while (i <= 2)
            if (value < 0) then
                return
            end if
            body_names: block
                character(len=:), allocatable :: next_type
                call find_parent(current_type, next_type)
                call move_alloc(next_type, current_type)
                if (value > 0) then
                    current_type = "parent"
                end if
            end block body_names
            i = i + 1
        end do
    end subroutine check
end module nested_procedure_constructs
