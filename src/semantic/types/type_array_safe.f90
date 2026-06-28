module type_array_safe
    ! Safe array type operations with built-in loop bounds
    ! Prevents infinite loops when traversing potentially malformed type structures
    use type_system_unified, only: mono_type_t, TARRAY
    implicit none
    private

    public :: safe_extract_array_rank, safe_peel_array_to_base
    public :: MAX_ARRAY_RANK

    ! Fortran standard maximum array rank
    integer, parameter :: MAX_ARRAY_RANK = 15

contains

    ! Safely extract array rank and base type with bounded loop
    ! Returns rank=0 if type is not an array
    subroutine safe_extract_array_rank(source_type, rank, base_type)
        type(mono_type_t), intent(in) :: source_type
        integer, intent(out) :: rank
        type(mono_type_t), intent(out) :: base_type
        type(mono_type_t) :: current
        integer :: safety_counter

        current = source_type
        call current%sync_from_arena()
        rank = 0
        safety_counter = 0

        do while (current%kind == TARRAY .and. current%has_args() .and. &
                safety_counter < MAX_ARRAY_RANK)
            rank = rank + 1
            safety_counter = safety_counter + 1
            current = current%get_arg(1)
            call current%sync_from_arena()
        end do

        base_type = current
        call base_type%sync_from_arena()
    end subroutine safe_extract_array_rank

    ! Safely peel all array layers to get base element type
    function safe_peel_array_to_base(array_type) result(element_type)
        type(mono_type_t), intent(in) :: array_type
        type(mono_type_t) :: element_type
        integer :: safety_counter

        element_type = array_type
        call element_type%sync_from_arena()
        safety_counter = 0

        do while (element_type%kind == TARRAY .and. &
                element_type%has_args() .and. &
                safety_counter < MAX_ARRAY_RANK)
            if (element_type%get_args_count() < 1) exit
            element_type = element_type%get_arg(1)
            call element_type%sync_from_arena()
            safety_counter = safety_counter + 1
        end do
    end function safe_peel_array_to_base

end module type_array_safe
