module semantic_array_type_builders
    ! Array type construction helpers for semantic analysis
    use type_system_unified, only: mono_type_t, create_mono_type, TARRAY
    implicit none
    private

    public :: build_deferred_shape_array
    public :: build_fixed_shape_array
    public :: collapse_array_rank

contains

    function build_deferred_shape_array(element_type, rank) result(array_type)
        type(mono_type_t), intent(in) :: element_type
        integer, intent(in) :: rank
        type(mono_type_t) :: array_type
        type(mono_type_t) :: current
        type(mono_type_t), allocatable :: args(:)
        integer :: dim

        current = element_type
        if (rank <= 0) then
            array_type = current
            return
        end if

        do dim = rank, 1, -1
            allocate (args(1))
            args(1) = current
            current = create_mono_type(TARRAY, args=args)
            current%size = 0
            current%alloc_info%is_allocatable = .true.
            current%alloc_info%needs_allocation_check = .true.
            current%alloc_info%is_pointer = .false.
            current%alloc_info%needs_allocatable_string = .false.
            deallocate (args)
        end do

        array_type = current
    end function build_deferred_shape_array

    function build_fixed_shape_array(element_type, dimension_sizes) &
            result(array_type)
        type(mono_type_t), intent(in) :: element_type
        integer, intent(in) :: dimension_sizes(:)
        type(mono_type_t) :: array_type
        type(mono_type_t) :: current
        type(mono_type_t), allocatable :: args(:)
        integer :: dim
        integer :: rank

        current = element_type
        rank = size(dimension_sizes)
        if (rank <= 0) then
            array_type = current
            return
        end if

        do dim = rank, 1, -1
            allocate (args(1))
            args(1) = current
            current = create_mono_type(TARRAY, args=args, &
                array_size=dimension_sizes(dim))
            current%alloc_info%is_allocatable = .false.
            current%alloc_info%needs_allocation_check = .false.
            current%alloc_info%is_pointer = .false.
            current%alloc_info%needs_allocatable_string = .false.
            deallocate (args)
        end do

        array_type = current
    end function build_fixed_shape_array

    function collapse_array_rank(array_type, rank) result(element_type)
        type(mono_type_t), intent(in) :: array_type
        integer, intent(in) :: rank
        type(mono_type_t) :: element_type
        integer :: level

        element_type = array_type
        if (rank <= 0) return

        do level = 1, rank
            if (element_type%kind == TARRAY) then
                if (element_type%get_args_count() > 0) then
                    element_type = element_type%get_arg(1)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function collapse_array_rank

end module semantic_array_type_builders
