module parser_type_hooks_module
    implicit none
    private

    type, public :: type_annotation_t
        integer :: decl_index = 0
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: var_names(:)
        logical :: has_kind = .false.
        integer :: kind_value = 0
        logical :: is_parameter = .false.
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: has_dimensions = .false.
        integer, allocatable :: dimension_indices(:)
    end type type_annotation_t

    type(type_annotation_t), allocatable :: registry(:)

    public :: register_type_annotation
    public :: consume_type_annotations
    public :: has_type_annotations

contains

    subroutine register_type_annotation(decl_index, type_name, var_names, has_kind, kind_value, &
                              is_parameter, is_allocatable, is_pointer, dimension_indices)
        integer, intent(in) :: decl_index
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_names(:)
        logical, intent(in), optional :: has_kind
        integer, intent(in), optional :: kind_value
        logical, intent(in), optional :: is_parameter, is_allocatable, is_pointer
        integer, intent(in), optional :: dimension_indices(:)
        type(type_annotation_t), allocatable :: temp(:)
        integer :: entry_index, n, max_len, i

        if (.not. allocated(registry)) then
            allocate (registry(1))
        else
            n = size(registry)
            allocate (temp(n + 1))
            temp(1:n) = registry
            call move_alloc(temp, registry)
        end if
        entry_index = size(registry)

        registry(entry_index)%decl_index = decl_index
        registry(entry_index)%type_name = trim(type_name)

        max_len = 1
        do i = 1, size(var_names)
            max_len = max(max_len, len_trim(var_names(i)))
        end do
     allocate (character(len=max_len) :: registry(entry_index)%var_names(size(var_names)))
        do i = 1, size(var_names)
            registry(entry_index)%var_names(i) = adjustl(trim(var_names(i)))
        end do

        if (present(has_kind)) then
            registry(entry_index)%has_kind = has_kind
        else
            registry(entry_index)%has_kind = .false.
        end if

        if (present(kind_value)) then
            registry(entry_index)%kind_value = kind_value
        else
            registry(entry_index)%kind_value = 0
        end if

        if (present(is_parameter)) then
            registry(entry_index)%is_parameter = is_parameter
        else
            registry(entry_index)%is_parameter = .false.
        end if

        if (present(is_allocatable)) then
            registry(entry_index)%is_allocatable = is_allocatable
        else
            registry(entry_index)%is_allocatable = .false.
        end if

        if (present(is_pointer)) then
            registry(entry_index)%is_pointer = is_pointer
        else
            registry(entry_index)%is_pointer = .false.
        end if

        if (present(dimension_indices)) then
            registry(entry_index)%has_dimensions = .true.
            if (size(dimension_indices) > 0) then
               allocate (registry(entry_index)%dimension_indices(size(dimension_indices)))
                registry(entry_index)%dimension_indices = dimension_indices
            else
                allocate (registry(entry_index)%dimension_indices(0))
            end if
        else
            registry(entry_index)%has_dimensions = .false.
        end if
    end subroutine register_type_annotation

    logical function has_type_annotations()
        if (.not. allocated(registry)) then
            has_type_annotations = .false.
        else
            has_type_annotations = size(registry) > 0
        end if
    end function has_type_annotations

    subroutine consume_type_annotations(entries)
        type(type_annotation_t), allocatable, intent(out) :: entries(:)

        if (.not. allocated(registry)) then
            allocate (entries(0))
            return
        end if

        call move_alloc(registry, entries)
    end subroutine consume_type_annotations

end module parser_type_hooks_module
