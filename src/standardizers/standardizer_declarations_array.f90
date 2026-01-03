module standardizer_declarations_array
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_bounds, only: range_expression_node
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_nodes_data, only: declaration_node
    use ast_base, only: LITERAL_INTEGER
    use string_utils_mod, only: to_lower
    use type_system_unified, only: mono_type_t, TARRAY
    use type_array_safe, only: safe_extract_array_rank
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: parse_dimension_attribute
    public :: set_array_properties_from_type
    public :: check_has_explicit_bounds

contains

    logical function check_has_explicit_bounds(arena, decl_node) result(valid)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl_node
        integer :: i, dim_idx

        valid = .false.
        if (.not. allocated(decl_node%dimension_indices)) return
        if (size(decl_node%dimension_indices) == 0) return

        valid = .true.
        do i = 1, size(decl_node%dimension_indices)
            dim_idx = decl_node%dimension_indices(i)
            if (dim_idx <= 0) then
                valid = .false.
                exit
            else if (dim_idx > arena%size) then
                cycle
            else if (.not. allocated(arena%entries(dim_idx)%node)) then
                valid = .false.
                exit
            else
                select type (dim_node => arena%entries(dim_idx)%node)
                type is (range_expression_node)
                    if (dim_node%start_index <= 0 .or. &
                        dim_node%end_index <= 0) then
                        valid = .false.
                        exit
                    end if
                class default
                    cycle
                end select
            end if
        end do
    end function check_has_explicit_bounds

    subroutine parse_dim_string_to_array(dims_str, dimensions)
        character(len=*), intent(in) :: dims_str
        integer, allocatable, intent(out) :: dimensions(:)
        integer :: i, comma_count, ndims, iostat
        integer :: start_pos, end_pos, comma_pos, dim_size
        character(len=20) :: dim_str

        comma_count = 0
        do i = 1, len_trim(dims_str)
            if (dims_str(i:i) == ',') comma_count = comma_count + 1
        end do
        ndims = comma_count + 1

        allocate (dimensions(ndims))

        start_pos = 1
        do i = 1, ndims
            if (i < ndims) then
                comma_pos = index(dims_str(start_pos:), ',')
                if (comma_pos > 0) then
                    end_pos = start_pos + comma_pos - 2
                else
                    end_pos = len_trim(dims_str)
                end if
            else
                end_pos = len_trim(dims_str)
            end if

            dim_str = dims_str(start_pos:end_pos)
            read (dim_str, *, iostat=iostat) dim_size
            if (iostat == 0) then
                dimensions(i) = dim_size
            else
                dimensions(i) = 0
            end if

            start_pos = end_pos + 2
        end do
    end subroutine parse_dim_string_to_array

    logical function validate_dimensions_match(arena, decl_node, dimensions) &
        result(should_return)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl_node
        integer, intent(in) :: dimensions(:)
        integer :: i, ndims, iostat, existing_dim, dim_idx_local
        logical :: dimensions_match, dimensions_known

        should_return = .false.
        ndims = size(dimensions)

        if (size(decl_node%dimension_indices) /= ndims) return

        dimensions_match = .true.
        dimensions_known = .true.
        do i = 1, ndims
            dim_idx_local = decl_node%dimension_indices(i)
            if (.not. arena%has_node_at(dim_idx_local)) then
                dimensions_known = .false.
                exit
            end if
            select type (dim_node_local => arena%entries(dim_idx_local)%node)
            type is (literal_node)
                read (dim_node_local%value, *, iostat=iostat) existing_dim
                if (iostat /= 0) then
                    dimensions_known = .false.
                    exit
                end if
                if (existing_dim /= dimensions(i)) then
                    dimensions_match = .false.
                    exit
                end if
            class default
                dimensions_known = .false.
                exit
            end select
        end do

        should_return = (.not. dimensions_known) .or. dimensions_match
    end function validate_dimensions_match

    subroutine create_dimension_literals(arena, prog_index, dimensions, &
                                         decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: dimensions(:)
        type(declaration_node), intent(inout) :: decl_node
        integer :: i, ndims
        type(literal_node) :: size_literal
        character(len=20) :: size_str

        ndims = size(dimensions)
        decl_node%is_array = .true.
        if (allocated(decl_node%dimension_indices)) &
            deallocate (decl_node%dimension_indices)
        allocate (decl_node%dimension_indices(ndims))

        do i = 1, ndims
            if (dimensions(i) > 0 .or. &
                (dimensions(i) == 0 .and. .not. decl_node%is_allocatable)) then
                write (size_str, '(i0)') dimensions(i)
                size_literal%uid = generate_uid()
                size_literal%value = trim(size_str)
                size_literal%literal_kind = LITERAL_INTEGER
                size_literal%line = 1
                size_literal%column = 1
                call arena%push(size_literal, "literal", prog_index)
                decl_node%dimension_indices(i) = arena%size
            else
                decl_node%dimension_indices(i) = 0
            end if
        end do
    end subroutine create_dimension_literals

    subroutine parse_dimension_attribute(arena, prog_index, var_type, &
                                         dim_pos, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index, dim_pos
        character(len=*), intent(in) :: var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: paren_pos
        integer, allocatable :: dimensions(:)
        character(len=100) :: dims_str
        logical :: has_explicit_bounds

        has_explicit_bounds = check_has_explicit_bounds(arena, decl_node)

        if (decl_node%is_parameter) then
            decl_node%is_array = .true.
            return
        end if

        paren_pos = index(var_type(dim_pos:), ')')
        if (paren_pos > 10) then
            dims_str = var_type(dim_pos + 10:dim_pos + paren_pos - 2)
        else
            if (has_explicit_bounds) then
                decl_node%is_array = .true.
                decl_node%is_allocatable = .false.
            end if
            return
        end if

        if (trim(dims_str) == ':') then
            if (has_explicit_bounds) then
                decl_node%is_array = .true.
                decl_node%is_allocatable = .false.
                return
            end if
            decl_node%is_array = .true.
            ! Deferred shape - allocatable UNLESS its a pointer
            ! ISO/IEC 1539-1:2018: pointer arrays can be deferred shape
            if (.not. decl_node%is_pointer) then
                decl_node%is_allocatable = .true.
            end if
            if (allocated(decl_node%dimension_indices)) &
                deallocate (decl_node%dimension_indices)
            allocate (decl_node%dimension_indices(1))
            decl_node%dimension_indices(1) = 0
            return
        end if

        call parse_dim_string_to_array(dims_str, dimensions)

        if (has_explicit_bounds) then
            if (validate_dimensions_match(arena, decl_node, dimensions)) then
                deallocate (dimensions)
                return
            end if
        end if

        call create_dimension_literals(arena, prog_index, dimensions, &
                                       decl_node)

        deallocate (dimensions)
    end subroutine parse_dimension_attribute

    function count_array_dimensions(inferred_type) result(ndims)
        type(mono_type_t), intent(in) :: inferred_type
        integer :: ndims
        type(mono_type_t) :: base_type

        call safe_extract_array_rank(inferred_type, ndims, base_type)

        if (ndims > 1) then
            ndims = 2
        end if
    end function count_array_dimensions

    subroutine extract_dimension_sizes(inferred_type, ndims, dim_sizes)
        type(mono_type_t), intent(in) :: inferred_type
        integer, intent(in) :: ndims
        integer, allocatable, intent(out) :: dim_sizes(:)
        type(mono_type_t) :: current_type
        integer :: dim_idx

        allocate (dim_sizes(ndims))

        if (ndims == 2) then
            dim_sizes(1) = inferred_type%size
            if (inferred_type%has_args() .and. &
                inferred_type%get_args_count() > 0) then
                current_type = inferred_type%get_arg(1)
                if (current_type%kind == TARRAY) then
                    dim_sizes(2) = current_type%size
                else
                    dim_sizes(2) = 0
                end if
            else
                dim_sizes(2) = 0
            end if
        else
            current_type = inferred_type
            do dim_idx = 1, ndims
                if (current_type%kind /= TARRAY) exit
                dim_sizes(dim_idx) = current_type%size
                if (.not. current_type%has_args() .or. &
                    current_type%get_args_count() < 1) exit
                current_type = current_type%get_arg(1)
            end do
        end if
    end subroutine extract_dimension_sizes

    subroutine set_array_properties_from_type(arena, var_name, prog_index, &
                                              decl_node)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: prog_index
        type(declaration_node), intent(inout) :: decl_node
        integer :: j, i
        type(literal_node) :: size_literal
        character(len=20) :: size_str
        integer :: ndims
        integer, allocatable :: dim_sizes(:)
        character(len=64) :: target_name
        character(len=64) :: candidate_name
        logical :: has_explicit_bounds

        target_name = to_lower(trim(var_name))

        if (decl_node%is_parameter) return

        ! Preserve existing explicit array bounds (standard Fortran declarations)
        ! ISO/IEC 1539-1:2018: explicit-shape arrays have declared dimensions
        has_explicit_bounds = check_has_explicit_bounds(arena, decl_node)
        if (has_explicit_bounds) return

        do j = 1, arena%size
            if (allocated(arena%entries(j)%node)) then
                select type (node => arena%entries(j)%node)
                type is (identifier_node)
                    candidate_name = to_lower(trim(node%name))
                    if (trim(candidate_name) == trim(target_name)) then
                        if (node%inferred_type%kind == TARRAY) then
                            if (node%inferred_type%alloc_info%is_allocatable) then
                                decl_node%is_array = .true.
                                decl_node%is_allocatable = .true.
                                ndims = count_array_dimensions(node%inferred_type)

                                if (allocated(decl_node%dimension_indices)) &
                                    deallocate (decl_node%dimension_indices)
                                allocate (decl_node%dimension_indices(ndims))

                                do i = 1, ndims
                                    decl_node%dimension_indices(i) = 0
                                end do
                                exit
                            end if
                        end if
                    end if
                end select
            end if
        end do

        if (decl_node%is_allocatable) return

        do j = 1, arena%size
            if (allocated(arena%entries(j)%node)) then
                select type (node => arena%entries(j)%node)
                type is (identifier_node)
                    candidate_name = to_lower(trim(node%name))
                    if (trim(candidate_name) == trim(target_name)) then
                        if (node%inferred_type%kind == TARRAY) then
                            decl_node%is_array = .true.

                            ndims = count_array_dimensions(node%inferred_type)

                            if (allocated(decl_node%dimension_indices)) &
                                deallocate (decl_node%dimension_indices)
                            allocate (decl_node%dimension_indices(ndims))

                            call extract_dimension_sizes(node%inferred_type, &
                                                         ndims, dim_sizes)

                            do i = 1, ndims
                                if (dim_sizes(i) > 0 .or. &
                                    (dim_sizes(i) == 0 .and. &
                                     .not. decl_node%is_allocatable)) then
                                    write (size_str, '(i0)') dim_sizes(i)
                                    size_literal%uid = generate_uid()
                                    size_literal%value = trim(size_str)
                                    size_literal%literal_kind = LITERAL_INTEGER
                                    size_literal%line = 1
                                    size_literal%column = 1
                                    call arena%push(size_literal, "literal", &
                                                    prog_index)
                                    decl_node%dimension_indices(i) = arena%size
                                else
                                    decl_node%dimension_indices(i) = 0
                                end if
                            end do

                            deallocate (dim_sizes)
                            exit
                        end if
                    end if
                end select
            end if
        end do
    end subroutine set_array_properties_from_type

end module standardizer_declarations_array
