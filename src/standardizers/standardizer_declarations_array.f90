module standardizer_declarations_array
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_bounds, only: range_expression_node
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_nodes_data, only: declaration_node
    use ast_base, only: LITERAL_INTEGER
    use type_system_unified, only: mono_type_t, TARRAY
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: parse_dimension_attribute
    public :: set_array_properties_from_type

contains

    subroutine parse_dimension_attribute(arena, prog_index, var_type, &
                                         dim_pos, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index, dim_pos
        character(len=*), intent(in) :: var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: paren_pos, iostat, dim_size, i, comma_count, ndims
        integer :: start_pos, end_pos, comma_pos
        character(len=20) :: dim_str
        type(literal_node) :: size_literal
        character(len=20) :: size_str
        integer, allocatable :: dimensions(:)
        character(len=100) :: dims_str
        logical :: has_explicit_bounds
        integer :: dim_idx

        has_explicit_bounds = .false.
        if (allocated(decl_node%dimension_indices)) then
            if (size(decl_node%dimension_indices) > 0) then
                has_explicit_bounds = .true.
                do i = 1, size(decl_node%dimension_indices)
                    dim_idx = decl_node%dimension_indices(i)
                    if (dim_idx <= 0) then
                        has_explicit_bounds = .false.
                        exit
                    else if (dim_idx > arena%size) then
                        cycle
                    else if (.not. allocated(arena%entries(dim_idx)%node)) then
                        has_explicit_bounds = .false.
                        exit
                    else
                        select type (dim_node => arena%entries(dim_idx)%node)
                        type is (range_expression_node)
                            if (dim_node%start_index <= 0 .or. &
                                dim_node%end_index <= 0) then
                                has_explicit_bounds = .false.
                                exit
                            end if
                        class default
                            cycle
                        end select
                    end if
                end do
            end if
        end if

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
            decl_node%is_allocatable = .true.
            if (allocated(decl_node%dimension_indices)) &
                deallocate (decl_node%dimension_indices)
            allocate (decl_node%dimension_indices(1))
            decl_node%dimension_indices(1) = 0
            return
        end if

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

        if (has_explicit_bounds) then
            if (size(decl_node%dimension_indices) == ndims) then
                block
                    logical :: dimensions_match, dimensions_known
                    integer :: existing_dim, dim_idx_local

                    dimensions_match = .true.
                    dimensions_known = .true.
                    do i = 1, ndims
                        dim_idx_local = decl_node%dimension_indices(i)
                        if (dim_idx_local <= 0 .or. dim_idx_local > arena%size) cycle
                        if (.not. allocated(arena%entries(dim_idx_local)%node)) then
                            dimensions_known = .false.
                            exit
                        end if
                        select type (dim_node_local => &
                                     arena%entries(dim_idx_local)%node)
                        type is (literal_node)
                            read (dim_node_local%value, *, iostat=iostat) &
                                existing_dim
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
                    if (.not. dimensions_known) then
                        deallocate (dimensions)
                        return
                    end if
                    if (dimensions_match) then
                        deallocate (dimensions)
                        return
                    end if
                end block
            end if
        end if

        decl_node%is_array = .true.
        if (allocated(decl_node%dimension_indices)) &
            deallocate (decl_node%dimension_indices)
        allocate (decl_node%dimension_indices(ndims))

        do i = 1, ndims
            if (dimensions(i) > 0) then
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

        deallocate (dimensions)
    end subroutine parse_dimension_attribute

    subroutine set_array_properties_from_type(arena, var_name, prog_index, &
                                              decl_node)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: prog_index
        type(declaration_node), intent(inout) :: decl_node
        integer :: j, i
        type(literal_node) :: size_literal
        character(len=20) :: size_str
        type(mono_type_t) :: current_type
        integer :: ndims, dim_idx
        integer, allocatable :: dim_sizes(:)

        if (decl_node%is_parameter) then
            return
        end if

        do j = 1, arena%size
            if (allocated(arena%entries(j)%node)) then
                select type (node => arena%entries(j)%node)
                type is (identifier_node)
                    if (trim(node%name) == trim(var_name)) then
                        if (node%inferred_type%kind > 0) then
                            if (node%inferred_type%kind == TARRAY) then
                                decl_node%is_array = .true.

                                current_type = node%inferred_type
                                ndims = 0

                                do while (current_type%kind == TARRAY)
                                    ndims = ndims + 1
                                    if (.not. current_type%has_args() .or. &
                                        current_type%get_args_count() < 1) exit
                                    current_type = current_type%get_arg(1)
                                end do

                                if (ndims > 1) then
                                    ndims = 2
                                end if

                                if (allocated(decl_node%dimension_indices)) &
                                    deallocate (decl_node%dimension_indices)
                                allocate (decl_node%dimension_indices(ndims))
                                allocate (dim_sizes(ndims))

                                if (ndims == 2) then
                                    dim_sizes(1) = node%inferred_type%size
                                    if (node%inferred_type%has_args() .and. &
                                        node%inferred_type%get_args_count() > 0) then
                                        current_type = node%inferred_type%get_arg(1)
                                        if (current_type%kind == TARRAY) then
                                            dim_sizes(2) = current_type%size
                                        else
                                            dim_sizes(2) = 0
                                        end if
                                    else
                                        dim_sizes(2) = 0
                                    end if
                                else
                                    current_type = node%inferred_type
                                    dim_idx = 1
                                    do while (current_type%kind == TARRAY .and. &
                                              dim_idx <= ndims)
                                        dim_sizes(dim_idx) = current_type%size
                                        if (.not. current_type%has_args() .or. &
                                            current_type%get_args_count() < 1) exit
                                        current_type = current_type%get_arg(1)
                                        dim_idx = dim_idx + 1
                                    end do
                                end if

                                do i = 1, ndims
                                    if (dim_sizes(i) > 0) then
                                        if (.not. node%inferred_type%alloc_info% &
                                            is_allocatable) then
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
                                    else
                                        decl_node%dimension_indices(i) = 0
                                    end if
                                end do

                                deallocate (dim_sizes)
                                exit
                            end if
                        end if
                    end if
                end select
            end if
        end do
    end subroutine set_array_properties_from_type

end module standardizer_declarations_array
