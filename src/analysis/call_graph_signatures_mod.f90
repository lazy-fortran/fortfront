module call_graph_signatures_mod
    implicit none
    private

    public :: type_signature_t, signatures_map_t
    public :: create_signatures_map, add_signature, get_unique_signatures
    public :: signature_exists, signatures_map_deep_copy

    type :: type_signature_t
        integer, allocatable :: param_kinds(:)
        character(len=:), allocatable :: param_type_strings(:)
        character(len=:), allocatable :: return_type_string
        integer :: return_kind = 0
        integer :: call_site_node = 0
        integer :: line = 0
        integer :: column = 0
    end type type_signature_t

    type :: procedure_signatures_t
        character(len=:), allocatable :: procedure_name
        type(type_signature_t), allocatable :: signatures(:)
        integer :: sig_count = 0
        integer :: sig_capacity = 0
    end type procedure_signatures_t

    type :: signatures_map_t
        type(procedure_signatures_t), allocatable :: proc_sigs(:)
        integer :: proc_count = 0
        integer :: proc_capacity = 0
    contains
        procedure :: add_signature_method => signatures_map_add_signature
        procedure :: get_signatures => signatures_map_get_signatures
        procedure :: deep_copy => signatures_map_deep_copy
        procedure :: assign => signatures_map_assign
        generic :: assignment(=) => assign
    end type signatures_map_t

contains

    function create_signatures_map() result(map)
        type(signatures_map_t) :: map

        map%proc_capacity = 16
        allocate (map%proc_sigs(map%proc_capacity))
        map%proc_count = 0
    end function create_signatures_map

    function signature_exists(signatures, new_sig) result(exists)
        type(type_signature_t), intent(in) :: signatures(:)
        type(type_signature_t), intent(in) :: new_sig
        logical :: exists
        integer :: i, j
        integer :: n_params
        logical :: param_types_match
        logical :: return_type_match

        exists = .false.
        do i = 1, size(signatures)
            if (.not. allocated(signatures(i)%param_kinds)) cycle
            if (.not. allocated(new_sig%param_kinds)) cycle
            n_params = size(signatures(i)%param_kinds)
            if (n_params /= size(new_sig%param_kinds)) cycle

            if (.not. all(signatures(i)%param_kinds == new_sig%param_kinds)) cycle
            if (signatures(i)%return_kind /= new_sig%return_kind) cycle

            param_types_match = .true.
            if (allocated(signatures(i)%param_type_strings) .or. &
                allocated(new_sig%param_type_strings)) then
                if (.not. allocated(signatures(i)%param_type_strings)) cycle
                if (.not. allocated(new_sig%param_type_strings)) cycle
                if (size(signatures(i)%param_type_strings) /= &
                    size(new_sig%param_type_strings)) cycle
                do j = 1, size(signatures(i)%param_type_strings)
                    if (trim(signatures(i)%param_type_strings(j)) /= &
                        trim(new_sig%param_type_strings(j))) then
                        param_types_match = .false.
                        exit
                    end if
                end do
            end if
            if (.not. param_types_match) cycle

            return_type_match = .true.
            if (allocated(signatures(i)%return_type_string) .or. &
                allocated(new_sig%return_type_string)) then
                if (.not. allocated(signatures(i)%return_type_string)) cycle
                if (.not. allocated(new_sig%return_type_string)) cycle
                if (trim(signatures(i)%return_type_string) /= &
                    trim(new_sig%return_type_string)) cycle
            end if

            if (return_type_match) then
                exists = .true.
                return
            end if
        end do
    end function signature_exists

    subroutine add_signature(map, proc_name, param_kinds, return_kind, &
                             call_site_node, line, column, param_type_strings, &
                             return_type_string)
        type(signatures_map_t), intent(inout) :: map
        character(len=*), intent(in) :: proc_name
        integer, intent(in) :: param_kinds(:)
        integer, intent(in) :: return_kind
        integer, intent(in) :: call_site_node
        integer, intent(in) :: line, column
        character(len=*), intent(in), optional :: param_type_strings(:)
        character(len=*), intent(in), optional :: return_type_string

        type(type_signature_t) :: new_sig
        type(procedure_signatures_t), allocatable :: temp_procs(:)
        type(type_signature_t), allocatable :: temp_sigs(:)
        integer :: proc_idx, i
        integer :: max_len

        allocate (new_sig%param_kinds(size(param_kinds)))
        new_sig%param_kinds = param_kinds
        new_sig%return_kind = return_kind
        new_sig%call_site_node = call_site_node
        new_sig%line = line
        new_sig%column = column

        if (present(param_type_strings)) then
            max_len = 0
            do i = 1, size(param_type_strings)
                max_len = max(max_len, len_trim(param_type_strings(i)))
            end do
            if (max_len < 1) max_len = 1
            allocate (character(len=max_len) :: new_sig%param_type_strings( &
                      size(param_type_strings)))
            do i = 1, size(param_type_strings)
                new_sig%param_type_strings(i) = trim(param_type_strings(i))
            end do
        end if

        if (present(return_type_string)) then
            if (len_trim(return_type_string) > 0) then
                allocate (character(len=len_trim(return_type_string)) :: &
                          new_sig%return_type_string)
                new_sig%return_type_string = trim(return_type_string)
            end if
        end if

        proc_idx = 0
        do i = 1, map%proc_count
            if (map%proc_sigs(i)%procedure_name == proc_name) then
                proc_idx = i
                exit
            end if
        end do

        if (proc_idx == 0) then
            if (map%proc_count >= map%proc_capacity) then
                map%proc_capacity = max(map%proc_capacity + map%proc_capacity / 2, &
                                        map%proc_capacity + 16, 16)
                allocate (temp_procs(map%proc_capacity))
                if (map%proc_count > 0) then
                    do i = 1, map%proc_count
                        temp_procs(i) = map%proc_sigs(i)
                    end do
                end if
                call move_alloc(temp_procs, map%proc_sigs)
            end if

            map%proc_count = map%proc_count + 1
            proc_idx = map%proc_count
            map%proc_sigs(proc_idx)%procedure_name = proc_name
            map%proc_sigs(proc_idx)%sig_capacity = 4
            allocate (map%proc_sigs(proc_idx)%signatures( &
                      map%proc_sigs(proc_idx)%sig_capacity))
            map%proc_sigs(proc_idx)%sig_count = 0
        end if

        if (allocated(map%proc_sigs(proc_idx)%signatures)) then
            if (signature_exists(map%proc_sigs(proc_idx)%signatures( &
                                 1:map%proc_sigs(proc_idx)%sig_count), new_sig)) then
                return
            end if
        end if

        if (map%proc_sigs(proc_idx)%sig_count >= &
            map%proc_sigs(proc_idx)%sig_capacity) then
            map%proc_sigs(proc_idx)%sig_capacity = max( &
                                             map%proc_sigs(proc_idx)%sig_capacity * 2, &
                                            map%proc_sigs(proc_idx)%sig_capacity + 4, 4)
            allocate (temp_sigs(map%proc_sigs(proc_idx)%sig_capacity))
            if (map%proc_sigs(proc_idx)%sig_count > 0) then
                do i = 1, map%proc_sigs(proc_idx)%sig_count
                    temp_sigs(i) = map%proc_sigs(proc_idx)%signatures(i)
                end do
            end if
            call move_alloc(temp_sigs, map%proc_sigs(proc_idx)%signatures)
        end if

        map%proc_sigs(proc_idx)%sig_count = map%proc_sigs(proc_idx)%sig_count + 1
        map%proc_sigs(proc_idx)%signatures(map%proc_sigs(proc_idx)%sig_count) = new_sig
    end subroutine add_signature

    subroutine signatures_map_add_signature(this, proc_name, param_kinds, &
                                            return_kind, call_site_node, line, column, &
                                            param_type_strings, return_type_string)
        class(signatures_map_t), intent(inout) :: this
        character(len=*), intent(in) :: proc_name
        integer, intent(in) :: param_kinds(:)
        integer, intent(in) :: return_kind
        integer, intent(in) :: call_site_node
        integer, intent(in) :: line, column
        character(len=*), intent(in), optional :: param_type_strings(:)
        character(len=*), intent(in), optional :: return_type_string

        call add_signature(this, proc_name, param_kinds, return_kind, &
                           call_site_node, line, column, param_type_strings, &
                           return_type_string)
    end subroutine signatures_map_add_signature

    function get_unique_signatures(map, proc_name, unique_sigs) result(count)
        type(signatures_map_t), intent(in) :: map
        character(len=*), intent(in) :: proc_name
        type(type_signature_t), allocatable, intent(out) :: unique_sigs(:)
        integer :: count
        integer :: i, proc_idx

        count = 0
        proc_idx = 0

        do i = 1, map%proc_count
            if (map%proc_sigs(i)%procedure_name == proc_name) then
                proc_idx = i
                exit
            end if
        end do

        if (proc_idx == 0) then
            allocate (unique_sigs(0))
            return
        end if

        count = map%proc_sigs(proc_idx)%sig_count
        if (count > 0) then
            allocate (unique_sigs(count))
            do i = 1, count
                unique_sigs(i) = map%proc_sigs(proc_idx)%signatures(i)
            end do
        else
            allocate (unique_sigs(0))
        end if
    end function get_unique_signatures

    function signatures_map_get_signatures(this, proc_name, unique_sigs) &
        result(count)
        class(signatures_map_t), intent(in) :: this
        character(len=*), intent(in) :: proc_name
        type(type_signature_t), allocatable, intent(out) :: unique_sigs(:)
        integer :: count

        count = get_unique_signatures(this, proc_name, unique_sigs)
    end function signatures_map_get_signatures

    subroutine signatures_map_deep_copy(dst, src)
        class(signatures_map_t), intent(out) :: dst
        class(signatures_map_t), intent(in) :: src
        integer :: i, j, k
        integer :: max_len

        dst%proc_count = src%proc_count
        dst%proc_capacity = src%proc_capacity

        if (allocated(src%proc_sigs)) then
            allocate (dst%proc_sigs(dst%proc_capacity))
            do i = 1, dst%proc_count
                if (allocated(src%proc_sigs(i)%procedure_name)) then
                    dst%proc_sigs(i)%procedure_name = src%proc_sigs(i)%procedure_name
                end if
                dst%proc_sigs(i)%sig_count = src%proc_sigs(i)%sig_count
                dst%proc_sigs(i)%sig_capacity = src%proc_sigs(i)%sig_capacity

                if (allocated(src%proc_sigs(i)%signatures)) then
                    allocate (dst%proc_sigs(i)%signatures( &
                              dst%proc_sigs(i)%sig_capacity))
                    do j = 1, dst%proc_sigs(i)%sig_count
                        if (allocated(src%proc_sigs(i)%signatures(j)%param_kinds)) then
                            allocate (dst%proc_sigs(i)%signatures(j)%param_kinds( &
                                      size(src%proc_sigs(i)%signatures(j)%param_kinds)))
                            dst%proc_sigs(i)%signatures(j)%param_kinds = &
                                src%proc_sigs(i)%signatures(j)%param_kinds
                        end if
                        dst%proc_sigs(i)%signatures(j)%return_kind = &
                            src%proc_sigs(i)%signatures(j)%return_kind
                        dst%proc_sigs(i)%signatures(j)%call_site_node = &
                            src%proc_sigs(i)%signatures(j)%call_site_node
                        dst%proc_sigs(i)%signatures(j)%line = &
                            src%proc_sigs(i)%signatures(j)%line
                        dst%proc_sigs(i)%signatures(j)%column = &
                            src%proc_sigs(i)%signatures(j)%column
                  if (allocated(src%proc_sigs(i)%signatures(j)%param_type_strings)) then
                            max_len = 0
                       do k = 1, size(src%proc_sigs(i)%signatures(j)%param_type_strings)
                                max_len = max(max_len, len_trim( &
                                  src%proc_sigs(i)%signatures(j)%param_type_strings(k)))
                            end do
                            if (max_len < 1) max_len = 1
                            allocate (character(len=max_len) :: &
                                    dst%proc_sigs(i)%signatures(j)%param_type_strings( &
                               size(src%proc_sigs(i)%signatures(j)%param_type_strings)))
                       do k = 1, size(src%proc_sigs(i)%signatures(j)%param_type_strings)
                                dst%proc_sigs(i)%signatures(j)%param_type_strings(k) = &
                              trim(src%proc_sigs(i)%signatures(j)%param_type_strings(k))
                            end do
                        end if
                  if (allocated(src%proc_sigs(i)%signatures(j)%return_type_string)) then
                   max_len = len_trim(src%proc_sigs(i)%signatures(j)%return_type_string)
                            if (max_len < 1) max_len = 1
                            allocate (character(len=max_len) :: &
                                      dst%proc_sigs(i)%signatures(j)%return_type_string)
                            dst%proc_sigs(i)%signatures(j)%return_type_string = &
                                trim(src%proc_sigs(i)%signatures(j)%return_type_string)
                        end if
                    end do
                end if
            end do
        end if
    end subroutine signatures_map_deep_copy

    subroutine signatures_map_assign(dst, src)
        class(signatures_map_t), intent(out) :: dst
        class(signatures_map_t), intent(in) :: src

        call signatures_map_deep_copy(dst, src)
    end subroutine signatures_map_assign

end module call_graph_signatures_mod
