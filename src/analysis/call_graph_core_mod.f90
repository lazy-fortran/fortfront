module call_graph_core_mod
    implicit none
    private

    public :: procedure_info_t, call_edge_t, call_graph_t
    public :: create_call_graph, add_procedure, add_call
    public :: call_graph_deep_copy, call_graph_assign

    type :: procedure_info_t
        character(len=:), allocatable :: name
        integer :: definition_node
        integer :: line
        integer :: column
        logical :: is_main_program
        logical :: is_intrinsic
        logical :: is_external
    end type procedure_info_t

    type :: call_edge_t
        character(len=:), allocatable :: caller
        character(len=:), allocatable :: callee
        integer :: call_site_node
        integer :: line
        integer :: column
    end type call_edge_t

    type :: call_graph_t
        type(procedure_info_t), allocatable :: procedures(:)
        type(call_edge_t), allocatable :: calls(:)
        integer :: proc_count = 0
        integer :: call_count = 0
        integer :: proc_capacity = 0
        integer :: call_capacity = 0
    contains
        procedure :: add_proc => graph_add_procedure
        procedure :: add_call_edge => graph_add_call
        procedure :: find_proc_index => graph_find_procedure_index
        procedure :: deep_copy => call_graph_deep_copy
        procedure :: assign => call_graph_assign
        generic :: assignment(=) => assign
    end type call_graph_t

contains

    function create_call_graph() result(graph)
        type(call_graph_t) :: graph

        graph%proc_capacity = 16
        graph%call_capacity = 16
        allocate (graph%procedures(graph%proc_capacity))
        allocate (graph%calls(graph%call_capacity))
        graph%proc_count = 0
        graph%call_count = 0
    end function create_call_graph

    subroutine add_procedure(graph, name, def_node, line, column, is_main, &
            is_intrinsic, is_external)
        type(call_graph_t), intent(inout) :: graph
        character(len=*), intent(in) :: name
        integer, intent(in) :: def_node
        integer, intent(in) :: line, column
        logical, intent(in), optional :: is_main
        logical, intent(in), optional :: is_intrinsic
        logical, intent(in), optional :: is_external

        type(procedure_info_t) :: new_proc
        type(procedure_info_t), allocatable :: temp_procs(:)
        integer :: i

        do i = 1, graph%proc_count
            if (graph%procedures(i)%name == name) then
                if (present(is_main)) graph%procedures(i)%is_main_program = is_main
                if (present(is_intrinsic)) graph%procedures(i)%is_intrinsic = &
                    is_intrinsic
                if (present(is_external)) graph%procedures(i)%is_external = is_external
                return
            end if
        end do

        new_proc%name = name
        new_proc%definition_node = def_node
        new_proc%line = line
        new_proc%column = column
        new_proc%is_main_program = .false.
        new_proc%is_intrinsic = .false.
        new_proc%is_external = .false.
        if (present(is_main)) new_proc%is_main_program = is_main
        if (present(is_intrinsic)) new_proc%is_intrinsic = is_intrinsic
        if (present(is_external)) new_proc%is_external = is_external

        if (graph%proc_count >= graph%proc_capacity) then
            graph%proc_capacity = max(graph%proc_capacity + graph%proc_capacity / 2, &
                graph%proc_capacity + 16, 16)
            allocate (temp_procs(graph%proc_capacity))
            if (graph%proc_count > 0) then
                temp_procs(1:graph%proc_count) = graph%procedures
            end if
            call move_alloc(temp_procs, graph%procedures)
        end if

        graph%proc_count = graph%proc_count + 1
        graph%procedures(graph%proc_count) = new_proc
    end subroutine add_procedure

    subroutine add_call(graph, caller_name, callee_name, call_node, line, column)
        type(call_graph_t), intent(inout) :: graph
        character(len=*), intent(in) :: caller_name
        character(len=*), intent(in) :: callee_name
        integer, intent(in) :: call_node
        integer, intent(in) :: line, column

        type(call_edge_t) :: new_call
        type(call_edge_t), allocatable :: temp_calls(:)

        new_call%caller = caller_name
        new_call%callee = callee_name
        new_call%call_site_node = call_node
        new_call%line = line
        new_call%column = column

        if (graph%call_count >= graph%call_capacity) then
            graph%call_capacity = max(graph%call_capacity + graph%call_capacity / 2, &
                graph%call_capacity + 16, 16)
            allocate (temp_calls(graph%call_capacity))
            if (graph%call_count > 0) then
                temp_calls(1:graph%call_count) = graph%calls
            end if
            call move_alloc(temp_calls, graph%calls)
        end if

        graph%call_count = graph%call_count + 1
        graph%calls(graph%call_count) = new_call
    end subroutine add_call

    subroutine graph_add_procedure(this, name, def_node, line, column, &
            is_main, is_intrinsic, is_external)
        class(call_graph_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: def_node
        integer, intent(in) :: line, column
        logical, intent(in), optional :: is_main, is_intrinsic, is_external

        call add_procedure(this, name, def_node, line, column, &
            is_main, is_intrinsic, is_external)
    end subroutine graph_add_procedure

    subroutine graph_add_call(this, caller_name, callee_name, call_node, &
            line, column)
        class(call_graph_t), intent(inout) :: this
        character(len=*), intent(in) :: caller_name
        character(len=*), intent(in) :: callee_name
        integer, intent(in) :: call_node
        integer, intent(in) :: line, column

        call add_call(this, caller_name, callee_name, call_node, line, column)
    end subroutine graph_add_call

    function graph_find_procedure_index(this, name) result(index)
        class(call_graph_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: index
        integer :: i

        index = 0
        do i = 1, this%proc_count
            if (this%procedures(i)%name == name) then
                index = i
                return
            end if
        end do
    end function graph_find_procedure_index

    subroutine call_graph_deep_copy(dst, src)
        class(call_graph_t), intent(out) :: dst
        class(call_graph_t), intent(in) :: src
        integer :: i

        dst%proc_count = src%proc_count
        dst%call_count = src%call_count
        dst%proc_capacity = src%proc_capacity
        dst%call_capacity = src%call_capacity

        if (allocated(src%procedures)) then
            allocate (dst%procedures(dst%proc_capacity))
            do i = 1, dst%proc_count
                if (allocated(src%procedures(i)%name)) then
                    dst%procedures(i)%name = src%procedures(i)%name
                end if
                dst%procedures(i)%definition_node = src%procedures(i)%definition_node
                dst%procedures(i)%line = src%procedures(i)%line
                dst%procedures(i)%column = src%procedures(i)%column
                dst%procedures(i)%is_main_program = src%procedures(i)%is_main_program
                dst%procedures(i)%is_intrinsic = src%procedures(i)%is_intrinsic
                dst%procedures(i)%is_external = src%procedures(i)%is_external
            end do
        end if

        if (allocated(src%calls)) then
            allocate (dst%calls(dst%call_capacity))
            do i = 1, dst%call_count
                if (allocated(src%calls(i)%caller)) then
                    dst%calls(i)%caller = src%calls(i)%caller
                end if
                if (allocated(src%calls(i)%callee)) then
                    dst%calls(i)%callee = src%calls(i)%callee
                end if
                dst%calls(i)%call_site_node = src%calls(i)%call_site_node
                dst%calls(i)%line = src%calls(i)%line
                dst%calls(i)%column = src%calls(i)%column
            end do
        end if
    end subroutine call_graph_deep_copy

    subroutine call_graph_assign(dst, src)
        class(call_graph_t), intent(out) :: dst
        class(call_graph_t), intent(in) :: src

        call call_graph_deep_copy(dst, src)
    end subroutine call_graph_assign

end module call_graph_core_mod
