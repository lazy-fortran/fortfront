module call_graph_queries_mod
    use call_graph_core_mod, only: call_graph_t
    use call_graph_constants_mod, only: max_proc_name_len
    implicit none
    private

    public :: get_callers, get_callees
    public :: is_procedure_used
    public :: get_all_procedures, get_call_count

contains

    pure function simple_name_of(name) result(simple_name)
        character(len=*), intent(in) :: name
        character(len=max_proc_name_len) :: simple_name
        integer :: sep

        simple_name = trim(name)
        sep = index(simple_name, '::', back=.true.)
        if (sep > 0) simple_name = simple_name(sep + 2:)
        simple_name = trim(simple_name)
    end function simple_name_of

    function get_callers(graph, procedure_name) result(caller_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: caller_names(:)
        character(len=max_proc_name_len), allocatable :: temp_names(:)
        integer :: i, count
        character(len=max_proc_name_len) :: simple_callee

        if (.not. allocated(graph%calls)) then
            allocate (character(len=1) :: caller_names(0))
            return
        end if

        allocate (temp_names(graph%call_count))
        count = 0

        do i = 1, graph%call_count
            simple_callee = simple_name_of(graph%calls(i)%callee)

            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                block
                    character(len=max_proc_name_len) :: extracted_caller
                    extracted_caller = simple_name_of(graph%calls(i)%caller)

                    if (.not. any(temp_names(1:count) == trim(extracted_caller))) then
                        count = count + 1
                        temp_names(count) = trim(extracted_caller)
                    end if
                end block
            end if
        end do

        if (count > 0) then
            allocate (character(len=maxval(len_trim(temp_names(1:count)))) :: &
                      caller_names(count))
            do i = 1, count
                caller_names(i) = trim(temp_names(i))
            end do
        else
            allocate (character(len=1) :: caller_names(0))
        end if
    end function get_callers

    function get_callees(graph, procedure_name) result(callee_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: callee_names(:)
        character(len=max_proc_name_len), allocatable :: temp_names(:)
        integer :: i, count
        character(len=max_proc_name_len) :: simple_caller

        if (.not. allocated(graph%calls)) then
            allocate (character(len=1) :: callee_names(0))
            return
        end if

        allocate (temp_names(graph%call_count))
        count = 0

        do i = 1, graph%call_count
            simple_caller = simple_name_of(graph%calls(i)%caller)

            if (graph%calls(i)%caller == procedure_name .or. &
                trim(simple_caller) == procedure_name) then
                block
                    character(len=max_proc_name_len) :: extracted_callee
                    extracted_callee = simple_name_of(graph%calls(i)%callee)

                    if (.not. any(temp_names(1:count) == trim(extracted_callee))) then
                        count = count + 1
                        temp_names(count) = trim(extracted_callee)
                    end if
                end block
            end if
        end do

        if (count > 0) then
            allocate (character(len=maxval(len_trim(temp_names(1:count)))) :: &
                      callee_names(count))
            do i = 1, count
                callee_names(i) = trim(temp_names(i))
            end do
        else
            allocate (character(len=1) :: callee_names(0))
        end if
    end function get_callees

    function is_procedure_used(graph, procedure_name) result(is_used)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        logical :: is_used
        integer :: i
        character(len=max_proc_name_len) :: simple_name, simple_callee

        is_used = .false.

        if (allocated(graph%procedures)) then
            do i = 1, graph%proc_count
                simple_name = simple_name_of(graph%procedures(i)%name)

                if ((graph%procedures(i)%name == procedure_name .or. &
                     trim(simple_name) == procedure_name) .and. &
                    graph%procedures(i)%is_main_program) then
                    is_used = .true.
                    return
                end if
            end do
        end if

        if (.not. allocated(graph%calls)) return

        do i = 1, graph%call_count
            simple_callee = simple_name_of(graph%calls(i)%callee)

            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                is_used = .true.
                return
            end if
        end do
    end function is_procedure_used

    function get_all_procedures(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        integer :: i, max_len
        character(len=max_proc_name_len) :: simple_name

        if (graph%proc_count > 0 .and. allocated(graph%procedures)) then
            max_len = 0
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = simple_name_of(graph%procedures(i)%name)
                    max_len = max(max_len, len_trim(simple_name))
                end if
            end do

            allocate (character(len=max_len) :: proc_names(graph%proc_count))
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = simple_name_of(graph%procedures(i)%name)
                    proc_names(i) = trim(simple_name)
                else
                    proc_names(i) = ""
                end if
            end do
        else
            allocate (character(len=1) :: proc_names(0))
        end if
    end function get_all_procedures

    function get_call_count(graph) result(count)
        type(call_graph_t), intent(in) :: graph
        integer :: count

        count = graph%call_count
    end function get_call_count

end module call_graph_queries_mod
