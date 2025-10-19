module call_graph_constants_mod
    implicit none
    private

    public :: max_proc_name_len
    public :: initial_symbol_table_capacity

    integer, parameter :: max_proc_name_len = 256
    integer, parameter :: initial_symbol_table_capacity = 256
end module call_graph_constants_mod
