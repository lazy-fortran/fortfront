module call_graph_constants_mod
    use fortfront_constants, only: FORTFRONT_MAX_PROC_NAME_LEN => &
                                   MAX_PROC_NAME_LEN, &
                                   FORTFRONT_INITIAL_SYMBOL_TABLE_CAPACITY => &
                                   INITIAL_SYMBOL_TABLE_CAPACITY
    implicit none
    private

    public :: max_proc_name_len
    public :: initial_symbol_table_capacity

    integer, parameter :: max_proc_name_len = FORTFRONT_MAX_PROC_NAME_LEN
    integer, parameter :: initial_symbol_table_capacity = &
        FORTFRONT_INITIAL_SYMBOL_TABLE_CAPACITY
end module call_graph_constants_mod
