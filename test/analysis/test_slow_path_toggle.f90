program test_slow_path_toggle
    use frontend, only: transform_lazy_fortran_string
    use slow_path_config, only: reset_slow_path_config, set_slow_path_enabled
    use slow_path_analyzers, only: clear_slow_path_results, &
                                   get_slow_path_invocation_count, &
                                   fetch_call_graph_result
    use call_graph_module, only: call_graph_t
    implicit none

    integer :: failures
    character(len=:), allocatable :: output, error_msg
    logical :: available
    type(call_graph_t) :: graph

    failures = 0

    call reset_slow_path_config()
    call clear_slow_path_results()
    call transform_lazy_fortran_string('x = 1', output, error_msg)
    if (len_trim(error_msg) /= 0) failures = failures + 1
    if (get_slow_path_invocation_count() /= 0) failures = failures + 1
    call fetch_call_graph_result(graph, available)
    if (available) failures = failures + 1

    call set_slow_path_enabled(.true.)
    call clear_slow_path_results()
    call transform_lazy_fortran_string('y = x + 1', output, error_msg)
    if (len_trim(error_msg) /= 0) failures = failures + 1
    if (get_slow_path_invocation_count() /= 1) failures = failures + 1
    call fetch_call_graph_result(graph, available)
    if (.not. available) failures = failures + 1

    call set_slow_path_enabled(.false.)
    call clear_slow_path_results()
    call transform_lazy_fortran_string('z = y + 2', output, error_msg)
    if (len_trim(error_msg) /= 0) failures = failures + 1
    if (get_slow_path_invocation_count() /= 0) failures = failures + 1

    if (failures == 0) then
        stop 0
    else
        stop 1
    end if
end program test_slow_path_toggle
