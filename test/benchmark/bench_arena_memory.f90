program bench_arena_memory
    ! Benchmark memory usage: track memory usage, fragmentation, overhead
    use ast_arena_modern
    use iso_fortran_env, only: real64, int64
    implicit none

    ! Test sizes - REDUCED FOR CI PERFORMANCE
    integer, parameter :: SMALL_SIZE = 1000
    integer, parameter :: MEDIUM_SIZE = 5000
    integer, parameter :: LARGE_SIZE = 10000     ! Reduced from 100K to 10K
    integer, parameter :: HUGE_SIZE = 25000      ! Reduced from 1M to 25K
    
    type :: memory_result_t
        character(len=64) :: test_name = ""
        integer :: node_count = 0
        integer(int64) :: total_memory = 0
        integer(int64) :: used_memory = 0
        integer(int64) :: overhead_memory = 0
        real(real64) :: utilization = 0.0
        real(real64) :: fragmentation = 0.0
        real(real64) :: bytes_per_node = 0.0
        integer :: free_slots = 0
        integer :: active_nodes = 0
    end type memory_result_t
    
    type(memory_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Memory Usage Benchmark"
    print *, "============================="
    print *
    
    allocate(results(100))
    
    ! Memory usage for different sizes
    call benchmark_memory_usage(SMALL_SIZE)
    call benchmark_memory_usage(MEDIUM_SIZE)
    call benchmark_memory_usage(LARGE_SIZE)
    call benchmark_memory_usage(HUGE_SIZE)
    
    ! Fragmentation analysis
    call benchmark_fragmentation(MEDIUM_SIZE, 10)    ! 10% fragmentation
    call benchmark_fragmentation(MEDIUM_SIZE, 25)    ! 25% fragmentation
    call benchmark_fragmentation(MEDIUM_SIZE, 50)    ! 50% fragmentation
    call benchmark_fragmentation(MEDIUM_SIZE, 75)    ! 75% fragmentation
    
    ! Memory overhead analysis
    call benchmark_overhead(SMALL_SIZE)
    call benchmark_overhead(MEDIUM_SIZE)
    call benchmark_overhead(LARGE_SIZE)
    
    ! Growth pattern analysis
    call benchmark_growth_pattern(100, 1000)    ! Start small, grow by 1000
    call benchmark_growth_pattern(1000, 10000)  ! Start medium, grow by 10000
    
    ! Report results
    call print_results()
    
contains

    subroutine benchmark_memory_usage(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        type(memory_result_t) :: result
        integer :: i
        integer(int64) :: node_size, expected_memory
        
        arena = create_ast_arena(size)
        
        ! Allocate all nodes
        do i = 1, size
            node%node_kind = i
            node%string_data = "benchmark_node"
            node%integer_data = i * 2
            node%source_line = i
            node%source_column = mod(i, 80)
            handle = store_ast_node(arena, node)
        end do
        
        ! Get memory statistics
        stats = arena%get_stats()
        
        ! Calculate memory metrics
        node_size = storage_size(node) / 8  ! Size in bytes
        expected_memory = node_size * size
        
        result%test_name = "Memory Usage"
        result%node_count = size
        result%total_memory = stats%total_memory
        result%used_memory = expected_memory
        result%overhead_memory = stats%total_memory - expected_memory
        result%utilization = stats%utilization
        result%fragmentation = 0.0  ! No fragmentation in sequential allocation
        result%bytes_per_node = real(stats%total_memory, real64) / real(size, real64)
        result%active_nodes = stats%node_count
        result%free_slots = 0
        
        call add_result(result)
        call destroy_ast_arena(arena)
        
    end subroutine benchmark_memory_usage
    
    subroutine benchmark_fragmentation(size, fragmentation_percent)
        integer, intent(in) :: size, fragmentation_percent
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        type(ast_arena_stats_t) :: free_stats
        type(memory_result_t) :: result
        character(len=64) :: test_name
        integer :: i, nodes_to_free, freed_count
        real :: rand_val
        logical, allocatable :: is_freed(:)
        
        allocate(handles(size))
        allocate(is_freed(size))
        is_freed = .false.
        
        arena = create_ast_arena(size)
        
        ! Allocate all nodes
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Free random nodes to create fragmentation
        nodes_to_free = (size * fragmentation_percent) / 100
        freed_count = 0
        
        do while (freed_count < nodes_to_free)
            call random_number(rand_val)
            i = int(rand_val * size) + 1
            
            if (.not. is_freed(i)) then
                block
                    type(ast_free_result_t) :: free_result
                    free_result = arena%free_node(handles(i))
                    ! Could check free_result%success if needed
                end block
                is_freed(i) = .true.
                freed_count = freed_count + 1
            end if
        end do
        
        ! Re-allocate some nodes to measure fragmentation impact
        do i = 1, size
            if (is_freed(i)) then
                node%node_kind = -i  ! Negative to indicate reallocation
                handles(i) = store_ast_node(arena, node)
                is_freed(i) = .false.
                exit  ! Just reallocate one to measure
            end if
        end do
        
        ! Get memory statistics
        stats = arena%get_stats()
        free_stats = arena%get_free_stats()
        
        write(test_name, '(A,I0,A)') "Fragmentation (", fragmentation_percent, "%)"
        result%test_name = test_name
        result%node_count = size
        result%total_memory = stats%total_memory
        result%used_memory = stats%total_memory * int(stats%utilization, int64)
        result%overhead_memory = stats%total_memory - result%used_memory
        result%utilization = stats%utilization
        result%fragmentation = real(fragmentation_percent, real64) / 100.0
        result%bytes_per_node = real(stats%total_memory, real64) / real(size - freed_count + 1, real64)
        result%active_nodes = free_stats%active_nodes
        result%free_slots = free_stats%freed_nodes  ! Use freed_nodes as free slots
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        deallocate(is_freed)
        
    end subroutine benchmark_fragmentation
    
    subroutine benchmark_overhead(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats_empty, stats_full
        type(memory_result_t) :: result
        integer :: i
        integer(int64) :: actual_data_size, total_overhead
        
        allocate(handles(size))
        
        ! Measure empty arena overhead
        arena = create_ast_arena(size)
        stats_empty = arena%get_stats()
        
        ! Fill arena
        do i = 1, size
            node%node_kind = i
            node%string_data = "test"
            handles(i) = store_ast_node(arena, node)
        end do
        
        stats_full = arena%get_stats()
        
        ! Calculate overhead
        actual_data_size = (storage_size(node) / 8) * size
        total_overhead = stats_full%total_memory - actual_data_size
        
        result%test_name = "Memory Overhead"
        result%node_count = size
        result%total_memory = stats_full%total_memory
        result%used_memory = actual_data_size
        result%overhead_memory = total_overhead
        result%utilization = real(actual_data_size, real64) / real(stats_full%total_memory, real64)
        result%fragmentation = 0.0
        result%bytes_per_node = real(stats_full%total_memory, real64) / real(size, real64)
        result%active_nodes = size
        result%free_slots = 0
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_overhead
    
    subroutine benchmark_growth_pattern(initial_size, growth_increment)
        integer, intent(in) :: initial_size, growth_increment
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        type(memory_result_t) :: result
        character(len=64) :: test_name
        integer :: i, total_nodes
        integer(int64) :: memory_before, memory_after
        real(real64) :: growth_factor
        
        arena = create_ast_arena(initial_size)
        
        ! Initial allocation
        do i = 1, initial_size
            node%node_kind = i
            handle = store_ast_node(arena, node)
        end do
        
        stats = arena%get_stats()
        memory_before = stats%total_memory
        
        ! Grow the arena
        do i = initial_size + 1, initial_size + growth_increment
            node%node_kind = i
            handle = store_ast_node(arena, node)
        end do
        
        stats = arena%get_stats()
        memory_after = stats%total_memory
        total_nodes = initial_size + growth_increment
        
        ! Calculate growth metrics
        if (memory_before > 0) then
            growth_factor = real(memory_after, real64) / real(memory_before, real64)
        else
            growth_factor = 0.0
        end if
        
        write(test_name, '(A,I0,A,I0,A)') "Growth (", initial_size, " +", growth_increment, ")"
        result%test_name = test_name
        result%node_count = total_nodes
        result%total_memory = memory_after
        result%used_memory = (storage_size(node) / 8) * total_nodes
        result%overhead_memory = memory_after - result%used_memory
        result%utilization = stats%utilization
        result%fragmentation = 1.0 - stats%utilization  ! Inverse of utilization
        result%bytes_per_node = real(memory_after, real64) / real(total_nodes, real64)
        result%active_nodes = total_nodes
        result%free_slots = 0
        
        call add_result(result)
        call destroy_ast_arena(arena)
        
    end subroutine benchmark_growth_pattern
    
    subroutine add_result(result)
        type(memory_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, "Memory Usage Analysis Results"
        print *, "============================="
        print *
        print '(A20,A10,A12,A12,A12,A10,A10,A12,A10,A10)', &
            "Test", "Nodes", "Total MB", "Used MB", "Overhead MB", "Util %", "Frag %", "Bytes/Node", "Active", "Free"
        print '(A20,A10,A12,A12,A12,A10,A10,A12,A10,A10)', &
            "----", "-----", "--------", "-------", "-----------", "------", "------", "----------", "------", "----"
        
        do i = 1, result_count
            print '(A20,I10,F12.3,F12.3,F12.3,F10.1,F10.1,F12.1,I10,I10)', &
                results(i)%test_name, &
                results(i)%node_count, &
                real(results(i)%total_memory) / (1024.0 * 1024.0), &
                real(results(i)%used_memory) / (1024.0 * 1024.0), &
                real(results(i)%overhead_memory) / (1024.0 * 1024.0), &
                results(i)%utilization * 100.0, &
                results(i)%fragmentation * 100.0, &
                results(i)%bytes_per_node, &
                results(i)%active_nodes, &
                results(i)%free_slots
        end do
        
        print *
        print *, "Memory analysis complete."
    end subroutine print_results

end program bench_arena_memory