program bench_arena_allocation
    ! Benchmark allocation performance: sequential, random, bulk allocations
    use ast_arena_modern
    use iso_fortran_env, only: real64, int64
    implicit none

    integer, parameter :: WARMUP_ITERATIONS = 10
    integer, parameter :: BENCH_ITERATIONS = 50  ! Reduced from 1000 to 50
    
    ! Test sizes - REDUCED FOR CI PERFORMANCE
    integer, parameter :: SMALL_SIZE = 1000
    integer, parameter :: MEDIUM_SIZE = 5000
    integer, parameter :: LARGE_SIZE = 10000    ! Reduced from 100K to 10K
    integer, parameter :: HUGE_SIZE = 25000     ! Reduced from 1M to 25K
    
    type :: benchmark_result_t
        character(len=64) :: test_name = ""
        integer :: node_count = 0
        real(real64) :: total_time = 0.0
        real(real64) :: ops_per_second = 0.0
        real(real64) :: time_per_op_us = 0.0
        integer(int64) :: memory_used = 0
    end type benchmark_result_t
    
    type(benchmark_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Allocation Performance Benchmark"
    print *, "======================================"
    print *
    
    allocate(results(100))
    
    ! Sequential allocation benchmarks
    call benchmark_sequential_allocation(SMALL_SIZE)
    call benchmark_sequential_allocation(MEDIUM_SIZE)
    call benchmark_sequential_allocation(LARGE_SIZE)
    call benchmark_sequential_allocation(HUGE_SIZE)
    
    ! Random pattern allocation
    call benchmark_random_allocation(SMALL_SIZE)
    call benchmark_random_allocation(MEDIUM_SIZE)
    call benchmark_random_allocation(LARGE_SIZE)
    
    ! Bulk allocation patterns
    call benchmark_bulk_allocation(100, 100)    ! 100 batches of 100
    call benchmark_bulk_allocation(100, 1000)   ! 100 batches of 1000
    call benchmark_bulk_allocation(1000, 100)   ! 1000 batches of 100
    call benchmark_bulk_allocation(10, 10000)   ! 10 batches of 10000
    
    ! Mixed allocation patterns
    call benchmark_mixed_allocation(MEDIUM_SIZE)
    
    ! Report results
    call print_results()
    
contains

    subroutine benchmark_sequential_allocation(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, iter
        type(ast_arena_stats_t) :: stats
        type(benchmark_result_t) :: result
        
        ! Warmup
        arena = create_ast_arena(size)
        do iter = 1, WARMUP_ITERATIONS
            do i = 1, min(size/10, 100)
                handle = store_ast_node(arena, node)
            end do
            call arena%reset()
        end do
        call destroy_ast_arena(arena)
        
        ! Actual benchmark
        arena = create_ast_arena(size)
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                node%node_kind = i
                node%string_data = "test_node"
                node%integer_data = i * 2
                handle = store_ast_node(arena, node)
            end do
            if (iter < BENCH_ITERATIONS) call arena%reset()
        end do
        
        call cpu_time(end_time)
        
        ! Record results
        stats = arena%get_stats()
        result%test_name = "Sequential Allocation"
        result%node_count = size
        result%total_time = end_time - start_time
        result%ops_per_second = real(size * BENCH_ITERATIONS, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(size * BENCH_ITERATIONS, real64)
        result%memory_used = stats%total_memory
        
        call add_result(result)
        call destroy_ast_arena(arena)
        
    end subroutine benchmark_sequential_allocation
    
    subroutine benchmark_random_allocation(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        real :: rand_val
        integer :: i, iter, idx
        type(ast_arena_stats_t) :: stats
        type(benchmark_result_t) :: result
        
        allocate(handles(size))
        
        ! Warmup with random access pattern
        arena = create_ast_arena(size)
        do iter = 1, WARMUP_ITERATIONS/10
            do i = 1, min(size/10, 100)
                handle = store_ast_node(arena, node)
            end do
            call arena%reset()
        end do
        call destroy_ast_arena(arena)
        
        ! Actual benchmark with random free/allocate pattern
        arena = create_ast_arena(size)
        
        ! Pre-allocate all nodes
        do i = 1, size
            node%node_kind = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Random free and reallocate pattern
            do i = 1, size/10
                call random_number(rand_val)
                idx = int(rand_val * size) + 1
                if (is_node_active(arena, handles(idx))) then
                    block
                        type(ast_free_result_t) :: free_result
                        free_result = arena%free_node(handles(idx))
                        ! Could check free_result%success if needed for accuracy
                    end block
                end if
                node%node_kind = idx
                node%integer_data = iter * 1000 + idx
                handles(idx) = store_ast_node(arena, node)
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Record results
        stats = arena%get_stats()
        result%test_name = "Random Allocation Pattern"
        result%node_count = size
        result%total_time = end_time - start_time
        result%ops_per_second = real((size/10) * BENCH_ITERATIONS, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real((size/10) * BENCH_ITERATIONS, real64)
        result%memory_used = stats%total_memory
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_random_allocation
    
    subroutine benchmark_bulk_allocation(batches, batch_size)
        integer, intent(in) :: batches, batch_size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, b, iter
        type(ast_arena_stats_t) :: stats
        type(benchmark_result_t) :: result
        character(len=64) :: test_name
        
        ! Warmup
        arena = create_ast_arena(batches * batch_size)
        do iter = 1, min(WARMUP_ITERATIONS/10, 10)
            do b = 1, min(batches, 10)
                do i = 1, min(batch_size, 100)
                    handle = store_ast_node(arena, node)
                end do
            end do
            call arena%reset()
        end do
        call destroy_ast_arena(arena)
        
        ! Actual benchmark
        arena = create_ast_arena(batches * batch_size)
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS/10  ! Fewer iterations for bulk
            do b = 1, batches
                ! Bulk allocate batch_size nodes
                do i = 1, batch_size
                    node%node_kind = b * 1000 + i
                    node%integer_data = b
                    handle = store_ast_node(arena, node)
                end do
            end do
            if (iter < BENCH_ITERATIONS/10) call arena%reset()
        end do
        
        call cpu_time(end_time)
        
        ! Record results
        stats = arena%get_stats()
        write(test_name, '(A,I0,A,I0,A)') "Bulk Allocation (", batches, "x", batch_size, ")"
        result%test_name = test_name
        result%node_count = batches * batch_size
        result%total_time = end_time - start_time
        result%ops_per_second = real(batches * batch_size * (BENCH_ITERATIONS/10), real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(batches * batch_size * (BENCH_ITERATIONS/10), real64)
        result%memory_used = stats%total_memory
        
        call add_result(result)
        call destroy_ast_arena(arena)
        
    end subroutine benchmark_bulk_allocation
    
    subroutine benchmark_mixed_allocation(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        real :: rand_val
        integer :: i, iter, op_count
        type(ast_arena_stats_t) :: stats
        type(benchmark_result_t) :: result
        
        allocate(handles(size))
        op_count = 0
        
        arena = create_ast_arena(size)
        
        ! Pre-fill arena with initial nodes
        do i = 1, size/2
            node%node_kind = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        call cpu_time(start_time)
        
        ! Mixed operations: allocate, free, reallocate
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size/20  ! 5% of nodes per iteration
                call random_number(rand_val)
                
                if (rand_val < 0.3) then
                    ! Free operation (30%)
                    if (i <= size/2 .and. is_node_active(arena, handles(i))) then
                        block
                            type(ast_free_result_t) :: free_result
                            free_result = arena%free_node(handles(i))
                            if (free_result%success) op_count = op_count + 1
                        end block
                    end if
                else if (rand_val < 0.7) then
                    ! Allocate new (40%)
                    node%node_kind = iter * 1000 + i
                    handle = store_ast_node(arena, node)
                    if (i + size/2 <= size) handles(i + size/2) = handle
                    op_count = op_count + 1
                else
                    ! Reallocate freed slot (30%)
                    if (i <= size/2 .and. .not. is_node_active(arena, handles(i))) then
                        node%node_kind = -i
                        handles(i) = store_ast_node(arena, node)
                        op_count = op_count + 1
                    end if
                end if
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Record results
        stats = arena%get_stats()
        result%test_name = "Mixed Allocation Pattern"
        result%node_count = size
        result%total_time = end_time - start_time
        result%ops_per_second = real(op_count, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(op_count, real64)
        result%memory_used = stats%total_memory
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_mixed_allocation
    
    subroutine add_result(result)
        type(benchmark_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, "Allocation Performance Results"
        print *, "=============================="
        print *
        print '(A20,A12,A15,A15,A15,A12)', &
            "Test", "Nodes", "Time (s)", "Ops/sec", "us/op", "Memory (MB)"
        print '(A20,A12,A15,A15,A15,A12)', &
            "----", "-----", "-------", "-------", "-----", "-----------"
        
        do i = 1, result_count
            print '(A20,I12,F15.6,E15.3,F15.3,F12.2)', &
                results(i)%test_name, &
                results(i)%node_count, &
                results(i)%total_time, &
                results(i)%ops_per_second, &
                results(i)%time_per_op_us, &
                real(results(i)%memory_used) / (1024.0 * 1024.0)
        end do
        
        print *
        print *, "Benchmark complete."
    end subroutine print_results

end program bench_arena_allocation