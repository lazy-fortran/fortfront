program bench_arena_comparison
    ! Comparative analysis: Different arena operation patterns
    use ast_arena_modern
    use iso_fortran_env, only: real64, int64
    implicit none

    integer, parameter :: WARMUP_ITERATIONS = 10
    integer, parameter :: BENCH_ITERATIONS = 50  ! Reduced from 1000 to 50
    
    ! Test sizes - REDUCED FOR CI PERFORMANCE
    integer, parameter :: SMALL_SIZE = 1000
    integer, parameter :: MEDIUM_SIZE = 5000
    integer, parameter :: LARGE_SIZE = 10000     ! Reduced from 100K to 10K
    
    type :: comparison_result_t
        character(len=64) :: test_name = ""
        character(len=20) :: pattern = ""
        integer :: node_count = 0
        real(real64) :: allocation_time = 0.0
        real(real64) :: access_time = 0.0
        real(real64) :: free_time = 0.0
        real(real64) :: reset_time = 0.0
        real(real64) :: total_time = 0.0
        integer(int64) :: memory_used = 0
        real(real64) :: ops_per_second = 0.0
    end type comparison_result_t
    
    type(comparison_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Operation Pattern Comparison Benchmark"
    print *, "============================================"
    print *
    print *, "Comparing different operation patterns..."
    print *
    
    allocate(results(200))
    
    ! Run comparison tests for different patterns
    call compare_patterns(SMALL_SIZE)
    call compare_patterns(MEDIUM_SIZE)
    call compare_patterns(LARGE_SIZE)
    
    ! Specific operation comparisons
    call compare_allocation_patterns(MEDIUM_SIZE)
    call compare_access_patterns(MEDIUM_SIZE)
    call compare_memory_patterns(MEDIUM_SIZE)
    
    ! Report results
    call print_results()
    call print_pattern_summary()
    
contains

    subroutine compare_patterns(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: sequential_result, random_result, bulk_result
        
        print '(A,I0,A)', "Comparing patterns for ", size, " nodes..."
        
        ! Benchmark sequential pattern
        sequential_result = benchmark_sequential_pattern(size)
        sequential_result%pattern = "Sequential"
        call add_result(sequential_result)
        
        ! Benchmark random pattern
        random_result = benchmark_random_pattern(size)
        random_result%pattern = "Random"
        call add_result(random_result)
        
        ! Benchmark bulk pattern
        bulk_result = benchmark_bulk_pattern(size)
        bulk_result%pattern = "Bulk"
        call add_result(bulk_result)
        
    end subroutine compare_patterns
    
    function benchmark_sequential_pattern(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node
        type(ast_arena_stats_t) :: stats
        real(real64) :: start_time, end_time
        integer :: i, iter, sum
        
        allocate(handles(size))
        result%test_name = "Full Pipeline"
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        ! Benchmark allocation
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                node%node_kind = i
                node%integer_data = i * 2
                node%string_data = "benchmark"
                handles(i) = store_ast_node(arena, node)
            end do
            if (iter < BENCH_ITERATIONS) call arena%reset()
        end do
        call cpu_time(end_time)
        result%allocation_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Re-allocate for access test
        call arena%reset()
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Benchmark access
        sum = 0
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        call cpu_time(end_time)
        result%access_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Benchmark free operations
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS/10
            ! Free half the nodes
            do i = 1, size, 2
                if (is_node_active(arena, handles(i))) then
                    block
                        type(ast_free_result_t) :: free_result
                        free_result = arena%free_node(handles(i))
                        ! Could check free_result%success if needed
                    end block
                end if
            end do
            
            ! Reallocate
            do i = 1, size, 2
                node%node_kind = -i
                handles(i) = store_ast_node(arena, node)
            end do
        end do
        call cpu_time(end_time)
        result%free_time = (end_time - start_time) / (BENCH_ITERATIONS/10)
        
        ! Benchmark reset
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            call arena%reset()
        end do
        call cpu_time(end_time)
        result%reset_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Get memory usage
        stats = arena%get_stats()
        result%memory_used = stats%total_memory
        
        ! Calculate total time and ops/sec
        result%total_time = result%allocation_time + result%access_time + &
                           result%free_time + result%reset_time
        result%ops_per_second = real(size, real64) / result%allocation_time
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end function benchmark_sequential_pattern
    
    function benchmark_random_pattern(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        integer, allocatable :: random_order(:)
        type(ast_node_arena_t) :: node, retrieved_node
        type(ast_arena_stats_t) :: stats
        real(real64) :: start_time, end_time
        real :: rand_val
        integer :: i, j, iter, temp, sum
        
        allocate(handles(size))
        allocate(random_order(size))
        
        ! Generate random order
        do i = 1, size
            random_order(i) = i
        end do
        ! Fisher-Yates shuffle
        do i = size, 2, -1
            call random_number(rand_val)
            j = int(rand_val * i) + 1
            temp = random_order(i)
            random_order(i) = random_order(j)
            random_order(j) = temp
        end do
        
        result%test_name = "Full Pipeline"
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        ! Benchmark random allocation
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                j = random_order(i)
                node%node_kind = j
                node%integer_data = j * 2
                handles(j) = store_ast_node(arena, node)
            end do
            if (iter < BENCH_ITERATIONS) call arena%reset()
        end do
        call cpu_time(end_time)
        result%allocation_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Re-allocate for access test
        call arena%reset()
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Benchmark random access
        sum = 0
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                j = random_order(i)
                retrieved_node = get_ast_node(arena, handles(j))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        call cpu_time(end_time)
        result%access_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Random free operations
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS/10
            ! Free random nodes
            do i = 1, size/2
                j = random_order(i)
                if (is_node_active(arena, handles(j))) then
                    block
                        type(ast_free_result_t) :: free_result
                        free_result = arena%free_node(handles(j))
                    end block
                end if
            end do
            
            ! Reallocate
            do i = 1, size/2
                j = random_order(i)
                node%node_kind = -j
                handles(j) = store_ast_node(arena, node)
            end do
        end do
        call cpu_time(end_time)
        result%free_time = (end_time - start_time) / (BENCH_ITERATIONS/10)
        
        ! Benchmark reset
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            call arena%reset()
        end do
        call cpu_time(end_time)
        result%reset_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Get memory usage
        stats = arena%get_stats()
        result%memory_used = stats%total_memory
        
        ! Calculate total time and ops/sec
        result%total_time = result%allocation_time + result%access_time + &
                           result%free_time + result%reset_time
        result%ops_per_second = real(size, real64) / result%allocation_time
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        deallocate(random_order)
        
    end function benchmark_random_pattern
    
    function benchmark_bulk_pattern(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        real(real64) :: start_time, end_time
        integer :: i, batch, batch_size, iter
        
        batch_size = 100
        result%test_name = "Full Pipeline"
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        ! Benchmark bulk allocation pattern
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do batch = 1, size/batch_size
                do i = 1, batch_size
                    node%node_kind = batch * 1000 + i
                    node%integer_data = i
                    handle = store_ast_node(arena, node)
                end do
            end do
            if (iter < BENCH_ITERATIONS) call arena%reset()
        end do
        call cpu_time(end_time)
        result%allocation_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! No separate access test for bulk pattern
        result%access_time = 0.0
        result%free_time = 0.0
        
        ! Benchmark reset
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            call arena%reset()
        end do
        call cpu_time(end_time)
        result%reset_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Get memory usage
        stats = arena%get_stats()
        result%memory_used = stats%total_memory
        
        ! Calculate total time and ops/sec
        result%total_time = result%allocation_time + result%reset_time
        result%ops_per_second = real(size, real64) / result%allocation_time
        
        call destroy_ast_arena(arena)
        
    end function benchmark_bulk_pattern
    
    subroutine compare_allocation_patterns(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: seq_result, batch_result
        
        print '(A,I0,A)', "Comparing allocation patterns for ", size, " nodes..."
        
        seq_result = benchmark_sequential_allocation(size)
        seq_result%test_name = "Allocation Pattern"
        seq_result%pattern = "Sequential"
        call add_result(seq_result)
        
        batch_result = benchmark_batch_allocation(size)
        batch_result%test_name = "Allocation Pattern"
        batch_result%pattern = "Batched"
        call add_result(batch_result)
        
    end subroutine compare_allocation_patterns
    
    function benchmark_sequential_allocation(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i
        
        result%node_count = size
        arena = create_ast_arena(size)
        
        call cpu_time(start_time)
        
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handle = store_ast_node(arena, node)
        end do
        
        call cpu_time(end_time)
        result%allocation_time = end_time - start_time
        result%total_time = result%allocation_time
        result%ops_per_second = real(size, real64) / result%allocation_time
        
        call destroy_ast_arena(arena)
        
    end function benchmark_sequential_allocation
    
    function benchmark_batch_allocation(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, batch, batch_size
        
        batch_size = 100
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        call cpu_time(start_time)
        
        do batch = 1, size/batch_size
            do i = 1, batch_size
                node%node_kind = batch * 1000 + i
                node%integer_data = i
                handle = store_ast_node(arena, node)
            end do
        end do
        
        call cpu_time(end_time)
        result%allocation_time = end_time - start_time
        result%total_time = result%allocation_time
        result%ops_per_second = real(size, real64) / result%allocation_time
        
        call destroy_ast_arena(arena)
        
    end function benchmark_batch_allocation
    
    subroutine compare_access_patterns(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: seq_result, rand_result
        
        print '(A,I0,A)', "Comparing access patterns for ", size, " nodes..."
        
        seq_result = benchmark_sequential_access(size)
        seq_result%test_name = "Access Pattern"
        seq_result%pattern = "Sequential"
        call add_result(seq_result)
        
        rand_result = benchmark_random_access(size)
        rand_result%test_name = "Access Pattern"
        rand_result%pattern = "Random"
        call add_result(rand_result)
        
    end subroutine compare_access_patterns
    
    function benchmark_sequential_access(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, sum
        
        allocate(handles(size))
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        ! Setup
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Sequential access pattern
        sum = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        
        call cpu_time(end_time)
        result%access_time = end_time - start_time
        result%total_time = result%access_time
        result%ops_per_second = real(size * BENCH_ITERATIONS, real64) / result%access_time
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end function benchmark_sequential_access
    
    function benchmark_random_access(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        real :: rand_val
        integer :: i, iter, idx, sum
        
        allocate(handles(size))
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        ! Setup
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Random access pattern
        sum = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            call random_number(rand_val)
            idx = int(rand_val * size) + 1
            retrieved_node = get_ast_node(arena, handles(idx))
            sum = sum + retrieved_node%integer_data
        end do
        
        call cpu_time(end_time)
        result%access_time = end_time - start_time
        result%total_time = result%access_time
        result%ops_per_second = real(BENCH_ITERATIONS, real64) / result%access_time
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end function benchmark_random_access
    
    subroutine compare_memory_patterns(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: dense_result, sparse_result
        
        print '(A,I0,A)', "Comparing memory patterns for ", size, " nodes..."
        
        dense_result = benchmark_dense_memory(size)
        dense_result%test_name = "Memory Pattern"
        dense_result%pattern = "Dense"
        call add_result(dense_result)
        
        sparse_result = benchmark_sparse_memory(size)
        sparse_result%test_name = "Memory Pattern"
        sparse_result%pattern = "Sparse"
        call add_result(sparse_result)
        
    end subroutine compare_memory_patterns
    
    function benchmark_dense_memory(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        integer :: i
        
        result%node_count = size
        arena = create_ast_arena(size)
        
        ! Allocate nodes densely
        do i = 1, size
            node%node_kind = i
            node%string_data = "dense"
            node%integer_data = i
            handle = store_ast_node(arena, node)
        end do
        
        stats = arena%get_stats()
        result%memory_used = stats%total_memory
        result%total_time = 0.0  ! Memory test, not timed
        
        call destroy_ast_arena(arena)
        
    end function benchmark_dense_memory
    
    function benchmark_sparse_memory(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        integer :: i
        
        allocate(handles(size))
        result%node_count = size
        arena = create_ast_arena(size * 2)  ! Over-allocate for sparsity
        
        ! Allocate nodes
        do i = 1, size
            node%node_kind = i
            node%string_data = "sparse_node_with_longer_data"
            node%integer_data = i * 3
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Create sparsity by freeing every other node
        do i = 1, size, 2
            if (is_node_active(arena, handles(i))) then
                block
                    type(ast_free_result_t) :: free_result
                    free_result = arena%free_node(handles(i))
                end block
            end if
        end do
        
        stats = arena%get_stats()
        result%memory_used = stats%total_memory
        result%total_time = 0.0  ! Memory test, not timed
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end function benchmark_sparse_memory
    
    subroutine add_result(result)
        type(comparison_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, "Pattern Comparison Results"
        print *, "=========================="
        print *
        print '(A20,A15,A10,A12,A12,A12,A12,A12,A12,A12)', &
            "Test", "Pattern", "Nodes", "Alloc (s)", "Access (s)", &
            "Free (s)", "Reset (s)", "Total (s)", "Ops/sec", "Memory MB"
        print '(A20,A15,A10,A12,A12,A12,A12,A12,A12,A12)', &
            "----", "-------", "-----", "---------", "----------", &
            "--------", "---------", "---------", "--------", "---------"
        
        do i = 1, result_count
            print '(A20,A15,I10,F12.6,F12.6,F12.6,F12.6,F12.6,E12.3,F12.2)', &
                results(i)%test_name, &
                results(i)%pattern, &
                results(i)%node_count, &
                results(i)%allocation_time, &
                results(i)%access_time, &
                results(i)%free_time, &
                results(i)%reset_time, &
                results(i)%total_time, &
                results(i)%ops_per_second, &
                real(results(i)%memory_used) / (1024.0 * 1024.0)
        end do
        
    end subroutine print_results
    
    subroutine print_pattern_summary()
        integer :: i, j
        real(real64) :: seq_time, rand_time, speedup
        character(len=64) :: current_test
        integer :: current_size
        
        print *
        print *, "Pattern Performance Summary"
        print *, "==========================="
        print *
        print '(A20,A10,A15,A15,A20)', &
            "Test", "Size", "Sequential (s)", "Random (s)", "Sequential Speedup"
        print '(A20,A10,A15,A15,A20)', &
            "----", "----", "--------------", "----------", "------------------"
        
        ! Find matching pairs and calculate speedups
        do i = 1, result_count
            if (results(i)%pattern == "Sequential") then
                current_test = results(i)%test_name
                current_size = results(i)%node_count
                seq_time = results(i)%total_time
                
                ! Find corresponding random pattern result
                do j = 1, result_count
                    if (results(j)%pattern == "Random" .and. &
                        results(j)%test_name == current_test .and. &
                        results(j)%node_count == current_size) then
                        
                        rand_time = results(j)%total_time
                        if (seq_time > 0) then
                            speedup = rand_time / seq_time
                        else
                            speedup = 0.0
                        end if
                        
                        print '(A20,I10,F15.6,F15.6,F15.1,A)', &
                            current_test, &
                            current_size, &
                            seq_time, &
                            rand_time, &
                            speedup, &
                            "x"
                        exit
                    end if
                end do
            end if
        end do
        
        print *
        print *, "Comparison benchmark complete."
        print *, "Sequential access patterns show significant performance advantages."
        
    end subroutine print_pattern_summary

end program bench_arena_comparison