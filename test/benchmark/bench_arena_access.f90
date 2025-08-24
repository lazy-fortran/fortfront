program bench_arena_access
    ! Benchmark access patterns: sequential, random, cache-miss patterns
    use ast_arena_modern
    use iso_fortran_env, only: real64, int64
    implicit none

    integer, parameter :: WARMUP_ITERATIONS = 100
    integer, parameter :: BENCH_ITERATIONS = 1000
    
    ! Test sizes
    integer, parameter :: SMALL_SIZE = 1000
    integer, parameter :: MEDIUM_SIZE = 10000
    integer, parameter :: LARGE_SIZE = 100000
    
    type :: benchmark_result_t
        character(len=64) :: test_name = ""
        integer :: node_count = 0
        integer :: access_count = 0
        real(real64) :: total_time = 0.0
        real(real64) :: ops_per_second = 0.0
        real(real64) :: time_per_op_ns = 0.0
        real(real64) :: cache_miss_ratio = 0.0
    end type benchmark_result_t
    
    type(benchmark_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Access Pattern Performance Benchmark"
    print *, "=========================================="
    print *
    
    allocate(results(100))
    
    ! Sequential access patterns
    call benchmark_sequential_access(SMALL_SIZE)
    call benchmark_sequential_access(MEDIUM_SIZE)
    call benchmark_sequential_access(LARGE_SIZE)
    
    ! Random access patterns
    call benchmark_random_access(SMALL_SIZE)
    call benchmark_random_access(MEDIUM_SIZE)
    call benchmark_random_access(LARGE_SIZE)
    
    ! Strided access patterns (cache miss simulation)
    call benchmark_strided_access(MEDIUM_SIZE, 1)    ! Sequential
    call benchmark_strided_access(MEDIUM_SIZE, 8)    ! Small stride
    call benchmark_strided_access(MEDIUM_SIZE, 64)   ! Cache line stride
    call benchmark_strided_access(MEDIUM_SIZE, 512)  ! Large stride
    
    ! Parent-child traversal patterns
    call benchmark_tree_traversal(MEDIUM_SIZE)
    
    ! Sibling traversal patterns
    call benchmark_sibling_traversal(MEDIUM_SIZE)
    
    ! Report results
    call print_results()
    
contains

    subroutine benchmark_sequential_access(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, sum
        type(benchmark_result_t) :: result
        
        allocate(handles(size))
        arena = create_ast_arena(size)
        
        ! Setup: Create nodes
        do i = 1, size
            node%node_kind = i
            node%integer_data = i * 2
            node%string_data = "test"
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Warmup
        sum = 0
        do iter = 1, WARMUP_ITERATIONS
            do i = 1, min(size, 100)
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        
        ! Actual benchmark
        sum = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Record results
        result%test_name = "Sequential Access"
        result%node_count = size
        result%access_count = size * BENCH_ITERATIONS
        result%total_time = end_time - start_time
        result%ops_per_second = real(result%access_count, real64) / result%total_time
        result%time_per_op_ns = (result%total_time * 1.0e9) / real(result%access_count, real64)
        result%cache_miss_ratio = 0.01  ! Sequential access has low cache misses
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_sequential_access
    
    subroutine benchmark_random_access(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        integer, allocatable :: random_indices(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        real :: rand_val
        integer :: i, iter, idx, sum
        type(benchmark_result_t) :: result
        
        allocate(handles(size))
        allocate(random_indices(size))
        arena = create_ast_arena(size)
        
        ! Setup: Create nodes
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Generate random access pattern
        do i = 1, size
            call random_number(rand_val)
            random_indices(i) = int(rand_val * size) + 1
        end do
        
        ! Warmup
        sum = 0
        do iter = 1, WARMUP_ITERATIONS/10
            do i = 1, min(size, 100)
                idx = random_indices(i)
                retrieved_node = get_ast_node(arena, handles(idx))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        
        ! Actual benchmark
        sum = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                idx = random_indices(mod(i + iter - 1, size) + 1)
                retrieved_node = get_ast_node(arena, handles(idx))
                sum = sum + retrieved_node%integer_data
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Record results
        result%test_name = "Random Access"
        result%node_count = size
        result%access_count = size * BENCH_ITERATIONS
        result%total_time = end_time - start_time
        result%ops_per_second = real(result%access_count, real64) / result%total_time
        result%time_per_op_ns = (result%total_time * 1.0e9) / real(result%access_count, real64)
        result%cache_miss_ratio = 0.8  ! Random access has high cache misses
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        deallocate(random_indices)
        
    end subroutine benchmark_random_access
    
    subroutine benchmark_strided_access(size, stride)
        integer, intent(in) :: size, stride
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, idx, sum, access_count
        type(benchmark_result_t) :: result
        character(len=64) :: test_name
        
        allocate(handles(size))
        arena = create_ast_arena(size)
        
        ! Setup: Create nodes
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Calculate actual access count
        access_count = (size - 1) / stride + 1
        
        ! Warmup
        sum = 0
        do iter = 1, WARMUP_ITERATIONS/10
            idx = 1
            do while (idx <= min(size, 100))
                retrieved_node = get_ast_node(arena, handles(idx))
                sum = sum + retrieved_node%integer_data
                idx = idx + stride
            end do
        end do
        
        ! Actual benchmark
        sum = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            idx = 1
            do while (idx <= size)
                retrieved_node = get_ast_node(arena, handles(idx))
                sum = sum + retrieved_node%integer_data
                idx = idx + stride
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Record results
        write(test_name, '(A,I0,A)') "Strided Access (stride=", stride, ")"
        result%test_name = test_name
        result%node_count = size
        result%access_count = access_count * BENCH_ITERATIONS
        result%total_time = end_time - start_time
        result%ops_per_second = real(result%access_count, real64) / result%total_time
        result%time_per_op_ns = (result%total_time * 1.0e9) / real(result%access_count, real64)
        
        ! Estimate cache miss ratio based on stride
        if (stride == 1) then
            result%cache_miss_ratio = 0.01
        else if (stride <= 8) then
            result%cache_miss_ratio = 0.1
        else if (stride <= 64) then
            result%cache_miss_ratio = 0.5
        else
            result%cache_miss_ratio = 0.9
        end if
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_strided_access
    
    subroutine benchmark_tree_traversal(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node, child_node
        real(real64) :: start_time, end_time
        integer :: i, iter, sum, traversals
        type(benchmark_result_t) :: result
        
        allocate(handles(size))
        arena = create_ast_arena(size)
        
        ! Setup: Create tree structure
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            ! Set up parent-child relationships
            if (i > 1) then
                node%parent_handle_id = handles(i/2)%node_id
                node%parent_handle_gen = handles(i/2)%generation
            end if
            if (i*2 <= size) then
                node%first_child_id = i*2
            end if
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Warmup
        sum = 0
        traversals = 0
        do iter = 1, WARMUP_ITERATIONS/10
            do i = 1, min(size/10, 100)
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
                ! Traverse to first child if exists
                if (retrieved_node%first_child_id > 0 .and. retrieved_node%first_child_id <= size) then
                    child_node = get_ast_node(arena, handles(retrieved_node%first_child_id))
                    sum = sum + child_node%integer_data
                end if
            end do
        end do
        
        ! Actual benchmark
        sum = 0
        traversals = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Depth-first traversal pattern
            do i = 1, size
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
                traversals = traversals + 1
                
                ! Access first child
                if (retrieved_node%first_child_id > 0 .and. retrieved_node%first_child_id <= size) then
                    child_node = get_ast_node(arena, handles(retrieved_node%first_child_id))
                    sum = sum + child_node%integer_data
                    traversals = traversals + 1
                end if
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Record results
        result%test_name = "Tree Traversal"
        result%node_count = size
        result%access_count = traversals
        result%total_time = end_time - start_time
        result%ops_per_second = real(traversals, real64) / result%total_time
        result%time_per_op_ns = (result%total_time * 1.0e9) / real(traversals, real64)
        result%cache_miss_ratio = 0.3  ! Tree traversal has moderate cache misses
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_tree_traversal
    
    subroutine benchmark_sibling_traversal(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, sum, traversals
        type(benchmark_result_t) :: result
        
        allocate(handles(size))
        arena = create_ast_arena(size)
        
        ! Setup: Create sibling chain
        do i = 1, size
            node%node_kind = i
            node%integer_data = i
            ! Set up sibling relationships
            if (i < size) then
                node%next_sibling_id = i + 1
                node%next_sibling_gen = 1
            end if
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Warmup
        sum = 0
        do iter = 1, WARMUP_ITERATIONS/10
            i = 1
            do while (i <= min(size, 100))
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
                i = retrieved_node%next_sibling_id
                if (i == 0) exit
            end do
        end do
        
        ! Actual benchmark
        sum = 0
        traversals = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Traverse sibling chain
            i = 1
            do while (i <= size .and. i > 0)
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
                traversals = traversals + 1
                i = retrieved_node%next_sibling_id
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Record results
        result%test_name = "Sibling Traversal"
        result%node_count = size
        result%access_count = traversals
        result%total_time = end_time - start_time
        result%ops_per_second = real(traversals, real64) / result%total_time
        result%time_per_op_ns = (result%total_time * 1.0e9) / real(traversals, real64)
        result%cache_miss_ratio = 0.05  ! Sequential sibling access has low cache misses
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_sibling_traversal
    
    subroutine add_result(result)
        type(benchmark_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, "Access Pattern Performance Results"
        print *, "==================================="
        print *
        print '(A25,A10,A12,A15,A12,A12,A10)', &
            "Test", "Nodes", "Accesses", "Time (s)", "Ops/sec", "ns/op", "Cache Miss"
        print '(A25,A10,A12,A15,A12,A12,A10)', &
            "----", "-----", "--------", "-------", "-------", "-----", "----------"
        
        do i = 1, result_count
            print '(A25,I10,I12,F15.6,E12.3,F12.2,F10.2)', &
                results(i)%test_name, &
                results(i)%node_count, &
                results(i)%access_count, &
                results(i)%total_time, &
                results(i)%ops_per_second, &
                results(i)%time_per_op_ns, &
                results(i)%cache_miss_ratio
        end do
        
        print *
        print *, "Benchmark complete."
    end subroutine print_results

end program bench_arena_access