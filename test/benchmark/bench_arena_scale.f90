program bench_arena_scale
    ! Benchmark scalability: 1K, 10K, 100K, 1M nodes
    use ast_arena_modern
    use iso_fortran_env, only: real64, int64
    implicit none

    integer, parameter :: ITERATIONS = 10
    
    ! Test scales
    integer, parameter :: SCALE_1K = 1000
    integer, parameter :: SCALE_10K = 10000
    integer, parameter :: SCALE_100K = 100000
    integer, parameter :: SCALE_1M = 1000000
    
    type :: scalability_result_t
        character(len=64) :: operation = ""
        integer :: scale = 0
        real(real64) :: time_total = 0.0
        real(real64) :: time_per_op = 0.0
        real(real64) :: ops_per_second = 0.0
        integer(int64) :: memory_used = 0
        real(real64) :: memory_per_node = 0.0
        real(real64) :: scaling_factor = 0.0
    end type scalability_result_t
    
    type(scalability_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Scalability Benchmark"
    print *, "==========================="
    print *
    
    allocate(results(200))
    
    ! Test different operations at each scale
    call benchmark_allocation_scaling()
    call benchmark_access_scaling()
    call benchmark_traversal_scaling()
    call benchmark_free_reuse_scaling()
    call benchmark_reset_scaling()
    
    ! Report results
    call print_results()
    call print_scaling_analysis()
    
contains

    subroutine benchmark_allocation_scaling()
        real(real64) :: time_1k, time_10k, time_100k, time_1m
        
        print *, "Testing allocation scalability..."
        
        time_1k = benchmark_allocation(SCALE_1K)
        time_10k = benchmark_allocation(SCALE_10K)
        time_100k = benchmark_allocation(SCALE_100K)
        time_1m = benchmark_allocation(SCALE_1M)
        
        call record_scaling("Allocation", SCALE_1K, time_1k, time_1k)
        call record_scaling("Allocation", SCALE_10K, time_10k, time_1k)
        call record_scaling("Allocation", SCALE_100K, time_100k, time_1k)
        call record_scaling("Allocation", SCALE_1M, time_1m, time_1k)
        
    end subroutine benchmark_allocation_scaling
    
    function benchmark_allocation(scale) result(total_time)
        integer, intent(in) :: scale
        real(real64) :: total_time
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, iter
        type(ast_arena_stats_t) :: stats
        type(scalability_result_t) :: result
        
        total_time = 0.0
        
        do iter = 1, ITERATIONS
            arena = create_ast_arena(scale)
            
            call cpu_time(start_time)
            
            do i = 1, scale
                node%node_kind = i
                node%integer_data = i * 2
                node%string_data = "scalability_test"
                handle = store_ast_node(arena, node)
            end do
            
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
            
            if (iter == ITERATIONS) then
                stats = arena%get_stats()
                result%operation = "Allocation"
                result%scale = scale
                result%time_total = total_time / ITERATIONS
                result%time_per_op = result%time_total / real(scale, real64)
                result%ops_per_second = real(scale, real64) / result%time_total
                result%memory_used = stats%total_memory
                result%memory_per_node = real(stats%total_memory, real64) / real(scale, real64)
                call add_result(result)
            end if
            
            call destroy_ast_arena(arena)
        end do
        
        total_time = total_time / ITERATIONS
        
    end function benchmark_allocation
    
    subroutine benchmark_access_scaling()
        real(real64) :: time_1k, time_10k, time_100k, time_1m
        
        print *, "Testing access scalability..."
        
        time_1k = benchmark_access(SCALE_1K)
        time_10k = benchmark_access(SCALE_10K)
        time_100k = benchmark_access(SCALE_100K)
        time_1m = benchmark_access(SCALE_1M)
        
        call record_scaling("Access", SCALE_1K, time_1k, time_1k)
        call record_scaling("Access", SCALE_10K, time_10k, time_1k)
        call record_scaling("Access", SCALE_100K, time_100k, time_1k)
        call record_scaling("Access", SCALE_1M, time_1m, time_1k)
        
    end subroutine benchmark_access_scaling
    
    function benchmark_access(scale) result(total_time)
        integer, intent(in) :: scale
        real(real64) :: total_time
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_node_arena_t) :: retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, sum
        type(scalability_result_t) :: result
        
        allocate(handles(scale))
        total_time = 0.0
        
        arena = create_ast_arena(scale)
        
        ! Setup nodes
        do i = 1, scale
            node%node_kind = i
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        do iter = 1, ITERATIONS
            sum = 0
            call cpu_time(start_time)
            
            do i = 1, scale
                retrieved_node = get_ast_node(arena, handles(i))
                sum = sum + retrieved_node%integer_data
            end do
            
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        result%operation = "Access"
        result%scale = scale
        result%time_total = total_time / ITERATIONS
        result%time_per_op = result%time_total / real(scale, real64)
        result%ops_per_second = real(scale, real64) / result%time_total
        call add_result(result)
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
        total_time = total_time / ITERATIONS
        
    end function benchmark_access
    
    subroutine benchmark_traversal_scaling()
        real(real64) :: time_1k, time_10k, time_100k
        
        print *, "Testing traversal scalability..."
        
        time_1k = benchmark_traversal(SCALE_1K)
        time_10k = benchmark_traversal(SCALE_10K)
        time_100k = benchmark_traversal(SCALE_100K)
        
        call record_scaling("Traversal", SCALE_1K, time_1k, time_1k)
        call record_scaling("Traversal", SCALE_10K, time_10k, time_1k)
        call record_scaling("Traversal", SCALE_100K, time_100k, time_1k)
        
    end subroutine benchmark_traversal_scaling
    
    function benchmark_traversal(scale) result(total_time)
        integer, intent(in) :: scale
        real(real64) :: total_time
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_node_arena_t) :: retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, current, sum
        type(scalability_result_t) :: result
        
        allocate(handles(scale))
        total_time = 0.0
        
        arena = create_ast_arena(scale)
        
        ! Setup linked list structure
        do i = 1, scale
            node%node_kind = i
            node%integer_data = i
            if (i < scale) then
                node%next_sibling_id = i + 1
                node%next_sibling_gen = 1
            else
                node%next_sibling_id = 0
            end if
            handles(i) = store_ast_node(arena, node)
        end do
        
        do iter = 1, ITERATIONS
            sum = 0
            call cpu_time(start_time)
            
            ! Traverse linked list
            current = 1
            do while (current > 0 .and. current <= scale)
                retrieved_node = get_ast_node(arena, handles(current))
                sum = sum + retrieved_node%integer_data
                current = retrieved_node%next_sibling_id
                if (current == 0) exit
            end do
            
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        result%operation = "Traversal"
        result%scale = scale
        result%time_total = total_time / ITERATIONS
        result%time_per_op = result%time_total / real(scale, real64)
        result%ops_per_second = real(scale, real64) / result%time_total
        call add_result(result)
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
        total_time = total_time / ITERATIONS
        
    end function benchmark_traversal
    
    subroutine benchmark_free_reuse_scaling()
        real(real64) :: time_1k, time_10k, time_100k
        
        print *, "Testing free/reuse scalability..."
        
        time_1k = benchmark_free_reuse(SCALE_1K)
        time_10k = benchmark_free_reuse(SCALE_10K)
        time_100k = benchmark_free_reuse(SCALE_100K)
        
        call record_scaling("Free/Reuse", SCALE_1K, time_1k, time_1k)
        call record_scaling("Free/Reuse", SCALE_10K, time_10k, time_1k)
        call record_scaling("Free/Reuse", SCALE_100K, time_100k, time_1k)
        
    end subroutine benchmark_free_reuse_scaling
    
    function benchmark_free_reuse(scale) result(total_time)
        integer, intent(in) :: scale
        real(real64) :: total_time
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, iter, ops
        type(scalability_result_t) :: result
        
        allocate(handles(scale))
        total_time = 0.0
        ops = 0
        
        arena = create_ast_arena(scale)
        
        ! Initial allocation
        do i = 1, scale
            node%node_kind = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        do iter = 1, ITERATIONS
            call cpu_time(start_time)
            
            ! Free half the nodes
            do i = 1, scale, 2
                if (is_node_active(arena, handles(i))) then
                    block
                        type(ast_free_result_t) :: free_result
                        free_result = arena%free_node(handles(i))
                        ! Could check free_result%success if needed
                    end block
                    ops = ops + 1
                end if
            end do
            
            ! Reallocate freed nodes
            do i = 1, scale, 2
                node%node_kind = -i
                handles(i) = store_ast_node(arena, node)
                ops = ops + 1
            end do
            
            call cpu_time(end_time)
            total_time = total_time + (end_time - start_time)
        end do
        
        result%operation = "Free/Reuse"
        result%scale = scale
        result%time_total = total_time / ITERATIONS
        result%time_per_op = result%time_total / real(ops/ITERATIONS, real64)
        result%ops_per_second = real(ops/ITERATIONS, real64) / result%time_total
        call add_result(result)
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
        total_time = total_time / ITERATIONS
        
    end function benchmark_free_reuse
    
    subroutine benchmark_reset_scaling()
        real(real64) :: time_1k, time_10k, time_100k, time_1m
        
        print *, "Testing reset scalability..."
        
        time_1k = benchmark_reset(SCALE_1K)
        time_10k = benchmark_reset(SCALE_10K)
        time_100k = benchmark_reset(SCALE_100K)
        time_1m = benchmark_reset(SCALE_1M)
        
        call record_scaling("Reset", SCALE_1K, time_1k, time_1k)
        call record_scaling("Reset", SCALE_10K, time_10k, time_1k)
        call record_scaling("Reset", SCALE_100K, time_100k, time_1k)
        call record_scaling("Reset", SCALE_1M, time_1m, time_1k)
        
    end subroutine benchmark_reset_scaling
    
    function benchmark_reset(scale) result(total_time)
        integer, intent(in) :: scale
        real(real64) :: total_time
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time, reset_time
        integer :: i, iter
        type(scalability_result_t) :: result
        
        total_time = 0.0
        reset_time = 0.0
        
        arena = create_ast_arena(scale)
        
        do iter = 1, ITERATIONS * 10  ! More iterations for reset
            ! Allocate nodes
            do i = 1, scale
                node%node_kind = i
                handle = store_ast_node(arena, node)
            end do
            
            ! Time the reset operation
            call cpu_time(start_time)
            call arena%reset()
            call cpu_time(end_time)
            
            reset_time = reset_time + (end_time - start_time)
        end do
        
        total_time = reset_time / (ITERATIONS * 10)
        
        result%operation = "Reset"
        result%scale = scale
        result%time_total = total_time
        result%time_per_op = total_time  ! Reset is single operation
        result%ops_per_second = 1.0 / total_time
        call add_result(result)
        
        call destroy_ast_arena(arena)
        
    end function benchmark_reset
    
    subroutine record_scaling(operation, scale, time, baseline_time)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: scale
        real(real64), intent(in) :: time, baseline_time
        type(scalability_result_t) :: result
        
        result%operation = operation
        result%scale = scale
        result%time_total = time
        result%scaling_factor = time / baseline_time
        
        ! Don't duplicate if already added in benchmark function
        ! This is for scaling factor recording only
    end subroutine record_scaling
    
    subroutine add_result(result)
        type(scalability_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, "Scalability Test Results"
        print *, "========================"
        print *
        print '(A15,A12,A15,A15,A15,A12)', &
            "Operation", "Scale", "Time (s)", "Time/Op (us)", "Ops/sec", "Memory MB"
        print '(A15,A12,A15,A15,A15,A12)', &
            "---------", "-----", "-------", "------------", "-------", "---------"
        
        do i = 1, result_count
            print '(A15,I12,F15.6,F15.3,E15.3,F12.2)', &
                results(i)%operation, &
                results(i)%scale, &
                results(i)%time_total, &
                results(i)%time_per_op * 1.0e6, &
                results(i)%ops_per_second, &
                real(results(i)%memory_used) / (1024.0 * 1024.0)
        end do
        
    end subroutine print_results
    
    subroutine print_scaling_analysis()
        integer :: i, j
        character(len=15) :: current_op
        real(real64) :: base_time, scale_factor
        logical :: first
        
        print *
        print *, "Scaling Analysis (Time Complexity)"
        print *, "=================================="
        print *
        print '(A15,A12,A15,A20)', "Operation", "Scale", "Scaling Factor", "Complexity"
        print '(A15,A12,A15,A20)', "---------", "-----", "--------------", "----------"
        
        current_op = ""
        base_time = 0.0
        first = .true.
        
        do i = 1, result_count
            if (results(i)%operation /= current_op) then
                current_op = results(i)%operation
                base_time = results(i)%time_total
                first = .true.
            end if
            
            if (.not. first) then
                scale_factor = results(i)%time_total / base_time
                print '(A15,I12,F15.2,A20)', &
                    results(i)%operation, &
                    results(i)%scale, &
                    scale_factor, &
                    analyze_complexity(results(i)%scale/results(1)%scale, scale_factor)
            else
                print '(A15,I12,F15.2,A20)', &
                    results(i)%operation, &
                    results(i)%scale, &
                    1.0, &
                    "Baseline"
                first = .false.
            end if
        end do
        
        print *
        print *, "Scalability benchmark complete."
        
    end subroutine print_scaling_analysis
    
    function analyze_complexity(size_factor, time_factor) result(complexity)
        integer, intent(in) :: size_factor
        real(real64), intent(in) :: time_factor
        character(len=20) :: complexity
        real(real64) :: ratio
        
        ratio = time_factor / real(size_factor, real64)
        
        if (abs(time_factor - 1.0) < 0.5) then
            complexity = "O(1) - Constant"
        else if (abs(ratio - 1.0) < 0.2) then
            complexity = "O(n) - Linear"
        else if (abs(time_factor - size_factor * log(real(size_factor))/log(2.0)) < size_factor) then
            complexity = "O(n log n)"
        else if (abs(time_factor - size_factor * size_factor) < size_factor) then
            complexity = "O(n²) - Quadratic"
        else
            complexity = "Complex"
        end if
        
    end function analyze_complexity

end program bench_arena_scale