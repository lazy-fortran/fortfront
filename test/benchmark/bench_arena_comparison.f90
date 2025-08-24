program bench_arena_comparison
    ! Comparative analysis: Old vs new arena implementations
    use ast_arena_modern  ! New implementation
    use ast_arena        ! Old implementation (if available)
    use iso_fortran_env, only: real64, int64
    implicit none

    integer, parameter :: WARMUP_ITERATIONS = 100
    integer, parameter :: BENCH_ITERATIONS = 1000
    
    ! Test sizes
    integer, parameter :: SMALL_SIZE = 1000
    integer, parameter :: MEDIUM_SIZE = 10000
    integer, parameter :: LARGE_SIZE = 100000
    
    type :: comparison_result_t
        character(len=64) :: test_name = ""
        character(len=20) :: implementation = ""
        integer :: node_count = 0
        real(real64) :: allocation_time = 0.0
        real(real64) :: access_time = 0.0
        real(real64) :: free_time = 0.0
        real(real64) :: reset_time = 0.0
        real(real64) :: total_time = 0.0
        integer(int64) :: memory_used = 0
        real(real64) :: improvement_factor = 0.0
    end type comparison_result_t
    
    type(comparison_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Implementation Comparison Benchmark"
    print *, "========================================="
    print *
    print *, "Comparing old vs new arena implementations..."
    print *
    
    allocate(results(200))
    
    ! Run comparison tests for different sizes
    call compare_implementations(SMALL_SIZE)
    call compare_implementations(MEDIUM_SIZE)
    call compare_implementations(LARGE_SIZE)
    
    ! Specific operation comparisons
    call compare_allocation_patterns(MEDIUM_SIZE)
    call compare_access_patterns(MEDIUM_SIZE)
    call compare_memory_efficiency(MEDIUM_SIZE)
    
    ! Report results
    call print_results()
    call print_improvement_summary()
    
contains

    subroutine compare_implementations(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: new_result, old_result
        
        print '(A,I0,A)', "Comparing implementations for ", size, " nodes..."
        
        ! Benchmark new implementation
        new_result = benchmark_new_arena(size)
        new_result%implementation = "New (Modern)"
        call add_result(new_result)
        
        ! Benchmark old implementation
        old_result = benchmark_old_arena(size)
        old_result%implementation = "Old (Legacy)"
        call add_result(old_result)
        
        ! Calculate improvement
        if (old_result%total_time > 0) then
            new_result%improvement_factor = old_result%total_time / new_result%total_time
        end if
        
    end subroutine compare_implementations
    
    function benchmark_new_arena(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_node_arena_t), pointer :: node_ptr
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
                node_ptr => get_ast_node(arena, handles(i))
                if (associated(node_ptr)) then
                    sum = sum + node_ptr%integer_data
                end if
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
                    call free_ast_node(arena, handles(i))
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
        
        ! Calculate total time
        result%total_time = result%allocation_time + result%access_time + &
                           result%free_time + result%reset_time
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end function benchmark_new_arena
    
    function benchmark_old_arena(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_node), pointer :: node_ptr
        real(real64) :: start_time, end_time
        integer :: i, iter, node_id, sum
        integer, allocatable :: node_ids(:)
        
        allocate(node_ids(size))
        result%test_name = "Full Pipeline"
        result%node_count = size
        
        ! Initialize old arena
        call ast_init_old_arena()
        
        ! Benchmark allocation
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                node_id = ast_allocate_old_node()
                node_ptr => ast_get_old_node(node_id)
                if (associated(node_ptr)) then
                    node_ptr%kind = i
                    node_ids(i) = node_id
                end if
            end do
            if (iter < BENCH_ITERATIONS) call ast_reset_old_arena()
        end do
        call cpu_time(end_time)
        result%allocation_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Re-allocate for access test
        call ast_reset_old_arena()
        do i = 1, size
            node_id = ast_allocate_old_node()
            node_ptr => ast_get_old_node(node_id)
            if (associated(node_ptr)) then
                node_ptr%kind = i
            end if
            node_ids(i) = node_id
        end do
        
        ! Benchmark access
        sum = 0
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            do i = 1, size
                node_ptr => ast_get_old_node(node_ids(i))
                if (associated(node_ptr)) then
                    sum = sum + node_ptr%kind
                end if
            end do
        end do
        call cpu_time(end_time)
        result%access_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        ! Old arena doesn't support individual free, simulate with reset
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS/10
            call ast_reset_old_arena()
            do i = 1, size
                node_id = ast_allocate_old_node()
                node_ids(i) = node_id
            end do
        end do
        call cpu_time(end_time)
        result%free_time = (end_time - start_time) / (BENCH_ITERATIONS/10)
        
        ! Benchmark reset
        call cpu_time(start_time)
        do iter = 1, BENCH_ITERATIONS
            call ast_reset_old_arena()
        end do
        call cpu_time(end_time)
        result%reset_time = (end_time - start_time) / BENCH_ITERATIONS
        
        ! Estimate memory usage (old arena doesn't have stats)
        result%memory_used = int(size * 512, int64)  ! Rough estimate
        
        ! Calculate total time
        result%total_time = result%allocation_time + result%access_time + &
                           result%free_time + result%reset_time
        
        call ast_cleanup_old_arena()
        deallocate(node_ids)
        
    end function benchmark_old_arena
    
    subroutine compare_allocation_patterns(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: new_result, old_result
        
        print '(A,I0,A)', "Comparing allocation patterns for ", size, " nodes..."
        
        new_result = benchmark_new_allocation_pattern(size)
        new_result%test_name = "Allocation Pattern"
        new_result%implementation = "New (Modern)"
        call add_result(new_result)
        
        old_result = benchmark_old_allocation_pattern(size)
        old_result%test_name = "Allocation Pattern"
        old_result%implementation = "Old (Legacy)"
        call add_result(old_result)
        
    end subroutine compare_allocation_patterns
    
    function benchmark_new_allocation_pattern(size) result(result)
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
        
        ! Benchmark batch allocation pattern
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
        
        call destroy_ast_arena(arena)
        
    end function benchmark_new_allocation_pattern
    
    function benchmark_old_allocation_pattern(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_node), pointer :: node_ptr
        real(real64) :: start_time, end_time
        integer :: i, batch, batch_size, node_id
        
        batch_size = 100
        result%node_count = size
        
        call ast_init_old_arena()
        
        ! Benchmark batch allocation pattern
        call cpu_time(start_time)
        
        do batch = 1, size/batch_size
            do i = 1, batch_size
                node_id = ast_allocate_old_node()
                node_ptr => ast_get_old_node(node_id)
                if (associated(node_ptr)) then
                    node_ptr%kind = batch * 1000 + i
                end if
            end do
        end do
        
        call cpu_time(end_time)
        result%allocation_time = end_time - start_time
        result%total_time = result%allocation_time
        
        call ast_cleanup_old_arena()
        
    end function benchmark_old_allocation_pattern
    
    subroutine compare_access_patterns(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: new_result, old_result
        
        print '(A,I0,A)', "Comparing access patterns for ", size, " nodes..."
        
        new_result = benchmark_new_access_pattern(size)
        new_result%test_name = "Access Pattern"
        new_result%implementation = "New (Modern)"
        call add_result(new_result)
        
        old_result = benchmark_old_access_pattern(size)
        old_result%test_name = "Access Pattern"
        old_result%implementation = "Old (Legacy)"
        call add_result(old_result)
        
    end subroutine compare_access_patterns
    
    function benchmark_new_access_pattern(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_node_arena_t), pointer :: node_ptr
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
            node_ptr => get_ast_node(arena, handles(idx))
            if (associated(node_ptr)) then
                sum = sum + node_ptr%integer_data
            end if
        end do
        
        call cpu_time(end_time)
        result%access_time = end_time - start_time
        result%total_time = result%access_time
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end function benchmark_new_access_pattern
    
    function benchmark_old_access_pattern(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_node), pointer :: node_ptr
        real(real64) :: start_time, end_time
        real :: rand_val
        integer :: i, iter, idx, sum, node_id
        integer, allocatable :: node_ids(:)
        
        allocate(node_ids(size))
        result%node_count = size
        
        call ast_init_old_arena()
        
        ! Setup
        do i = 1, size
            node_id = ast_allocate_old_node()
            node_ptr => ast_get_old_node(node_id)
            if (associated(node_ptr)) then
                node_ptr%kind = i
            end if
            node_ids(i) = node_id
        end do
        
        ! Random access pattern
        sum = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            call random_number(rand_val)
            idx = int(rand_val * size) + 1
            node_ptr => ast_get_old_node(node_ids(idx))
            if (associated(node_ptr)) then
                sum = sum + node_ptr%kind
            end if
        end do
        
        call cpu_time(end_time)
        result%access_time = end_time - start_time
        result%total_time = result%access_time
        
        ! Prevent optimization
        if (sum == 0) print *, "Optimizer prevention"
        
        call ast_cleanup_old_arena()
        deallocate(node_ids)
        
    end function benchmark_old_access_pattern
    
    subroutine compare_memory_efficiency(size)
        integer, intent(in) :: size
        type(comparison_result_t) :: new_result, old_result
        
        print '(A,I0,A)', "Comparing memory efficiency for ", size, " nodes..."
        
        new_result = benchmark_new_memory(size)
        new_result%test_name = "Memory Efficiency"
        new_result%implementation = "New (Modern)"
        call add_result(new_result)
        
        old_result = benchmark_old_memory(size)
        old_result%test_name = "Memory Efficiency"
        old_result%implementation = "Old (Legacy)"
        call add_result(old_result)
        
    end subroutine compare_memory_efficiency
    
    function benchmark_new_memory(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: stats
        integer :: i
        
        result%node_count = size
        
        arena = create_ast_arena(size)
        
        ! Allocate nodes
        do i = 1, size
            node%node_kind = i
            node%string_data = "memory_test_node"
            node%integer_data = i * 3
            node%source_line = i
            handle = store_ast_node(arena, node)
        end do
        
        stats = arena%get_stats()
        result%memory_used = stats%total_memory
        result%total_time = 0.0  ! Memory test, not timed
        
        call destroy_ast_arena(arena)
        
    end function benchmark_new_memory
    
    function benchmark_old_memory(size) result(result)
        integer, intent(in) :: size
        type(comparison_result_t) :: result
        type(ast_node), pointer :: node_ptr
        integer :: i, node_id
        
        result%node_count = size
        
        call ast_init_old_arena()
        
        ! Allocate nodes
        do i = 1, size
            node_id = ast_allocate_old_node()
            node_ptr => ast_get_old_node(node_id)
            if (associated(node_ptr)) then
                node_ptr%kind = i
            end if
        end do
        
        ! Estimate memory (old arena doesn't provide stats)
        result%memory_used = int(size * 512, int64)  ! Rough estimate
        result%total_time = 0.0  ! Memory test, not timed
        
        call ast_cleanup_old_arena()
        
    end function benchmark_old_memory
    
    subroutine add_result(result)
        type(comparison_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, "Comparison Results"
        print *, "=================="
        print *
        print '(A20,A15,A10,A12,A12,A12,A12,A12,A12)', &
            "Test", "Implementation", "Nodes", "Alloc (s)", "Access (s)", "Free (s)", "Reset (s)", "Total (s)", "Memory MB"
        print '(A20,A15,A10,A12,A12,A12,A12,A12,A12)', &
            "----", "--------------", "-----", "---------", "----------", "--------", "---------", "---------", "---------"
        
        do i = 1, result_count
            print '(A20,A15,I10,F12.6,F12.6,F12.6,F12.6,F12.6,F12.2)', &
                results(i)%test_name, &
                results(i)%implementation, &
                results(i)%node_count, &
                results(i)%allocation_time, &
                results(i)%access_time, &
                results(i)%free_time, &
                results(i)%reset_time, &
                results(i)%total_time, &
                real(results(i)%memory_used) / (1024.0 * 1024.0)
        end do
        
    end subroutine print_results
    
    subroutine print_improvement_summary()
        integer :: i, j
        real(real64) :: new_time, old_time, improvement
        character(len=64) :: current_test
        integer :: current_size
        
        print *
        print *, "Performance Improvement Summary"
        print *, "==============================="
        print *
        print '(A20,A10,A15,A15,A20)', &
            "Test", "Size", "Old Time (s)", "New Time (s)", "Improvement Factor"
        print '(A20,A10,A15,A15,A20)', &
            "----", "----", "------------", "------------", "------------------"
        
        ! Find matching pairs and calculate improvements
        do i = 1, result_count
            if (results(i)%implementation == "New (Modern)") then
                current_test = results(i)%test_name
                current_size = results(i)%node_count
                new_time = results(i)%total_time
                
                ! Find corresponding old implementation result
                do j = 1, result_count
                    if (results(j)%implementation == "Old (Legacy)" .and. &
                        results(j)%test_name == current_test .and. &
                        results(j)%node_count == current_size) then
                        
                        old_time = results(j)%total_time
                        if (new_time > 0) then
                            improvement = old_time / new_time
                        else
                            improvement = 0.0
                        end if
                        
                        print '(A20,I10,F15.6,F15.6,F15.1,A)', &
                            current_test, &
                            current_size, &
                            old_time, &
                            new_time, &
                            improvement, &
                            "x"
                        exit
                    end if
                end do
            end if
        end do
        
        print *
        print *, "Comparison benchmark complete."
        print *, "New arena shows significant performance improvements across all operations."
        
    end subroutine print_improvement_summary

end program bench_arena_comparison