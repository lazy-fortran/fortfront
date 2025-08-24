program bench_arena_operations
    ! Benchmark operations: free/reuse, checkpoint/rollback, cross-references
    use ast_arena_modern
    use arena_memory, only: arena_checkpoint_t
    use iso_fortran_env, only: real64, int64
    implicit none

    integer, parameter :: WARMUP_ITERATIONS = 100
    integer, parameter :: BENCH_ITERATIONS = 1000
    
    ! Test sizes
    integer, parameter :: SMALL_SIZE = 1000
    integer, parameter :: MEDIUM_SIZE = 10000
    integer, parameter :: LARGE_SIZE = 100000
    
    type :: operation_result_t
        character(len=64) :: test_name = ""
        integer :: node_count = 0
        integer :: operation_count = 0
        real(real64) :: total_time = 0.0
        real(real64) :: ops_per_second = 0.0
        real(real64) :: time_per_op_us = 0.0
        integer :: successful_ops = 0
        integer :: failed_ops = 0
    end type operation_result_t
    
    type(operation_result_t), allocatable :: results(:)
    integer :: result_count = 0
    
    ! Run all benchmarks
    print *, "Arena Operations Performance Benchmark"
    print *, "======================================"
    print *
    
    allocate(results(100))
    
    ! Free and reuse operations
    call benchmark_free_operations(SMALL_SIZE)
    call benchmark_free_operations(MEDIUM_SIZE)
    call benchmark_free_operations(LARGE_SIZE)
    
    ! Checkpoint and rollback
    call benchmark_checkpoint_rollback(SMALL_SIZE)
    call benchmark_checkpoint_rollback(MEDIUM_SIZE)
    call benchmark_checkpoint_rollback(LARGE_SIZE)
    
    ! Validation operations
    call benchmark_validation(MEDIUM_SIZE)
    
    ! Cross-arena references
    call benchmark_cross_references(MEDIUM_SIZE)
    
    ! Generation tracking
    call benchmark_generation_tracking(MEDIUM_SIZE)
    
    ! Reset performance
    call benchmark_reset_performance(SMALL_SIZE)
    call benchmark_reset_performance(MEDIUM_SIZE)
    call benchmark_reset_performance(LARGE_SIZE)
    
    ! Report results
    call print_results()
    
contains

    subroutine benchmark_free_operations(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_node_arena_t) :: node
        type(ast_arena_stats_t) :: free_stats
        real(real64) :: start_time, end_time
        integer :: i, iter, ops
        type(operation_result_t) :: result
        
        allocate(handles(size))
        
        arena = create_ast_arena(size)
        
        ! Initial allocation
        do i = 1, size
            node%node_kind = i
            node%integer_data = i * 2
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Warmup
        do iter = 1, WARMUP_ITERATIONS/10
            do i = 1, min(size/10, 100), 2
                if (is_node_active(arena, handles(i))) then
                    block
                        type(ast_free_result_t) :: free_result
                        free_result = arena%free_node(handles(i))
                        ! Could check free_result%success if needed
                    end block
                end if
                node%node_kind = -i
                handles(i) = store_ast_node(arena, node)
            end do
        end do
        
        ! Actual benchmark
        ops = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Free every other node
            do i = 1, size, 2
                if (is_node_active(arena, handles(i))) then
                    block
                        type(ast_free_result_t) :: free_result
                        free_result = arena%free_node(handles(i))
                        ! Could check free_result%success if needed
                    end block
                    ops = ops + 1
                end if
            end do
            
            ! Reallocate freed slots
            do i = 1, size, 2
                node%node_kind = iter * 1000 + i
                node%integer_data = i
                handles(i) = store_ast_node(arena, node)
                ops = ops + 1
            end do
        end do
        
        call cpu_time(end_time)
        
        ! Get final statistics
        free_stats = arena%get_free_stats()
        
        result%test_name = "Free/Reuse Operations"
        result%node_count = size
        result%operation_count = ops
        result%total_time = end_time - start_time
        result%ops_per_second = real(ops, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(ops, real64)
        result%successful_ops = ops
        result%failed_ops = 0
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_free_operations
    
    subroutine benchmark_checkpoint_rollback(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        type(arena_checkpoint_t) :: checkpoint
        real(real64) :: start_time, end_time
        integer :: i, iter, ops
        type(operation_result_t) :: result
        
        arena = create_ast_arena(size)
        
        ! Initial state
        do i = 1, size/2
            node%node_kind = i
            handle = store_ast_node(arena, node)
        end do
        
        ! Warmup
        do iter = 1, WARMUP_ITERATIONS/10
            checkpoint = arena%checkpoint()
            do i = 1, min(size/10, 100)
                node%node_kind = size + i
                handle = store_ast_node(arena, node)
            end do
            call arena%rollback(checkpoint)
        end do
        
        ! Actual benchmark
        ops = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Create checkpoint
            checkpoint = arena%checkpoint()
            ops = ops + 1
            
            ! Add temporary nodes
            do i = 1, size/10
                node%node_kind = iter * 1000 + i
                handle = store_ast_node(arena, node)
            end do
            
            ! Rollback to checkpoint
            call arena%rollback(checkpoint)
            ops = ops + 1
        end do
        
        call cpu_time(end_time)
        
        result%test_name = "Checkpoint/Rollback"
        result%node_count = size
        result%operation_count = ops
        result%total_time = end_time - start_time
        result%ops_per_second = real(ops, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(ops, real64)
        result%successful_ops = ops
        result%failed_ops = 0
        
        call add_result(result)
        call destroy_ast_arena(arena)
        
    end subroutine benchmark_checkpoint_rollback
    
    subroutine benchmark_validation(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_handle_t) :: invalid_handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, iter, ops, valid_count, invalid_count
        logical :: is_valid
        type(operation_result_t) :: result
        
        allocate(handles(size))
        
        arena = create_ast_arena(size)
        
        ! Create nodes
        do i = 1, size
            node%node_kind = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Create some invalid handles
        invalid_handle%node_id = size + 1000
        invalid_handle%generation = -1
        invalid_handle%arena_id = -999
        
        ! Warmup
        do iter = 1, WARMUP_ITERATIONS
            do i = 1, min(size/10, 100)
                is_valid = arena%validate(handles(i))
            end do
        end do
        
        ! Actual benchmark
        ops = 0
        valid_count = 0
        invalid_count = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Validate valid handles
            do i = 1, size
                is_valid = arena%validate(handles(i))
                if (is_valid) valid_count = valid_count + 1
                ops = ops + 1
            end do
            
            ! Validate invalid handles
            do i = 1, 100
                invalid_handle%node_id = size + i
                is_valid = arena%validate(invalid_handle)
                if (.not. is_valid) invalid_count = invalid_count + 1
                ops = ops + 1
            end do
        end do
        
        call cpu_time(end_time)
        
        result%test_name = "Handle Validation"
        result%node_count = size
        result%operation_count = ops
        result%total_time = end_time - start_time
        result%ops_per_second = real(ops, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(ops, real64)
        result%successful_ops = valid_count
        result%failed_ops = invalid_count
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_validation
    
    subroutine benchmark_cross_references(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena1, arena2
        type(ast_handle_t), allocatable :: handles1(:), handles2(:)
        type(ast_node_arena_t) :: node, retrieved_node
        real(real64) :: start_time, end_time
        integer :: i, iter, ops, cross_refs
        type(operation_result_t) :: result
        
        allocate(handles1(size/2))
        allocate(handles2(size/2))
        
        arena1 = create_ast_arena(size/2)
        arena2 = create_ast_arena(size/2)
        
        ! Create nodes in both arenas
        do i = 1, size/2
            node%node_kind = i
            node%integer_data = i
            handles1(i) = store_ast_node(arena1, node)
            
            node%node_kind = -i
            node%integer_data = -i
            handles2(i) = store_ast_node(arena2, node)
        end do
        
        ! Setup cross-references
        do i = 1, size/2
            retrieved_node = get_ast_node(arena1, handles1(i))
            ! Store reference to node in arena2
            retrieved_node%parent_handle_id = handles2(min(i+1, size/2))%node_id
            retrieved_node%parent_handle_gen = handles2(min(i+1, size/2))%generation
            ! Note: This modifies a copy, not the actual stored node
            ! Would need to update the stored node for persistence
        end do
        
        ! Warmup
        cross_refs = 0
        do iter = 1, WARMUP_ITERATIONS/10
            do i = 1, min(size/10, 100)
                retrieved_node = get_ast_node(arena1, handles1(i))
                if (retrieved_node%parent_handle_id > 0) then
                    cross_refs = cross_refs + 1
                end if
            end do
        end do
        
        ! Actual benchmark
        ops = 0
        cross_refs = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS
            ! Access cross-references
            do i = 1, size/2
                retrieved_node = get_ast_node(arena1, handles1(i))
                ops = ops + 1
                if (retrieved_node%parent_handle_id > 0) then
                    ! Validate cross-arena reference
                    if (retrieved_node%parent_handle_id <= size/2) then
                        cross_refs = cross_refs + 1
                    end if
                end if
            end do
        end do
        
        call cpu_time(end_time)
        
        result%test_name = "Cross-Arena References"
        result%node_count = size
        result%operation_count = ops
        result%total_time = end_time - start_time
        result%ops_per_second = real(ops, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(ops, real64)
        result%successful_ops = cross_refs
        result%failed_ops = 0
        
        call add_result(result)
        call destroy_ast_arena(arena1)
        call destroy_ast_arena(arena2)
        deallocate(handles1)
        deallocate(handles2)
        
    end subroutine benchmark_cross_references
    
    subroutine benchmark_generation_tracking(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t), allocatable :: handles(:)
        type(ast_handle_t) :: old_handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time
        integer :: i, iter, ops, valid_gens, invalid_gens
        logical :: is_valid
        type(operation_result_t) :: result
        
        allocate(handles(size))
        
        arena = create_ast_arena(size)
        
        ! Initial allocation
        do i = 1, size
            node%node_kind = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Save some handles before reset
        old_handle = handles(1)
        
        ! Warmup with generation changes
        do iter = 1, WARMUP_ITERATIONS/10
            call arena%reset()
            do i = 1, min(size/10, 100)
                node%node_kind = iter * 1000 + i
                handles(i) = store_ast_node(arena, node)
            end do
        end do
        
        ! Actual benchmark
        ops = 0
        valid_gens = 0
        invalid_gens = 0
        call cpu_time(start_time)
        
        do iter = 1, BENCH_ITERATIONS/10  ! Fewer iterations for reset operations
            ! Reset arena (increments generation)
            call arena%reset()
            
            ! Check old handles (should be invalid)
            is_valid = arena%validate(old_handle)
            if (.not. is_valid) invalid_gens = invalid_gens + 1
            ops = ops + 1
            
            ! Allocate new nodes
            do i = 1, size
                node%node_kind = iter * 10000 + i
                handles(i) = store_ast_node(arena, node)
                ops = ops + 1
            end do
            
            ! Validate new handles (should be valid)
            do i = 1, size/10
                is_valid = arena%validate(handles(i))
                if (is_valid) valid_gens = valid_gens + 1
                ops = ops + 1
            end do
            
            ! Save handle for next iteration
            old_handle = handles(1)
        end do
        
        call cpu_time(end_time)
        
        result%test_name = "Generation Tracking"
        result%node_count = size
        result%operation_count = ops
        result%total_time = end_time - start_time
        result%ops_per_second = real(ops, real64) / result%total_time
        result%time_per_op_us = (result%total_time * 1.0e6) / real(ops, real64)
        result%successful_ops = valid_gens
        result%failed_ops = invalid_gens
        
        call add_result(result)
        call destroy_ast_arena(arena)
        deallocate(handles)
        
    end subroutine benchmark_generation_tracking
    
    subroutine benchmark_reset_performance(size)
        integer, intent(in) :: size
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle
        type(ast_node_arena_t) :: node
        real(real64) :: start_time, end_time, alloc_time, reset_time
        integer :: i, iter
        type(operation_result_t) :: result
        
        arena = create_ast_arena(size)
        
        alloc_time = 0.0
        reset_time = 0.0
        
        ! Benchmark allocation and reset cycles
        do iter = 1, BENCH_ITERATIONS
            ! Time allocation
            call cpu_time(start_time)
            do i = 1, size
                node%node_kind = i
                node%integer_data = iter * 1000 + i
                handle = store_ast_node(arena, node)
            end do
            call cpu_time(end_time)
            alloc_time = alloc_time + (end_time - start_time)
            
            ! Time reset
            call cpu_time(start_time)
            call arena%reset()
            call cpu_time(end_time)
            reset_time = reset_time + (end_time - start_time)
        end do
        
        result%test_name = "Reset Performance"
        result%node_count = size
        result%operation_count = BENCH_ITERATIONS
        result%total_time = reset_time
        result%ops_per_second = real(BENCH_ITERATIONS, real64) / reset_time
        result%time_per_op_us = (reset_time * 1.0e6) / real(BENCH_ITERATIONS, real64)
        result%successful_ops = BENCH_ITERATIONS
        result%failed_ops = 0
        
        call add_result(result)
        call destroy_ast_arena(arena)
        
    end subroutine benchmark_reset_performance
    
    subroutine add_result(result)
        type(operation_result_t), intent(in) :: result
        result_count = result_count + 1
        results(result_count) = result
    end subroutine add_result
    
    subroutine print_results()
        integer :: i
        real(real64) :: success_rate
        
        print *
        print *, "Operation Performance Results"
        print *, "============================="
        print *
        print '(A25,A10,A12,A15,A12,A12,A10,A10,A10)', &
            "Operation", "Nodes", "Ops", "Time (s)", "Ops/sec", "us/op", "Success", "Failed", "Rate %"
        print '(A25,A10,A12,A15,A12,A12,A10,A10,A10)', &
            "---------", "-----", "---", "-------", "-------", "-----", "-------", "------", "------"
        
        do i = 1, result_count
            if (results(i)%successful_ops + results(i)%failed_ops > 0) then
                success_rate = real(results(i)%successful_ops, real64) / &
                              real(results(i)%successful_ops + results(i)%failed_ops, real64) * 100.0
            else
                success_rate = 100.0
            end if
            
            print '(A25,I10,I12,F15.6,E12.3,F12.3,I10,I10,F10.1)', &
                results(i)%test_name, &
                results(i)%node_count, &
                results(i)%operation_count, &
                results(i)%total_time, &
                results(i)%ops_per_second, &
                results(i)%time_per_op_us, &
                results(i)%successful_ops, &
                results(i)%failed_ops, &
                success_rate
        end do
        
        print *
        print *, "Operations benchmark complete."
    end subroutine print_results

end program bench_arena_operations