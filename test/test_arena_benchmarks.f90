program test_arena_benchmarks
    ! Comprehensive arena performance benchmark suite - Issue #400
    ! Tests arena allocation, access, validation, and real-world workloads  
    ! Provides performance baselines and regression detection
    
    use arena_memory
    use iso_fortran_env, only: real64, int64, error_unit, output_unit
    implicit none

    ! Timer state (simple implementation)
    integer, parameter :: MAX_TIMERS = 16
    real(real64), save :: timer_starts(MAX_TIMERS) = 0.0_real64
    logical, save :: timer_active(MAX_TIMERS) = .false.

    ! Benchmark parameters
    integer, parameter :: SMALL_OPERATIONS = 1000
    integer, parameter :: MEDIUM_OPERATIONS = 10000
    integer, parameter :: LARGE_OPERATIONS = 100000
    integer, parameter :: SMALL_NODE_COUNT = 100
    integer, parameter :: MEDIUM_NODE_COUNT = 1000
    integer, parameter :: LARGE_NODE_COUNT = 10000

    ! Test counters
    integer :: total_benchmarks = 0
    integer :: successful_benchmarks = 0
    integer :: failed_benchmarks = 0
    real(real64) :: total_time = 0.0_real64

    write(output_unit, '(A)') "======================================="
    write(output_unit, '(A)') "Arena Performance Benchmark Suite"
    write(output_unit, '(A)') "Issue #400 - Comprehensive Performance"
    write(output_unit, '(A)') "======================================="
    write(output_unit, '(A)') ""

    ! Run basic arena benchmarks
    call run_basic_arena_benchmarks()
    call run_allocation_benchmarks()
    call run_validation_benchmarks()
    call run_comparison_benchmarks()

    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "======================================="
    write(output_unit, '(A)') "BENCHMARK SUMMARY"
    write(output_unit, '(A)') "======================================="
    write(output_unit, '(A, I0)') "Total benchmarks: ", total_benchmarks
    write(output_unit, '(A, I0)') "Successful: ", successful_benchmarks
    write(output_unit, '(A, I0)') "Failed: ", failed_benchmarks
    write(output_unit, '(A, F0.3, A)') "Total time: ", total_time, " ms"
    
    if (successful_benchmarks > 0) then
        write(output_unit, '(A, F0.1)') "Average ops/sec: ", &
            real(successful_benchmarks, real64) * 1000.0_real64 / total_time
    end if

    if (failed_benchmarks == 0 .and. successful_benchmarks > 0) then
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "All arena benchmarks PASSED!"
        write(output_unit, '(A)') "Performance baseline established successfully."
    else
        write(error_unit, '(A, I0, A)') "Arena benchmarks FAILED: ", failed_benchmarks, " failures"
        stop 1
    end if

    write(output_unit, '(A)') "======================================="

contains

    ! Start timing (returns timer ID)
    function start_timer() result(timer_id)
        integer :: timer_id
        integer :: i

        ! Find free timer slot
        timer_id = 0
        do i = 1, MAX_TIMERS
            if (.not. timer_active(i)) then
                timer_id = i
                exit
            end if
        end do

        if (timer_id > 0) then
            call cpu_time(timer_starts(timer_id))
            timer_active(timer_id) = .true.
        end if
    end function start_timer

    ! End timing and return duration in milliseconds
    function end_timer(timer_id) result(duration_ms)
        integer, intent(in) :: timer_id
        real(real64) :: duration_ms
        real(real64) :: end_time

        duration_ms = -1.0_real64  ! Error value

        if (timer_id < 1 .or. timer_id > MAX_TIMERS) return
        if (.not. timer_active(timer_id)) return

        call cpu_time(end_time)
        duration_ms = (end_time - timer_starts(timer_id)) * 1000.0_real64

        ! Release timer
        timer_active(timer_id) = .false.
        timer_starts(timer_id) = 0.0_real64
    end function end_timer

    ! Run basic arena operation benchmarks
    subroutine run_basic_arena_benchmarks()
        write(output_unit, '(A)') "Basic Arena Operations:"
        write(output_unit, '(A)') "-----------------------"
        
        call run_benchmark("Arena Creation/Destruction", benchmark_arena_lifecycle())
        call run_benchmark("Arena Reset", benchmark_arena_reset())
        call run_benchmark("Arena Statistics", benchmark_arena_stats())
    end subroutine run_basic_arena_benchmarks

    ! Run allocation pattern benchmarks
    subroutine run_allocation_benchmarks()
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "Allocation Patterns:"
        write(output_unit, '(A)') "--------------------"
        
        call run_benchmark("Sequential Small Allocations", benchmark_sequential_allocation(SMALL_OPERATIONS))
        call run_benchmark("Sequential Medium Allocations", benchmark_sequential_allocation(MEDIUM_OPERATIONS))
        call run_benchmark("Sequential Large Allocations", benchmark_sequential_allocation(LARGE_OPERATIONS))
        call run_benchmark("Arena Growth", benchmark_arena_growth())
    end subroutine run_allocation_benchmarks

    ! Run validation benchmarks
    subroutine run_validation_benchmarks()
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "Handle Validation:"
        write(output_unit, '(A)') "------------------"
        
        call run_benchmark("Valid Handle Validation", benchmark_valid_handle_validation(MEDIUM_OPERATIONS))
        call run_benchmark("Invalid Handle Validation", benchmark_invalid_handle_validation(MEDIUM_OPERATIONS))
        call run_benchmark("Generation Tracking", benchmark_generation_tracking())
    end subroutine run_validation_benchmarks

    ! Run comparison benchmarks
    subroutine run_comparison_benchmarks()
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "Performance Comparisons:"
        write(output_unit, '(A)') "------------------------"
        
        call run_benchmark("Arena vs Naive (Small)", benchmark_arena_vs_naive(SMALL_NODE_COUNT))
        call run_benchmark("Arena vs Naive (Medium)", benchmark_arena_vs_naive(MEDIUM_NODE_COUNT))
        call run_benchmark("Arena vs Naive (Large)", benchmark_arena_vs_naive(LARGE_NODE_COUNT))
    end subroutine run_comparison_benchmarks

    ! Run individual benchmark and track results
    subroutine run_benchmark(name, ops_per_second)
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: ops_per_second
        
        total_benchmarks = total_benchmarks + 1
        
        if (ops_per_second > 0.0_real64) then
            successful_benchmarks = successful_benchmarks + 1
            write(output_unit, '(A, ": ", F0.1, " ops/sec")') trim(name), ops_per_second
            total_time = total_time + 1000.0_real64 / ops_per_second  ! Estimate time
        else
            failed_benchmarks = failed_benchmarks + 1
            write(output_unit, '(A, ": FAILED")') trim(name)
        end if
    end subroutine run_benchmark

    ! Benchmark arena lifecycle (creation/destruction)
    function benchmark_arena_lifecycle() result(ops_per_second)
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        integer :: timer_id, i
        real(real64) :: duration_ms
        integer, parameter :: iterations = 1000

        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            return
        end if

        do i = 1, iterations
            arena = create_arena()
            call destroy_arena(arena)
        end do

        duration_ms = end_timer(timer_id)
        if (duration_ms <= 0.0_real64) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (iterations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_arena_lifecycle

    ! Benchmark arena reset performance
    function benchmark_arena_reset() result(ops_per_second)
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_handle_t) :: handle
        integer :: timer_id, i
        real(real64) :: duration_ms
        integer, parameter :: iterations = 10000

        arena = create_arena()

        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        do i = 1, iterations
            handle = arena%allocate(64)
            call arena%reset()
        end do

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (iterations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_arena_reset

    ! Benchmark statistics collection
    function benchmark_arena_stats() result(ops_per_second)
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_stats_t) :: stats
        integer :: timer_id, i
        real(real64) :: duration_ms
        integer, parameter :: iterations = 100000

        arena = create_arena()

        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        do i = 1, iterations
            stats = arena%get_stats()
        end do

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (iterations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_arena_stats

    ! Benchmark sequential allocation
    function benchmark_sequential_allocation(num_operations) result(ops_per_second)
        integer, intent(in) :: num_operations
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_handle_t) :: handle
        integer :: timer_id, i
        real(real64) :: duration_ms

        arena = create_arena()

        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        do i = 1, num_operations
            handle = arena%allocate(64)
            if (.not. is_valid_handle(handle)) then
                ops_per_second = -1.0_real64
                call destroy_arena(arena)
                return
            end if
        end do

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (num_operations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_sequential_allocation

    ! Benchmark arena growth
    function benchmark_arena_growth() result(ops_per_second)
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_handle_t) :: handle
        integer :: timer_id, i
        real(real64) :: duration_ms
        integer, parameter :: iterations = 1000

        arena = create_arena(chunk_size=4096)  ! Start small

        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        do i = 1, iterations
            handle = arena%allocate(4096)  ! Force growth
            if (.not. is_valid_handle(handle)) then
                ops_per_second = -1.0_real64
                call destroy_arena(arena)
                return
            end if
        end do

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (iterations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_arena_growth

    ! Benchmark valid handle validation
    function benchmark_valid_handle_validation(num_operations) result(ops_per_second)
        integer, intent(in) :: num_operations
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_handle_t), allocatable :: handles(:)
        integer :: timer_id, i, validation_count
        real(real64) :: duration_ms
        logical :: valid

        arena = create_arena()
        allocate(handles(min(num_operations, 1000)))

        ! Create valid handles
        do i = 1, size(handles)
            handles(i) = arena%allocate(64)
        end do

        validation_count = 0
        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        do i = 1, num_operations
            valid = arena%validate(handles(mod(i - 1, size(handles)) + 1))
            if (valid) validation_count = validation_count + 1
        end do

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64 .or. validation_count /= num_operations) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (num_operations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_valid_handle_validation

    ! Benchmark invalid handle validation
    function benchmark_invalid_handle_validation(num_operations) result(ops_per_second)
        integer, intent(in) :: num_operations
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_handle_t) :: invalid_handle
        integer :: timer_id, i, invalid_count
        real(real64) :: duration_ms
        logical :: valid

        arena = create_arena()
        invalid_handle = null_handle()

        invalid_count = 0
        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        do i = 1, num_operations
            valid = arena%validate(invalid_handle)
            if (.not. valid) invalid_count = invalid_count + 1
        end do

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64 .or. invalid_count /= num_operations) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (num_operations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_invalid_handle_validation

    ! Benchmark generation tracking
    function benchmark_generation_tracking() result(ops_per_second)
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        type(arena_stats_t) :: stats1, stats2
        integer :: timer_id, i
        real(real64) :: duration_ms
        integer, parameter :: iterations = 10000

        arena = create_arena()

        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        stats1 = arena%get_stats()
        do i = 1, iterations
            call arena%reset()  ! Should increment generation
        end do
        stats2 = arena%get_stats()

        duration_ms = end_timer(timer_id)
        call destroy_arena(arena)

        if (duration_ms <= 0.0_real64 .or. stats2%current_generation <= stats1%current_generation) then
            ops_per_second = -1.0_real64
        else
            ops_per_second = (iterations * 1000.0_real64) / duration_ms
        end if
    end function benchmark_generation_tracking

    ! Benchmark arena vs naive allocation
    function benchmark_arena_vs_naive(allocation_count) result(ops_per_second)
        integer, intent(in) :: allocation_count
        real(real64) :: ops_per_second
        type(arena_t) :: arena
        integer :: timer_id, i
        real(real64) :: arena_time, naive_time

        ! Benchmark arena allocation
        arena = create_arena()
        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            call destroy_arena(arena)
            return
        end if

        block
            type(arena_handle_t) :: handle
            do i = 1, allocation_count
                handle = arena%allocate(64)
            end do
        end block

        arena_time = end_timer(timer_id)
        call destroy_arena(arena)

        ! Benchmark naive allocation
        timer_id = start_timer()
        if (timer_id <= 0) then
            ops_per_second = -1.0_real64
            return
        end if

        block
            integer, allocatable :: naive_data(:)
            do i = 1, allocation_count
                allocate(naive_data(16))  ! 64 bytes / 4 bytes per integer
                deallocate(naive_data)
            end do
        end block

        naive_time = end_timer(timer_id)

        if (arena_time <= 0.0_real64 .or. naive_time <= 0.0_real64) then
            ops_per_second = -1.0_real64
        else
            ! Return arena performance (speedup is arena_time/naive_time)
            ops_per_second = (allocation_count * 1000.0_real64) / arena_time
        end if
    end function benchmark_arena_vs_naive

end program test_arena_benchmarks