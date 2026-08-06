program test_reparse_benchmark_slow
    use, intrinsic :: iso_fortran_env, only: dp => real64, int64
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t
    implicit none

    logical :: all_passed
    character(len=:), allocatable :: source_200, source_1000, source_5000
    integer :: err

    print *, '=== Full Reparse Benchmark (LSP didChange scale) ==='
    print *

    call load_file('examples/f90/benchmark_200_lines.f90', source_200, err)
    if (err > 0) then
        print *, 'FAIL: cannot read benchmark_200_lines.f90'
        stop 1
    end if

    call load_file('examples/f90/benchmark_1000_lines.f90', source_1000, err)
    if (err > 0) then
        print *, 'FAIL: cannot read benchmark_1000_lines.f90'
        stop 1
    end if

    call load_file('examples/f90/benchmark_5000_lines.f90', source_5000, err)
    if (err > 0) then
        print *, 'FAIL: cannot read benchmark_5000_lines.f90'
        stop 1
    end if

    all_passed = .true.
    if (.not. run_benchmark(source_200, 200, all_passed)) all_passed = .false.
    if (.not. run_benchmark(source_1000, 1000, all_passed)) all_passed = .false.
    if (.not. run_benchmark(source_5000, 5000, all_passed)) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All reparse benchmarks within budget.'
        stop 0
    else
        print *, 'Reparse benchmark budget exceeded!'
        stop 1
    end if

contains

    subroutine load_file(filepath, content, error_code)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: error_code
        integer(int64) :: file_size
        integer :: unit, stat, char_len
        logical :: exists

        error_code = 0
        inquire (file=filepath, exist=exists, size=file_size)
        if (.not. exists) then
            error_code = 1
            allocate (character(len=0) :: content)
            return
        end if

        char_len = int(file_size)
        if (char_len <= 0) then
            allocate (character(len=0) :: content)
            return
        end if

        allocate (character(len=char_len) :: content)
        open (newunit=unit, file=filepath, status='old', action='read', &
            access='stream', form='unformatted', iostat=stat)
        if (stat /= 0) then
            error_code = 2
            deallocate (content)
            allocate (character(len=0) :: content)
            return
        end if
        read (unit, pos=1, iostat=stat) content
        close (unit)
        if (stat /= 0) then
            error_code = 3
            deallocate (content)
            allocate (character(len=0) :: content)
        end if
    end subroutine load_file

    logical function run_benchmark(source, line_count, all_passed)
        character(len=*), intent(in) :: source
        integer, intent(in) :: line_count
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        character(len=:), allocatable :: error_msg
        integer :: root_index, i, iters
        integer :: start_clock, end_clock, clock_rate
        real(dp) :: elapsed_ms, best_ms, worst_ms, total_ms
        character(len=200) :: label

        iters = 10
        best_ms = huge(1.0_dp)
        worst_ms = 0.0_dp
        total_ms = 0.0_dp

        options = tooling_parse_options_t()
        options%run_semantics = .false.
        options%reuse_arena = .true.

        call system_clock(count_rate=clock_rate)
        if (clock_rate <= 0) then
            print *, '  WARN: system_clock not reliable, skipping timing'
            run_benchmark = .true.
            return
        end if

        do i = 1, iters
            call system_clock(start_clock)
            call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                options)
            call system_clock(end_clock)

            elapsed_ms = real(end_clock - start_clock, dp) / &
                real(clock_rate, dp) * 1000.0_dp

            total_ms = total_ms + elapsed_ms
            if (elapsed_ms < best_ms) best_ms = elapsed_ms
            if (elapsed_ms > worst_ms) worst_ms = elapsed_ms
        end do

        write(label, '(A,I0,A,F10.3,A,F10.3,A,F10.3,A)') &
            ' ', line_count, ' lines: best=', best_ms, &
            ' ms  worst=', worst_ms, ' ms  mean=', &
            total_ms / real(iters, dp), ' ms'
        print '(A)', label

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, '  FAIL: parse error -> '//trim(error_msg)
            run_benchmark = .false.
            return
        end if

        if (root_index <= 0) then
            print *, '  FAIL: root index not set'
            run_benchmark = .false.
            return
        end if

        if (best_ms > 100.0_dp) then
            print '(A,F10.3,A)', &
                '  FLAG: best time exceeds 100 ms budget: ', best_ms, ' ms'
        end if

        run_benchmark = .true.
    end function run_benchmark

end program test_reparse_benchmark_slow
