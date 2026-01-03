module debug_trace
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, int64
    use cli_env, only: is_truthy
    implicit none
    private

    integer, save :: depth = 0
    logical, save :: enabled = .false.
    logical, save :: initialized = .false.
    integer, parameter :: MAX_DEPTH = 2000
    integer, save :: file_u = -1
    character(len=256), save :: file_name = ''
    logical, save :: profile_enabled = .false.
    integer(int64), save :: profile_clock_rate = 0_int64

    type :: profile_section_t
        character(len=:), allocatable :: name
        integer(int64) :: call_count = 0_int64
        integer(int64) :: total_counts = 0_int64
        integer(int64) :: self_counts = 0_int64
    end type profile_section_t

    type :: profile_frame_t
        integer :: section_index = 0
        integer(int64) :: start_count = 0_int64
        integer(int64) :: child_counts = 0_int64
    end type profile_frame_t

    type(profile_section_t), allocatable, save :: profile_sections(:)
    type(profile_frame_t), allocatable, save :: profile_stack(:)
    integer, save :: profile_depth = 0
    integer(int64), save :: profile_root_total_counts = 0_int64

    public :: trace_finalize
    public :: trace_init, trace_enter, trace_leave, trace_is_enabled
    public :: trace_profile_get_stat
    public :: trace_profile_reset
    public :: trace_set_profile_enabled

contains

    subroutine trace_init()
        character(len=64) :: val
        integer :: stat
        integer :: rate_default
        if (initialized) return
        val = ''
        call get_environment_variable('FORTFRONT_TRACE', val, status=stat)
        if (stat == 0) then
            ! Opt-in: enable only for truthy values; still avoid enabling on
            ! Windows to prevent CI pipe stack overflows.
            if (is_truthy(trim(val)) .and. .not. is_windows_platform()) then
                enabled = .true.
            end if
        end if
        val = ''
        call get_environment_variable('FORTFRONT_PROFILE', val, status=stat)
        if (stat == 0) then
            if (is_truthy(trim(val)) .and. .not. is_windows_platform()) then
                profile_enabled = .true.
            end if
        end if
        ! Optional: file logging (open only if tracing enabled)
        if (enabled) then
            call get_environment_variable('FORTFRONT_TRACE_FILE', file_name, &
                                          status=stat)
            if (stat == 0 .and. len_trim(file_name) > 0) then
                ! Preserve any early CLI trace lines by appending instead of replacing.
                open (newunit=file_u, file=trim(file_name), status='unknown', &
                      position='append', action='write')
            end if
        end if

        profile_clock_rate = 0_int64
        call system_clock(count_rate=rate_default)
        if (rate_default > 0) profile_clock_rate = int(rate_default, kind=int64)

        initialized = .true.
    end subroutine trace_init

    logical function is_windows_platform()
        character(len=16) :: os_name
        integer :: stat
        call get_environment_variable('OS', os_name, status=stat)
        if (stat == 0) then
            is_windows_platform = index(os_name, 'Windows') > 0
            return
        end if
        call get_environment_variable('WINDIR', os_name, status=stat)
        is_windows_platform = (stat == 0)
    end function is_windows_platform

    subroutine trace_enter(name)
        character(len=*), intent(in) :: name
        integer :: section_index
        integer :: start_default
        integer(int64) :: start_count

        if (.not. initialized) call trace_init()
        if (.not. enabled .and. .not. profile_enabled) return

        if (enabled) then
            depth = depth + 1
            if (depth > MAX_DEPTH) then
                write (error_unit, '(A,I0,1X,A)') 'TRACE: Max depth exceeded: ', &
                    depth, trim(name)
                error stop 1
            end if
            write (error_unit, '(A,I0,2X,A)') '>> depth', depth, trim(name)
            if (file_u > 0) then
                write (file_u, '(A,I0,2X,A)') '>> depth', depth, trim(name)
                flush (file_u)
            end if
        end if

        if (.not. profile_enabled) return

        if (.not. allocated(profile_stack)) then
            allocate (profile_stack(MAX_DEPTH))
        end if

        profile_depth = profile_depth + 1
        if (profile_depth > MAX_DEPTH) then
            write (error_unit, '(A,I0,1X,A)') 'PROFILE: Max depth exceeded: ', &
                profile_depth, trim(name)
            error stop 1
        end if

        section_index = ensure_profile_section(trim(name))
        call system_clock(count=start_default)
        start_count = int(start_default, kind=int64)
        profile_sections(section_index)%call_count = &
            profile_sections(section_index)%call_count + 1_int64

        profile_stack(profile_depth)%section_index = section_index
        profile_stack(profile_depth)%start_count = start_count
        profile_stack(profile_depth)%child_counts = 0_int64
    end subroutine trace_enter

    subroutine trace_leave(name)
        character(len=*), intent(in) :: name
        integer :: end_default
        integer(int64) :: end_count
        integer(int64) :: elapsed_counts, self_counts
        integer :: section_index

        if (.not. initialized) call trace_init()
        if (.not. enabled .and. .not. profile_enabled) return

        if (enabled) then
            write (error_unit, '(A,I0,2X,A)') '<< depth', depth, trim(name)
            if (file_u > 0) then
                write (file_u, '(A,I0,2X,A)') '<< depth', depth, trim(name)
                flush (file_u)
            end if
            if (depth > 0) depth = depth - 1
        end if

        if (.not. profile_enabled) return
        if (profile_depth <= 0) return

        call system_clock(count=end_default)
        end_count = int(end_default, kind=int64)

        section_index = profile_stack(profile_depth)%section_index
        elapsed_counts = end_count - profile_stack(profile_depth)%start_count
        self_counts = elapsed_counts - profile_stack(profile_depth)%child_counts
        if (elapsed_counts < 0_int64) elapsed_counts = 0_int64
        if (self_counts < 0_int64) self_counts = 0_int64

        profile_sections(section_index)%total_counts = &
            profile_sections(section_index)%total_counts + elapsed_counts
        profile_sections(section_index)%self_counts = &
            profile_sections(section_index)%self_counts + self_counts

        if (profile_depth == 1) then
            profile_root_total_counts = profile_root_total_counts + elapsed_counts
        else
            profile_stack(profile_depth - 1)%child_counts = &
                profile_stack(profile_depth - 1)%child_counts + elapsed_counts
        end if

        profile_depth = profile_depth - 1
    end subroutine trace_leave

    pure logical function trace_is_enabled()
        trace_is_enabled = enabled
    end function trace_is_enabled

    subroutine trace_set_profile_enabled(val)
        logical, intent(in) :: val
        if (.not. initialized) call trace_init()
        profile_enabled = val
        if (.not. profile_enabled) call trace_profile_reset()
    end subroutine trace_set_profile_enabled

    subroutine trace_profile_reset()
        if (allocated(profile_sections)) deallocate (profile_sections)
        if (allocated(profile_stack)) deallocate (profile_stack)
        profile_depth = 0
        profile_root_total_counts = 0_int64
    end subroutine trace_profile_reset

    subroutine trace_profile_get_stat(name, call_count, total_counts, self_counts, &
                                      found)
        character(len=*), intent(in) :: name
        integer(int64), intent(out) :: call_count
        integer(int64), intent(out) :: total_counts
        integer(int64), intent(out) :: self_counts
        logical, intent(out) :: found
        integer :: i

        call_count = 0_int64
        total_counts = 0_int64
        self_counts = 0_int64
        found = .false.
        if (.not. allocated(profile_sections)) return

        do i = 1, size(profile_sections)
            if (.not. allocated(profile_sections(i)%name)) cycle
            if (trim(profile_sections(i)%name) == trim(name)) then
                call_count = profile_sections(i)%call_count
                total_counts = profile_sections(i)%total_counts
                self_counts = profile_sections(i)%self_counts
                found = .true.
                return
            end if
        end do
    end subroutine trace_profile_get_stat

    subroutine trace_finalize()
        integer :: i
        real(dp) :: total_ms, self_ms

        if (.not. initialized) return
        if (.not. profile_enabled) return

        write (error_unit, '(A)') '=== Fortfront Profile ==='
        if (profile_clock_rate > 0_int64) then
            total_ms = counts_to_ms(profile_root_total_counts)
            write (error_unit, '(A,F10.3)') 'total_ms: ', total_ms
        else
            write (error_unit, '(A)') 'total_ms: unavailable'
        end if

        if (allocated(profile_sections)) then
            do i = 1, size(profile_sections)
                if (.not. allocated(profile_sections(i)%name)) cycle
                if (profile_sections(i)%call_count <= 0_int64) cycle
                total_ms = counts_to_ms(profile_sections(i)%total_counts)
                self_ms = counts_to_ms(profile_sections(i)%self_counts)
                write (error_unit, '(A,1X,I0,1X,A,F10.3,1X,A,F10.3)') &
                    trim(profile_sections(i)%name), &
                    profile_sections(i)%call_count, &
                    'self_ms:', self_ms, &
                    'total_ms:', total_ms
            end do
        end if
        flush (error_unit)

        call trace_profile_reset()
        profile_enabled = .false.
    end subroutine trace_finalize

    integer function ensure_profile_section(name) result(section_index)
        character(len=*), intent(in) :: name
        integer :: i
        type(profile_section_t), allocatable :: tmp(:)

        if (.not. allocated(profile_sections)) then
            allocate (profile_sections(1))
            profile_sections(1)%name = trim(name)
            section_index = 1
            return
        end if

        do i = 1, size(profile_sections)
            if (.not. allocated(profile_sections(i)%name)) cycle
            if (profile_sections(i)%name == trim(name)) then
                section_index = i
                return
            end if
        end do

        allocate (tmp(size(profile_sections) + 1))
        tmp(1:size(profile_sections)) = profile_sections
        tmp(size(tmp))%name = trim(name)
        call move_alloc(tmp, profile_sections)
        section_index = size(profile_sections)
    end function ensure_profile_section

    real(dp) function counts_to_ms(counts) result(ms)
        integer(int64), intent(in) :: counts
        if (profile_clock_rate <= 0_int64) then
            ms = 0.0_dp
        else
            ms = 1000.0_dp * real(counts, kind=dp) / real(profile_clock_rate, kind=dp)
        end if
    end function counts_to_ms

end module debug_trace
