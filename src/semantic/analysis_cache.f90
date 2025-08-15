module analysis_cache
    use base_analyzer, only: analysis_results_t
    implicit none
    private

    public :: analysis_cache_t, cache_entry_t

    type :: cache_entry_t
        character(len=64) :: analyzer_name = ""
        integer :: pass_number = 0
        type(analysis_results_t) :: results
        logical :: valid = .false.
    end type cache_entry_t

    type :: analysis_cache_t
        type(cache_entry_t), allocatable :: entries(:)
        integer :: size = 0
        integer :: capacity = 100
    contains
        procedure :: store_results => cache_store_results
        procedure :: retrieve_results => cache_retrieve_results
        procedure :: invalidate => cache_invalidate
        procedure :: clear_all => cache_clear_all
        procedure :: expand_capacity => cache_expand_capacity
    end type analysis_cache_t

contains

    subroutine cache_store_results(this, analyzer_name, pass_num, results)
        class(analysis_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: analyzer_name
        integer, intent(in) :: pass_num
        type(analysis_results_t), intent(in) :: results
        integer :: i, free_slot

        if (.not. allocated(this%entries)) then
            allocate(this%entries(this%capacity))
            this%size = 0
        end if

        ! Look for existing entry first
        do i = 1, this%size
            if (this%entries(i)%valid .and. &
                trim(this%entries(i)%analyzer_name) == trim(analyzer_name) .and. &
                this%entries(i)%pass_number == pass_num) then
                ! Update existing entry
                this%entries(i)%results = results
                return
            end if
        end do

        ! Find free slot or expand
        free_slot = 0
        do i = 1, this%capacity
            if (.not. this%entries(i)%valid) then
                free_slot = i
                exit
            end if
        end do

        if (free_slot == 0) then
            call this%expand_capacity()
            free_slot = this%capacity
        end if

        ! Store new entry
        this%entries(free_slot)%analyzer_name = analyzer_name
        this%entries(free_slot)%pass_number = pass_num
        this%entries(free_slot)%results = results
        this%entries(free_slot)%valid = .true.
        this%size = max(this%size, free_slot)
    end subroutine cache_store_results

    subroutine cache_retrieve_results(this, analyzer_name, pass_num, results, &
                                      success)
        class(analysis_cache_t), intent(in) :: this
        character(len=*), intent(in) :: analyzer_name
        integer, intent(in) :: pass_num
        type(analysis_results_t), intent(out) :: results
        logical, intent(out) :: success
        integer :: i

        success = .false.

        if (.not. allocated(this%entries)) then
            return
        end if

        do i = 1, this%size
            if (this%entries(i)%valid .and. &
                trim(this%entries(i)%analyzer_name) == trim(analyzer_name) .and. &
                this%entries(i)%pass_number == pass_num) then
                results = this%entries(i)%results
                success = .true.
                return
            end if
        end do
    end subroutine cache_retrieve_results

    subroutine cache_invalidate(this, analyzer_name)
        class(analysis_cache_t), intent(inout) :: this
        character(len=*), intent(in) :: analyzer_name
        integer :: i

        if (.not. allocated(this%entries)) then
            return
        end if

        do i = 1, this%size
            if (this%entries(i)%valid .and. &
                trim(this%entries(i)%analyzer_name) == trim(analyzer_name)) then
                this%entries(i)%valid = .false.
            end if
        end do
    end subroutine cache_invalidate

    subroutine cache_clear_all(this)
        class(analysis_cache_t), intent(inout) :: this
        integer :: i

        if (.not. allocated(this%entries)) then
            return
        end if

        do i = 1, this%size
            this%entries(i)%valid = .false.
        end do
        this%size = 0
    end subroutine cache_clear_all

    subroutine cache_expand_capacity(this)
        class(analysis_cache_t), intent(inout) :: this
        type(cache_entry_t), allocatable :: temp_entries(:)
        integer :: old_capacity

        if (.not. allocated(this%entries)) then
            allocate(this%entries(this%capacity))
            return
        end if

        old_capacity = this%capacity
        this%capacity = this%capacity * 2

        allocate(temp_entries(this%capacity))
        temp_entries(1:old_capacity) = this%entries
        deallocate(this%entries)
        allocate(this%entries(this%capacity))
        this%entries = temp_entries
    end subroutine cache_expand_capacity

end module analysis_cache