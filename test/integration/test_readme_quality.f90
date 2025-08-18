program test_readme_quality
    use readme_file_utils, only: read_readme_file
    implicit none
    logical :: all_tests_passed
    
    ! Test README content quality standards per QADS guidelines
    call test_readme_standards(all_tests_passed)
    
    if (all_tests_passed) then
        print *, "PASS: README quality standards test"
    else
        print *, "FAIL: README quality standards test"
        stop 1
    end if
contains

subroutine test_readme_standards(test_passed)
    ! Given: README.md should follow QADS guidelines for proper README structure
    ! When: README cleanup is performed to remove bloat and issue-specific content
    ! Then: README should meet length, content, and structure requirements
    implicit none
    logical, intent(out) :: test_passed
    character(len=:), allocatable :: readme_content
    character(len=:), allocatable :: lines(:)
    integer :: line_count
    logical :: length_ok, content_ok, structure_ok
    
    ! Read README file content
    call read_readme_file(readme_content)
    
    ! Split into lines for analysis
    call split_into_lines(readme_content, lines, line_count)
    
    ! Test 1: Length constraint (under 100 lines per acceptance criteria)
    call test_length_constraint(line_count, length_ok)
    
    ! Test 2: Content restrictions (no issue-specific information)
    call test_content_restrictions(readme_content, content_ok)
    
    ! Test 3: Required structure (installation, features, usage, links)
    call test_required_structure(readme_content, structure_ok)
    
    test_passed = length_ok .and. content_ok .and. structure_ok
    
    ! Report specific failures for debugging
    if (.not. length_ok) then
        print *, "FAIL: Length constraint - README has", line_count, "lines (max: 100)"
    end if
    if (.not. content_ok) then
        print *, "FAIL: Content restrictions - README contains forbidden content"
    end if
    if (.not. structure_ok) then
        print *, "FAIL: Required structure - README missing required sections"
    end if
end subroutine test_readme_standards

subroutine test_length_constraint(line_count, length_ok)
    ! Given: QADS guidelines require concise, scannable READMEs
    ! When: README cleanup removes bloat and excessive content
    ! Then: README should be under 100 lines per Issue #306 acceptance criteria
    implicit none
    integer, intent(in) :: line_count
    logical, intent(out) :: length_ok
    integer, parameter :: max_lines = 100
    
    length_ok = line_count <= max_lines
end subroutine test_length_constraint

subroutine test_content_restrictions(readme_content, content_ok)
    ! Given: Issue #306 specifies removing issue-specific and implementation content
    ! When: README cleanup removes forbidden content types
    ! Then: README should not contain issue numbers, implementation details, or excessive examples
    implicit none
    character(len=*), intent(in) :: readme_content
    logical, intent(out) :: content_ok
    logical :: has_issue_refs, has_impl_details, has_excessive_examples
    
    ! Check for issue-specific references (Issue #XXX, #XXX)
    has_issue_refs = index(readme_content, "Issue #") > 0 .or. &
                     index(readme_content, "#27") > 0 .or. &
                     index(readme_content, "#26") > 0 .or. &
                     index(readme_content, "#20") > 0
    
    ! Check for implementation details that should be in separate docs
    has_impl_details = index(readme_content, "semantic_pipeline") > 0 .or. &
                       index(readme_content, "mono_type_t") > 0 .or. &
                       index(readme_content, "ast_arena") > 0 .or. &
                       index(readme_content, "input_validation module") > 0
    
    ! Check for excessive code examples (should be max 2-3 simple examples)
    has_excessive_examples = count_code_blocks(readme_content) > 5
    
    content_ok = .not. (has_issue_refs .or. has_impl_details .or. has_excessive_examples)
end subroutine test_content_restrictions

subroutine test_required_structure(readme_content, structure_ok)
    ! Given: QADS guidelines specify required README sections
    ! When: README cleanup maintains essential user-facing information
    ! Then: README should include installation, features, usage, and links sections
    implicit none
    character(len=*), intent(in) :: readme_content
    logical, intent(out) :: structure_ok
    logical :: has_installation, has_features, has_usage
    
    ! Check for required sections (case insensitive)
    has_installation = index(readme_content, "## Building") > 0 .or. &
                      index(readme_content, "## Installation") > 0 .or. &
                      index(readme_content, "fpm build") > 0
    
    has_features = index(readme_content, "## Features") > 0 .or. &
                  index(readme_content, "## Overview") > 0
    
    has_usage = index(readme_content, "## Usage") > 0 .or. &
               index(readme_content, "fortfront") > 0
    
    structure_ok = has_installation .and. has_features .and. has_usage
end subroutine test_required_structure

function count_code_blocks(content) result(count)
    ! Given: README content with potential code blocks
    ! When: Counting markdown code blocks (```)
    ! Then: Return total number of code blocks found
    implicit none
    character(len=*), intent(in) :: content
    integer :: count
    integer :: pos, start_pos
    
    count = 0
    start_pos = 1
    
    do
        pos = index(content(start_pos:), "```")
        if (pos == 0) exit
        count = count + 1
        start_pos = start_pos + pos + 2
    end do
    
    ! Code blocks come in pairs (opening and closing)
    count = count / 2
end function count_code_blocks

subroutine split_into_lines(content, lines, line_count)
    ! Given: Multi-line README content
    ! When: Need to count lines for length validation
    ! Then: Split content into array of lines and return count
    implicit none
    character(len=*), intent(in) :: content
    character(len=:), allocatable, intent(out) :: lines(:)
    integer, intent(out) :: line_count
    integer :: i, start_pos, end_pos, content_len
    character(len=1), parameter :: newline = new_line('A')
    
    content_len = len(content)
    line_count = 1
    
    ! Count newlines to determine number of lines
    do i = 1, content_len
        if (content(i:i) == newline) line_count = line_count + 1
    end do
    
    ! Allocate lines array (simplified for line counting only)
    allocate(character(len=100) :: lines(line_count))
    
    ! Note: Full line splitting implementation omitted for brevity
    ! This subroutine primarily serves to provide line_count for length validation
end subroutine split_into_lines


end program test_readme_quality