module readme_file_utils
    ! Shared utilities for README file operations in integration tests
    ! Provides common file reading functionality to eliminate code duplication
    implicit none
    private
    public :: read_readme_file

contains

subroutine read_readme_file(content)
    ! Given: README.md file exists in project root
    ! When: Need to read entire file content for validation
    ! Then: Read all lines and concatenate into single string
    implicit none
    character(len=:), allocatable, intent(out) :: content
    integer :: unit, ios
    character(len=1000) :: line
    character(len=:), allocatable :: temp_content
    
    ! Open README.md file
    open(newunit=unit, file="README.md", status="old", &
         action="read", iostat=ios)
    
    if (ios /= 0) then
        print *, "Error: Cannot open README.md for validation"
        stop 1
    end if
    
    ! Read file line by line
    temp_content = ""
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        temp_content = temp_content // trim(line) // new_line('A')
    end do
    
    close(unit)
    
    ! Return the content
    content = temp_content
end subroutine read_readme_file

end module readme_file_utils