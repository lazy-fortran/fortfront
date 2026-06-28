! Test case for issue #2453: Character array constructor with type specification
! This tests that the type specification in array constructors is preserved
! during roundtrip transformation.
program issue_2453_character_array_constructor
    implicit none
    character(len=10) :: strings(3)

    ! Character array constructor with type specification for mixed lengths
    ! The type spec [character(len=10) ::] ensures all elements are padded/truncated to length 10
    strings = [character(len=10) :: "short", "medium len", "verylongstring"]

    ! Verify correct padding/truncation
    print *, trim(strings(1)) ! Expected: "short"
    print *, trim(strings(2)) ! Expected: "medium len"
    print *, trim(strings(3)) ! Expected: "verylongst"
end program issue_2453_character_array_constructor
