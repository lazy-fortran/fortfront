module mymod
    use othermod
    include 'file.inc'
    interface
        module procedure foo
    end interface
    type :: mytype
        integer :: n
    end type
end module
