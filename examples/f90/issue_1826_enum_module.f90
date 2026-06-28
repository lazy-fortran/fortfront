! Issue #1826: enum construct inside a module body
module colors_enum
    enum, bind(c)
    enumerator :: RED = 1
    enumerator :: GREEN = 2
    enumerator :: BLUE = 3
    end enum
contains
    subroutine report()
        print *, RED
    end subroutine report
end module colors_enum

program demo_enum
    use colors_enum
    call report()
end program demo_enum
