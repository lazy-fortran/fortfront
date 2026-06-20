module enum_demo
    implicit none
    enum, bind(c)
        enumerator :: red, green = 5, blue
    end enum
end module enum_demo
