program statements
    class(*), allocatable :: object
    integer :: values(3), choice, ios
    logical :: mask(3), opened
    open(unit=10,file='data.txt',status='old',access='sequential',form='formatted',recl=80,blank='null',position='rewind',action='read',delim='quote',pad='yes',iostat=ios,err=900)
    close(unit=10,status='keep',iostat=ios,err=900)
    close(unit=11)
    rewind(unit=10,iostat=ios,err=900)
    backspace(unit=10,iostat=ios,err=900)
    endfile(unit=10,iostat=ios,err=900)
    inquire(unit=10,opened=opened,iostat=ios,err=900)
    print '(I0)', choice
    write(unit=10,fmt='(I0)',iostat=ios,err=900) choice
    read(unit=10,fmt='(I0)',iostat=ios,end=800,err=900) choice
    700 format(I0)
    associate(alias => choice)
        values(1) = alias
    end associate
    block
        integer :: local
        local = choice
    end block
    allocate(object, source=choice)
    select type (object)
        type is (integer)
        values(1) = object
    class default
        values(1) = 0
    end select
    select rank(values)
        rank(1)
        values = 1
        rank default
        values = 0
    end select
    where(mask)
        values = 1
    elsewhere(.not. mask)
        values = 2
    elsewhere
        values = 3
    end where
    where(mask) values = 3
    goto 100
    goto (100,200), choice
    pause 1
    pause 'hold'
    pause
    100 continue
    200 continue
    800 continue
    900 continue
contains
    subroutine inner
        goto 100
        100 continue
    end subroutine inner
end program statements
