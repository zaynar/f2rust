program test
    implicit none

    integer iostat
    integer n(11)
    data n /1,2,3,4,5,6,7,8,9,10,11/

    write (*, "('a', I2, 2('b', I2, SP), 'c' I3)", iostat=iostat) n
    print *, iostat
end
