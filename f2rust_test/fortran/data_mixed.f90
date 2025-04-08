program test
    implicit none

    integer ints(3,2)
    real reals(3)
    character*(4) chars(3)

    integer i
    data (ints(i,1), ints(i,2), reals(i), chars(i), i = 1,3) / 1, 1, 2.0, '3333', 2*4, 5.0, '6666', 2*7, 8.0, '9999' /

    do i = 1, 3
        print *, ints(i,1), ints(i,2), reals(i), chars(i)
    end do
end
