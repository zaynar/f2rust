program test
    implicit none
    integer i, j

    print *,'normal loop'
    do i = 1, 5
        print *,i
    end do

    print *,'step'
    do i = -5, 6, 2
        print *,i
    end do

    print *,'negative step'
    do i = 6, -5, -2
        print *,i
    end do

    print *,'no iterations'
    do i = 5, -5, 2
        print *,i
    end do

    print *,'var used outside loop'
    do j = -5, 6, 2
        print *,j
    end do
    print *,j

end
