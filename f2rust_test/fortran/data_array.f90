program test
    implicit none

    integer number
    integer array1(4)
    integer array2(2, 10:11)

    integer i, j

    data number / 10 /
    data array1 / 20, 30, 40, 50 /
    data array2 / 100, 200, 300, 400 /

    print *, number
    do i = 1, 4
        print *, array1(i)
    end do
    do j = 10, 11
        do i = 1, 2
            print *, array2(i, j)
        end do
    end do
end
