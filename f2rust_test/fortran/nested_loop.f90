program test
    implicit none
    integer i, j

    do i = 1, 5, 2
        do j = 1, 5
            print *,i,j
        end do
    end do

    ! Force the non-range-based loop
    print *,i,j
end
