program test
    implicit none

    character*(16) text
    character*(8) words(2)
    character*(8) letters(7)

    integer i, n
    parameter (n=3)

    data text / 'Hello world' /
    data words / 'Hello', 'world' /
    data letters / 'a', 2*'b', n*'c', 'd' /

    print *, text
    print *, words(1)
    print *, words(2)
    do i = 1, 7
      print *, letters(i)
    end do
end
