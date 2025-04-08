program test
    implicit none

    character*(16) text
    character*(8) words(2)
    character*(8) letters(7)

    character*(16) text2
    character*(8) words2(2)

    integer i, n
    parameter (n=3)

    data text / 'Hello world' /
    data words / 'Hello', 'world' /
    data letters / 'a', 2*'b', n*'c', 'd' /

    save text, words, letters

    print *, text
    print *, words(1)
    print *, words(2)
    do i = 1, 7
      print *, letters(i)
    end do

    text2 = text
    words2(1) = words(1)
    words2(2) = words(2)

    print *, text2
    print *, words2(1)
    print *, words2(2)
end
