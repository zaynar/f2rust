program test
    implicit none

    character*(20) text
    character*(20) text2(2)

    data text(1:5) / '12345' /
    data text(6:10) / '67890' /
    data text(11:15) / 'abcde' /

    data text2(1)(11:15) / 'KLMNO' /
    data text2(1)(6:10) / 'FGHIJ' /
    data text2(1)(1:5) / 'ABCDE' /

    print *, text
    print *, text2(1)
end
