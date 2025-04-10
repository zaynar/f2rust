program test
    implicit none

    integer a, b, c
    parameter (a=10)
    integer i
    integer s1, s2, s3, s4
    save c

    s1(i) = i + 1
    s2(i) = i + a + 1
    s3(i) = i + b + 1
    s4(i) = i + c + 1

    b = 100
    c = 1000
    print *, s1(a), s1(b), s1(c)
    print *, s2(a), s2(b), s2(c)
    print *, s3(a), s3(b), s3(c)
    print *, s4(a), s4(b), s4(c)
end
