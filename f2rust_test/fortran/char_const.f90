program test
    implicit none

    character*(*) a
    parameter (a="Hello world 1")

    character*(20) b
    parameter (b="Hello world 2")

    integer n
    parameter (n=30)

    character*(n) c
    parameter (c="Hello world 3")

    print *, a
    print *, b
    print *, c
end
