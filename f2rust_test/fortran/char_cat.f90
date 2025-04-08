program test
    implicit none

    character*(*) a
    parameter (a="Hello ")

    character*(10) b

    character*(20) saved
    character*(20) out

    save saved

    data b / "world" /

    out = a // b
    saved = "Hello " // b // "!"

    print *, a // b
    print *, out
    print *, saved
    print *, saved // "?"
end
