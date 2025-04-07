program test
    implicit none

    print *,'add', 1 + 2 + 3 + 4
    print *,'precedence', 1 * 2 + 3 * 4
    print *,'parens', 1 * (2 + 3) * 4
    print *,'pow assoc', 4 ** 3 ** 2 ** 1
    print *,'unary', -1 - 2 + (+3)
    print *,'pow precedence', -1 ** 2
    print *,'negative pow', 1 ** (-2)
    print *,'negative pow', 2 ** (-3)
    print *,'division', 8 / 3
    print *,'neg division', (-8) / 3
end
