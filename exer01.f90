program exer01
    IMPLICIT NONE
    ! Esse é um exemplo de comentario
    real(4) :: a = 1e0
    real(4) :: s
    integer :: i = 0
    real(8) :: b = 1d0
    real(8) :: d
    real(16) :: c = 1_16
    real(16) :: q


    print *, 'PRECISÃO SIMPLES'

    do
        s = 1 + a
        a = a / 2
        print *, a, s
        if (s == 1) exit
        i = i + 1   
    end do

    print *, i

    i = 0

    print *, 'PRECISÃO DUPLA'

    do
        d = 1 + b
        b = b / 2
        print *, b, d
        if (d == 1) exit
        i = i + 1
    end do

    print *, i

    i = 0

    print *, 'PRECISÃO QUÁDRUPLA'

    do
        q = 1 + c
        c = c / 2
        print *, c, q
        if (q == 1) exit
        i = i + 1
    end do

    print *, i

  end program exer01