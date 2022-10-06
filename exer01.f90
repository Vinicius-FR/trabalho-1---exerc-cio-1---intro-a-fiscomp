program exer01
    IMPLICIT NONE
    ! Criando os parâmetros auxiliares para os loops de cada precisão
    integer :: i = 0, j = 0, k = 0 ! Número de bits
    real(4) :: a = 1e0, s
    real(8) :: b = 1d0, d
    real(16) :: c = 1_16, q


    print *, 'PRECISÃO SIMPLES'

    do
        s = 1 + a
        a = a / 2
        print *, a, s
        if (s == 1) exit
        i = i + 1   
    end do

    print *, 'PRECISÃO DUPLA'

    do
        d = 1 + b
        b = b / 2
        print *, b, d
        if (d == 1) exit
        j = j + 1
    end do

    print *, 'PRECISÃO QUÁDRUPLA'

    do
        q = 1 + c
        c = c / 2
        print *, c, q
        if (q == 1) exit
        k = k + 1
    end do

    print *, i, 2*a ! para printar o número de bits e o último termo que faz diferença na soma
    print *, j, 2*b
    print *, k, 2*c

  end program exer01