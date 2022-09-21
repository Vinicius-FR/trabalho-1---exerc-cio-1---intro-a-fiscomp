program exer02
    IMPLICIT NONE
    ! Criando as variáveis necessárias para o teste com precisão simples e precisão dupla separadamente
    integer :: fact, i, n ! Parâmetros para o cálculo do fatorial
    real(4) :: x = 0.1e0, y = 0.2e0, z = 0.3e0, w = 0.4e0, seno_s = 0e0, termo_s !valores de teste, série do seno começando em 0 e temo a ser alterado e somado a cada loop
    real(8) :: a = 0.1d0, b = 0.2d0, c = 0.3d0, d = 0.4d0, seno_d = 0d0, termo_d

    print *, "PRECISÃO SIMPLES"

    ! x = 0.1:

    n = 1

    do
        fact = 1
        do i = 1, 2 * n - 1  ! Loop para calcular o fatorial do n-ésimo número ímpar
            fact = fact*i
        end do
        
        termo_s = (x ** (2 * n - 1)) * ((-1)**(n+1))/ fact    !  Cria o próximo termo a ser somado na série
        if (seno_s /= seno_s + termo_s) then  ! Só somar se o termo adicionado alterar o valor atual
            seno_s = seno_s + termo_s
            n = n + 1  ! Atualiza n
        else
            termo_s = (x ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n) ! Calcula o termo anterior para imprimir o último que altera a série
            exit  ! Encerra o Loop caso o termo seja menor que a precisão
        end if
    end do

    print *, termo_s / seno_s , seno_s  ! Imprime a precisão relativa e o valor calculado para o sen(x)

    ! x = y = 0.2:

    n = 1  ! Reiniciando os parâmetros da série para começar um novo loop
    seno_s = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_s = (y ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_s /= seno_s + termo_s) then
            seno_s = seno_s + termo_s
            n = n + 1
        else
            termo_s = (y ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_s / seno_s , seno_s

    ! x = z = 0.3:

    n = 1
    seno_s = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_s = (z ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_s /= seno_s + termo_s) then
            seno_s = seno_s + termo_s
            n = n + 1
        else
            termo_s = (z ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_s / seno_s , seno_s

    ! x = w = 0.4:

    n = 1
    seno_s = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_s = (w ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_s /= seno_s + termo_s) then
            seno_s = seno_s + termo_s
            n = n + 1
        else
            termo_s = (w ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_s / seno_s , seno_s


    print *, "PRECISÃO DUPLA"

    ! x = a = 0.1:

    n = 1

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_d = (a ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_d /= seno_d + termo_d) then
            seno_d = seno_d + termo_d
            n = n + 1
        else
            termo_d = (a ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_d / seno_d , seno_d

    ! x = b = 0.2:

    n = 1
    seno_d = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_d = (b ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_d /= seno_d + termo_d) then
            seno_d = seno_d + termo_d
            n = n + 1
        else
            termo_d = (b ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_d / seno_d , seno_d

    ! x = c = 0.3:

    n = 1
    seno_d = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_d = (c ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_d /= seno_d + termo_d) then
            seno_d = seno_d + termo_d
            n = n + 1
        else
            termo_d = (c ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_d / seno_d , seno_d

    ! x = d = 0.4:

    n = 1
    seno_d = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo_d = (d ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno_d /= seno_d + termo_d) then
            seno_d = seno_d + termo_d
            n = n + 1
        else
            termo_d = (d ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n)
            exit
        end if
    end do

    print *, termo_d / seno_d , seno_d

    ! Ainda tenho q descobrir como printar isso: 'Como nota-se uma excelente precisão dos valores obtidos, pode-se dizer que calcular funções trigonométricas por meio de séries é válido. Porém, para valores maiores de x, a imprecisão do limite computacional pode ficar cada vez mais pronunciada, além de não funcionar, por exemplo, para a função tangente para todo x por só poder ser escrita na forma de série para um certo intervalo.'

  end program exer02