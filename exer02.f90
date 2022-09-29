subroutine seno_s(x, list) ! subrotina para calcular seno com precisão simples
    IMPLICIT NONE
    ! Criando as variáveis necessárias para o teste com precisão simples e precisão dupla separadamente
    REAL(4), INTENT(IN) :: x
    REAL(4), DIMENSION(2), INTENT(OUT) :: list
    INTEGER :: fact, i, n = 1 ! Parâmetros para o cálculo do fatorial
    REAL(4) :: termo, seno = 0e0, erro ! série do seno começando em 0 e temo a ser alterado e somado a cada loop

    do
        fact = 1
        do i = 1, 2 * n - 1  ! Loop para calcular o fatorial do n-ésimo número ímpar
            fact = fact*i
        end do
        
        termo = (x ** (2 * n - 1)) * ((-1)**(n+1))/ fact    !  Cria o próximo termo a ser somado na série
        if (seno /= seno + termo) then  ! Só somar se o termo adicionado alterar o valor atual
            seno = seno + termo
            n = n + 1  ! Atualiza n
        else
            termo = (x ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n) ! Calcula o termo anterior para imprimir o último que altera a série
            exit  ! Encerra o Loop caso o termo seja menor que a precisão
        end if
    end do

    erro = termo / seno
    list(1) = erro
    list(2) = seno ! retorna o seno calculado caso queira

    n = 1 ! reiniciando os parâmetros para o próximo loop
    seno = 0e0
    
    end subroutine seno_s

    subroutine seno_d(x, list) ! subrotina para calcular seno com precisão dupla
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: x
    REAL(8), DIMENSION(2), INTENT(OUT) :: list
    INTEGER :: fact, i, n = 1
    REAL(8) :: termo, seno = 0d0, erro

    do
        fact = 1
        do i = 1, 2 * n - 1 
            fact = fact*i
        end do
        
        termo = (x ** (2 * n - 1)) * ((-1)**(n+1))/ fact   
        if (seno /= seno + termo) then
            seno = seno + termo
            n = n + 1 
        else
            termo = (x ** (2 * (n - 1) - 1)) * ((-1)**((n - 1) + 1))/ (fact / n) 
            exit 
        end if
    end do

    erro = termo / seno
    list(1) = erro
    list(2) = seno

    n = 1
    seno = 0d0
    
    end subroutine seno_d

program exer02
    IMPLICIT NONE
    ! Criando as variáveis necessárias para o teste com precisão simples e precisão dupla separadamente
    integer :: fact, i, n ! Parâmetros para o cálculo do fatorial
    real(4) :: x = 0.1e0, y = 0.2e0, z = 0.3e0, w = 0.4e0 ! valores de teste
    real(4), dimension(2) :: list_s
    real(8) :: a = 0.1d0, b = 0.2d0, c = 0.3d0, d = 0.4d0
    real(8), dimension(2) :: list_d

    print *, "PRECISÃO SIMPLES"

    ! x = 0.1:

    call seno_s(x, list_s)
    print *, x, list_s(1)
    
    ! x = y = 0.2:

    call seno_s(y, list_s)
    print *, y, list_s(1)

    ! x = z = 0.3:

    call seno_s(z, list_s)
    print *, z, list_s(1)

    ! x = w = 0.4:

    call seno_s(w, list_s)
    print *, w, list_s(1)

    print *, "PRECISÃO DUPLA"

    ! x = a = 0.1:

    call seno_d(a, list_d)
    print *, a, list_d(1)

    ! x = b = 0.2:

    call seno_d(b, list_d)
    print *, b, list_d(1)

    ! x = c = 0.3:

    call seno_d(c, list_d)
    print *, c, list_d(1)

    ! x = d = 0.4:

    call seno_d(d, list_d)
    print *, d, list_d(1)

    print *, 'Conclusão:'
    print *, 'Como nota-se uma excelente precisão dos valores obtidos,'
    print *, 'pode-se dizer que calcular funções trigonométricas por meio de séries é válido.'
    print *, 'Porém, para valores maiores de x, a imprecisão do limite computacional pode'
    print *, 'ficar cada vez mais pronunciada, além de não funcionar, por exemplo, para a'
    print *, 'função tangente para todo x por só poder ser escrita na forma de série para um certo intervalo.'

  end program exer02