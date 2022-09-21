program exer03
   implicit none

   integer :: M, n, i = 3  ! Criando os parâmetros para os loops
   logical :: primo = .true.  ! Parâmetro auxiliar booleano para determinar se n é primo (.true.) ou não (.false)

   print *, "Digite um número inteiro:\n"
   read*, M
		
   !Cria ou reescreve um arquivo primos_out.dat, dando o apelido 1
   open(1,file = 'primos_out.dat', status='replace')
   
   if (M == 1) then  ! Como começaremos de n = 3, precisamos incluir os casos M = 1 e M = 2
        write (1, *) 1
   else
        write (1, *) 1
        write (1, *) 2

        do n = 3, M, 2  ! Loop só com os ímpares a partir de 3
                do while (i <= sqrt(float(n)))  ! Teste de Primalidade. Basta testar até raiz de n para garantir que n é primo

                    if (mod(n, i) == 0) then   ! Se o resto da divisão n / i for 0, n possui outros divisores, logo não é primo
                        primo = .false.
                        i = n   ! Para encerrar o primeiro loop
                    else
                        i = i + 2   ! Testando o próximo ímpar, pois n é ímpar
                    end if

                end do

                if (primo) then   ! Se n for primo, escrevê-lo no arquivo
                    write(1,*) n
                end if

                i = 3   ! Reiniciando os parâmetros para o próximo loop
                primo = .true.
        end do
    end if

    close(1)  ! Fechano o arquivo

end program exer03