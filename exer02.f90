program exer02
    IMPLICIT NONE
    ! Esse é um exemplo de comentario
    integer :: fact, i, n
    real :: x = 0.1, y = 0.2, z = 0.3, w = 0.4
    real :: seno = 0
    real :: termo

    ! x = 0.1:

    n = 1

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
            exit
        end if
    end do

    print *, seno

    ! x = y = 0.2:

    n = 1
    seno = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo = (y ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno /= seno + termo) then
            seno = seno + termo
            n = n + 1
        else
            exit
        end if
    end do

    print *, seno

    ! x = z = 0.3:

    n = 1
    seno = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo = (z ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno /= seno + termo) then
            seno = seno + termo
            n = n + 1
        else
            exit
        end if
    end do

    print *, seno

    ! x = w = 0.4:

    n = 1
    seno = 0

    do
        fact = 1
        do i = 1, 2 * n - 1
            fact = fact*i
        end do
        
        termo = (w ** (2 * n - 1)) * ((-1)**(n+1))/ fact
        if (seno /= seno + termo) then
            seno = seno + termo
            n = n + 1
        else
            exit
        end if
    end do

    print *, seno

  end program exer02