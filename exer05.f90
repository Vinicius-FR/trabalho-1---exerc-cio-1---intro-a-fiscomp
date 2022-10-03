module function
implicit none
contains

FUNCTION dot(x, y, n)
integer :: n, i
real, dimension(n, 1), intent(in) :: x, y
real :: dot

dot = 0
do i = 1, n

   dot = dot + x(i, 1) * y(i, 1)

end do

END FUNCTION

end module function

program exer05
   use function
   implicit none
   integer, parameter :: dp = kind(0.d0)
   integer :: i, j, n
   real (kind = dp), allocatable, dimension(:,:) :: M !matriz
   real, dimension(:,:), allocatable :: x, y
   real :: lambda = 0, erro
   
   ! lendo o erro, tamanho da matriz e a matriz

   print *, "Digite o erro:\n"
   read*, erro

   print *, "Digite a dimensão da matriz:\n"
   read*, n
   allocate(M(n,n), x(n,1), y(n,1))

   print*, "Digite as linhas da matriz:\n"
   do i = 1, n
      read(*, *) (M(i,j), j = 1, n)
   end do

   ! gerando os vetores testes x e y
   
   do i = 1, n
      x(i, 1) = 1
      y(i, 1) = 1
   end do

   ! loop para determinar o autovalor
   
   do
   y = matmul(M, x)

      if(abs(lambda - dot(x, y, 3) / dot(x, x, 3)) > erro) then
         lambda = dot(x, y, 3) / dot(x, x, 3)
         x = y
      else
         exit
      end if
   
   end do
   
   print*, 'autovalor =', lambda

   print*, 'autovetor =', x

end program exer05