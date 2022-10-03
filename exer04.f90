module functions ! agrupando todas as fuunções úteis em um módulo

implicit none

contains

FUNCTION cross(a, b) ! função produto vetorial
  REAL, DIMENSION(3) :: cross
  REAL, DIMENSION(3), INTENT(IN) :: a, b

  cross(1) = a(2) * b(3) - a(3) * b(2)
  cross(2) = a(3) * b(1) - a(1) * b(3)
  cross(3) = a(1) * b(2) - a(2) * b(1)
END FUNCTION cross

FUNCTION mag(a) ! função módulo/magnitude de um vetor para calcular a área pelo produto vetorial
    real :: mag
    real, dimension(3), intent(in) :: a

    mag = (a(1) ** 2 + a(2) ** 2 + a(3) ** 2) ** 0.5
END FUNCTION mag

FUNCTION sort(val, n) ! função que ordena uma array em ordem crescente excluindo termos iguais
        integer :: i = 0, n
        real :: min_val, max_val
        real, dimension(n) :: val, unique
        real, dimension(:), allocatable :: sort
        
        min_val = minval(val)-1
        max_val = maxval(val)
        
        do while (min_val<max_val)
            i = i+1
            min_val = minval(val, mask=val>min_val)
            unique(i) = min_val
        enddo
        
        if(allocated(sort)) then
            sort = unique(1:i)
        else
            allocate(sort(i), source=unique(1:i))
        endif
    END FUNCTION sort

end module functions

PROGRAM exer04
  use functions
  IMPLICIT NONE

  REAL, DIMENSION(3) :: a, b, c, d ! vetores a serem lidos
  REAL, DIMENSION(3) :: area ! vetor área (produto vetorial)
  REAL, DIMENSION(4) :: areas ! array de áreas calculadas
  INTEGER :: i
  REAL :: vol, sum = 0 ! volume e soma de áreas

  open(1, file = 'vet_in.dat', status='old', action = 'read') ! lendo os vetores do arquivo

    read(1,*)  (a(i), i=1, 3)
    read(1,*)  (b(i), i=1, 3)
    read(1,*)  (c(i), i=1, 3)
    read(1,*)  (d(i), i=1, 3)
  
  close(1)
  ! area 1
  area = cross(a-b, a-c) / 2
  areas(1) = mag(area)
  sum = sum + mag(area)
  ! area 2
  area = cross(a-b, a-d) / 2
  areas(2) = mag(area)
  sum = sum + mag(area)
  ! area 3
  area = cross(a-c, a-d) / 2
  areas(3) = mag(area)
  sum = sum + mag(area)
  ! area 4
  area = cross(b-c, b-d) / 2
  areas(4) = mag(area)
  sum = sum + mag(area)

  vol = ABS(DOT_PRODUCT(a-b, area) / 3) ! calculando o volume com produto misto (escalar e vetorial)
  
  open(2, file = 'tetra_out.dat', status='replace') ! cria arquivo de saída
    write(2,*) vol
    write(2,*) sum
    write(2,*) sort(areas, 4) ! lista ordenada
  close(2)

END PROGRAM exer04