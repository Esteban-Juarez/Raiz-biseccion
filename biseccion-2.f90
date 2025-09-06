program punto_medio

! Segunda version del codigo sin la variable imax
! Grafica la función para visualizar las raices y con esta información determinar el intervalo donde será aplicado el método de la bisección.
! Tambien distingue cuales de tus raices tiene un significado para tu problema.

implicit none

integer :: icount       ! Contador
real    :: a            ! Frontera izquierda
real    :: b            ! Frontera derecha
real    :: m            ! Punto medio del intervalo
real    :: eps          ! Tolerancia

! Entrada de datos 
open(100, file = "input_data.inp")
        read(100,*)
        read(100,*) a, b, eps
close(100)

! Inicializacon del contador
icount = 0

! Inicializacion del ciclo
do while (b-a > eps)
        m = (b + a) / 2.0       ! Punto medio
        write(*,*)icount, m
        ! Raiz o tolerancia alcanzada
        if (f(m) == 0.0)then
                write(*,20) "Contador", "Raiz"
                write(*,21) icount, m
                stop
        elseif(f(a)*f(m) < 0.0)then     ! Nuevo subintervalo (a, m)
                b = m 
        else 
                a = m                   ! Nuevo subintervalo (m, b)
        end if
        icount = icount + 1
end do

! No se alcanza la convergencia

20 format(2A15)
21 format(1I15, 1f15.8)

contains 

        function f(x)
                implicit none
                real :: x       
                real :: f
                f = x**2 - 5.0
        end function f

end program punto_medio
