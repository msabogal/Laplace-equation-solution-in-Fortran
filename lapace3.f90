program lapace3 
!Solucion de la ecuacion de Lapace en 3-D de cubo de lados con tamaño 'a'

implicit none 

integer, parameter :: N=20 !Numero de particiones
integer :: i,j,k,l  !Contadores      
real*8, dimension(N,N,N) :: phi, phi_aux ! Potencial a determinar 
real*8, parameter :: pi= 3.141592653589793D0
real*8 :: x  !Variable de convergencia

phi(:,:,:)= 0.D0

!Condiciones de frontera

!Para la superficie (x,y,0)

do i=1,N
        do j=1,N
                 phi(i,j,1)= sin((i-1)*pi/dble(N-1)) * sin((j-1)*pi/dble(N-1)) 
        enddo 
enddo

phi(:,:,N) = phi(:,:,1) !(x,y,a)
phi(1,:,:) = phi(:,:,1) !(0,y,z)
phi(N,:,:) = phi(:,:,1) !(a,y,z)
phi(:,1,:) = phi(:,:,1) !(x,0,z)
phi(:,N,:) = phi(:,:,1) !(x,a,z)


x=1.D0 ; l=0 !Asigancion de valores inicales

do while(x>1.d-8) !Criterio de convergencia

phi_aux(:,:,:) = phi(:,:,:)


 do i= 2,N-1 !Solucion de la ecuacion, encontrando phi 
        do j= 2,N-1
                do k= 2,N-1
                  phi(i,j,k) = (phi_aux(i+1,j,k)  + phi_aux(i-1,j,k) +phi_aux(i,j+1,k) +phi_aux(i,j-1,k) + phi_aux(i,j,k+1) + phi_aux(i,j,k-1) )/6.D0
                enddo        
        enddo
 enddo 

x = sum( abs( phi(:,:,:) - phi_aux(:,:,:) ) ) !Variable de convergencia 

l = l +1 ! contador de iteraciones

print *,l,x !Para graficar variable de convergencia Vs numero de iteraciones en Gnuplot con -> plot 'datos.dat'

enddo

stop !Para Graficar en 4D utilizando en Gnuplot -> splot 'datos.dat' u 1:2:3:4 with points linecolor palette
do i = 1,N
        do j= 1,N
                do k=1,N
                        print*, i,j,k,phi(i,j,k)
                enddo        
        enddo
        print *
enddo

end program lapace3
