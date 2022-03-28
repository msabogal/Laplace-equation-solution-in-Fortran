program lapace 
implicit none 
integer, parameter :: N=100
real*8, dimension(N,N) :: phi, phi_aux
integer :: i,j,k
real*8, parameter :: pi= 3.141592653589793D0
real*8 :: x,t1,t2
phi(:,:)= 0.D0

!Condiciones de frontera

!call cpu_time(t1)
do i=1,N
        phi(i,1)= sin( (i-1)*pi/dble(N-1) )
!        phi(i,N)= phi(i,1)
!        phi(1,i)= phi(i,1)
!        phi(N,i) =phi(i,1)
enddo 


phi(:,N) = phi(:,1)
phi(1,:) = phi(:,1)
phi(N,:) = phi(:,1)

!call cpu_time(t2)
!print*, t2-t1

!stop
x=1.D0

do while(x>1.d-8)

phi_aux(:,:) = phi(:,:)

!call cpu_time(t1)

do i= 2,N-1 
        do j= 2,N-1

                phi(i,j) = (phi_aux(i+1,j)  + phi_aux(i-1,j) +phi_aux(i,j+1) +phi_aux(i,j-1))/4.D0
        enddo
enddo 
!call cpu_time(t2)
!print *, t2-t1


!call cpu_time(t1)
!forall(i=2:N-1,j=2:N-1)
!        phi(i,j) = (phi(i+1,j)  + phi(i-1,j) +phi(i,j+1) +phi(i,j-1))/4.D0
!end forall 

!call cpu_time(t2)
!print *, t2-t1


!
!x= 0.D0

!do i= 1,N 
!        do j= 1,N 

!                x= x + abs( phi(i,j)-phi_aux(i,j) )
!        enddo 
!enddo
x = sum(abs(phi(:,:) - phi_aux(:,:)))

!print *, x

enddo

!stop

do i = 1,N
        do j= 1,N

                print*, i,j,phi(i,j)
        enddo
        print *
enddo

end program lapace 
