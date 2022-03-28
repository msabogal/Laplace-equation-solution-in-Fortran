program model 

implicit none 

integer :: i,j,k,parejas,posicion,first,rp

integer :: N=100

integer, dimension(:), allocatable :: poblacion

real :: x

x= 0.5

rp = 3

do k=1,100

allocate(poblacion(N))

do i=1,N
        call random_number(x)
        if (x <0.5) then 
                poblacion(i)=0
        else
                poblacion(i) = 1
        endif        
        
enddo 

!poblacion(1)=1
!print *, (poblacion(i) ,i=1,N)

!print *, '----------------------------------------------'

!0=hombre, 1=mujer 
parejas=0
j=1 
first=0
do while(j<N)

        if(poblacion(j)==1) then 
                if(poblacion(j+1) ==0) then 
                        parejas = parejas +1 
                        j = j+2

                else
                        j= j +1     
                endif 
        else 
                if(poblacion(j+1) ==1) then 
                        parejas = parejas +1 
                        j = j+2

                else
                        j= j +1     
                endif        
    
        endif 
        
        if(poblacion(1)+poblacion(2)==1) then  
                first= 1
        endif

        if((first==0).and.(j==N)) then 
                if (poblacion(1) + poblacion(N) ==1) then
                        parejas= parejas +1
                endif
        endif        

enddo

!print *, parejas 
if(parejas==0) then 
        stop
endif         
N = parejas*rp

deallocate(poblacion)

print *,k,N
enddo 



!posicion=0


!do i=1,N,2

!if (posicion/=i) then 

!j= poblacion(i)
!k= poblacion(i+1)

!if (j/=k) then 

!        parejas = parejas + 1

!else 

!        if (poblacion(i+2)/=k) then 
!                 parejas = parejas + 1
!                 posicion= i+2
!        endif
        

!endif 


!else 



!endif 

!enddo 


end program model 
