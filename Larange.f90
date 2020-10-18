module functions
    implicit NONE 
contains

    real function f(x)
        implicit NONE
        real, intent(in) :: x
        f = 1/x
        return
    end function

end module


program name
    Use functions
       implicit none
       
       real:: Loutput, X , a = 1, b = 2
       integer:: i, n = 3,k= 0  

   
       Do i=0, n
           if(k == i) then
            continue
           else
            Loutput = (x - f(i)) / (f(k) - f(i))
           end if
          
       end do
   end program name