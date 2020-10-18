module functions
    implicit NONE 
contains

    real function g(x)
        implicit NONE
        real, intent(in) :: x
        g = sqrt(10/(4+x))
        return
    end function

end module

program name
    Use functions
       implicit none
       
       real::p0 = 1
       real:: tolerance, p, n=0
       integer:: i, imax = 50   
       tolerance = 0.0001
   
       Do i=1, imax
           p = g(p0)
           print *, p
           n = n+1
           if(abs(p -p0) < tolerance) then
            print *, p, n
            exit
           else
            p0 = p
           end if
          
       end do
   end program name