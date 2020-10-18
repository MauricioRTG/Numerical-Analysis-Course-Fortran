module functions
    implicit NONE 
contains

    real function g(x)
        implicit NONE
        real, intent(in) :: x
        g = (x**3)+(4*X**2)-10
        return
    end function

    real function gder(x)
        implicit NONE
        real, intent(in) :: x
        gder = (3*x**2)+(8*X)
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
           p = p0 - g(p0)/gder(p0)
           n = n + 1 
           if(abs(p -p0) < tolerance) then
            print *, p, n
            exit
           else
            p0 = p
           end if
          
       end do
   end program name