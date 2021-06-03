module functions
    implicit NONE 
contains

    real function f(x)
        implicit NONE
        real, intent(in) :: x
        f = (x**3)+(4*X**2)-10
        return
    end function

end module

program name
    Use functions
       implicit none
       
       real::p0 = 1, p1 = 10
       real:: tolerance, p2, n=0
       integer:: i, imax = 50
   
       tolerance = 0.0001
   
       Do i=1, imax
           p2 = ((p0 * f(p1) - p1 * f(p0)) /(f(p1) - f(p0)) ); 
           n = n+1
           if(abs(p2 - p1) < tolerance) then
            print *, p2, n, i
            exit
           else
            p0 = p1
            p1 = p2
           end if
          
       end do
   end program name