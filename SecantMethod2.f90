module functions
    implicit NONE 
contains

    real function f(x)
        implicit NONE
        real, intent(in) :: x
        !real:: a, z
        f = (x**3)+(4*X**2)-10
        !z = x/(1 - x + x**2)
        !a=  z**21
        !f = (1+x)*a/2
        !f= -1564000 + 1000000*exp(x) + ((435000)*(exp(x)-1))/x
        return
    end function

end module

program name
    Use functions
       implicit none
       
       real::p0 = 1, p1 = 2
       real:: tolerance, p2, n=0, q1, q0
       integer:: i, imax = 50
   
       tolerance = 0.0001
       q0 = f(p0)
       q1 = f(p1)
   
       Do i=1, imax
           p2 = p1 - q1*(p1-p0)/(q1-q0) 
           if(abs(p2 - p1) < tolerance) then
            print *, p2, i
            exit
           else
            p0 = p1
            q0 =q1
            p1 = p2
            q1 = f(p2)
           end if
          
       end do
   end program name