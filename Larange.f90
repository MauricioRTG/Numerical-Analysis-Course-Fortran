module functions
    implicit NONE 
contains

    real function f(x)
        implicit NONE
        real, intent(in) :: x
        f = x**6 - x**2*sin(2*x)
        return
    end function

end module

MODULE  constants
    IMPLICIT NONE
    REAL, PARAMETER:: a=1
    REAL, PARAMETER:: b= 2
    INTEGER, PARAMETER:: n=2
    real:: alpha, integral
    integer:: i, j
    REAL, PARAMETER:: h=(b-a)/n 
  END MODULE

program name
    Use functions
    use constants
       implicit none
       
       real:: Loutput, x, x0 
       
    Do i=0, n
        x = x0 + h
           
        DO j = 0, n
            if(j == i) then
                continue
               else
                Loutput = Loutput * (x - x(j)) / (x(i) - x(j))
               end if 
        end do    
    end do
end program name