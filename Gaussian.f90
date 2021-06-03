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

MODULE  constants
    IMPLICIT NONE
    REAL, PARAMETER:: a=0.0001
    REAL, PARAMETER:: b= 2
    INTEGER, PARAMETER:: n=2
    real:: alpha, integral
    integer:: i
  END MODULE

program Gaussian
    Use functions
    use constants
       implicit none
       
       REAL, dimension(n)::cof
       REAL, dimension(n)::t
       
       Do i=1, n

        alpha = ((b-a)*t(i) + a + b)/2
        
        integral = integral + cof(i) * f(alpha)
          
       end do

       integral = (2*integral)/b-a
       
   end program Gaussian