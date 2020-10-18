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
       
        real:: Loutput, x0 = 1.8 ,h= 0.1


        Loutput = (f(x0+h) - f(x0)) / h
        print *, Loutput
           
          
       
   end program name