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
    
    real::x, a ,b 
    real:: step, tolerance, n = 0;
    integer:: i

    step = 1E-1
    tolerance = 1E-4
    x = 0.0
    a= x
    b = a + step

    Do while (abs(f(b) - 0.0) .gt. tolerance)
        n = n + 1
        if (f(a)*f(b) < 0) then
            a = a
            b = b +(-step/2)
            
        else 
            a = b
            b = b + step
        end if 
    end DO
    print *, b, n
end program name