module functions
    implicit NONE 
contains

    real function f(x)
        implicit NONE
        real, intent(in) :: x
        !f= -1564000 + 1000000*exp(x) + ((435000)*(exp(x)-1))/x
        f = sqrt(x) - cos(x)
        return
    end function

end module

program name
 Use functions
    implicit none
    
    real::a=0 ,b= 1
    real:: tolerance, p
    integer:: i, imax = 4

    tolerance = 0.0001
    Do i=1, imax
        p = a + (b-a)/2
        print *, i, f(p)
        if(abs(f(p) -0) < tolerance .or. (b-a)/2 <tolerance) then
            print *, i , p
            EXIT
        end if
        if(f(a)*f(p)<0) then
            b = p
            !print *, "negativo"
        end if
        if(f(a)*f(p)>0) then
            a = p
            !print *, "positivo"
        end if
    end do
end program name
