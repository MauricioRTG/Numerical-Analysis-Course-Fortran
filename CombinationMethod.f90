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

module Methods
    implicit NONE 
contains

    real function Newton(p0)
        Use functions
        real, intent(inout)::p0 
        real:: tolerance, p, n=0
        integer:: i, imax = 50

        tolerance = 0.0001

        Do i=1, imax
            Newton = p0 - g(p0)/gder(p0)
            !print*, Newton, p0
            n = n + 1 
            if(abs(Newton -p0) < tolerance) then
                print*, Newton
                return
                exit
            else
            p0 = Newton
            end if
        
        end do
    end function

    real function Bisection(a,b)
        Use functions
        real, intent(inout)::a ,b
        real:: tolerance, p, n= 0
        integer:: i, imax = 50

        tolerance = 0.00001
        !tolerance = 0.01
        Do i=1, imax
            Bisection = a + (b-a)/2
            !print *, p;
            !print *, f(p)
            if(abs(g(Bisection) -0) < tolerance .or. (b-a)/2 <tolerance) then
                print *, i , Bisection
                EXIT
            end if
            if(g(a)*g(Bisection)<0) then
                b = Bisection
                !print *, "negativo"
            end if
            if(g(a)*g(Bisection)>0) then
                a = Bisection
                !print *, "positivo"
            end if
            !print *, i
        end do
    end function
end module

program name
    Use functions
    Use Methods
       implicit none
       
       real :: ptotal, a = 1, b = 30, pbisection
        pbisection = Bisection(a,b)
        ptotal = Newton(pbisection)

        print *, ptotal
       
   end program name