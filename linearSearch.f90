!Busca las raices de polinomios

module functions
    implicit NONE 
contains

    real function f(x)
        implicit NONE
        real, intent(in) :: x
        !f = ((63*(x**5)) - (70*(x**3)) + (15*x))/8
        
        return
    end function

end module

program name
 Use functions
    implicit none

    real::a=-1 ,b, k = 1
    real:: tolerance, p, h, saveb, savea
    integer:: i, imax = 50

    h=(k-a)/5
    b = a + h
    saveb = b
    savea = a
    print *, b, saveb
    tolerance = 0.0001

    do while (b <= k)
        Do i=1, imax

            p = a + (b-a)/2
            !print *, i, f(p)
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
        b = saveb
        a = b
        b = b + h
        saveb = b
        
        print *, b , a 
    end do
end program name