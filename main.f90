
PROGRAM doubleIntegral

    IMPLICIT NONE
    DOUBLE PRECISION    :: a,b,c,d,y,integral,integralTemp,hx,hy
    INTEGER             :: i,j,NX,NY

    WRITE(*,*)"Aproximation of integration:"
  

    a=0.0D0  !lower value of first integral dx
    b=1.0D0  !upper value of first integral dx
    c=0.0D0  !lower value of second integral dy
    d=2.0D0  !upper value of second integral dy
    NY=14     !num. of division on y 
    NX=14     !num of divisions on x
    hx=(b-a)/(2*NX)    
    hy=(d-c)/(2*NY)

    y=c
    integral=0.0D0
    CALL integral1(NX,a,b,y,integralTemp)
    integral=integral+hy/3*integralTemp
    y=d
    CALL integral1(NX,a,b,y,integralTemp)
    integral=integral+hy/3*integralTemp

    DO j=1,2*NY-1,2
        y=c+j*hy
        CALL integral1(NY,a,b,y,integralTemp)
        integral=integral+4*hy/3*integralTemp
    END DO
    DO j=2,2*NY-2,2
        y=c+j*hy
        CALL integral1(NY,a,b,y,integralTemp)
        integral=integral+2*hy/3*integralTemp
    END DO

    WRITE(*,*)integral

    STOP

    CONTAINS

    SUBROUTINE integral1(NY,a,c,y,integral)

    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN)    :: a,c,y
    INTEGER, INTENT(IN)             :: NY
    DOUBLE PRECISION, INTENT(OUT)   :: integral
    DOUBLE PRECISION                :: h
    INTEGER                         :: i,k

    h=(c-a)/(2*NY)   ! Step size
    integral=2*h/3.0D0*0.5D0*(f(a,y)+f(c,y))
    DO  i=1,2*NY-1,2
        integral=integral+2*h/3.0D0*(2.0D0*f(a+i*h,y))
    END DO
    DO  i=2,2*NY-2,2
        integral=integral+2*h/3.0D0*f(a+i*h,y)
    END DO

    RETURN

    END SUBROUTINE integral1

    FUNCTION f(x,y) RESULT(z)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN)    :: x,y
        DOUBLE PRECISION                :: z

        !z = LOG(x+2*y)
        z = exp(-((x**2)+ (y**2)))    ! Here you put the function to be integrated. 

        RETURN
    END FUNCTION
END PROGRAM
