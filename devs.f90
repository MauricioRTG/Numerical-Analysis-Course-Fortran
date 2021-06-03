MODULE functions
CONTAINS

  REAL FUNCTION f(x)
    IMPLICIT NONE
    REAL, INTENT(IN)::x
    f = log(x)
    RETURN
  END FUNCTION

END MODULE

MODULE  constants
  IMPLICIT NONE
  REAL, PARAMETER:: a=0.0001
  REAL, PARAMETER:: b= 2
  INTEGER, PARAMETER:: n=1000
  REAL, PARAMETER:: h=(b-a)/n
END MODULE

PROGRAM derivatives
  use constants
  IMPLICIT NONE
  
  REAL:: dv, x
  REAL, dimension(n)::twoPointFArray
  REAL, dimension(n)::twoPointBArray
  REAL, dimension(n)::threeEndPointArray
  REAL, dimension(n)::threeMidPointArray
  REAL, dimension(n)::fiveEndPointArray
  REAL, dimension(n):: fiveMidPointArray
  INTEGER:: i
 
  open(7, file='datosTwoPointFoward.txt')
  open(8, file='datosTwoPointBackard.txt')
  open(9, file='datosThreeEndpoint.txt')
  open(10, file='datosThreePointMid.txt')
  open(11, file='datosFivePointEnd.txt')
  open(12, file='datosFivePointMid.txt')
  
 

  x= a
  DO i=1, n
    call twoPointF(x, dv)
    twoPointFArray(i)= dv

    call twoPointB(x, dv)
    twoPointBArray(i)=dv

    call threeEndPoint(x, dv)
    threeEndPointArray(i)=dv

    call threeMidPoint(x, dv)
    threeMidPointArray(i)=dv

    call fiveEndPoint(x, dv)
    fiveEndPointArray(i)=dv

    call fiveMidPoint(x, dv)
    fiveMidPointArray(i)= dv

    write(7,*) x, twoPointFArray(i)
    write(8,*) x, twoPointBArray(i)
    write(9,*) x, threeEndPointArray(i)
    write(10,*) x, threeMidPointArray(i)
    write(11,*) x, fiveEndPointArray(i)
    write(12,*) x, fiveMidPointArray(i)

    x= x+h

  END DO


END PROGRAM
!----------------------------------SUBROUTINES---------------------------
subroutine twoPointF(x, df)
  use functions
  use constants
  implicit NONE
  real:: x, df
  df= (f(x+h)-f(x))/h
end subroutine twoPointF

subroutine twoPointB(x, df)
  use functions
  use constants
  implicit NONE
  real:: x, df
  df= (f(x)-f(x-h))/h
end subroutine twoPointB

subroutine threeEndPoint(x, df)
  use functions
  use constants
  implicit NONE
  real:: x, df
  df= (1/(2*h))*(-3*f(x)+4*f(x+h)-f(x+2*h))
end subroutine threeEndPoint

subroutine threeMidPoint(x, df)
  use functions
  use constants
  implicit NONE
  real:: x, df
  df= (1/(2*h))*(f(x+h)-f(x-h))
end subroutine threeMidPoint

subroutine fiveEndPoint(x, df)
  use functions
  use constants
  implicit NONE
  real:: x, df
  df= (1/(12*h))*(-25*f(x)+48*f(x+h)-36*f(x+2*h)+16*f(x+3*h)-3*f(x+4*h))
end subroutine fiveEndPoint


subroutine fiveMidPoint(x, df)
  use functions
  use constants
  implicit NONE
  real:: x, df
  df= (1/(12*h))*(f(x-2*h)-8*f(x-h)+8*f(x+h)-f(x+2*h))
end subroutine fiveMidPoint
