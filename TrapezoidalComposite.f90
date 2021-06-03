MODULE functions
CONTAINS

  REAL FUNCTION f(x)
    IMPLICIT NONE
    REAL, INTENT(IN)::x
    f = 2*x
    RETURN
  END FUNCTION

END MODULE

MODULE  constants
  IMPLICIT NONE
  REAL, PARAMETER:: a=0
  REAL, PARAMETER:: b=20
  REAL, PARAMETER:: n = 100
  REAL, PARAMETER:: step=(b-a)/n
END MODULE
!------------------------------------MAIN PROGRAM-------------------------------
PROGRAM trapezoidalRule
  use constants
  use functions
  IMPLICIT NONE
  REAL:: integral
  INTEGER:: i, m=n

  integral = (0.5)*(f(a) + f(b))

  DO i=0, m
    integral = integral + f(a + step*i)

  END DO

  integral = integral*step

  PRINT *, integral

END PROGRAM trapezoidalRule
