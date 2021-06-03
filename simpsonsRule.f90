MODULE functions
CONTAINS

  REAL FUNCTION f(x)
    IMPLICIT NONE
    REAL, INTENT(IN)::x
    f = EXP(x)
    RETURN
  END FUNCTION

END MODULE

MODULE  constants
  IMPLICIT NONE
  REAL, PARAMETER:: a=0
  REAL, PARAMETER:: b=4
  REAL, PARAMETER:: h=(b-a)/2
END MODULE
!------------------------------------MAIN PROGRAM-------------------------------
PROGRAM simpsonsRule
  use constants
  use functions
  IMPLICIT NONE
  REAL:: simp, x0 = a, x2 = b, x1 = a+h, t1, t2

  t1=(f(x0) + 4*f(x1) + f(x2))
  t2=(h/3)
  simp = (h/3)*(f(x0) + 4*f(x1) + f(x2))

  PRINT *, t1
  PRINT *, t2
  PRINT *, simp

END PROGRAM
