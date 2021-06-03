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
      REAL, dimension(n)::Array

      INTEGER:: i

    x= a
  DO i=1, n
    call Simpson(x, dv)
    Array(i)= dv

    x= x+h

  END DO
    
END PROGRAM
    !----------------------------------SUBROUTINES---------------------------
subroutine Simpson(df)
    use functions
    use constants
    implicit NONE
    real:: x, x1, x2, df
    x1 = a + h
    x2 = a + (2*h)
    df= (h/3)*(f(a) + (4*f(x1)) + f(x2))
end subroutine Simpson

