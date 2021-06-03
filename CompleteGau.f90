MODULE functions
    CONTAINS
    
  REAL FUNCTION f(x)
    IMPLICIT NONE
    REAL, INTENT(IN)::x
    f = exp(x)*cos(x);
    RETURN
  END FUNCTION

  REAL FUNCTION Legandre(x)
    IMPLICIT NONE
    REAL, INTENT(IN)::x
    Legandre = (x**3) - ((3*x)/5)
    RETURN
  END FUNCTION

  REAL FUNCTION Lagrange(abc,roots,index)
    use constants
    IMPLICIT NONE
    REAL, INTENT(IN):: abc
    REAL, dimension(n), intent(in) :: roots
    INTEGER, INTENT(IN)::index
    REAL :: Loutput
    INTEGER :: j
    Loutput  = 1

    DO j = 1, n
      if(j .ne. index) then 
        Loutput = Loutput * ((abc - roots(j)) / (roots(index) - roots(j)))
        !print *, j, abc, roots(index), roots(j), Loutput

      end if 
    end do    
    Lagrange = Loutput
    RETURN
  END FUNCTION

END MODULE

MODULE  constants
  IMPLICIT NONE
  REAL, PARAMETER:: a= -1
  REAL, PARAMETER:: b= 0
  REAL, PARAMETER:: c= 1
  INTEGER, PARAMETER:: n=3
  REAL, PARAMETER:: h=(c-a)/2
      
END MODULE
    
PROGRAM derivatives
  use constants
  use functions
  IMPLICIT NONE

  INTEGER :: i
  REAL :: integral =  0
  REAL, dimension(n) :: x, coef

  !x(1) = 0
  !x(2) = 0.774
  !x(3) = -0.774
  
  call Bisection(x)
  !call Secant(x)

  call Simpson(coef, x)
  
  print*, x(1), x(2), x(3)

  Do i=1, n

    integral = integral + (coef(i) * f(x(i)))
    
  end do

   !Print *, coef
   !Print *, coef(3)
   print *, integral
      
END PROGRAM
    !----------------------------------SUBROUTINES---------------------------
  
subroutine Simpson(coeficients,roots)
    use functions
    use constants
    implicit NONE
    REAL, dimension(n), intent(in) :: roots
    integer:: i
    REAL,dimension(n) :: coeficients
    REAL :: secondPart, firstPart, thirdPart

    Do i=1, n
        firstPart = Lagrange(a,roots,i)
        secondPart = (4*Lagrange(b,roots,i))
        thirdPart = Lagrange(c,roots,i)
        coeficients(i) = (h/3)*( (firstPart) + (secondPart) + (ThirdPart)) 
        !if(i == 1)then
          !print *, firstPart
          !print *, secondPart
          !print *, thirdPart   
        !end if 
        
    end Do
    
end subroutine Simpson


subroutine Secant(roots)
  use functions
  use constants
    real::p0 = -1, p1, k = 1, step, saveb, savea
    real:: tolerance, p2, q1, q0
    integer:: i, imax = 50, rootcount
    REAL,dimension(n) :: roots

    rootcount = 1
    tolerance = 0.0001
    step=(k-a)/n
    p1 = p0 + step
    saveb = p1
    savea = p0

    do while (p1 <= k)
      q0 = Legandre(p0)
      q1 = Legandre(p1)
      
      Do i=1, imax
        p2 = p1 - q1*(p1-p0)/(q1-q0) 
        if(abs(p2 - p1) < tolerance) then
          !print *, p2, i
          if( p2 < 0.000001 .and. -0.000001 < p2) then
            roots(rootcount) = 0
            rootcount = rootcount + 1
            exit
          else
            roots(rootcount) = p2
            rootcount = rootcount + 1
            exit
          end if
        else
          p0 = p1
          q0 =q1
          p1 = p2
          q1 = Legandre(p2)
        end if
            
      end do
      p1 = saveb
      p0 = p1
      p1 = p1 + step
      saveb = p1
    end do
  print*, roots
end subroutine secant

subroutine Bisection(roots)
  use functions
  use constants
  implicit none

  real::a1=-1 ,b1, k = 1
  real:: tolerance, p, step, saveb, savea
  integer:: i, imax = 50, rootcount
  REAL,dimension(n) :: roots

  rootcount = 1
  step=(k-a)/n
  b1 = a + step
  saveb = b1
  savea = a1
  !print *, b1, saveb
  tolerance = 0.0001

  do while (b1 <= k)
      Do i=1, imax

          p = a1 + (b1-a1)/2
          !print *, i, f(p)
          if(abs(Legandre(p) -0) < tolerance .or. (b1-a1)/2 <tolerance) then
            if( p < 0.000001 .and. -0.000001 < p) then
              roots(rootcount) = 0
              rootcount = rootcount + 1
              exit
            else
              roots(rootcount) = p
              rootcount = rootcount + 1
              exit
            end if
          end if
          if(Legandre(a1)*Legandre(p)<0) then
              b1 = p
              !print *, "negativo"
          end if
          if(Legandre(a1)*Legandre(p)>0) then
              a1 = p
              !print *, "positivo"   
          end if    
      end do
      b1 = saveb
      a1 = b1
      b1 = b1 + step
      saveb = b1
      
      !print *, b1 , a1
  end do
end subroutine Bisection