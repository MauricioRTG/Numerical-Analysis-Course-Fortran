MODULE functiones
    implicit none
    contains

        real function f(x,y)
            implicit none
            real, intent (in) :: x,y
            f = x*y
            return
        end function

end MODULE

MODULE integral
  use functiones
  implicit none
  contains

      subroutine simpson_doble(a,b,c,d)
        REAL, intent(INOUT)::a,b,c,d
        real::n=2,m=2
        REAL:: h,k,res,tem1,tem2,peso

        real, dimension(3)::x_asys
        real, dimension(3)::y_asys


        x_asys(1)=a
        x_asys(2)=(b+a)/2
        x_asys(3)=b

        y_asys(1)=c
        y_asys(2)=(d+c)/2
        y_asys(3)=d


        h=(b-a)/2
        k=(d-c)/2
        peso=(k*h)/9
        write(*,*)'el resultado es x = ',peso

        res=1*(f(x_asys(1),y_asys(1)))  +4*(f(x_asys(2),y_asys(1)))+  1*(f(x_asys(3),y_asys(1)))
        res=res+4*(f(x_asys(1),y_asys(2)))+  16*(f(x_asys(2),y_asys(2)))+4*(f(x_asys(3),y_asys(2)))
        res=res+1*(f(x_asys(1),y_asys(3)))+   4*(f(x_asys(2),y_asys(3)))+   1*(f(x_asys(3),y_asys(3)))

      

        res=res*peso
        write(*,*)'el resultado es x = ',res
        return
      end subroutine

end MODULE

PROGRAM main
    USE integral
    IMPLICIT NONE
      REAL::a,b,c,d
      a=1
      b=2
      c=1
      d=2

      write(*,*)'la integral de x^2 '

      call simpson_doble(a,b,c,d)



END PROGRAM
