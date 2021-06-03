program Combiantional_coefficient
    implicit none
        integer, parameter:: m = 5
        integer, dimension(m):: combinational1

        call combinational(m, combinational1)
        
end program Combiantional_coefficient

subroutine combinational(coeficiente, combinational1)
    integer, intent(in):: coeficiente
    integer, dimension(coeficiente), intent(out):: combinational1
    integer::j
    integer:: factorial
    
   iloop: do j= 0,coeficiente
            combinational1(j)= factorial(5)/(factorial(j)*factorial(5-j))
            Print *, combinational1(j)
    end do iloop
    
end subroutine combinational

recursive function factorial(number) result(fact)
    integer, intent(in):: number
    integer :: fact
    if (number == 0) then
        fact = 1
    else
        fact = number * factorial(number-1)
    end if

end function factorial