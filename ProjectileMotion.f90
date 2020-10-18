program Parabola
    implicit none
        real, parameter::InitialVelocity = 10, Gravity= -9.81, pi= 3.1415927, InitialHeight = 1.12
        real::time, VxInitial,VyInitial, InitialAngle = 45, x, y, FinalVelocity, VxFinal, VyFinal, FinalAngle
        
        Write(*,*) "What is the time?"
        READ(*,*) time
        InitialAngle = InitialAngle * (pi/180)

        VxInitial = InitialVelocity*cos(0.77)
        VyInitial = InitialVelocity*sin(0.77)

        y = InitialHeight + (VyInitial*time) + (0.5*Gravity*time)
        x = VxInitial*time
        VxFinal = VxInitial
        VyFinal = VyInitial + (Gravity*time)
        FinalVelocity = sqrt(VyFinal**2 + VxFinal**2)
        FinalAngle = atan(VyFinal/VxFinal)

        Write(*,*) "X= ", x
        Write(*,*) "Y= ", y
        Write(*,*) "Vx= ", VxFinal
        Write(*,*) "Vy= ", VyFinal
        Write(*,*) "Magnitud de Velocidad= ", FinalVelocity
        Write(*,*) "Angle ", FinalAngle


end program Parabola