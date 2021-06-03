program name
       implicit none
       
       real:: Loutput, w=0.5, t = 0.2, h = 0.2, x
       x =  (w-(t**2)+1)+(h/2)*(w-(t**2) + 1-(2*t))+(h**2/6)*(w-(t**2)-(2*t)-1) +(h**3/24)*(w-(t**2)-(2*t)-1)
       Print *, x
end program name