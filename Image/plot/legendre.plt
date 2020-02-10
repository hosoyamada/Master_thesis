set xrange [0:pi]
#set yrange [-pi:pi]
p31(x) = (3/2)*(1-5*x**2)*(1-x**2)**(1/2)
p32(x) = 15*(1-x**2)
p33(x) = -15*(1-x**2)**(3/2)
ylm(x,y) = sqrt(35/(64*pi))*(sin(x)**3)*cos(3*y)
#splot ylm(x,y)
plot p33(cos(x)), p32(cos(x)), p31(cos(x))
pause -1
