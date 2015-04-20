FN=13
alpha=10
mi=c(0.19,0.2,0.21)
Fi=c(0.9,1,1.1)
ai=c(-5,0,5)
y=quote(1/9.8*(FN/m)^2*sin(2*alpha))
o=function(FN,alpha,m) 1/9.8*(FN/m)^2*sin(2*alpha/360*2*pi)
y1=c(
o(FN*Fi[1],(alpha+ai)[1],mi[1]),
o(FN*Fi[2],(alpha+ai)[2],mi[1]),
o(FN*Fi[3],(alpha+ai)[3],mi[1]),
o(FN*Fi[1],(alpha+ai)[2],mi[2]),
o(FN*Fi[2],(alpha+ai)[3],mi[2]),
o(FN*Fi[3],(alpha+ai)[1],mi[2]),
o(FN*Fi[1],(alpha+ai)[3],mi[3]),
o(FN*Fi[2],(alpha+ai)[1],mi[3]),
o(FN*Fi[3],(alpha+ai)[2],mi[3])
)
options(digits=3)
y1
mean(y1)
var(y1)
10*log10((mean(y1)^2-var(y1)/9)/var(y1))

6.09