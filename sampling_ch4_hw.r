#15题数据
x1=c(162,167,191,181,163,147,168,138,167,159)
y1=c(114,119,132,128,115,105,120,100,115,110)
x2=c(132,135,102,172,127,139,147,134,199,136)
y2=c(85,86,65,110,85,90,95,86,130,90)
x=c(x1,x2)
y=c(y1,y2)
mean(x)
mean(y)
(sx=sqrt(var(x)))
(sy=sqrt(var(y)))
(sxy=cov(x,y))
n1=10
n2=10
w1=90/176
w2=86/176
N=176
f=n/N
#xbar=X/N
xbar1=169.3
xbar2=140.2
R=mean(y)/mean(x)
(b=sxy/sx^2)

#比率估计
#R*xbar

#z为比率估计标准差的计算式
z=quote(sqrt((1-f)/n*(sy^2+R^2*sx^2-2*R*sxy)))
eval(z)

#分别比率估计
w1*mean(y1)/mean(x1)*xbar1+w2*mean(y2)/mean(x2)*xbar2

#联合比率估计
(w1*mean(y1)+w2*mean(y2))/(w1*mean(x1)+w2*mean(x2))*(xbar1*90+xbar2*86)/176

#16题数据
x1=c(10,8,25,16,19,12)
y1=c(590,480,1400,960,1200,710)
x2=c(45,40,48,64,75,60,70,65,55,90,80,88)
y2=c(3500,3200,4100,6500,5800,4500,5800,5400,4300,7500,6500,7100)
x=c(x1,x2)
y=c(y1,y2)
mean(x)
mean(y)
(sx1=sqrt(var(x1)))
(sy1=sqrt(var(y1)))
(sxy1=cov(x1,y1))
(sx2=sqrt(var(x2)))
(sy2=sqrt(var(y2)))
(sxy2=cov(x2,y2))
n1=6
n2=12
N1=600
N2=1200
N=1800
w1=N1/N
w2=N2/N
R=mean(y)/mean(x)
(b=sxy/sx^2)

#总体的回归估计
#N*(mean(y)+b*(xbar-mean(x)))

#v为回归估计标准差计算式
#v=quote(sqrt((1-f)/n*(n-1)/(n-2)*(sy^2-b^2*sx^2)))
#1.96*N*eval(v)

#分别回归估计
w1*(mean(y1)+sxy1/sx1^2*( ))
