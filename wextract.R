setwd("~/work/img")
library(jpeg)
windowsize=1400 # should be multipier of gap in this script
gap=100 #smaller gap, higher accuracy, more computation.
a=readJPEG('w_0.jpg',FALSE)   #a could be pretty big, like more than 100mb
dim(a) # height*width*channels
difsum<-function(te,mu) sum('^'(te-mu,2)) #comput the 'difference' inside a matrix
difsum3<-function(a,mu) difsum(a[,,1],mu)+difsum(a[,,2],mu)+difsum(a[,,3],mu) 
wextract<-function(a){
    height=dim(a)[1]
    width=dim(a)[2]
    mu=mean(a)
    h=(height-1)%/%gap
    w=(width-1)%/%gap
    pos=c(0,0) #height*width, upperleft pixel of the window
    dif=0
    for(i in 1:(w-windowsize/gap)){
        for(j in 1:(h-windowsize/gap)){
            p=c((j-1)*gap+1,(i-1)*gap+1)
            ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
            if(difsum3(ma,mu)>dif){
                pos=p
                dif=difsum3(ma,mu)
            }
        }
        p=c(height-windowsize+1,(i-1)*gap+1)  #buttom marginal
        ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
        if(difsum3(ma,mu)>dif){
            pos=p
            dif=difsum3(ma,mu)
        }
    }
    
    for(i in 1:(h-windowsize/gap)){ #right marginal
        p=c((i-1)*gap+1,width-windowsize+1)
        ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
        if(difsum3(ma,mu)>dif){
            pos=p
            dif=difsum3(ma,mu)
        }
    }
    p=c(height-windowsize+1,width-windowsize+1) #finally, the right bottom corner
    ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
    if(difsum3(ma,mu)>dif){
        pos=p
        dif=difsum3(ma,mu)
    }
    print(pos)
    return(a[pos[1]:(pos[1]+windowsize-1),pos[2]:(pos[2]+windowsize-1),])
}
ex=wextract(a) #it cost 10s on my laptop.Anyway, R, as an interpreted language, is not designed to do these loops.(hope you guys can post some faster codes :)  )
writeJPEG(ex, target = 'w_0e.jpeg')

b=readJPEG('w_3.jpg',FALSE)
ex=wextract(b) 
writeJPEG(ex, target = 'w_3e.jpeg')

?array
b=array(1,dim=c(10,10,3))
writeJPEG(b, target = 'test.jpeg')

