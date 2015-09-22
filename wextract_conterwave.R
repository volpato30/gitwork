setwd("~/work/img")
library(jpeg)
windowsize=1400 # should be multipier of gap in this script
gap=100 #smaller gap, higher accuracy, more computation.
a=readJPEG('w_0.jpg',FALSE)   #a could be pretty big, like more than 100mb
dim(a) # height*width*channels
difsum<-function(te,mu){ #mu is the mean of the whole picture, which should be very close to the color of water.
    sum('^'((te-mu),2)) 
} 
difsum3<-function(a,mu) difsum(a[,,1],mu[1])+difsum(a[,,2],mu[2])+difsum(a[,,3],mu[3]) 
wextract<-function(a){
    height=dim(a)[1]
    width=dim(a)[2]
    mu=c(mean(a[,,1]),mean(a[,,2]),mean(a[,,3]))
    h=(height-1)%/%gap
    w=(width-1)%/%gap
    pos=c(0,0) #height*width, upperleft pixel of the window
    dif=0
    for(i in 1:(w-windowsize/gap)){
        for(j in 1:(h-windowsize/gap)){
            p=c((j-1)*gap+1,(i-1)*gap+1)
            ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
            temp=difsum3(ma,mu)
            if(temp>dif){
                pos=p
                dif=temp
            }
        }
        p=c(height-windowsize+1,(i-1)*gap+1)  #buttom marginal
        ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
        temp=difsum3(ma,mu)
        if(temp>dif){
            pos=p
            dif=temp
        }
    }
    
    for(i in 1:(h-windowsize/gap)){ #right marginal
        p=c((i-1)*gap+1,width-windowsize+1)
        ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
        temp=difsum3(ma,mu)
        if(temp>dif){
            pos=p
            dif=temp
        }
    }
    p=c(height-windowsize+1,width-windowsize+1) #finally, the right bottom corner
    ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
    temp=difsum3(ma,mu)
    if(temp>dif){
        pos=p
        dif=temp
    }
    print(pos)
    return(a[pos[1]:(pos[1]+windowsize-1),pos[2]:(pos[2]+windowsize-1),])
}
ex=wextract(a) 
writeJPEG(ex, target = 'w_0e.jpeg')

