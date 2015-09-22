setwd("~/work/img/whaledetection/train")
library(MASS)
library(kernlab)
library(jpeg)
library(snow)
list.files(pattern ='w_[0-9][0-9]?_[a,b,a1,b1].jpg')
ma_w=matrix(nrow=0,ncol=3)
for(file in list.files(pattern ='w_[0-9][0-9]?_[a,b,a1,b1].jpg')){
    a=readJPEG(file,FALSE)
    d=dim(a)
    a=array(a,c(d[1]*d[2],3))
    ma_w=rbind(ma_w,a)
}
rm(a)
ma_o=matrix(nrow=0,ncol=3)
for(file in list.files(pattern ='w_[0-9][0-9]?_[c,c1].jpg')){
    a=readJPEG(file,FALSE)
    d=dim(a)
    a=array(a,c(d[1]*d[2],3))
    ma_o=rbind(ma_o,a)
}
rm(a)

s=matrix(1,nrow=500,ncol=3)
w=ma_w[sample(1:nrow(ma_w),500),]
o=ma_o[sample(1:nrow(ma_o),5000),]
X=rbind(w,o,s)
X=data.frame(X)
class=c(rep(1,nrow(w)),rep(2,nrow(o)),rep(3,nrow(s)))#1 for whale, 2 for ocean, 3 for spray.
class=as.factor(class)
X=cbind(X,class)
colnames(X)
fit=ksvm(class ~ .,X,kernal='vanilladot')

sum(predict(fit,ma_w)==1)/nrow(ma_w)  #about 6/7 of whale's pixel are correctly classified. 

lapply(list(ma_w[1:20,],ma_w[21:40,],ma_w[41:60,],ma_w[61:71,]), sumw,fit)

# sumw=function(vect,fitm) sum(predict(fitm,vect)==1)
# cl=makeCluster(4, type="SOCK")
# nrow(ma_w)
# clusterApply(cl, list(ma_w[1:200000,],ma_w[200001:400000,],ma_w[400001:600000,],ma_w[600001:717705,]), sumw,fit)     why doesn't work

#sum(predict(fit,ma_o)==2) #should paralell this.
setwd("~/work/img/")
windowsize=1400 # should be multipier of gap in this script
gap=100
a=readJPEG('w_37.jpg',FALSE)   #a could be pretty big, like more than 100mb
dim(a) # height*width*channels
difsum<-function(te) sum(predict(fit,te)==1) 
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
            ma=data.frame(array(ma,c(windowsize^2,3)))
            temp=difsum(ma)
            if(temp>dif){
                pos=p
                dif=temp
            }
        }
        p=c(height-windowsize+1,(i-1)*gap+1)  #buttom marginal
        ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
        ma=data.frame(array(ma,c(windowsize^2,3)))
        temp=difsum(ma)
        if(temp>dif){
            pos=p
            dif=temp
        }
    }
    
    for(i in 1:(h-windowsize/gap)){ #right marginal
        p=c((i-1)*gap+1,width-windowsize+1)
        ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
        ma=data.frame(array(ma,c(windowsize^2,3)))
        temp=difsum(ma)
        if(temp>dif){
            pos=p
            dif=temp
        }
    }
    p=c(height-windowsize+1,width-windowsize+1) #finally, the right bottom corner
    ma=a[p[1]:(p[1]+windowsize-1),p[2]:(p[2]+windowsize-1),]
    ma=data.frame(array(ma,c(windowsize^2,3)))
    temp=difsum(ma)
    if(temp>dif){
        pos=p
        dif=temp
    }
    print(pos)
    return(a[pos[1]:(pos[1]+windowsize-1),pos[2]:(pos[2]+windowsize-1),])
}
ex=wextract(a) 
writeJPEG(ex, target = 'w_37es.jpeg')

