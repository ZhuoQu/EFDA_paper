data(Australiasmoothfertility)


order=order(Australiasmoothfertility$x)
Data=Australiasmoothfertility$y

data1<-data.frame(
    x=15:49,
    y=(Data)
)

data1_long=melt(data1,id="x")

############################################
#### Clustering ############################
############################################
library(HMClust)
library(fda.usc)
#source("HMAlgo.R")
age<-data1$x

f.age<-data1[,-1]
f.age[35,which(is.na(colSums(f.age)))]<-0


#HM Algo
ClustHM<-HMAlgo(x=age,fx=f.age,parallel=FALSE,normalize=TRUE,TVD=TRUE)
VisClust(ClustHM)

IDC_HM<-cutk(ClustHM,kg=4)
ng<-unlist(lapply(IDC_HM, length))
year.name<-unique(data1_long$variable)

O4<-IDC_HM[[4]]
O3<-IDC_HM[[3]]
O2<-IDC_HM[[2]]
IDC_HM[[3]]<-O4
IDC_HM[[2]]<-O3
IDC_HM[[4]]<-O2
data1_long$IDC_HM<-0
for(g in 1:4)data1_long$IDC_HM[!is.na(match(data1_long$variable,year.name[IDC_HM[[g]]]))]<-paste("Cluster",g)

Cplot1<-ggplot(data=data1_long,aes(x=x,y=value,group=variable))+ geom_line(aes(colour=factor(IDC_HM)))+ xlab("Age")+ylab("Fertility Rate")+
    labs(title="") +scale_colour_manual(values = c("red", "blue", "green","purple"))+facet_grid(~IDC_HM)+
    ggtitle("Clustering using Hierarchical Merger and TV distance")+
    theme(plot.title = element_text(face = "bold",size=20,hjust = 0.5),legend.position="none")

fda.data<-fdata(t(f.age),argvals = age)

set.seed(3456)
ClustKmeansLp2<-kmeans.fd(fda.data,ncl = 4, metric = metric.lp)
idxtemp<-which(ClustKmeansLp2$cluster==2)
ClustKmeansLp2$cluster[ClustKmeansLp2$cluster==4]<-2
ClustKmeansLp2$cluster[idxtemp]<-4
data1_long$IDC_KMLp2<-0
data1_long$IDC_KMLp2<-rep(paste("Cluster",ClustKmeansLp2$cluster),each=35)

Cplot2<-ggplot(data=data1_long,aes(x=x,y=value,group=variable))+ geom_line(aes(colour=factor(IDC_KMLp2)))+ xlab("Age")+ylab("Fertility Rate")+
    labs(title="") +scale_colour_manual(values = c("red", "blue", "green","purple"))+facet_grid(~IDC_KMLp2)+
    ggtitle("Clustering using Kmeans and L2 distance")+
    theme(plot.title = element_text(face = "bold",size=20,hjust = 0.5),legend.position="none")

#L1 
mdist<-metric.lp(fda.data,lp=1)
ClustH1<-hclust(as.dist(mdist), "average")
plot(ClustH1)

membH1 <- cutree(ClustH1, k = 4)
data1_long$IDC_H1<-0
data1_long$IDC_H1<-rep(paste("Cluster",membH1),each=35)

Cplot3<-ggplot(data=data1_long,aes(x=x,y=value,group=variable))+ geom_line(aes(colour=factor(IDC_H1)))+ xlab("Age")+ylab("Population")+
    labs(title="") +scale_colour_manual(values = c("red", "blue", "green","purple"))+facet_grid(~IDC_H1)+
    ggtitle("Clustering using Hierarchical Method and L1 distance")+
    theme(plot.title = element_text(face = "bold",size=20,hjust = 0.5),legend.position="none")

pdf(file="FerClust1.pdf",width = 10,height = 5)
print(Cplot1)
dev.off()

pdf(file="FerClust2.pdf",width = 10,height = 5)
print(Cplot2)
dev.off()

pdf(file="FerClust3.pdf",width = 10,height = 5)
print(Cplot3)
dev.off()
