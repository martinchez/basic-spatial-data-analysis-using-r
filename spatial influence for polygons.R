library(raster)
#loading the shapefile
p<- shapefile(system.file("external/lux.shp", package = "raster"))

#To find adjacent polygons, we can use the spdep package.
library(spdep)

#We use poly2nb to create a “rook’s case” neighbors-list.
#And from that a neighbors matrix.

wr<- poly2nb(p,row.names=p$ID_2,queen=FALSE)
wr

wm<- nb2mat(wr,style="B",zero.policy=TRUE)
dim(wm)

#inspecting wr & wm
wr[1:6]

wm[1:6,1:11]

####computing the numbers for neighbours for each areas
i<- rowSums(wm)
i
#expressing  as percentages the areas
round(100*table(i)/length(i),1)

#plotting the links between polygons
par(mai=c(0,0,0,0))
plot(p,col='gray',border='blue')
xy<- coordinates(p)
plot(wr,xy,col='red',lwd=2,add=TRUE)


#some alternative approaches to compute spatial influence

#ditance computation using euclidean distance between the lowenst and the highest
wd10<- dnearneigh(xy,0,10)#identifies the by euclidean distance between the lowenst and the highest points.

wd25<- dnearneigh(xy,0,25,longlat = TRUE)

#nearest neightbours
k3<- knn2nb(knearneigh(xy,k=3, RANN=FALSE))
#k greater than one third of the data points
k6<- knn2nb(knearneigh(xy,k=6, RANN = FALSE))

#lag-two Rook
wr2<- wr
for (i in 1:length(wr)) {
  lag1<-wr[[i]]
  lag2<-wr[lag1]
  lag2<- sort(unique(unlist(lag2)))
  lag2<-lag2[!(lag2 %in% c(wr[[i]], i))]
  wr2[[i]]<-lag2
}

#adding the plot it function
plotit<- function(nb,lab=''){
  plot(p, col='gray', border='white')
  plot(nb,xy,add=TRUE,pch=20)
  text(6.3,50.1,paste0('(',lab,')'),cex=1.5)
}

par(mfrow=c(2,3),mai=c(0,0,0,0))
plotit(wr,'adjacency')
plotit(wr2,'lag-2 afj')
plotit(wd10,'10KM')
plotit(wd25,'25km')
plotit(k3,'k=3')
plotit(k6,'k=6')
