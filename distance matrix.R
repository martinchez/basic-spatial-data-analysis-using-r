A <- c(40,43)
B<- c(101,1)
C<-c(111,54)
D<- c(104,65)
E<- c(60,22)
F<- c(20,2)
pts <- rbind(A,B,C,D,E,F)
pts

plot(pts, xlim=c(0,120), ylim=c(0,120), pch=20, cex=2, col='red', xlab='X', ylab='Y', las=1)
text(pts+5, LETTERS[1:6])

#using dist function to make the distance matrix with a data set of any dimension
dis<- dist(pts)
dis

#apply the pythagoras to check the first point

sqrt((40-101)^2+(43-1)^2)


#transforming a distance matrix 
#into a normal matrix

D<- as.matrix(dis)
D
round(D) #ROUNDING OFF TO THE NEAREST WHOLE NUMBER

?hclust

#Question 4: Show R code to make a cluster dendogram summarizing the distances between these six sites, and plot it. See

cols<- apply(D,1,order)
#we then transpose the result
cols<-t(cols)

#we then get columns 2 to 3
cols <- cols[,2:3]
cols


rowcols<- cbind(rep(1:6, each=2),as.vector(t(cols)))
head(rowcols)


Ak3 <- Adj50 * 0



#weights matrix


w<- 1/D
round(w,4)


w[!is.finite(w)]<- NA

#computing the sum of rows
rtot<- rowSums(w, na.rm = TRUE)

rtot

#divide by their totals and check if they row sums add up to 1

w<- w/rtot
#checking if the rows add up to 1
rowSums(w,na.rm = TRUE)

colSums(w,na.rm = TRUE)


#spatial influence for polygons
library(raster)
p <- shapefile(system.file("external/lux.shp", package="raster"))

#use spdep library to find adjacent polygons
install.packages("spdep")
library(spdep)

