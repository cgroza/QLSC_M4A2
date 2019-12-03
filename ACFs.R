library(FCSlib)
my_acf<-array(data = NA,dim = c(2,8,1000))
my_ccf<-array(data = NA,dim = c(8,1000))

i<-1
j<-1
for (j in c(1:8)) {
  my_ccf[j,]<-fcs(x = responses[,1,j],y = responses[,2,j],nPoints = 1000)
  for (i in c(1:2)) {
    my_acf[i,j,]<-fcs(x = responses[,i,j],nPoints = 1000)
    # print(head(my_acf[i,j,1:25]/max(my_acf[i,j,])))
    # print(paste("Optimal T = ",((which(my_acf[i,j,]/max(my_acf[i,j,]))==0.5)*2)))
  } 
}

par(mfrow = c(4,4))
for (j in c(1:8)) {
  print(j)
  plot((my_acf[1,j,]/max(my_acf[1,j,]))~seq(1,1000,1),
       type = "l",
       col = "dodgerblue4",
       main = paste("Cell", j, "ACF"),
       ylab = "Adjusted Autocorrelation",
       xlab = "T")
  lines((my_acf[2,j,]/max(my_acf[2,j,]))~seq(1,1000,1),
       col = "darkorange")
  abline(h = 0, col = "red")
  abline(h = 0.5, col = "darkgrey")
  plot((my_acf[1,j,1:15]/max(my_acf[1,j,]))~seq(1,15,1), 
       type = "l", 
       col = "dodgerblue4",
       main = paste("Cell", j, "ACF (first 15 points)"),
       ylab = "Adjusted Autocorrelation",
       xlab = "T", ylim = c(0,1))
  lines((my_acf[2,j,1:15]/max(my_acf[2,j,]))~seq(1,15,1), 
       col = "darkorange")
  abline(h = 0, col = "red")
  abline(h = 0.5, col = "darkgrey")
  
}



dev.off()
max(my_acf)
my_acf<-fcs(x = responses[,1,1],nPoints = 1000)
plot(my_acf[1,1,]~seq(1,1000,1), type = "l")

head(my_acf)
matrix(data = ,nrow = ,ncol = )
plot(responses[,1,1], type = "l")
dim(responses)

my_ccf<-fcs(x = responses[,1,1], y = responses[,2,1], nPoints = 1000)
plot(my_ccf~seq(1,1000,1), type = "l")
my_ccf<-array(data = NA,dim = c(8,1000))
for (j in c(1:8)) {
  my_ccf[j,]<-fcs(x = responses[,1,j],y = responses[,2,j],nPoints = 1000)
  # plot((my_acf[i,j,]/max(my_acf[i,j,]))~seq(1,1000,1), 
  #      type = "l", 
  #      # col = "grey",
  #      main = paste("Run ",i," Cell", j),
  #      ylab = "Adjusted Autocorrelation",
  #      xlab = "T")
  plot((my_ccf[j,])~seq(1,1000,1), 
       type = "l", 
       # col = "grey",
       main = paste(" Cell", j),
       ylab = "Adjusted Cross-correlation",
       xlab = "T")
  abline(h = 0.5, col = "red")
  print(head(my_ccf[j,1:25]/max(my_ccf[j,])))
  # print(paste("Optimal T = ",((which(my_acf[i,j,]/max(my_acf[i,j,]))==0.5)*2)))
} 
