# Module 4 Assignment 2
library(R.matlab)

## 1) Load Data Into Matlab
retinadata <- readMat('RetinaData.mat')
responses <- retinadata$responses
stimulus <- retinadata$stimulus

# organize the input data
time <- 1:2000
for (i in 1:8) {
  assign(paste0("RGC",i),responses[ , , i])
}

# do a quick plot of the data
for (i in 1:8) {
  data <- get(paste0("RGC",i))
  plot(time,data[,1], type = "l", col = "red", 
       main = paste0("RGC",i),
       ylim = c(-1,1))
  lines(time, data[,2], col = "blue")
}

# plot the correlation of both datasets to each other
for (i in 1:8) {
  data <- get(paste0("RGC",i))
  plot(data[,1], data[,2], main = paste0("RGC",i),
       xlim = c(-0.6, 1), ylim = c(-0.6, 1))
  abline(lm(data[,1] ~ data[,2]))
  cor(data[,1], data[,2])
}

## 2) Estimate the explainable variance of each RGC

expVartable <- as.data.frame(matrix(nrow = 8, ncol = 1))
rownames(expVartable) <- paste("RGC",1:8)
colnames(expVartable) <- c("Explainable Variance (R2)")

png("cross-correlations.png")
par(mfrow = c(4,2))
for (i in 1:8) {
  RGC <- get(paste0("RGC",i))
  print(paste0("RGC",i))
  
  cross <- ccf(RGC[,1],RGC[,2], plot = FALSE)
  expVartable[i,1] <- max(cross$acf)
  plot(cross, type = "l", main = paste0("RGC",i," Cross Correlation\n Max Corr @ Time Lag 0 = ",round(max(cross$acf),2)),
       xlab = "Time Separation between Signals", ylab = "Correlation")

}
dev.off()


