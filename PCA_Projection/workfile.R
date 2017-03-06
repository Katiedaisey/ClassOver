data <- read.csv("/Users/katiedaisey/Desktop/local/data/chemometrics/corndata-mp5.csv", header = T)
meas <- data[,1:4]
data <- data[,-(1:4)]

matplot(t(data), type = "l")

# organize by spectra order at in middle channel
channel <- floor(length(data[1,])/2)


value <- data[,channel]
value <- cbind(value, 1:length(value))

color <- rainbow(n = (length(data[,1]) + length(data[,1])/10))[1:length(data[,1])]


value2 <- value[order(value[,1]),]
value2 <- cbind(value2, 1:length(value2[,1]))
value2 <- value2[order(value2[,2]),]


color2 <- color[value2[,3]]



matplot(t(data), type = "l", xlab = "Channel", ylab = "Intensity", main = "Unprocessed Spectra", lty = 1, col = color2)

pca1 <- prcomp(data, center = T, scale. = T)

out <- c(1:3)
newdata <- data
if (!is.null(out)) {
  newdata <- pca1$x[,-out] %*% t(pca1$rotation[,-out])
  if(pca1$scale != FALSE){
    newdata <- scale(newdata, center = FALSE , scale=1/pca1$scale)
  }
  if(pca1$center != FALSE){
    newdata <- scale(newdata, center = -1 * pca1$center, scale=FALSE)
  }
}

matplot(t(newdata), type = "l", xlab = "Channel", ylab = "Intensity", main = "Unprocessed Spectra", lty = 1, col = color2)


plsdata <- meas
plsdata$info <- as.matrix(data,ncol = 700)
head(meas)

pls1 <- plsr(Moisture~info, data = plsdata, validation = "CV", ncomp = 20)
plot(pls1, "validation")
pls1 <- plsr(Moisture~info, data = plsdata, validation = "CV", ncomp = 11)
pls1 <- predict(pls1)
pls1
plot(plsdata$Moisture, pls1[,,11])
abline(0,1)

rmsep <- sqrt(sum((plsdata$Moisture - pls1[,,11])^2)/length(plsdata$Moisture))
rmsep #0.04374363

plsdata <- meas
plsdata$info <- as.matrix(newdata, ncol = 700)


pls1 <- plsr(Moisture~info, data = plsdata, validation = "CV", ncomp = 10)
plot(pls1, "validation")
pls1 <- plsr(Moisture~info, data = plsdata, validation = "CV", ncomp = 6)
pls2 <- predict(pls1)
pls1
plot(plsdata$Moisture, pls2[,,3])
abline(0,1, col = "red")
rmsep <- sqrt(sum((plsdata$Moisture - pls2[,,5])^2)/length(plsdata$Moisture))
rmsep #0.2573028

plot(pls1, "coefficients", ncomp = 1:6)

