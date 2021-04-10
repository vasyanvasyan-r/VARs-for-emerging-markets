
#====== Daily ======

#first period
ch1p1 <- list(NaN)
for (m in 3:20){
  td <- dataindex1[, c(22, 23, m)]
  M <- modelvar.Ac.lagorder(3, td, 385, 0.9, 10)
  ch1p1[[m-2]] <- M
  names(ch1p1)[m-2] <- colnames(dataindex1)[m]
}
#second period
ch1p2  <- list(NaN)
for (m in 3:20){
  td <- dataindex2[, c(22, 23, m)]
  M <- modelvar.Ac.lagorder(3, td, 640, 0.9, 10)
  ch1p2[[m-2]] <- M
  names(ch1p2)[m-2] <- colnames(dataindex2)[m]
}

#third period
ch1p3  <- list(NaN)
for (m in 3:20){
  td <- dataindex3[, c(22, 23, m)]
  M <- modelvar.Ac.lagorder(3, td, 1044, 0.9, 10)
  ch1p3[[m-2]] <- M
  names(ch1p3)[m-2] <- colnames(dataindex3)[m]
}
chapter1 <- list(ch1p1, ch1p2, ch1p3)
names(chapter1) <- c("first period", "second period", "third period")

#причинность по грейнджеру
#first period
gl1 <- list(NA)
for (b in 1:length(ch1p1))
{
  x <- grangerclassic(ch1p1[[b]])
  colnames(x) <- colnames(ch1p1[[b]][["tseries"]])
  rownames(x) <- colnames(ch1p1[[b]][["tseries"]])
  gl1[[b]] <- x
}

#second period
gl2 <- list(NA)
for (b in c(1:length(ch1p2))[-12])
{
  x <- grangerclassic(ch1p2[[b]])
  colnames(x) <- colnames(ch1p2[[b]][["tseries"]])
  rownames(x) <- colnames(ch1p2[[b]][["tseries"]])
  gl2[[b]] <- x
}

#third period
gl3 <- list(NA)
for (b in 1:length(ch1p3))
{
  x <- grangerclassic(ch1p3[[b]])
  colnames(x) <- colnames(ch1p3[[b]][["tseries"]])
  rownames(x) <- colnames(ch1p3[[b]][["tseries"]])
  gl3[[b]] <- x
}

#names and mean matrix
names(gl1) <- colnames(dataindex1[, c(3:20)]) 
names(gl2) <- colnames(dataindex2[, c(3:20)])
names(gl3) <- colnames(dataindex3[, c(3:20)])

gl1[["mean"]] <-  Reduce('+', gl1)/18
colnames(gl1[[19]])[3] <- "EM"
rownames(gl1[[19]])[3] <- "EM"

gl2[["mean"]] <-  Reduce('+', gl2)/18
colnames(gl2[[18]])[3] <- "EM"
rownames(gl2[[18]])[3] <- "EM"

gl3[["mean"]] <-  Reduce('+', gl3)/18
colnames(gl3[[19]])[3] <- "EM"
rownames(gl3[[19]])[3] <- "EM"

chapter1.causuality <- list(gl1, gl2, gl3)
names(chapter1.causuality) <- c("first period", "second period", "third period")
rm(gl1, gl2, gl3)

#===== Weekly ======
#first period
ch1p1w <- list(NaN)
for (m in 3:20){
  td <- dataindex1week[, c(22, 23, m)]
  M <- modelvar.Ac.lagorder(3, td, 74, 0.9, 10)
  ch1p1w[[m-2]] <- M
  names(ch1p1w)[m-2] <- colnames(dataindex1)[m]
}

#second period
ch1p2w  <- list(NaN)
for (m in 3:20){
  td <- dataindex2week[, c(22, 23, m)]
  M <- modelvar.Ac.lagorder(3, td, 130, 0.9, 10)
  ch1p2w[[m-2]] <- M
  names(ch1p2w)[m-2] <- colnames(dataindex2week)[m]
}

#third period
ch1p3w  <- list(NaN)
for (m in 3:20){
  td <- dataindex3week[, c(22, 23, m)]
  M <- modelvar.Ac.lagorder(3, td, 204, 0.9, 10)
  ch1p3w[[m-2]] <- M
  names(ch1p3w)[m-2] <- colnames(dataindex3week)[m]
}
chapter1w <- list(ch1p1w, ch1p2w, ch1p3w)
names(chapter1w) <- c("first period", "second period", "third period")

gl1 <- list(NA)
for (b in 1:length(ch1p1w))
{
  x <- grangerclassic(ch1p1w[[b]])
  colnames(x) <- colnames(ch1p1w[[b]][["tseries"]])
  rownames(x) <- colnames(ch1p1w[[b]][["tseries"]])
  gl1[[b]] <- x
}

#second period
gl2 <- list(NA)
for (b in 1:length(ch1p2w))
{
  x <- grangerclassic(ch1p2w[[b]])
  colnames(x) <- colnames(ch1p2w[[b]][["tseries"]])
  rownames(x) <- colnames(ch1p2w[[b]][["tseries"]])
  gl2[[b]] <- x
}

#third period
gl3 <- list(NA)
for (b in 1:length(ch1p3w))
{
  x <- grangerclassic(ch1p3w[[b]])
  colnames(x) <- colnames(ch1p3w[[b]][["tseries"]])
  rownames(x) <- colnames(ch1p3w[[b]][["tseries"]])
  gl3[[b]] <- x
}

#names and mean matrix
names(gl1) <- colnames(dataindex1[, c(3:20)]) 
names(gl2) <- colnames(dataindex2[, c(3:20)])
names(gl3) <- colnames(dataindex3[, c(3:20)])

gl1[["mean"]] <-  Reduce('+', gl1)/18
colnames(gl1[[19]])[3] <- "EM"
rownames(gl1[[19]])[3] <- "EM"

gl2[["mean"]] <-  Reduce('+', gl2)/18
colnames(gl2[[19]])[3] <- "EM"
rownames(gl2[[19]])[3] <- "EM"

gl3[["mean"]] <-  Reduce('+', gl3)/18
colnames(gl3[[19]])[3] <- "EM"
rownames(gl3[[19]])[3] <- "EM"



chapter1w.causuality <- list(gl1, gl2, gl3)
names(chapter1w.causuality) <- c("first period", "second period", "third period")
rm(gl1, gl2, gl3)

#remove 
rm(ch1p1, ch1p2, ch1p3)
rm(ch1p1w, ch1p2w, ch1p3w)
rm(b, m, td, x, M)

#all in one list
K <- list(chapter1, chapter1.causuality)
names(K) <- c("VARs", "causuality")


chapter1 <- list(K)

K <- list(chapter1w, chapter1w.causuality)
names(K) <- c("VARs", "causuality")
chapter1[[2]] <- K

names(chapter1) <- c("daily", "weekly")
rm(chapter1.causuality, chapter1w, chapter1w.causuality, K)

#===== Orthogonal impulse response =====
#times = 10
times = 10

#EM to SP
#first period
#
models <- chapter1[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 2, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex1[, 23]/dataindex1[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]] <- list(list(list(oimp1)))
names(chapter1)[3] <- "orthogonal impulse response"
names(chapter1[[3]])[1] <- "EM to SP"
names(chapter1[[3]][[1]])[1] <- "daiy"
names(chapter1[[3]][[1]][[1]])[1] <- "first period"

#second period
models <- chapter1[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 2, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex2[, 23]/dataindex2[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[1]][[1]][[2]] <- oimp1
names(chapter1[[3]][[1]][[1]])[2] <- "second period"

#third period
models <- chapter1[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 2, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex3[, 23]/dataindex3[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[1]][[1]][[3]] <- oimp1
names(chapter1[[3]][[1]][[1]])[3] <- "third period"



#EM to 10-y yield
#first period
#
models <- chapter1[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 1, 10, Bo.and.W(models[[i]])[[1]])
}
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
#to %
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex1[, 22]/dataindex1[, i+2])
}
oimp1 <- oimp1*x
chapter1[[3]][[2]] <- list(list(oimp1))
names(chapter1[[3]])[2] <- "EM to 10-y yield"
names(chapter1[[3]][[2]])[1] <- "daiy"
names(chapter1[[3]][[2]][[1]])[1] <- "first period"

#second period
models <- chapter1[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 1, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex2[, 22]/dataindex2[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[2]][[1]][[2]] <- oimp1
names(chapter1[[3]][[2]][[1]])[2] <- "second period"

#third period
models <- chapter1[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 1, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex3[, 22]/dataindex3[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[2]][[1]][[3]] <- oimp1
names(chapter1[[3]][[2]][[1]])[3] <- "third period"


#weekdata
#EM to SP
#first period
models <- chapter1[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 2, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex1week[, 23]/dataindex1week[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[1]][[2]] <- list(oimp1)
names(chapter1)[3] <- "orthogonal impulse response EM to SP"
names(chapter1[[3]][[1]])[2] <- "weekly"
names(chapter1[[3]][[1]][[2]])[1] <- "first period"

#second period
models <- chapter1[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 2, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex2week[, 23]/dataindex2week[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[1]][[2]][[2]] <- oimp1
names(chapter1[[3]][[1]][[2]])[2] <- "second period"

#third period
models <- chapter1[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 2, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex3week[, 23]/dataindex3week[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[1]][[2]][[3]] <- oimp1
names(chapter1[[3]][[1]][[2]])[3] <- "third period"

#EM to 10-y yield
#first period
models <- chapter1[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 1, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex1week[, 22]/dataindex1week[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[2]][[2]] <- list(oimp1)
names(chapter1)[3] <- "orthogonal impulse response EM to 10-y yield"
names(chapter1[[3]][[2]])[2] <- "weekly"
names(chapter1[[3]][[2]][[2]])[1] <- "first period"

#second period
models <- chapter1[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 1, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex2week[, 22]/dataindex2week[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[2]][[2]][[2]] <- oimp1
names(chapter1[[3]][[2]][[2]])[2] <- "second period"

#third period
models <- chapter1[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 3, 1, 10, Bo.and.W(models[[i]])[[1]])
}
x <- rep(0, 18)
for (i in 1:18){
  x[i] <- mean(dataindex3week[, 22]/dataindex3week[, i+2])
}
oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter1[[3]][[2]][[2]][[3]] <- oimp1
names(chapter1[[3]][[2]][[2]])[3] <- "third period"

rm(oimp1, models)


# are residuals iid?
#daily
m <- chapter1[["daily"]][["VARs"]]
nwn <- matrix(rep(0, 3*18*3), nrow = 3)
for (i in 1:3){
  for (u in 1:18){
    for(z in 1:3) {
      if (m[[i]][[u]][["Lujng-Box test"]][z] <= 0.05){
        nwn[z, (i - 1)*18 + u] <- m[[i]][[u]][["Lujng-Box test"]][z]
      }
    }
  }
}

which(nwn != 0)
#non one is correlated 

#weekly
m <- chapter1[["weekly"]][["VARs"]]
nwn <- matrix(rep(0, 3*18*3), nrow = 3)
for (i in 1:3){
  for (u in 1:18){
    for(z in 1:3) {
      if (m[[i]][[u]][["Lujng-Box test"]][z] <= 0.05){
        nwn[z, (i - 1)*18 + u] <- m[[i]][[u]][["Lujng-Box test"]][z]
      }
    }
  }
}

which(nwn != 0)
#the same. non one is correlated 

rm(i, times, nwn, u, m, x, z)
#=====BOOTSTRAP CI for OIR=======
