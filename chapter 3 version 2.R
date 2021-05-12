#====== Daily ======

#first period
ch3p1 <- list(NaN)
for (m in 3:20){
  td <- dataindex1[, c(24, 23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(5, td, 385, 0.9, 10)
  ch3p1[[m-2]] <- M
  names(ch3p1)[m-2] <- colnames(dataindex1)[m]
}
#second period
ch3p2  <- list(NaN)
for (m in 3:20){
  td <- dataindex2[, c(24, 23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(5, td, 640, 0.9, 10)
  ch3p2[[m-2]] <- M
  names(ch3p2)[m-2] <- colnames(dataindex2)[m]
}

#third period
ch3p3  <- list(NaN)
for (m in 3:20){
  td <- dataindex3[, c(24, 23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(5, td, 1044, 0.9, 10)
  ch3p3[[m-2]] <- M
  names(ch3p3)[m-2] <- colnames(dataindex3)[m]
}
chapter3 <- list(ch3p1, ch3p2, ch3p3)
names(chapter3) <- c("first period", "second period", "third period")

#причинность по грейнджеру
#first period
gl1 <- list(NA)
for (b in 1:length(ch3p1))
{
  x <- grangerclassic(ch3p1[[b]])
  colnames(x) <- colnames(ch3p1[[b]][["tseries"]])
  rownames(x) <- colnames(ch3p1[[b]][["tseries"]])
  gl1[[b]] <- x
}

#second period
gl2 <- list(NA)
for (b in c(1:length(ch3p2))[-12])
{
  x <- grangerclassic(ch3p2[[b]])
  colnames(x) <- colnames(ch3p2[[b]][["tseries"]])
  rownames(x) <- colnames(ch3p2[[b]][["tseries"]])
  gl2[[b]] <- x
}

#third period
gl3 <- list(NA)
for (b in 1:length(ch3p3))
{
  x <- grangerclassic(ch3p3[[b]])
  colnames(x) <- colnames(ch3p3[[b]][["tseries"]])
  rownames(x) <- colnames(ch3p3[[b]][["tseries"]])
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

chapter3.causuality <- list(gl1, gl2, gl3)
names(chapter3.causuality) <- c("first period", "second period", "third period")
rm(gl1, gl2, gl3)

#====== Weekly ========

#first period
ch3p1w <- list(NaN)
for (m in 3:20){
  td <- dataindex1week[, c(24, 23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(5, td, 69, 0.9, 10)
  ch3p1w[[m-2]] <- M
  names(ch3p1w)[m-2] <- colnames(dataindex1)[m]
}

#second period
ch3p2w  <- list(NaN)
for (m in 3:20){
  td <- dataindex2week[, c(24, 23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(5, td, 130, 0.9, 10)
  ch3p2w[[m-2]] <- M
  names(ch3p2w)[m-2] <- colnames(dataindex2week)[m]
}

#third period
ch3p3w  <- list(NaN)
for (m in 3:20){
  td <- dataindex3week[, c(24, 23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(5, td, 204, 0.9, 10)
  ch3p3w[[m-2]] <- M
  names(ch3p3w)[m-2] <- colnames(dataindex3week)[m]
}
chapter3w <- list(ch3p1w, ch3p2w, ch3p3w)
names(chapter3w) <- c("first period", "second period", "third period")

gl1 <- list(NA)
for (b in 1:length(ch3p1w))
{
  x <- grangerclassic(ch3p1w[[b]])
  colnames(x) <- colnames(ch3p1w[[b]][["tseries"]])
  rownames(x) <- colnames(ch3p1w[[b]][["tseries"]])
  gl1[[b]] <- x
}

#second period
gl2 <- list(NA)
for (b in 1:length(ch3p2w))
{
  x <- grangerclassic(ch3p2w[[b]])
  colnames(x) <- colnames(ch3p2w[[b]][["tseries"]])
  rownames(x) <- colnames(ch3p2w[[b]][["tseries"]])
  gl2[[b]] <- x
}

#third period
gl3 <- list(NA)
for (b in 1:length(ch3p3w))
{
  x <- grangerclassic(ch3p3w[[b]])
  colnames(x) <- colnames(ch3p3w[[b]][["tseries"]])
  rownames(x) <- colnames(ch3p3w[[b]][["tseries"]])
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



chapter3w.causuality <- list(gl1, gl2, gl3)
names(chapter3w.causuality) <- c("first period", "second period", "third period")
rm(gl1, gl2, gl3)

#remove 
rm(ch3p1, ch3p2, ch3p3)
rm(ch3p1w, ch3p2w, ch3p3w)
rm(b, m, td, x, M)

#all in one list
K <- list(chapter3, chapter3.causuality)
names(K) <- c("VARs", "causuality")


chapter3 <- list(K)

K <- list(chapter3w, chapter3w.causuality)
names(K) <- c("VARs", "causuality")
chapter3[[2]] <- K

names(chapter3) <- c("daily", "weekly")
rm(chapter3.causuality, chapter3w, chapter3w.causuality, K)




#=====orthogonal impulse response======
#EM to SP
#first period
##times = 10
times = 10

models <- chapter3[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 3, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]] <- list(list(list(oimp1)))
names(chapter3)[3] <- "orthogonal impulse response"
names(chapter3[[3]])[1] <- "EM to SP"
names(chapter3[[3]][[1]])[1] <- "daily"
names(chapter3[[3]][[1]][[1]])[1] <- "first period"

#second period
models <- chapter3[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 3, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[1]][[1]][[2]] <- oimp1
names(chapter3[[3]][[1]][[1]])[2] <- "second period"

#third period
models <- chapter3[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 3, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[1]][[1]][[3]] <- oimp1
names(chapter3[[3]][[1]][[1]])[3] <- "third period"



#EM to DXY
#first period
#
models <- chapter3[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 2, times)
}
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
#to %

chapter3[[3]][[2]] <- list(list(oimp1))
names(chapter3[[3]])[2] <- "EM to DXY"
names(chapter3[[3]][[2]])[1] <- "daily"
names(chapter3[[3]][[2]][[1]])[1] <- "first period"

#second period
models <- chapter3[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 2, times)
}

oimp1 <- oimp1*x
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[2]][[1]][[2]] <- oimp1
names(chapter3[[3]][[2]][[1]])[2] <- "second period"

#third period
models <- chapter3[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 2, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[2]][[1]][[3]] <- oimp1
names(chapter3[[3]][[2]][[1]])[3] <- "third period"

#EM to 10-y yield
#first period
#
models <- chapter3[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 1, times)
}
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
#to %

chapter3[[3]][[3]] <- list(list(oimp1))
names(chapter3[[3]])[3] <- "EM to 10-y yield"
names(chapter3[[3]][[3]])[1] <- "daily"
names(chapter3[[3]][[3]][[1]])[1] <- "first period"

#second period
models <- chapter3[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 1, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[3]][[1]][[2]] <- oimp1
names(chapter3[[3]][[3]][[1]])[2] <- "second period"

#third period
models <- chapter3[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 1, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[3]][[1]][[3]] <- oimp1
names(chapter3[[3]][[3]][[1]])[3] <- "third period"
 

#EM to 10-y yield
#first period
models <- chapter3[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 1, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[3]][[2]] <- list(oimp1)
names(chapter3)[3] <- "orthogonal impulse response"
names(chapter3[[3]][[3]])[2] <- "weekly"
names(chapter3[[3]][[3]][[2]])[1] <- "first period"

#second period
models <- chapter3[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 1, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[3]][[2]][[2]] <- oimp1
names(chapter3[[3]][[3]][[2]])[2] <- "second period"

#third period
models <- chapter3[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 1, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[3]][[2]][[3]] <- oimp1
names(chapter3[[3]][[3]][[2]])[3] <- "third period"

rm(oimp1, models)

#========weekdata=======
#EM to SP
#first period
models <- chapter3[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 3, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[1]][[2]] <- list(oimp1)
names(chapter3)[3] <- "orthogonal impulse response"
names(chapter3[[3]][[1]])[2] <- "weekly"
names(chapter3[[3]][[1]][[2]])[1] <- "first period"

#second period
models <- chapter3[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 3, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[1]][[2]][[2]] <- oimp1
names(chapter3[[3]][[1]][[2]])[2] <- "second period"

#third period
models <- chapter3[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 3, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[1]][[2]][[3]] <- oimp1
names(chapter3[[3]][[1]][[2]])[3] <- "third period"

#EM to DXY
#first period
models <- chapter3[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 2, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[2]][[2]] <- list(oimp1)
names(chapter3)[3] <- "orthogonal impulse response"
names(chapter3[[3]][[2]])[2] <- "weekly"
names(chapter3[[3]][[2]][[2]])[1] <- "first period"

#second period
models <- chapter3[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 2, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[2]][[2]][[2]] <- oimp1
names(chapter3[[3]][[2]][[2]])[2] <- "second period"

#third period
models <- chapter3[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 5, 2, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter3[[3]][[2]][[2]][[3]] <- oimp1
names(chapter3[[3]][[2]][[2]])[3] <- "third period"

rm(oimp1, models)


#===are residuals iid?===
#daily
m <- chapter3[["daily"]][["VARs"]]
nwn <- matrix(rep(0, 3*18*5), nrow = 5)
for (i in 1:3){
  for (u in 1:18){
    for(z in 1:5) {
      if (m[[i]][[u]][["Lujng-Box test"]][z] <= 0.05){
        nwn[z, (i - 1)*18 + u] <- m[[i]][[u]][["Lujng-Box test"]][z]
      }
    }
  }
}

which(nwn != 0)
#non one is correlated 

#weekly
m <- chapter3[["weekly"]][["VARs"]]
nwn <- matrix(rep(0, 3*18*5), nrow = 5)
for (i in 1:3){
  for (u in 1:18){
    for(z in 1:5) {
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


#2000 simulations
R = 2000
times = 10
s <- rbind(c(5,3), #EM to SP 500
           c(5,2),#EM to DXY
           c(5,1)) #EM to 10y yield
#first period
set.seed(1)
models <- chapter3[["daily"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot.v2(models[[i]], R, s, 
                        times, Ortho.impulse)
    
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(2)
models <- chapter3[["daily"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot.v2(models[[i]], R, s, 
                        times, Ortho.impulse)
    
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(3)
models <- chapter3[["daily"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot.v2(models[[i]], R, s, 
                        times, Ortho.impulse)
    
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
Cies3 <- list(CIes1, CIes2, CIes3)

#===== Weekly CI ======

#first period
set.seed(10)
models <- chapter3[["weekly"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot.v2(models[[i]], R, s, 
                        times, Ortho.impulse)
    
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(20)
models <- chapter3[["weekly"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot.v2(models[[i]], R, s, 
                        times, Ortho.impulse)
    
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(30)
models <- chapter3[["weekly"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot.v2(models[[i]], R, s, 
                        times, Ortho.impulse)
    
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
CIes3w <- list(CIes1, CIes2, CIes3)


M <- Cies3
EMtoDXY3 <- list(list(NA), list(NA), list(NA))
EMtoSP3 <- list(list(NA), list(NA), list(NA))
EMto10y3 <- list(list(NA), list(NA), list(NA))
countrynames <- names(M[[1]])
for (i in 1:3){
  for (j in 1:18) {
    EMtoDXY3[[i]][[j]] <- M[[i]][[j]][[2]]
    EMtoSP3[[i]][[j]] <- M[[i]][[j]][[1]]
    EMto10y3 [[i]][[j]] <- M[[i]][[j]][[3]]
  }
  names(EMtoDXY3[[i]]) <- countrynames
  names(EMtoSP3[[i]]) <- countrynames
  names(EMto10y3[[i]]) <- countrynames
}
names(EMtoDXY3) <- c("first period", "second period", "third period")
names(EMtoSP3) <- c("first period", "second period", "third period")
names(EMto10y3) <- c("first period", "second period", "third period")

M <- CIes3w
EMtoDXY3w <- list(list(NA), list(NA), list(NA))
EMtoSP3w <- list(list(NA), list(NA), list(NA))
EMto10y3w <- list(list(NA), list(NA), list(NA))
countrynames <- names(M[[1]])
for (i in 1:3){
  for (j in 1:18) {
    EMtoDXY3w[[i]][[j]] <- M[[i]][[j]][[2]]
    EMtoSP3w[[i]][[j]] <- M[[i]][[j]][[1]]
    EMto10y3w [[i]][[j]] <- M[[i]][[j]][[3]]
  }
  names(EMtoDXY3w[[i]]) <- countrynames
  names(EMtoSP3w[[i]]) <- countrynames
  names(EMto10y3w[[i]]) <- countrynames
}
names(EMtoDXY3w) <- c("first period", "second period", "third period")
names(EMtoSP3w) <- c("first period", "second period", "third period")
names(EMto10y3w) <- c("first period", "second period", "third period")



#add confidence interval to impulse



M <- chapter3[["orthogonal impulse response"]][["EM to DXY"]]
M <- M[["daily"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoDXY3[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoDXY3[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)

chapter3[["orthogonal impulse response"]][["EM to DXY"]][["daily"]] <- N

M <- chapter3[["orthogonal impulse response"]][["EM to DXY"]]
M <- M[["weekly"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoDXY3w[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoDXY3w[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter3[["orthogonal impulse response"]][["EM to DXY"]][["weekly"]] <- N


M <- chapter3[["orthogonal impulse response"]][["EM to SP"]]
M <- M[["daily"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoSP3[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoSP3[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter3[["orthogonal impulse response"]][["EM to SP"]][["daily"]] <- N

M <- chapter3[["orthogonal impulse response"]][["EM to SP"]]
M <- M[["weekly"]]
N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoSP3w[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoSP3w[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter3[["orthogonal impulse response"]][["EM to SP"]][["weekly"]] <- N

M <- chapter3[["orthogonal impulse response"]][["EM to 10-y yield"]]
M <- M[["daily"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMto10y3[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMto10y3[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)

chapter3[["orthogonal impulse response"]][["EM to 10-y yield"]][["daily"]] <- N

M <- chapter3[["orthogonal impulse response"]][["EM to 10-y yield"]]
M <- M[["weekly"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMto10y3w[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMto10y3w[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter3[["orthogonal impulse response"]][["EM to 10-y yield"]][["weekly"]] <- N

rm(EMto10y3, EMto10y3w, EMtoDXY3, EMtoDXY3w, EMtoSP3, EMtoSP3w, N, M)
