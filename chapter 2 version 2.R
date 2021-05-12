#====== Daily ======

#first period
ch2p1 <- list(NaN)
for (m in 3:20){
  td <- dataindex1[, c(23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(4, td, 385, 0.9, 10)
  ch2p1[[m-2]] <- M
  names(ch2p1)[m-2] <- colnames(dataindex1)[m]
}
#second period
ch2p2  <- list(NaN)
for (m in 3:20){
  td <- dataindex2[, c(23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(4, td, 640, 0.9, 10)
  ch2p2[[m-2]] <- M
  names(ch2p2)[m-2] <- colnames(dataindex2)[m]
}

#third period
ch2p3  <- list(NaN)
for (m in 3:20){
  td <- dataindex3[, c(23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(4, td, 1044, 0.9, 10)
  ch2p3[[m-2]] <- M
  names(ch2p3)[m-2] <- colnames(dataindex3)[m]
}
chapter2 <- list(ch2p1, ch2p2, ch2p3)
names(chapter2) <- c("first period", "second period", "third period")

#причинность по грейнджеру
#first period
gl1 <- list(NA)
for (b in 1:length(ch2p1))
{
  x <- grangerclassic(ch2p1[[b]])
  colnames(x) <- colnames(ch2p1[[b]][["tseries"]])
  rownames(x) <- colnames(ch2p1[[b]][["tseries"]])
  gl1[[b]] <- x
}

#second period
gl2 <- list(NA)
for (b in c(1:length(ch2p2))[-12])
{
  x <- grangerclassic(ch2p2[[b]])
  colnames(x) <- colnames(ch2p2[[b]][["tseries"]])
  rownames(x) <- colnames(ch2p2[[b]][["tseries"]])
  gl2[[b]] <- x
}

#third period
gl3 <- list(NA)
for (b in 1:length(ch2p3))
{
  x <- grangerclassic(ch2p3[[b]])
  colnames(x) <- colnames(ch2p3[[b]][["tseries"]])
  rownames(x) <- colnames(ch2p3[[b]][["tseries"]])
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

chapter2.causuality <- list(gl1, gl2, gl3)
names(chapter2.causuality) <- c("first period", "second period", "third period")
rm(gl1, gl2, gl3)

#====== Weekly ========

#first period
ch2p1w <- list(NaN)
for (m in 3:20){
  td <- dataindex1week[, c(23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(4, td, 74, 0.9, 10)
  ch2p1w[[m-2]] <- M
  names(ch2p1w)[m-2] <- colnames(dataindex1)[m]
}

#second period
ch2p2w  <- list(NaN)
for (m in 3:20){
  td <- dataindex2week[, c(23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(4, td, 130, 0.9, 10)
  ch2p2w[[m-2]] <- M
  names(ch2p2w)[m-2] <- colnames(dataindex2week)[m]
}

#third period
ch2p3w  <- list(NaN)
for (m in 3:20){
  td <- dataindex3week[, c(23, 21, 22, m)]
  M <- modelvar.Ac.lagorder(4, td, 204, 0.9, 10)
  ch2p3w[[m-2]] <- M
  names(ch2p3w)[m-2] <- colnames(dataindex3week)[m]
}
chapter2w <- list(ch2p1w, ch2p2w, ch2p3w)
names(chapter2w) <- c("first period", "second period", "third period")

gl1 <- list(NA)
for (b in 1:length(ch2p1w))
{
  x <- grangerclassic(ch2p1w[[b]])
  colnames(x) <- colnames(ch2p1w[[b]][["tseries"]])
  rownames(x) <- colnames(ch2p1w[[b]][["tseries"]])
  gl1[[b]] <- x
}

#second period
gl2 <- list(NA)
for (b in 1:length(ch2p2w))
{
  x <- grangerclassic(ch2p2w[[b]])
  colnames(x) <- colnames(ch2p2w[[b]][["tseries"]])
  rownames(x) <- colnames(ch2p2w[[b]][["tseries"]])
  gl2[[b]] <- x
}

#third period
gl3 <- list(NA)
for (b in 1:length(ch2p3w))
{
  x <- grangerclassic(ch2p3w[[b]])
  colnames(x) <- colnames(ch2p3w[[b]][["tseries"]])
  rownames(x) <- colnames(ch2p3w[[b]][["tseries"]])
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



chapter2w.causuality <- list(gl1, gl2, gl3)
names(chapter2w.causuality) <- c("first period", "second period", "third period")
rm(gl1, gl2, gl3)

#remove 
rm(ch2p1, ch2p2, ch2p3)
rm(ch2p1w, ch2p2w, ch2p3w)
rm(b, m, td, x, M)

#all in one list
K <- list(chapter2, chapter2.causuality)
names(K) <- c("VARs", "causuality")


chapter2 <- list(K)

K <- list(chapter2w, chapter2w.causuality)
names(K) <- c("VARs", "causuality")
chapter2[[2]] <- K

names(chapter2) <- c("daily", "weekly")
rm(chapter2.causuality, chapter2w, chapter2w.causuality, K)




#=====orthogonal impulse response======
#EM to SP
#first period
##times = 10
times = 10

models <- chapter2[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 2, times)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]] <- list(list(list(oimp1)))
names(chapter2)[3] <- "orthogonal impulse response"
names(chapter2[[3]])[1] <- "EM to SP"
names(chapter2[[3]][[1]])[1] <- "daily"
names(chapter2[[3]][[1]][[1]])[1] <- "first period"

#second period
models <- chapter2[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 2, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[1]][[1]][[2]] <- oimp1
names(chapter2[[3]][[1]][[1]])[2] <- "second period"

#third period
models <- chapter2[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 2, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[1]][[1]][[3]] <- oimp1
names(chapter2[[3]][[1]][[1]])[3] <- "third period"



#EM to DXY
#first period
#
models <- chapter2[["daily"]][["VARs"]][["first period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 1, 10)
}
rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
#to %

chapter2[[3]][[2]] <- list(list(oimp1))
names(chapter2[[3]])[2] <- "EM to DXY"
names(chapter2[[3]][[2]])[1] <- "daily"
names(chapter2[[3]][[2]][[1]])[1] <- "first period"

#second period
models <- chapter2[["daily"]][["VARs"]][["second period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 1, 10)
}


rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[2]][[1]][[2]] <- oimp1
names(chapter2[[3]][[2]][[1]])[2] <- "second period"

#third period
models <- chapter2[["daily"]][["VARs"]][["third period"]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 1, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[2]][[1]][[3]] <- oimp1
names(chapter2[[3]][[2]][[1]])[3] <- "third period"


#weekdata
#EM to SP
#first period
models <- chapter2[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 2, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[1]][[2]] <- list(oimp1)
names(chapter2)[3] <- "orthogonal impulse response"
names(chapter2[[3]][[1]])[2] <- "weekly"
names(chapter2[[3]][[1]][[2]])[1] <- "first period"

#second period
models <- chapter2[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 2, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[1]][[2]][[2]] <- oimp1
names(chapter2[[3]][[1]][[2]])[2] <- "second period"

#third period
models <- chapter2[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 2, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[1]][[2]][[3]] <- oimp1
names(chapter2[[3]][[1]][[2]])[3] <- "third period"

#EM to DXY
#first period
models <- chapter2[["weekly"]][["VARs"]][[1]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 1, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[2]][[2]] <- list(oimp1)
names(chapter2)[3] <- "orthogonal impulse response"
names(chapter2[[3]][[2]])[2] <- "weekly"
names(chapter2[[3]][[2]][[2]])[1] <- "first period"

#second period
models <- chapter2[["weekly"]][["VARs"]][[2]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 1, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[2]][[2]][[2]] <- oimp1
names(chapter2[[3]][[2]][[2]])[2] <- "second period"

#third period
models <- chapter2[["weekly"]][["VARs"]][[3]]
oimp1 <- matrix(rep(0, 18*(times + 1)), nrow = 18, ncol = times+1)
for (i in 1:length(models)){
  oimp1[i, ] <- Ortho.impulse(models[[i]], 4, 1, 10)
}

rownames(oimp1) <- names(models)
colnames(oimp1) <- as.character(0:times)
chapter2[[3]][[2]][[2]][[3]] <- oimp1
names(chapter2[[3]][[2]][[2]])[3] <- "third period"

rm(oimp1, models)


#===are residuals iid?===
#daily
m <- chapter1[["daily"]][["VARs"]]
nwn <- matrix(rep(0, 3*18*4), nrow = 4)
for (i in 1:3){
  for (u in 1:18){
    for(z in 1:4) {
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
nwn <- matrix(rep(0, 3*18*4), nrow = 4)
for (i in 1:3){
  for (u in 1:18){
    for(z in 1:4) {
      if (m[[i]][[u]][["Lujng-Box test"]][z] <= 0.05){
        nwn[z, (i - 1)*18 + u] <- m[[i]][[u]][["Lujng-Box test"]][z]
      }
    }
  }
}

which(nwn != 0)
#the same. non one is correlated 

rm(i, times, nwn, u, m, z)

#=====BOOTSTRAP CI for OIR=======


#2000 simulations
R = 2000
times = 10
s <- rbind(c(4,2), #EM to SP 500
           c(4,1)) #EM to DXY
#first period
set.seed(1)
models <- chapter2[["daily"]][["VARs"]][["first period"]]
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
models <- chapter2[["daily"]][["VARs"]][["second period"]]
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
models <- chapter2[["daily"]][["VARs"]][["third period"]]
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
Cies2 <- list(CIes1, CIes2, CIes3)


#first period
set.seed(10)
models <- chapter2[["weekly"]][["VARs"]][["first period"]]
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
models <- chapter2[["weekly"]][["VARs"]][["second period"]]
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
models <- chapter2[["weekly"]][["VARs"]][["third period"]]
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
CIes2w <- list(CIes1, CIes2, CIes3)


M <- Cies2
EMtoDXY2 <- list(list(NA), list(NA), list(NA))
EMtoSP2 <- list(list(NA), list(NA), list(NA))
countrynames <- names(M[[1]])
for (i in 1:3){
  for (j in 1:18) {
    EMtoDXY2[[i]][[j]] <- M[[i]][[j]][[2]]
    EMtoSP2[[i]][[j]] <- M[[i]][[j]][[1]]
  }
  names(EMtoDXY2[[i]]) <- countrynames
  names(EMtoSP2[[i]]) <- countrynames
}
names(EMtoDXY2) <- c("first period", "second period", "third period")
names(EMtoSP2) <- c("first period", "second period", "third period")

M <- CIes2w
EMtoDXY2w <- list(list(NA), list(NA), list(NA))
EMtoSP2w <- list(list(NA), list(NA), list(NA))
countrynames <- names(M[[1]])

for (i in 1:3){
  for (j in 1:18) {
    EMtoDXY2w[[i]][[j]] <- M[[i]][[j]][[2]]
    EMtoSP2w[[i]][[j]] <- M[[i]][[j]][[1]]
  }
  names(EMtoDXY2w[[i]]) <- countrynames
  names(EMtoSP2w[[i]]) <- countrynames
}
names(EMtoDXY2) <- c("first period", "second period", "third period")
names(EMtoSP2) <- c("first period", "second period", "third period")


#add confidence interval to impulse



M <- chapter2[["orthogonal impulse response"]][["EM to DXY"]]
M <- M[["daily"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoDXY2[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoDXY2[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)

chapter2[["orthogonal impulse response"]][["EM to DXY"]][["daily"]] <- N

M <- chapter2[["orthogonal impulse response"]][["EM to DXY"]]
M <- M[["weekly"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoDXY2w[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoDXY2w[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter2[["orthogonal impulse response"]][["EM to DXY"]][["weekly"]] <- N


M <- chapter2[["orthogonal impulse response"]][["EM to SP"]]
M <- M[["daily"]]

N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoSP2[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoSP2[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter2[["orthogonal impulse response"]][["EM to SP"]][["daily"]] <- N

M <- chapter2[["orthogonal impulse response"]][["EM to SP"]]
M <- M[["weekly"]]
N <- list(list(NA), list(NA), list(NA))
for (j in 1:length(M)) {
  for (i in 1:nrow((M[[j]]))) {
    N[[j]][[i]] <- rbind(EMtoSP2w[[j]][[i]][1:3, ], t(M[[j]][i ,]), 
                         EMtoSP2w[[j]][[i]][4:6, ])
    rownames(N[[j]][[i]])[4] <- "imp"
  }
  names(N[[j]]) <- rownames(M[[j]])
}
names(N) <- names(M)
chapter2[["orthogonal impulse response"]][["EM to SP"]][["weekly"]] <- N
