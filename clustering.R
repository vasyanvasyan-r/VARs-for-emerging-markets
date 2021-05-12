
z <- function (x) mean(abs(diff(x)))

E1 <- chapter1[[1]][[1]]

n <- list(apply(dataindex1[, 3:20], 2, z))
n[[2]] <- apply(dataindex2[, 3:20], 2, z)
n[[3]] <- apply(dataindex3[, 3:20], 2, z)
Imp_p1 <- chapter1[[3]]
for (j in 1:3){
  m1 <- chapter1[["orthogonal impulse response"]][["EM to SP"]][["daily"]][[j]]
  m2 <- chapter1[["orthogonal impulse response"]][["EM to 10-y yield"]][["daily"]][[j]]
  for (i in 1:18){
    m <- E1[[j]][[i]]
    r <- m[["noncorrelated erorrs"]]
    unBo <- solve(Bo.and.W(m)[[1]]) 
    v <- apply(unBo%*%r, 1, z)
    m1[[i]] <- m1[[i]] * (v[2] / n[[j]][i])
    m2[[i]] <- m2[[i]] * (v[1] / n[[j]][i])
    Imp_p1[[1]][[1]][[j]] <- m1
    Imp_p1[[2]][[1]][[j]] <- m2
  }
}
E1 <- chapter2[[1]][[1]]
Imp_p2 <- chapter2[[3]]
for (j in 1:3){
  m1 <- chapter2[["orthogonal impulse response"]][["EM to SP"]][["daily"]][[j]]
  m2 <- chapter2[["orthogonal impulse response"]][["EM to DXY"]][["daily"]][[j]]
  for (i in 1:18){
    m <- E1[[j]][[i]]
    r <- m[["noncorrelated erorrs"]]
    unBo <- solve(Bo.and.W(m)[[1]]) 
    v <- apply(unBo%*%r, 1, z)
    m1[[i]] <- m1[[i]] * (v[2] / n[[j]][i])
    m2[[i]] <- m2[[i]] * (v[1] / n[[j]][i])
    Imp_p1[[1]][[1]][[j]] <- m1
    Imp_p1[[2]][[1]][[j]] <- m2
  }
}
E1 <- chapter3[[1]][[1]]
Imp_p3 <- chapter3[[3]]
for (j in 1:3){
  m1 <- chapter3[["orthogonal impulse response"]][["EM to SP"]][["daily"]][[j]]
  m2 <- chapter3[["orthogonal impulse response"]][["EM to DXY"]][["daily"]][[j]]
  m3 <- chapter3[["orthogonal impulse response"]][["EM to 10-y yield"]][["daily"]][[j]]
  for (i in 1:18){
    m <- E1[[j]][[i]]
    r <- m[["noncorrelated erorrs"]]
    unBo <- solve(Bo.and.W(m)[[1]]) 
    v <- apply(unBo%*%r, 1, z)
    m1[[i]] <- m1[[i]] * v[3] / n[[j]][i]
    m2[[i]] <- m2[[i]] * v[2]/ n[[j]][i]
    m3[[i]] <- m2[[i]] * v[1]/ n[[j]][i]
    Imp_p3[[1]][[1]][[j]] <- m1
    Imp_p3[[2]][[1]][[j]] <- m2
    Imp_p3[[3]][[1]][[j]] <- m3
  }
}

n <- list(apply(dataindex1week[, 3:20], 2, z))
n[[2]] <- apply(dataindex2week[, 3:20], 2, z)
n[[3]] <- apply(dataindex3week[, 3:20], 2, z)

E1 <- chapter1[[2]][[1]]
for (j in 1:3){
  m1 <- chapter1[["orthogonal impulse response"]][["EM to SP"]][["weekly"]][[j]]
  m2 <- chapter1[["orthogonal impulse response"]][["EM to 10-y yield"]][["weekly"]][[j]]
  for (i in 1:18){
    m <- E1[[j]][[i]]
    r <- m[["noncorrelated erorrs"]]
    unBo <- solve(Bo.and.W(m)[[1]]) 
    v <- apply(unBo%*%r, 1, z)
    m1[[i]] <- m1[[i]] * (v[2] / n[[j]][i])
    m2[[i]] <- m2[[i]] * (v[1] / n[[j]][i])
    Imp_p1[[1]][[2]][[j]] <- m1
    Imp_p1[[2]][[2]][[j]] <- m2
  }
}
E1 <- chapter2[[2]][[1]]
for (j in 1:3){
  m1 <- chapter2[["orthogonal impulse response"]][["EM to SP"]][["weekly"]][[j]]
  m2 <- chapter2[["orthogonal impulse response"]][["EM to DXY"]][["weekly"]][[j]]
  for (i in 1:18){
    m <- E1[[j]][[i]]
    r <- m[["noncorrelated erorrs"]]
    unBo <- solve(Bo.and.W(m)[[1]]) 
    v <- apply(unBo%*%r, 1, z)
    m1[[i]] <- m1[[i]] * (v[2] / n[[j]][i])
    m2[[i]] <- m2[[i]] * (v[1] / n[[j]][i])
    Imp_p2[[1]][[2]][[j]] <- m1
    Imp_p2[[2]][[2]][[j]] <- m2
  }
}

E1 <- chapter3[[2]][[1]]
for (j in 1:3){
  m1 <- chapter3[["orthogonal impulse response"]][["EM to SP"]][["weekly"]][[j]]
  m2 <- chapter3[["orthogonal impulse response"]][["EM to DXY"]][["weekly"]][[j]]
  m3 <- chapter3[["orthogonal impulse response"]][["EM to 10-y yield"]][["weekly"]][[j]]
  for (i in 1:18){
    m <- E1[[j]][[i]]
    r <- m[["noncorrelated erorrs"]]
    unBo <- solve(Bo.and.W(m)[[1]]) 
    v <- apply(unBo%*%r, 1, z)
    m1[[i]] <- m1[[i]] * v[3] / n[[j]][i]
    m2[[i]] <- m2[[i]] * v[2]/ n[[j]][i]
    m3[[i]] <- m2[[i]] * v[1]/ n[[j]][i]
    Imp_p3[[1]][[2]][[j]] <- m1
    Imp_p3[[2]][[2]][[j]] <- m2
    Imp_p3[[3]][[2]][[j]] <- m3
  }
}




IMP <- array(0 , dim = c(2, 7, 18, 3))


for (i in 1:2) {
  for (j in 1:2) {
    for (h in 1:3) {
      for (g in 1:18) {
        IMP[i, j, g, h] <- Imp_p1[[j]][[i]][[h]][[g]][4, 1] 
        
      }
    }
    rownames(IMP[i, j, , ]) <- countrynames
    colnames(IMP[i, j, , ]) <- c("fp", "sp", "tp")
  }  
  
}



for (i in 1:2) {
  for (j in 1:2) {
    for (h in 1:3) {
      for (g in 1:18) {
        IMP[i, j+2, g, h] <-Imp_p2[[j]][[i]][[h]][[g]][4, 1] 
        
      }
    }
    rownames(IMP[i, j+2, , ]) <- countrynames
    colnames(IMP[i, j+2, , ]) <- c("fp", "sp", "tp")
  }
}


for (i in 1:2) {
  for (j in 1:3) {
    for (h in 1:3) {
      for (g in 1:18) {
        IMP[i, j+4, g, h] <-Imp_p3[[j]][[i]][[h]][[g]][4, 1] 
      }
    }
    rownames(IMP[i, j+4, , ]) <- countrynames
    colnames(IMP[i, j+4, , ]) <- c("fp", "sp", "tp")
  }
}



IMPlong <- array(0 , dim = c(2, 7, 18, 3))


for (i in 1:2) {
  for (j in 1:2) {
    for (h in 1:3) {
      for (g in 1:18) {
        IMPlong[i, j, g, h] <- if(sum(abs(Imp_p1[[j]][[i]][[h]][[g]][4, 2:3]))
                                  > max(abs(Imp_p1[[j]][[i]][[h]][[g]][4, 11]),
                                        abs(Imp_p1[[j]][[i]][[h]][[g]][4, 10])))
        {
          sum(Imp_p1[[j]][[i]][[h]][[g]][4, 2:11])
        } else {
          0                     
        }
      }
    }
    rownames(IMPlong[i, j, , ]) <- countrynames
    colnames(IMPlong[i, j, , ]) <- c("fp", "sp", "tp")
  }  
  
}



for (i in 1:2) {
  for (j in 1:2) {
    for (h in 1:3) {
      for (g in 1:18) {
        IMPlong[i, j+2, g, h] <- if(sum(abs(Imp_p2[[j]][[i]][[h]][[g]][4, 2:3]))
                                    > max(abs(Imp_p2[[j]][[i]][[h]][[g]][4, 11]),
                                          abs(Imp_p2[[j]][[i]][[h]][[g]][4, 10])))
        {
          sum(Imp_p2[[j]][[i]][[h]][[g]][4, 2:11])
        } else {
          0                     
        }
      }
    }
    rownames(IMPlong[i, j+2, , ]) <- countrynames
    colnames(IMPlong[i, j+2, , ]) <- c("fp", "sp", "tp")
  }
}


for (i in 1:2) {
  for (j in 1:3) {
    for (h in 1:3) {
      for (g in 1:18) {
        IMPlong[i, j+4, g, h] <- if(sum(abs(Imp_p3[[j]][[i]][[h]][[g]][4, 2:3]))
                                    > max(abs(Imp_p3[[j]][[i]][[h]][[g]][4, 11]),
                                          abs(Imp_p3[[j]][[i]][[h]][[g]][4, 10])))
        {
          sum(Imp_p3[[j]][[i]][[h]][[g]][4, 2:11])
        } else {
          0                     
        }
      }
    }
    rownames(IMPlong[i, j+4, , ]) <- countrynames
    colnames(IMPlong[i, j+4, , ]) <- c("fp", "sp", "tp")
  }
}

IMPs <- IMP + IMPlong


FXclstr <- list(list(list(NA), list(NA)), list(list(NA), list(NA)))
for (i in 1:2) {
  for (j in 1:7) {
    y <- (as.vector(IMPs[i, j, ,]) - mean(as.vector(IMPs[i, j, ,])))/
      sd(as.vector(IMPs[i, j, ,]))
    x <- (as.vector(cf) - mean(as.vector(cf)))/
      sd(as.vector(cf))
    d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
    k <- rep(0, 6)
    for (g in 2:6){
      set.seed(1)
      clstr <- kmeans(d[, -c(3, 4)], g, nstart = 1000)
      k[g] <- clstr[["betweenss"]]/clstr[["totss"]]
    }
    oline <- (k[6]/5)*c(0:5)
    k <- which( k-oline == max(k-oline))
    set.seed(1)
    FXclstr[[1]][[i]][[j]] <- kmeans(d[, -c(3, 4)], k+1, nstart = 1000)
  }
}
for (i in 1:2) {
  for (j in 1:7) {
    y <- (as.vector(IMPs[i, j, ,]) - mean(as.vector(IMPs[i, j, ,])))/
      sd(as.vector(IMPs[i, j, ,]))
    x <- (as.vector(fo) - mean(as.vector(fo)))/
      sd(as.vector(fo))
    d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
    k <- rep(0, 6)
    for (g in 2:6){
      set.seed(1)
      clstr <- kmeans(d[, -c(3, 4)], g, nstart = 1000)
      k[g] <- clstr[["betweenss"]]/clstr[["totss"]]
    }
    oline <- (k[6]/5)*c(0:5)
    k <- which( k-oline == max(k-oline))
    set.seed(1)
    FXclstr[[2]][[i]][[j]] <- kmeans(d[, -c(3, 4)], k+1, nstart = 1000)
  }
}

names(FXclstr) <- c("cashflow", "financial openess")
names(FXclstr[[1]]) <- timeframe
names(FXclstr[[2]]) <- timeframe
names(FXclstr[[1]][[1]]) <- war
names(FXclstr[[1]][[2]]) <- war
names(FXclstr[[2]][[1]]) <- war
names(FXclstr[[2]][[2]]) <- war

FXclstr.long <- list(list(list(NA), list(NA)), list(list(NA), list(NA)))
for (i in 1:2) {
  for (j in 1:7) {
    y <- (as.vector(IMPs[i, j, ,]) - mean(as.vector(IMPs[i, j, ,])))/
      sd(as.vector(IMPs[i, j, ,]))
    x <- (as.vector(cf) - mean(as.vector(cf)))/
      sd(as.vector(cf))
    d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
    k <- rep(0, 6)
    for (g in 2:6){
      set.seed(1)
      clstr <- kmeans(d[, -c(3, 4)], g, nstart = 1000)
      k[g] <- clstr[["betweenss"]]/clstr[["totss"]]
    }
    oline <- (k[6]/5)*c(0:5)
    k <- which( k-oline == max(k-oline))
    set.seed(1)
    FXclstr.long[[1]][[i]][[j]] <- kmeans(d[, -c(3, 4)], k+1, nstart = 1000)
  }
}
for (i in 1:2) {
  for (j in 1:7) {
    y <- (as.vector(IMPs[i, j, ,]) - mean(as.vector(IMPs[i, j, ,])))/
      sd(as.vector(IMPs[i, j, ,]))
    x <- (as.vector(fo) - mean(as.vector(fo)))/
      sd(as.vector(fo))
    d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
    k <- rep(0, 6)
    for (g in 2:6){
      set.seed(1)
      clstr <- kmeans(d[, -c(3, 4)], g, nstart = 1000)
      k[g] <- clstr[["betweenss"]]/clstr[["totss"]]
    }
    oline <- (k[6]/5)*c(0:5)
    k <- which( k-oline == max(k-oline))
    set.seed(1)
    FXclstr.long[[2]][[i]][[j]] <- kmeans(d[, -c(3, 4)], k+1, nstart = 1000)
  }
}

names(FXclstr.long) <- c("cashflow", "financial openess")
names(FXclstr.long[[1]]) <- timeframe
names(FXclstr.long[[2]]) <- timeframe
names(FXclstr.long[[1]][[1]]) <- war
names(FXclstr.long[[1]][[2]]) <- war
names(FXclstr.long[[2]][[1]]) <- war
names(FXclstr.long[[2]][[2]]) <- war


FXclstr.s <- list(list(list(NA), list(NA)), list(list(NA), list(NA)))
for (i in 1:2) {
  for (j in 1:7) {
    y <- (as.vector(IMPs[i, j, ,]) - mean(as.vector(IMPs[i, j, ,])))/
      sd(as.vector(IMPs[i, j, ,]))
    x <- (as.vector(cf) - mean(as.vector(cf)))/
      sd(as.vector(cf))
    d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
    k <- rep(0, 6)
    for (g in 2:6){
      set.seed(1)
      clstr <- kmeans(d[, -c(3, 4)], g, nstart = 1000)
      k[g] <- clstr[["betweenss"]]/clstr[["totss"]]
    }
    oline <- (k[6]/5)*c(0:5)
    k <- which( k-oline == max(k-oline))
    set.seed(1)
    FXclstr.s[[1]][[i]][[j]] <- kmeans(d[, -c(3, 4)], k+1, nstart = 1000)
  }
}
for (i in 1:2) {
  for (j in 1:7) {
    y <- (as.vector(IMPs[i, j, ,]) - mean(as.vector(IMPs[i, j, ,])))/
      sd(as.vector(IMPs[i, j, ,]))
    x <- (as.vector(fo) - mean(as.vector(fo)))/
      sd(as.vector(fo))
    d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
    k <- rep(0, 6)
    for (g in 2:6){
      set.seed(1)
      clstr <- kmeans(d[, -c(3, 4)], g, nstart = 1000)
      k[g] <- clstr[["betweenss"]]/clstr[["totss"]]
    }
    oline <- (k[6]/5)*c(0:5)
    k <- which( k-oline == max(k-oline))
    set.seed(1)
    FXclstr.s[[2]][[i]][[j]] <- kmeans(d[, -c(3, 4)], k+1, nstart = 1000)
  }
}

names(FXclstr.s) <- c("cashflow", "financial openess")
names(FXclstr.s[[1]]) <- timeframe
names(FXclstr.s[[2]]) <- timeframe
names(FXclstr.s[[1]][[1]]) <- war
names(FXclstr.s[[1]][[2]]) <- war
names(FXclstr.s[[2]][[1]]) <- war
names(FXclstr.s[[2]][[2]]) <- war


 
x <- cbind((FXclstr[[1]][[1]][[1]][[1]] == 1)[1:18], 
      (FXclstr[[1]][[1]][[1]][[1]] == 1)[19:36], 
      (FXclstr[[1]][[1]][[1]][[1]] == 1)[37:54])

