#требуемые пакеты
library(AER)
library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(vars)
library(dLagM)
library(tseries)
library(data.table)
library(nlme)
library(data.table)
library(caroline)
library(corrplot)

Sys.setenv(LANG = "en")

setwd("C:/Users/vasil/Desktop/D is not for dragons/YandexDisk/D is not for Dragons/r scripts")


weeek <- function (x) {
    x <-  format(strptime(as.character(x), "%d.%m.%Y"), "%Y-%m-%d") ## меняем формат даты
    paste(as.character(week(x)), as.character(year(x)), collapse = "." ) ## возвращаем номер недели и года
}

weekdata <- function(x, fn) {
  x$week <- as.character(lapply(X = x$date, FUN = weeek)) ## столбец номером недели и года
  wdata <- aggregate(x[, 2:(ncol(x)-1)], by = list(x$week), FUN = fn) ## возвращаем 
  wdata <- wdata[with(wdata, order(t)), ]
  wdata$t <- 1:length(wdata$t)
  return(wdata)
}
intonumber <- function(x) {
  x <- as.numeric(gsub(" ", "", as.character(x)))
  return(x)
}
#регрессоры
xt <- function(n, p, var_data){
  var_data <- as.matrix.data.frame(var_data)
  t1 <- length(var_data)/n-p + 1
  t <- length(var_data)/n
  x <- c(1, as.vector(t(var_data[t:t1, ])))
  t <- t - 1 
  for (b in t:p) {
    b1 <- b - p + 1
    x <- cbind(x, c(1, as.vector(t(var_data[b:b1, ]))))
  }
  x <- x[, -1]
  return(x)
}
#зависимые
yt <- function(n, p, var_data){
  var_data <- as.matrix.data.frame(var_data)
  t <- length(var_data)/n
  x <- c(1, as.vector(t(var_data[t, ])))
  t <- t - 1 
  for (b in t:p) {
    x <- cbind(x, c(1, as.vector(t(var_data[b, ]))))
  }
  x <- x[-1, -ncol(x)]
  return(x)
  
}
#квадратичная форма
qfs <- function(x, y){
  z <- x[, 1]%*%t(y[, 1])
  matrixlist <- list(z)
  for (b in 2:ncol(x)) {
    matrixlist[[b]] <- x[, b]%*%t(y[, b])
  }
  m <- Reduce('+', matrixlist)
  return(m)
  
}
#матрица коэффициентов
P <- function(x, y){
  p <- qfs(y, x)%*%solve(qfs(x, x))
  return(p)
}
#остатки
res <- function(x, y, P){
  e <- y - P%*%x
  return(e)
}
#матрица ковариций остатков
teta <- function(x, y, P){
  e <- res(x, y, P)
  teta <- (1/ncol(e))*qfs(e,e)
  return(teta)
}
#интегрированность ряда

idiff <- function(x){
  d <- ur.df(x, "trend", 1)@teststat[1]
  ts <- ur.df(x, "trend", 1)@cval[1, 3]
  i <- 0 
  while (d > ts){
    x <- c(0, diff(x))
    d <- ur.df(x, "trend", 1)@teststat[1]
    ts <- ur.df(x, "trend", 1)@cval[1, 3]
    i <- i + 1
  }
  return(list(x, i))
} 
#все в месте
modelvar <- function(n, p, vardata){
  vardata1 <- idiff(vardata[, 1])[[1]]
  i <- idiff(vardata[, 1])[[2]]
  for (v in 2:ncol(vardata)){
    vardata1 <- cbind(vardata1, idiff(vardata[, v])[[1]])
    i <- c(i, idiff(vardata[, v])[[2]])
  }
  z <- which(i == 0)
  if (length(z) == 0) {
  } else {
    for (u in 1:length(z)){
      vardata1[, z[u]] <- c(0, diff(vardata[, z[u]]))
    }
  }
  x <- xt(n, p , vardata1)
  y <- yt(n, p, vardata1)
  PO <- P(x, y)
  e <- res(x, y, PO)
  teta <- teta(x, y, PO)
  k <- nrow(vardata1)
  l <- list(x, y, PO, e, teta, vardata1, i, p, k)
  names(l) <- c("x", "y", "P", "res", "teta", "tseries", "I", "lags", "v")
  return(l)
}

#тест на количество лагов
lctest <- function(model1, model0){
  p1 <- model1[["lags"]]#кол-во лагов h1
  p0 <- model0[["lags"]]#кол-во лагов h0
  t  <- ncol(model0[["y"]])
  xi <- t(log(det(model0[["teta"]])) - log(det(model1[["teta"]])))#статистика
  n  <- nrow(model1[["P"]])
  df <- n*n*(p1-p0) #степени свободы
  prob <- pchisq(xi, df)
  return(prob)
}
#тест на значимость коэфициентов
significant <- function(model){
  disp <- kronecker(model[["teta"]], solve(qfs(model[["x"]], model[["x"]])))
  s <- abs(as.vector(model[["P"]])/sqrt(diag(disp)))
  s <- apply(as.matrix(s), 2, pnorm)
  return(s)
}
#причинность грейнджеру

xtrest <- function(p, var_data){
  var_data <- as.vector(var_data)
  t1 <- length(var_data) - p + 1
  t <- length(var_data)
  x <- c(1, as.vector(var_data[t:t1]))
  t <- t - 1 
  for (b in t:p) {
    b1 <- b - p + 1
    x <- cbind(x, c(1, as.vector(var_data[b:b1])))
  }
  x <- x[, -1]
  return(x)
}
ytrest <- function(p, var_data){
  var_data <- as.vector(var_data)
  t <- length(var_data)
  x <- c(1, as.vector(var_data[t]))
  t <- t - 1
  for (b in t:p) {
    x <- cbind(x, c(1, as.vector(var_data[b])))
  }
  x <- x[-1, -ncol(x)]
  return(x)
}
qfsrest <- function(x, y){
  z <- x[1]%*%t(y[, 1])
  matrixlist <- list(z)
  for (b in 2:ncol(y)) {
    matrixlist[[b]] <- x[ b]%*%t(y[, b])
  }
  m <- Reduce('+', matrixlist)
  return(m)
  
}

grangerclassic <- function(model){
  #extract data
  tseries <- model[["tseries"]]
  n <- model[["n"]]
  p <- model[["lags"]]
  ess1 <- diag(model[["teta"]])
  t <- nrow(model[["tseries"]])
  
  
  #matrix of probabilities
  grangerprob <- matrix(rep(NA, n*n), nrow = n, ncol = n)
  r <- c(1:n)
  for (u in 1:n){
    m <- modelvar(n-1, p, vardata = tseries[, -u])
    ess0 <- diag(m[["teta"]])
    chi <- t*(ess0-ess1[-u])/ess0
    grangerprob[r[-u], u] <- pchisq(chi, p)
  }
  diag(grangerprob) <- 1
  return(grangerprob)
}

#функция разделения матрицы П на матрицы отдельных лагов
pl <- function (m) {
  p <- m[["lags"]]
  pl <- list(rep(NA, p))
  n <- nrow(m[["new P"]])
  for (b in 1:p){
    v1 <- 2 + n*(b - 1)
    v2 <- 1 + n*b
    pl[[b]] <- m[["new P"]][, v1:v2]
  }
  return(pl) 
}

#неортогональный импульс
irl <- function (model, t){
  L <- pl(model)
  v <- nrow(model[["new P"]])
  l <- model[["lags"]]
  
  imp <- replicate(t,
                   replicate(l, matrix(rep(0, v*v), nrow = v, ncol = v),
                             simplify = FALSE),
                   simplify = FALSE)
  imp[[1]] <- L
  if (l == 1){
    for (i in 2:t){
      imp[[i]][[1]] <- imp[[i - 1]][[1]]%*%L[[1]]
    }
  } else {
    for (i in 2:t){
      for (n in 1:(l - 1)){
        imp[[i]][[n]] <- imp[[i-1]][[1]]%*%L[[n]] + imp[[i-1]][[n + 1]]
      }
      imp[[i]][[l]] <- imp[[i-1]][[1]]%*%L[[l]]
    }
  }
  
  
  return(imp)
} 

aicvar <- function(model){
  v <- model[["v"]]
  t <- nrow(model[["tseries"]])
  aic <- - log(det(model[["teta"]])) + 2*model[["lags"]]*v*v/t
  return(aic)
}

bicvar <- function(model){
  v <- model[["v"]]
  t <- nrow(model[["tseries"]])
  bic <- -log(det(model[["teta"]])) + log(t)*model[["lags"]]*v*v/t
  return(bic)
}

lagorder <- function(tseries){
  n <- ncol(tseries)
  p <- 1
  m0 <- modelvar(n, p, tseries)
  m1 <- modelvar(n, (p + 1), tseries)
  while(lctest(m1, m0) >= 0.9){
    p <- p + 1
    m0 <- modelvar(n, p, tseries)
    m1 <- modelvar(n, (p + 1), tseries)
  }
  if (p == 1){
    return(p)
  }
  else {
    if(bicvar(m1) > bicvar(m0)) {
      return(p)
    }
    else {
      return(p + 1)
    }
  }
}

bg <- function(resudias, p){
  r <- c(resudias[-1], 0)
  rt <- as.matrix(c(resudias[-1], 0))
  r0 <- resudias
  if (p  == 1){
    
  } else {
    for (i in 2:p) {
      rt <- cbind(rt, c(r[-1], 0))
      r <- c(r[-1], 0)
    }
  }
  C <- P(t(rt), t(r0))
  r1 <- res(t(rt), t(r0), C)
  chi <- (1 - (r1%*%t(r1))/(r0%*%r0))*(length(r0) - p)
  return(pchisq(chi, p))
}

aclist <- function(n, p, vardata, k, lm, ac.prob) {
  x <- xt(n, p , vardata)[, 1:k]
  y <- yt(n, p, vardata)[, 1:k]
  PO <- P(x, y)
  e <- res(x, y, PO)
  teta1 <- teta(x, y, PO)
  
  #lags for residuals
  x0 <- xt(n, p , vardata)
  y0 <- yt(n, p, vardata)
  ef <- res(x0, y0, PO)
  
  bretest <- function(elags, x, r0, a1, a2){
    if (a1[1] != 0) {
      r1 <- elags[a1, ]
      gt1 <- rbind(x, r1)
      b <- P(gt1, t(r0))
      r <- res(gt1, t(r0), b)
      ess1 <- r%*%t(r)
      tss <- t(r0)%*%r0
      chi1 <- (k - 1)*(1 - ess1/tss)
      g1 <- list(chi1, gt1, r)
    } else {
      chi1 <- 0
      g1 <- list(chi1, x, matrix(r0, nrow = 1))
    }
    if (a2[1] != 0) {
      r1 <- elags[a2, ]
      gt2 <- rbind(x, r1)
      b <- P(gt2, t(r0))
      r <- res(gt2, t(r0), b)
      ess2 <- r%*%t(r)
      tss <- t(r0)%*%r0
      chi2 <- (k - 1)*(1 - ess2/tss)
      g2 <- list(chi2, gt2, r)
    } else {
      chi2 <- 0
      g2 <- list(chi2, x, matrix(r0, nrow = 1))
    }
    return(list(g1, g2))
  }
  
  al <- list(NA)
  xac <- list(NA)
  paf <- list(NA)
  rac <- NA
  for (u in 1:n) {
    #generate matrix
    elags <- matrix(rep(0, k*lm), ncol =  k, nrow = lm)
    for (j in 1:lm) {
      elags[j, ] <- ef[u, (j+1):(k+j)]
    }
    #definitions
    #r0    -- former residuals
    #r1    -- AR regressors
    #r     -- new residuals
    #gt    -- Breush-Pagan regressors
    #ac    -- significant pacf's picks
    #chi   -- Breush-Pagan statistics
    #rac   -- new residuals matrix
    #elags -- matrix lags errors
    
    r0 <- e[u, ] 
    eac <- ef[u, 1:(k+lm-1)]  #errors pacf's picks
    #pacf function
    paf[[u]] <- pacf(eac, lag.max = lm)
    ac <- which(abs(paf[[u]][["acf"]]) >= qnorm(ac.prob)/sqrt(k+lm))
    if (length(ac) == 0) { #without significant pacf's picks
      
      g <- tryCatch({
        g <- bretest(elags, x, r0, 1, 0)
      }, 
      error = function(er) { #for det = 0
        g <- bretest(elags, x, r0, 0, 0)
        return(g)
      })
      chi1 <- g[[1]][[1]]
      if (chi1 == 0){
        al[[u]] <- c(0, 0.12345)
        xac[[u]] <- x
        r <- g[[1]][[3]]
      } else {
        if (pchisq(chi1, 1) >= ac.prob){ #is autocorrelation still here?
          al[[u]] <- c(1, pchisq(chi1, 1))
          xac[[u]] <- g[[1]][[2]]
          r <- g[[1]][[3]]
        } else {
          al[[u]] <- c(0, pchisq(chi1, 1))
          xac[[u]] <- x
          r <- g[[1]][[3]]
        }
      }
    } else { #fit AR(p') with pacf picks
      #Firstly, only pacf form
      
      g <- tryCatch({
        g <- bretest(elags, x, r0, ac, 1:tail(ac, 1))
      }, 
      error = function(er) { #AR(pacf form)
        tryCatch({
          g <- bretest(elags, x, r0, ac, 0)
        }, 
        error = function(er) { #AR('futherst lag in pacf')
          tryCatch({
            g <- bretest(elags, x, r0, 1:tail(ac, 1), 0)
          }, 
          error = function(er) { #AR('the biggest pacf lag') 
            tryCatch({
              g <- bretest(elags, x, r0, 
                           which(paf[[u]][["acf"]] == max(paf[[u]][["acf"]])), 0)
            },
            error = function(er) { #no AR
              g <- bretest(elags, x, r0, 0, 0)
              return(g)
            })
            return(g)        
          })
          return(g)      
        })
        return(g)
      })
      #Comparison of AR(p') and AR('futherst lag in pacf')
      chi1 <- g[[1]][[1]]
      chi2 <- g[[2]][[1]]
      
      if (chi1 == 0) {
        al[[u]] <- c(ac, 0.12345)
        xac[[u]] <- g[[1]][[2]]
        r <- g [[1]][[3]]
      } else {
        if (pchisq(chi1, nrow(g[[1]][[2]])-n*p-1) >= 
            pchisq(chi2, tail(ac, 1))){
          if (pchisq(chi1, nrow(g[[1]][[2]])-n*p-1) >= ac.prob) {
            al[[u]] <- c(ac, pchisq(chi1, nrow(g[[1]][[2]])-n*p-1))
            xac[[u]] <- g[[1]][[2]]
            r <- g [[1]][[3]]
          } else{
            al[[u]] <- c(ac, pchisq(chi1, nrow(g[[1]][[2]])-n*p-1))
            xac[[u]] <- x
            r <- r0
          }
        } else {
          if (pchisq(chi2, length(ac)) >= ac.prob) {
            al[[u]] <- c(1:tail(ac, 1), pchisq(chi2, tail(ac, 1)))
            xac[[u]] <- g[[2]][[2]]
            r <- g [[2]][[3]]
          } else{
            al[[u]] <- c(ac, pchisq(chi2, length(ac)))
            xac[[u]] <- x
            r <- r0
          }
        } 
      }
      
      
    } # else code part end
    #pacf function
    paf[[u]] <- pacf(eac, lag.max = lm)
    #noncorrelated erorrs 
    rac <- rbind(rac, r)
  }
  
  #Pac -- new regression coef
  #PI  -- new P
  #PR  -- coef of errors regression 
  
  rac <- rac[-1, ]
  #Coef matrix with robust errors
  Pac <- list(NA)
  PI  <- NA
  PR  <- list(NA)
  for (u in 1:n){
    Pac[[u]] <- P(xac[[u]], t(y[u,]))
    PI <- rbind(PI, Pac[[u]][, 1:(n*p + 1)])
    PR[[u]] <- Pac[[u]][, -c(1:(n*p + 1))]
  }
  PI <- PI[-1, ]
  
  #robust errors
  e1 <- y - PI%*%x
  teta1 <- teta(x, y, PO)
  tetaac <- rac%*%t(rac)
  for (u in 1:n){
    if (diag(tetaac)[u] == 0){
      tetaac[u, ] <- teta1[u, ]
      tetaac[, u] <- teta1[u, ]
    }
  }
  
  #Lujng-Box test for noncorrelated errors
  Ljung <- rep(0, n)
  for (u in 1:n){
    Ljung[u] <- Box.test(rac[u, ], type="Ljung",lag=10)[["p.value"]]
  }
  
  l <- list(x, y, PO, e, teta1, vardata, p, k, al, Ljung, 
            paf, rac, Pac, PI, PR, e1, tetaac, ac.prob, lm, n, xac)
  names(l) <- c("x", "y", "P", "res", "teta", "tseries", "lags", "v", "al",
                "Lujng-Box test","pacf", "noncorrelated erorrs", 
                "new regression coef","new P", "coef of error  regression", 
                "robast errors", "new teta", "ac.prob", "lm", "n", "x+errors")
  return(l)
}

modelvar.Ac.lagorder <- function(n, d, k, ac.prob, lm) {
  
  vardata1 <- idiff(d[, 1])[[1]]
  i <- idiff(d[, 1])[[2]]
  for (v in 2:ncol(d)){
    vardata1 <- cbind(vardata1, idiff(d[, v])[[1]])
    i <- c(i, idiff(d[, v])[[2]])
  }
  z <- which(i == 0)
  if (length(z) == 0) {
  } else {
    for (u in 1:length(z)){
      vardata1[, z[u]] <- c(0, diff(d[, z[u]]))
    }
  }
  if (lm + k >= nrow(d) - max(i)){
    return("lm+k is to large")
  } else {
    p1 <- 1
    lmprob = 1
    while (lmprob >= 0.5 & p1 <= lm - 1) {
      l1 <- aclist(n, p1, vardata1, k, lm, ac.prob)
      p1 <-p1 + 1
      l2 <- aclist(n, (p1), vardata1, k, lm, ac.prob)
      lmprob <- pchisq(k*(log(det(l1[["new teta"]])) - log(det(l2[["new teta"]]))),
                       n*n)
    }
    l1[[22]] <- i
    names(l1)[[22]] <- "I"
    return(l1)
  }
  
}

Bo.and.W <- function(model){
  X <- model[["new teta"]]
  B <- t(chol(X))
  W <- diag(B)
  diag(B) <- 1
  return(list(B, W))
}

Ortho.impulse <- function(m, dv, iv, times) {
  n  <- m[["n"]]
  lm <- m[["lm"]]
  
  l <- Bo.and.W(m)[[1]]
  
  #VMA
  pimp <- irl(m, times)
  #ACfunctionMA
  eimp <- list(NULL)
  j <- irl(m, times)
  ac <- list(NULL)
  
  for (h in 1:n) {
    if (tail(m[["al"]][[h]], 1) >= 0.9){
      ac[[h]] <- m[["al"]][[h]][-length(m[["al"]][[h]])]
      
      g <- length(ac[[h]])
      z <- list(rbind(ac[[h]], m[["coef of error  regression"]][[h]]))
      zo <- rbind(ac[[h]], m[["coef of error  regression"]][[h]])
      i <- 2
      
      while (!(is.na(z[[i-1]][1]))) {
        zt <- NA
        if (is.null(dim(z[[i-1]])[1])) {
          z1 <- z[[i-1]][[1]] + zo[1, 1]
          z1 <- rbind(z1, z[[i-1]][[2]] * zo[2, 1])
          
          if (z1[1, 1] <= times){
            rownames(z1)[1] <- ""
            zt <- z1[, z1[1, ] <= times]
          } 
        } else {
          z1 <- z[[i-1]][1, 1] + zo[1, 1]
          z1 <- rbind(z1, z[[i-1]][2, 1] * zo[2, 1])
          if (g == 1) {
            if (z1[1, 1] <= times){
              rownames(z1)[1] <- ""
              zt <- z1[, z1[1, ] <= times]
            }
          } else {
            for (j in 2:g) {
              zj <- z[[i-1]][1, 1] + zo[1, j]
              zj <- rbind(zj, z[[i-1]][2, 1] * zo[2, j])
              z1 <- cbind(z1, zj)
            }
            if (z1[1, 1] <= times){
              rownames(z1)[1] <- ""
              zt <- z1[, z1[1, ] <= times]
            } 
            
            for (u in 2:ncol(z[[i-1]])) {
              z1 <- z[[i-1]][1, u] + zo[1, 1]
              z1 <- rbind(z1, z[[i-1]][2, u] * zo[2, 1])
              for (j in 2:g) {
                zj <- z[[i-1]][1, u] + zo[1, j]
                zj <- rbind(zj, z[[i-1]][2, u] * zo[2, j])
                z1 <- cbind(z1, zj)
              }
              if (z1[1, 1] <= times){
                rownames(z1)[1] <- ""
                zt <- cbind(zt, z1[, z1[1, ] <= times])
              }
            }
          }
        }
        z[[i]] <- zt
        i <- i + 1
      }
      z[[i-1]] <-NULL
      eimp0 <- Reduce(cbind, z)
      eimp0 <- aggregate(eimp0[2, ], by = list(eimp0[1, ]), FUN = sum)
      eimp0 <- eimp0[which(eimp0[, 1] <= times), ]
      z <- rep(0, times)
      z[eimp0[, 1]] <- eimp0[, 2]
      eimp[[h]] <- z
    } else {
      ac[[h]] <- 0
      eimp[[h]] <- rep(0, times)
    }
  }
  
  #add ACfunctionMA in VMA
  z <- list(pimp[[1]][[1]])
  for (j in 2:times) {
    z[[j]] <- pimp[[j]][[1]]
  }
  
  for (i in 1:n) {
    if (ac[[i]][1] != 0) {
      
      z0 <- list(z[[1]][, i])
      for (j in 2:times) {
        z0[[j]] <- z[[j]][, i]
      }
      z0 <- Reduce(cbind, z0)
      
      eimp0 <- eimp[[i]]
      eimp1 <- eimp[[i]]
      for (j in 2:times) {
        eimp0 <- c(0, eimp0[-times])
        eimp1 <- rbind(eimp1, eimp0)
      }
      for (j in 1:n) {
        z0[j, ] <- z0[1, ] + apply((z0[1, ]*eimp1), 2, sum)
      }
      
      for (j in 1:times) {
        z[[j]][, i] <- z0[, j]
      }
    } 
  }
  
  o <- function(x , a) x%*%a
  z <- lapply(z, o, l)
  
  imp <- c(l[dv, iv], rep(0, times))
  for (j in 1:times) {
    imp[1+j] <- z[[j]][dv, iv]
  }
  return(imp)
}

rdu <- function(n,k) sample(1:k,n,replace=T)

CI.fromboot <- function(m, R, FUN.oir, Bo.dec, dv, iv, times) {
  v <- m[["v"]]
  p <- m[["lags"]]
  n <- m[["n"]]
  ct <- 0
  cs <- 0
  imp <- NULL
  
  while(cs < R) {
    imp <- tryCatch({
      index <- rdu(v, v)
      
      r <- NULL
      for (i in 1:n) {
        x <- m[["x+errors"]][[i]][, index]
        y <- m[["y"]][i, index] 
        m[["new P"]][i, ] <- t(P(x, t(y))[, 1:(1 + n*p)])
        m[["coef of error  regression"]][[i]] <- t(P(x, t(y))[, -c(1:(1 + n*p))])
        r <- rbind(r, res(x, y, P(x, t(y))))
      }
      m[["new teta"]] <- r%*%t(r)
      ct <- ct + 1
      imp <- rbind(imp, FUN.oir(m, dv, iv, times, Bo.dec(m)))
      imp <- list(imp, ct)
    },
    error = function(er) {
      tryCatch({
        index <- rdu(v, v)
        
        r <- NULL
        for (i in 1:n) {
          x <- m[["x+errors"]][[i]][, index]
          y <- m[["y"]][i, index] 
          m[["new P"]][i, ] <- t(P(x, t(y))[, 1:(1 + n*p)])
          m[["coef of error  regression"]][[i]] <- t(P(x, t(y))[, -c(1:(1 + n*p))])
          r <- rbind(r, res(x, y, P(x, t(y))))
        }
        m[["new teta"]] <- r%*%t(r)
        ct <- ct + 1
        imp <- rbind(imp, FUN.oir(m, dv, iv, times, Bo.dec(m)))
        imp <- list(imp, ct)
      },
      error = function(er) {
        index <- rdu(v, v)
        
        r <- NULL
        for (i in 1:n) {
          x <- m[["x+errors"]][[i]][, index]
          y <- m[["y"]][i, index] 
          m[["new P"]][i, ] <- t(P(x, t(y))[, 1:(1 + n*p)])
          m[["coef of error  regression"]][[i]] <- t(P(x, t(y))[, -c(1:(1 + n*p))])
          r <- rbind(r, res(x, y, P(x, t(y))))
        }
        m[["new teta"]] <- r%*%t(r)
        ct <- ct + 3
        imp <- rbind(imp, FUN.oir(m, dv, iv, times, Bo.dec(m)))
        imp <- list(imp, ct)
        return(imp)
      })
      return(imp)
    })
    ct <- imp[[2]]
    imp <- imp[[1]]
    cs <- cs + 1
  }
  
  print(paste(ct-cs, "simulations dropped"))
  
  
  x<-apply(imp, 2, quantile, c(.01, .05, .1, .90, 0.95, .99))
  return(x)
}

CI.fromboot.v2 <- function(m, R, s, times, FUN.oir) {
  al <- m[["al"]]
  lm <- m[["lm"]]
  p <- m[["lags"]]
  n <- m[["n"]]
  v <- m[["v"]]
  newteta <- m[["new teta"]]
  newP <- m[["new P"]]
  errorcoef <- m[["coef of error  regression"]]
  
  mt <- list(al, newP, lm, newteta, p, n, v, errorcoef)
  names(mt) <- c("al", "new P", "lm", "new teta", "lags", "n", "v",
                 "coef of error  regression")
  
  x<- list(NA)
  index <- matrix(rdu(v*R, v), nrow = R, ncol = v)
  X <- list(NA) 
  for (j in 1:n) {
    for (i in 1:R) {
      x[[i]] <- rbind(m[["y"]][j, index[i, ]], 
                      m[["x+errors"]][[j]][, index[i, ]])
    }
    X[[j]] <- x
  }
  o <- function(x) {
    Po <- t(P(x[-1, ], t(x[1, ])))
    return(Po)
  }
  tryCatch({
    Po <- list(NA)
    for (j in 1:n) {
      x <- X[[j]]
      Po[[j]] <- lapply(x, o)
    }
    print( "no errors")
  }, error = function(e){
    x<- list(NA)
    index <- matrix(rdu(v*R, v), nrow = R, ncol = v)
    X <- list(NA) 
    for (j in 1:n) {
      for (i in 1:R) {
        x[[i]] <- rbind(m[["y"]][j, index[i, ]],
                        m[["x+errors"]][[j]][, index[i, ]])
      }
      X[[j]] <- x
    }
    for (j in 1:n) {
      x <- X[[j]]
      Po[[j]] <- lapply(x, o)
    }
    print("smt in new P went wrong")
    return(Po)
  })
  
  
  RES <- list(NA)
  for (j in 1:n) {
    r <- list(NA)
    for (i in 1:R) {
      r[[i]] <-  t(X[[j]][[i]][1, ]) - t(Po[[j]][[i]])%*%X[[j]][[i]][-1, ]
    }
    RES[[j]] <- r
  }
  
  M <- list(NA)
  for (i in 1:R) {
    Pt <- list(NA)
    tta <- matrix(rep(0, n*v), nrow = n)
    for (j in 1:n){
      l <- length(mt[["coef of error  regression"]][[j]])
      if (l == 0){
        Pt[[j]] <- t(Po[[j]][[i]])
      } else {
        Pt[[j]] <- t(Po[[j]][[i]])[, 1:(1+p*n)]
        mt[["coef of error  regression"]][[j]] <- 
          t(Po[[j]][[i]])[,(2+p*n):(1+l+p*n)]
      }
      tta[j, ] <- RES[[j]][[i]]
    }
    mt[["new P"]] <- Reduce(rbind, Pt)
    mt[["new teta"]] <- tta%*%t(tta)
    M[[i]] <- mt
    names(M[[i]]) <- c("al", "new P", "lm", "new teta", "lags", "n", "v",
                       "coef of error  regression")
  }
  
  x <- list(NA)
  for (i in 1:nrow(s)) {
    g <- s[i, 1]
    w <- s[i, 2]
    imp <- lapply(M, Ortho.impulse, dv = g, iv = w, times = times)
    imp <- Reduce(rbind, imp)
    x[[i]] <-apply(imp, 2, quantile, c(.01, .05, .1, .90, 0.95, .99))
  }
  return(x)
}
#==== Данные ====
dataindex <- read.csv("index-a.csv", header = TRUE, sep = ";", dec = ".")
colnames(dataindex)[1] <- "date"

dataindex1 <- subset(dataindex, t %in% 1:436)
dataindex2 <- subset(dataindex, t %in% 386:1076)
dataindex3 <- subset(dataindex, t %in% 1026:2120)


dataindexweek <- weekdata(dataindex, fn = mean)
dataindex1week <- subset(dataindexweek, t %in% 1:89)
dataindex2week <- subset(dataindexweek, t %in% 69:218)
dataindex3week <- subset(dataindexweek, t %in% 200:431)

#financial openness and capital flow
FO <- read.csv("financial openess.csv", header = TRUE, sep = ";", dec = ",")
CF <- read.csv("capital flow.csv", header = TRUE, sep = ";", dec = ",")
FX <- read.csv("FX.csv", header = TRUE, sep = ";", dec = ",")
names(FO) <- c("COUNTRY", 2010:2018)
names(CF) <- c("COUNTRY", 2010:2018)

#periods from days to part of the year
x <- rbind(c(0, 2, 5, 9),
           c(354, 235, 33, 32))
d <- c(x[1, 1] + x[2, 1]/365-0.5, x[1, 2] + x[2, 2]/365 -0.5,
       x[1, 3] + x[2, 3]/365 -0.5, x[1, 4] + x[2, 4]/365 -0.5)

#mean function
o <- function(v, d){
  v <- as.numeric(v)
  st <- as.integer(d[1])+1
  ed <- as.integer((d[2]))+1
  s <- (v[st]*(- d[1] + st) +v[ed]*(d[2]-(ed-1)) + sum(v[(st+1):(ed-1)]))/
    (d[2]-d[1])
  return(s)
}

#financial openness
fo <-  cbind(apply(FO[, -1], 1, o , d[1:2]), apply(FO[, -1], 1, o , d[2:3]),
             apply(FO[, -1], 1, o , d[3:4]))
colnames(fo) <- c("fp", "sp", "tp")
rownames(fo) <- FO[, 1]
#capital flow
cf <- cbind(apply(CF[, -1], 1, o , d[1:2]), apply(CF[, -1], 1, o , d[2:3]),
            apply(CF[, -1], 1, o , d[3:4]))
colnames(cf) <- c("fp", "sp", "tp")
rownames(cf) <- CF[, 1]

rm(x, d, o)
countrynames <- colnames(dataindex)[3:20]



