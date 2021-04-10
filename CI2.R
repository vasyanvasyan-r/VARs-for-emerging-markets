#====== CI for Chapter Two =======


#==== EM to DXY (daily) ====
#first period
set.seed(4)
models <- chapter2[["daily"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 1, 10)
    x <- mean(dataindex1[, 21]/dataindex1[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(5)
models <- chapter2[["daily"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 1, 10)
    x <- mean(dataindex2[, 21]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(6)
models <- chapter2[["daily"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 4, 1, 10)
    x <- mean(dataindex2[, 21]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMtoDXY2 <- list(CIes1, CIes2, CIes3)

#==== EM to SP (daily) ====
#first period
set.seed(4)
models <- chapter2[["daily"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 2, 10)
    x <- mean(dataindex1[, 23]/dataindex1[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(5)
models <- chapter2[["daily"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 2, 10)
    x <- mean(dataindex2[, 23]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(5)
models <- chapter2[["daily"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 4, 2, 10)
    x <- mean(dataindex3[, 23]/dataindex3[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMtoSP2 <- list(CIes1, CIes2, CIes3)


#==== EM to DXY (weekly) ====
#first period
set.seed(40)
models <- chapter2[["weekly"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 1, 10)
    x <- mean(dataindex1week[, 21]/dataindex1week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(50)
models <- chapter2[["weekly"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 1, 10)
    x <- mean(dataindex2week[, 21]/dataindex2week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(60)
models <- chapter2[["weekly"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 4, 1, 10)
    x <- mean(dataindex3week[, 21]/dataindex3week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMtoDXY2w <- list(CIes1, CIes2, CIes3)

#==== EM to SP (weekly) ====
#first period
set.seed(40)
models <- chapter2[["weekly"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 2, 10)
    x <- mean(dataindex1week[, 22]/dataindex1week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(50)
models <- chapter2[["weekly"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 4, 2, 10)
    x <- mean(dataindex2[, 22]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(60)
models <- chapter2[["weekly"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 4, 2, 10)
    x <- mean(dataindex2[, 22]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMtoSP2w <- list(CIes1, CIes2, CIes3)
