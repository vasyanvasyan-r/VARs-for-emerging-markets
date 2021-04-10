#====== CI for Chapter One =======


#==== EM to 10-y yield (daily) ====
#first period
set.seed(1)
models <- chapter1[["daily"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 1, 10)
    x <- mean(dataindex1[, 22]/dataindex1[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(2)
models <- chapter1[["daily"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 1, 10)
    x <- mean(dataindex2[, 22]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(3)
models <- chapter1[["daily"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 3, 1, 10)
    x <- mean(dataindex2[, 22]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMto10y1 <- list(CIes1, CIes2, CIes3)

#==== EM to SP (daily) ====
#first period
set.seed(1)
models <- chapter1[["daily"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 2, 10)
    x <- mean(dataindex1[, 23]/dataindex1[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(2)
models <- chapter1[["daily"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 2, 10)
    x <- mean(dataindex2[, 23]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(3)
models <- chapter1[["daily"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 3, 2, 10)
    x <- mean(dataindex3[, 23]/dataindex3[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMtoSP1 <- list(CIes1, CIes2, CIes3)


#==== EM to 10-y yield (weekly) ====
#first period
set.seed(10)
models <- chapter1[["weekly"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 1, 10)
    x <- mean(dataindex1week[, 22]/dataindex1week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(20)
models <- chapter1[["weekly"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 1, 10)
    x <- mean(dataindex2week[, 22]/dataindex2week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(30)
models <- chapter1[["weekly"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 3, 1, 10)
    x <- mean(dataindex3week[, 22]/dataindex3week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMto10y1w <- list(CIes1, CIes2, CIes3)

#==== EM to SP (weekly) ====
#first period
set.seed(10)
models <- chapter1[["weekly"]][["VARs"]][["first period"]]
CIes1 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes1[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 2, 10)
    x <- mean(dataindex1week[, 23]/dataindex1week[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes1) <- names(models)

#second period
set.seed(20)
models <- chapter1[["weekly"]][["VARs"]][["second period"]]
CIes2 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes2[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000, 
                     Ortho.impulse, Bo.and.W, 3, 2, 10)
    x <- mean(dataindex2[, 23]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes2) <- names(models)

#third period
set.seed(30)
models <- chapter1[["weekly"]][["VARs"]][["third period"]]
CIes3 <- list(NA)
#confidence interval
for (i in 1:length(models)){
  print(names(models)[i])
  CIes3[[i]] <- tryCatch({
    z <- CI.fromboot(models[[i]], 2000,
                     Ortho.impulse, Bo.and.W, 3, 2, 10)
    x <- mean(dataindex2[, 23]/dataindex2[, i+2])
    z <- z*x
  }, error = function(er){
    print("smt went wrong")
  })
}
names(CIes3) <- names(models)
EMtoSP1w <- list(CIes1, CIes2, CIes3)
