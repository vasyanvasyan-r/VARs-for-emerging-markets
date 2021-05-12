library(plotrix)
timeframe <- c("daily", "weekly")
war <- c("EM to SP500 ch1", "EM to 10y yield ch1", "EM to SP500 ch2", 
         "EM to Dollar index ch2", "EM to SP500 ch3", "EM to 10y yield ch3", 
         "EM to Dollar index ch3")
periods <-  c("first period", "second period", "third period")
#Elastic version

setwd("C:/Users/vasil/Desktop/D is not for dragons/YandexDisk/D is not for Dragons/r scripts/plots")
# Basic plot of x and y :

for (i in 1:2) { # type of impulse
  for (j in 1:2) { #weekly or daily
    for (h in 1:3) { #periods
      for (g in 1:18) { #country
        d <- chapter1[[3]][[i]][[j]][[h]][[g]][, -1]
        y <- d[4, ] 
        x <- 1:10 
        cn <- paste(names(chapter1[[3]][[i]][[j]][[h]][g]), 
                    names(chapter1[[3]][i]), names(chapter1[[3]][[i]][j]), 
                    names(chapter1[[3]][[i]][[j]][h]), "ch1", sep = ", ")
        pdf(paste(cn, "pdf", sep = "."))
        plot(x,y,col=rgb(0.4,0.4,0.8,0.6), pch=16, cex=1.5,
             xlab=cn, ylab="impulse",)
        lines(x, y, col=2, lwd=2 )
        lines(x, rep(0, 10), col=1, lwd=1)
        polygon(c(rev(x), x), c(rev(d[2, ]), d[6, ]),
                col = rgb(0.7,0.7,0.7,0.4), border = NA)
        dev.off()
      }
    }
  }
}

for (i in 1:2) { # type of impulse
  for (j in 1:2) { #weekly or daily
    for (h in 1:3) { #periods
      for (g in 1:18) { #country
        d <- chapter2[[3]][[i]][[j]][[h]][[g]][, -1]
        y <- d[4, ] 
        x <- 1:10 
        cn <- paste(names(chapter2[[3]][[i]][[j]][[h]][g]), 
                    names(chapter2[[3]][i]), names(chapter2[[3]][[i]][j]), 
                    names(chapter2[[3]][[i]][[j]][h]), "ch1", sep = ", ")
        pdf(paste(cn, "pdf", sep = "."))
        plot(x,y,col=rgb(0.4,0.4,0.8,0.6), pch=16, cex=1.5,
             xlab=cn, ylab="impulse",)
        lines(x, y, col=2, lwd=2 )
        lines(x, rep(0, 10), col=1, lwd=1)
        polygon(c(rev(x), x), c(rev(d[2, ]), d[6, ]),
                col = rgb(0.7,0.7,0.7,0.4), border = NA)
        dev.off()
      }
    }
  }
}

for (i in 1:3) { # type of impulse
  for (j in 1:2) { #weekly or daily
    for (h in 1:3) { #periods
      for (g in 1:18) { #country
        d <- chapter3[[3]][[i]][[j]][[h]][[g]][, -1]
        y <- d[4, ] 
        x <- 1:10 
        cn <- paste(names(chapter3[[3]][[i]][[j]][[h]][g]), 
                    names(chapter3[[3]][i]), names(chapter3[[3]][[i]][j]), 
                    names(chapter3[[3]][[i]][[j]][h]), "ch3", sep = ", ")
        pdf(paste(cn, "pdf", sep = "."))
        plot(x,y,col=rgb(0.4,0.4,0.8,0.6), pch=16, cex=1.5,
             xlab=cn, ylab="impulse",)
        lines(x, y, col=2, lwd=2 )
        lines(x, rep(0, 10), col=1, lwd=1)
        polygon(c(rev(x), x), c(rev(d[2, ]), d[6, ]),
                col = rgb(0.7,0.7,0.7,0.4), border = NA)
        dev.off()
      }
    }
  }
}
#Cross country plots


z <- function(dataimp, ch, cl, i, j, g, tp, regr) {
  
  y <- as.vector(dataimp[i, j, ,])
  x <- as.vector(ch)
  d <- as.data.frame(cbind(x, y, c(rep(1, 18), rep(2, 18), rep(3, 18))))
  d$cn <- rep(countrynames, 3)
  colnames(d) <- c("xe", "impulse", "p", "cn")
  k <- cl[[g]][[i]][[j]]
  plot(xe ~ impulse, data = d[d[,3] == 1 & k$cluster == 1, ], col = "green",
       pch = 15, main = paste( war[j], tp, timeframe[i], sep = ", "),
       xlab = regr, ylab = "impulse",
       xlim = c(min(d[, 1]), max(d[, 1])),
       ylim = c(min(d[, 2]), max(d[, 2])))
  points(d[d[,3] == 2 & k$cluster == 1, ], col = "blue", pch = 15)
  points(d[d[,3] == 3 & k$cluster == 1, ], col = "red", pch = 15)
  if (length(k$size) == 2){ 
    points(d[d[,3] == 2 & k$cluster == 2, ], col = "blue", pch = 16)
    points(d[d[,3] == 3 & k$cluster == 2, ], col = "red", pch = 16)
    points(d[d[,3] == 1 & k$cluster == 2, ], col = "green", pch = 16)
    
  } else {
    if (length(k$size) == 3) {
      points(d[d[,3] == 2 & k$cluster == 2, ], col = "blue", pch = 16)
      points(d[d[,3] == 3 & k$cluster == 2, ], col = "red", pch = 16)
      points(d[d[,3] == 1 & k$cluster == 2, ], col = "green", pch = 16)
      points(d[d[,3] == 2 & k$cluster == 3, ], col = "blue", pch = 17)
      points(d[d[,3] == 3 & k$cluster == 3, ], col = "red", pch = 17)
      points(d[d[,3] == 1 & k$cluster == 3, ], col = "green", pch = 17)
    } else{
      if (length(k$size) == 4) {
        points(d[d[,3] == 2 & k$cluster == 2, ], col = "blue", pch = 16)
        points(d[d[,3] == 3 & k$cluster == 2, ], col = "red", pch = 16)
        points(d[d[,3] == 1 & k$cluster == 2, ], col = "green", pch = 16)
        points(d[d[,3] == 2 & k$cluster == 3, ], col = "blue", pch = 17)
        points(d[d[,3] == 3 & k$cluster == 3, ], col = "red", pch = 17)
        points(d[d[,3] == 1 & k$cluster == 3, ], col = "green", pch = 17)
        points(d[d[,3] == 2 & k$cluster == 4, ], col = "blue", pch = 13)
        points(d[d[,3] == 3 & k$cluster == 4, ], col = "red", pch = 13)
        points(d[d[,3] == 1 & k$cluster == 4, ], col = "green", pch = 13)
      } else {
        if (length(k$size) == 5) {
          points(d[d[,3] == 2 & k$cluster == 2, ], col = "blue", pch = 16)
          points(d[d[,3] == 3 & k$cluster == 2, ], col = "red", pch = 16)
          points(d[d[,3] == 1 & k$cluster == 2, ], col = "green", pch = 16)
          points(d[d[,3] == 2 & k$cluster == 3, ], col = "blue", pch = 17)
          points(d[d[,3] == 3 & k$cluster == 3, ], col = "red", pch = 17)
          points(d[d[,3] == 1 & k$cluster == 3, ], col = "green", pch = 17)
          points(d[d[,3] == 2 & k$cluster == 4, ], col = "blue", pch = 13)
          points(d[d[,3] == 3 & k$cluster == 4, ], col = "red", pch = 13)
          points(d[d[,3] == 1 & k$cluster == 4, ], col = "green", pch = 13)
          points(d[d[,3] == 2 & k$cluster == 5, ], col = "blue", pch = 12)
          points(d[d[,3] == 3 & k$cluster == 5, ], col = "red", pch = 12)
          points(d[d[,3] == 1 & k$cluster == 5, ], col = "green", pch = 12)
        } else {
          
          points(d[d[,3] == 2 & k$cluster == 2, ], col = "blue", pch = 16)
          points(d[d[,3] == 3 & k$cluster == 2, ], col = "red", pch = 16)
          points(d[d[,3] == 1 & k$cluster == 2, ], col = "green", pch = 16)
          points(d[d[,3] == 2 & k$cluster == 3, ], col = "blue", pch = 17)
          points(d[d[,3] == 3 & k$cluster == 3, ], col = "red", pch = 17)
          points(d[d[,3] == 1 & k$cluster == 3, ], col = "green", pch = 17)
          points(d[d[,3] == 2 & k$cluster == 4, ], col = "blue", pch = 13)
          points(d[d[,3] == 3 & k$cluster == 4, ], col = "red", pch = 13)
          points(d[d[,3] == 1 & k$cluster == 4, ], col = "green", pch = 13)
          points(d[d[,3] == 2 & k$cluster == 5, ], col = "blue", pch = 12)
          points(d[d[,3] == 3 & k$cluster == 5, ], col = "red", pch = 12)
          points(d[d[,3] == 1 & k$cluster == 5, ], col = "green", pch = 12)
          points(d[d[,3] == 2 & k$cluster == 6, ], col = "blue", pch = 8)
          points(d[d[,3] == 3 & k$cluster == 6, ], col = "red", pch = 8)
          points(d[d[,3] == 1 & k$cluster == 6, ], col = "green", pch = 8)
        }
      }
    }
  }
}

for (j in 1:7) {
  
    plotname <- paste(war[j], "cashflow", sep = ", ")
    pdf(paste(plotname, "pdf", sep = "."))
    par(mfcol = c(3, 2))
    z(IMP, cf, FXclstr, 1, j, 1, "instant", "cashflow")
    z(IMPlong, cf, FXclstr.long, 1, j, 1, "10 days accumulated", "cashflow")
    z(IMPs, cf, FXclstr.s, 1, j, 1, "after 10 days", "cashflow")
    z(IMP, cf, FXclstr, 2, j, 1, "instant", "cashflow")
    z(IMPlong, cf, FXclstr.long, 2, j, 1, "10 days accumulated", "cashflow")
    z(IMPs, cf, FXclstr.s, 2, j, 1, "after 10 days", "cashflow")
    dev.off()
  
}
for (j in 1:7) {
  
  plotname <- paste(war[j], "financial openness", sep = ", ")
  pdf(paste(plotname, "pdf", sep = "."))
  par(mfcol = c(3, 2))
  z(IMP, fo, FXclstr, 1, j, 2, "instant", "financial openness")
  z(IMPlong, fo, FXclstr.long, 1, j, 2, "10 days accumulated", "financial openness")
  z(IMPs, fo, FXclstr.s, 1, j, 2, "after 10 days", "financial openness")
  z(IMP, fo, FXclstr, 2, j, 2, "instant", "financial openness")
  z(IMPlong, fo, FXclstr.long, 2, j, 2, "10 days accumulated", "financial openness")
  z(IMPs, fo, FXclstr.s, 2, j, 2, "after 10 days", "financial openness")
  dev.off()
  
}
