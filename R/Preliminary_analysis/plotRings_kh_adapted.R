function (year, trwN, trwS = NA, trwE = NA, trwW = NA, animation = FALSE, 
          sys.sleep = 0.2, year.labels = TRUE, d2pith = NA, col.rings = "grey", 
          col.outring = "black", x.rings = "none", col.x.rings = "red", 
          species.name = NA, saveGIF = FALSE, fname = "GIF_plotRings.gif") 
{
  # this assigns a dataframe with year as row.names, and with columsn as the different cores sampled from a single tree:
  TRW <- data.frame(row.names = year, trwN = trwN, trwS = if (exists("trwS") == 
                                                              TRUE) 
    trwS
    else NA, trwE = if (exists("trwE") == TRUE) 
      trwE
    else NA, trwW = if (exists("trwW") == TRUE) 
      trwW
    else NA)
  
  # get the rows that are non-NA for the tree
  TRW <- TRW[as.logical((rowSums(is.na(TRW)) - length(TRW))), ]
  TRW$trw.means <- rowMeans(TRW, na.rm = T) # get mean growth from each core
  
  # if their is a value for d2pith:
  if (!is.na(mean(d2pith, na.rm = T))) {
    TRW.d2pith <- TRW[, 1:4]
    if (!is.na(d2pith[1])) {
      TRW.d2pith$trwN[which.min(is.na(TRW.d2pith$trwN))] <- TRW.d2pith$trwN[which.min(is.na(TRW.d2pith$trwN))] + 
        d2pith[1]
    }
    if (!is.na(d2pith[2])) {
      TRW.d2pith$trwS[which.min(is.na(TRW.d2pith$trwS))] <- TRW.d2pith$trwS[which.min(is.na(TRW.d2pith$trwS))] + 
        d2pith[2]
    }
    if (!is.na(d2pith[3])) {
      TRW.d2pith$trwE[which.min(is.na(TRW.d2pith$trwE))] <- TRW.d2pith$trwE[which.min(is.na(TRW.d2pith$trwE))] + 
        d2pith[3]
    }
    if (!is.na(d2pith[4])) {
      TRW.d2pith$trwW[which.min(is.na(TRW.d2pith$trwW))] <- TRW.d2pith$trwW[which.min(is.na(TRW.d2pith$trwW))] + 
        d2pith[4]
    }
    TRW$trw.means[1] <- rowMeans(TRW.d2pith[1, ], na.rm = T)
  }
  
 
  TRW$trw.acc <- cumsum(TRW$trw.means) # cumulative sum of the tree ring width means
  y <- TRW$trwN - TRW$trwS # difference between the N and southe cores
  y[is.na(y)] <- 0 # just assign this difference to 0 if the values are NA
 
   if (exists("y") == TRUE) 
    TRW$N_S <- y
  
  x <- TRW$trwE - TRW$trwW # difference bewteen E and W cores
  x[is.na(x)] <- 0
  if (exists("x") == TRUE) 
    TRW$E_W <- x
  
  z <- TRW$trw.acc # create the cumulative sum of all RWI means
  
  # calculate quantiles for narrow and wide rings
  q2 <- as.numeric(quantile(TRW[, 5])[2])
  col.narrow.rings <- ifelse(TRW[, 5] <= q2, col.x.rings, col.rings)
  q4 <- as.numeric(quantile(TRW[, 5])[4])
  col.wider.rings <- ifelse(TRW[, 5] >= q4, col.x.rings, col.rings)
  TRW$bai.acc <- pi * (TRW$trw.acc)^2
  TRW$bai.ind <- c(TRW$bai.acc[1], TRW$bai.acc[2:nrow(TRW)] - 
                     TRW$bai.acc[1:nrow(TRW) - 1])
  
  # create gif animation if true:
  if (animation == TRUE) {
    
    for (i in 1:length(x)) { # for each year:
      
      par(mar = c(1, 4, 1, 1) + 0.1)
      cols <- c(rep(col.rings, i - 1), col.outring)
      narrow.cols <- c(col.narrow.rings[1:i - 1], col.outring)
      wider.cols <- c(col.wider.rings[1:i - 1], col.outring)
      max.acc <- max(z, na.rm = T) * 2.5
      symbols(y = y[1:i], x = if (length(x) > 0) 
        y[1:i]
        else x[1:i], circles = z[1:i], inches = FALSE, xlim = c(-max.acc, 
                                                                max.acc), ylim = c(-max.acc, max.acc), xlab = "", 
        ylab = "Width [mm]", main = mtext(bquote(~bold(.("Annual tree growth"))), 
                                          line = 1.5, adj = 0.5, side = 3, cex = 1.5), 
        sub = if (!is.na(species.name)) 
          mtext(bquote(~plain(.("(")) ~ italic(.(species.name)) ~ 
                         plain(.(")"))), line = 0.5, adj = 0.5, side = 3, 
                cex = 1), fg = if (x.rings == "narrow.rings") 
                  narrow.cols
        else if (x.rings == "wider.rings") 
          wider.cols
        else if (x.rings == "none") 
          cols)
      if (year.labels == TRUE) 
        legend("topright", legend = year[i], box.lty = 0, 
               inset = 0.01, cex = 2)
      Sys.sleep(sys.sleep)
    }
  }
  else {
    par(mar = c(1, 4, 1, 1) + 0.1)
    cols <- c(rep(col.rings, length(x) - 1), col.outring)
    narrow.cols <- c(col.narrow.rings[1:length(x) - 1], col.outring)
    wider.cols <- c(col.wider.rings[1:length(x) - 1], col.outring)
    rings.lwd <- c(rep(1, length(x)), 3)
    max.acc <- max(z, na.rm = T) * 2.5
    symbols(y = y, x = if (length(x) > 0) 
      y
      else x, circles = z, inches = FALSE, xlim = c(-max.acc, 
                                                    max.acc), ylim = c(-max.acc, max.acc), xlab = "", 
      ylab = "Width [mm]", main = mtext(bquote(~bold(.("Annual tree growth"))), 
                                        line = 1.5, adj = 0.5, side = 3, cex = 1.5), 
      sub = if (!is.na(species.name)) 
        mtext(bquote(~plain(.("(")) ~ italic(.(species.name)) ~ 
                       plain(.(")"))), line = 0.5, adj = 0.5, side = 3, 
              cex = 1), fg = if (x.rings == "narrow.rings") 
                narrow.cols
      else if (x.rings == "wider.rings") 
        wider.cols
      else if (x.rings == "none") 
        cols)
    if (year.labels == TRUE) 
      legend("topright", legend = paste(range(year)[1], 
                                        "-", range(year)[2]), box.lty = 0, inset = 0.01, 
             cex = 1.2)
  }
  if (saveGIF == TRUE) {
    saveGIF({
      par(bg = "white")
      for (i in 1:length(x)) {
        par(mar = c(1, 4, 1, 1) + 0.1, cex = 1.5)
        cols <- c(rep(col.rings, i - 1), col.outring)
        narrow.cols <- c(col.narrow.rings[1:i - 1], col.outring)
        wider.cols <- c(col.wider.rings[1:i - 1], col.outring)
        max.acc <- max(z, na.rm = T) * 2.5
        symbols(y = y[1:i], x = if (length(x) > 0) 
          y[1:i]
          else x[1:i], circles = z[1:i], inches = FALSE, 
          xlim = c(-max.acc, max.acc), ylim = c(-max.acc, 
                                                max.acc), xlab = "", ylab = "Width [mm]", 
          main = mtext(bquote(~bold(.("Annual tree growth"))), 
                       line = 1.5, adj = 0.5, side = 3, cex = 1.5), 
          sub = if (!is.na(species.name)) 
            mtext(bquote(~plain(.("(")) ~ italic(.(species.name)) ~ 
                           plain(.(")"))), line = 0.5, adj = 0.5, 
                  side = 3, cex = 1), fg = if (x.rings == 
                                               "narrow.rings") 
                    narrow.cols
          else if (x.rings == "wider.rings") 
            wider.cols
          else if (x.rings == "none") 
            cols)
        if (year.labels == TRUE) 
          legend("topright", legend = year[i], box.lty = 0, 
                 inset = 0.01, cex = 2)
      }
    }, movie.name = fname, interval = sys.sleep, nmax = 10, 
    ani.width = 1000, ani.height = 1000)
  }
  else {
    par(mar = c(1, 4, 1, 1) + 0.1)
    cols <- c(rep(col.rings, length(x) - 1), col.outring)
    narrow.cols <- c(col.narrow.rings[1:length(x) - 1], col.outring)
    wider.cols <- c(col.wider.rings[1:length(x) - 1], col.outring)
    rings.lwd <- c(rep(1, length(x)), 3)
    max.acc <- max(z, na.rm = T) * 2.5
    symbols(y = y, x = if (length(x) > 0) 
      y
      else x, circles = z, inches = FALSE, xlim = c(-max.acc, 
                                                    max.acc), ylim = c(-max.acc, max.acc), xlab = "", 
      ylab = "Width [mm]", main = mtext(bquote(~bold(.("Annual tree growth"))), 
                                        line = 1.5, adj = 0.5, side = 3, cex = 1.5), 
      sub = if (!is.na(species.name)) 
        mtext(bquote(~plain(.("(")) ~ italic(.(species.name)) ~ 
                       plain(.(")"))), line = 0.5, adj = 0.5, side = 3, 
              cex = 1), fg = if (x.rings == "narrow.rings") 
                narrow.cols
      else if (x.rings == "wider.rings") 
        wider.cols
      else if (x.rings == "none") 
        cols)
    if (year.labels == TRUE) 
      legend("topright", legend = paste(range(year)[1], 
                                        "-", range(year)[2]), box.lty = 0, inset = 0.01, 
             cex = 1.2)
  }
  print("Output data:")
  if (sum(TRW$trwN, na.rm = TRUE) > 0) 
    print(paste("Length Radius N:  ", round(sum(TRW$trwN, 
                                                na.rm = TRUE), digits = 2), sep = " ", "mm/100"))
  if (sum(TRW$trwS, na.rm = TRUE) > 0) 
    print(paste("Length Radius S:  ", round(sum(TRW$trwS, 
                                                na.rm = TRUE), digits = 2), sep = " ", "mm/100"))
  if (sum(TRW$trwE, na.rm = TRUE) > 0) 
    print(paste("Length Radius E:  ", round(sum(TRW$trwE, 
                                                na.rm = TRUE), digits = 2), sep = " ", "mm/100"))
  if (sum(TRW$trwW, na.rm = TRUE) > 0) 
    print(paste("Length Radius W:  ", round(sum(TRW$trwW, 
                                                na.rm = TRUE), digits = 2), sep = " ", "mm/100"))
  if (sum(TRW$trw.means, na.rm = TRUE) > 0) 
    print(paste("Disc diameter:  ", round(sum(TRW$trw.means, 
                                              na.rm = TRUE) * 2, digits = 2), sep = " ", "mm/100"))
  if (sum(TRW$bai.ind, na.rm = TRUE) > 0) 
    print(paste("Basal Area of the disc:  ", round(sum(TRW$bai.ind, 
                                                       na.rm = TRUE)/10^6, digits = 2), sep = " ", "mm2"))
  TRW
}