read_detrend_rwl <- function(rwl, name){
  #pdf(paste0('outputs/spagplots/',name,'.pdf'))
  #plot(rwl, plot.type = 'spag')
  #dev.off()
  stats <- rwi.stats(rwl)
  #detrend
  rwl.rwi <- detrend(rwl = rwl, method = "ModNegExp")
  rwl <- chron(rwl.rwi)
  #pdf(paste0('outputs/cronplots/', name, '.pdf'))
  #plot(rwl)
  #dev.off()
  #rwl.bai <- chron(bai.out(rwl.rwi))
  #rwl.bai$Year <- min(as.numeric(rownames(rwl))):max(as.numeric(rownames(rwl)))
  #rwl.bai$Site <- name
  rwl$Year <- min(as.numeric(rownames(rwl))):max(as.numeric(rownames(rwl)))
  rwl$Site <- name
  rwl
}