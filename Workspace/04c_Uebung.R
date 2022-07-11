
# Grundgerüst ----
nc <- 5 # n cluster
mcs <- 4 # mean cluster size
mcs*nc
dat <- data.frame(idschool = rep(1:mcs, each = nc),
                  idstud = rep(1:nc, mcs),
                  Leistung = round(rnorm(mcs*nc) * 100 + 500, 0),
                  weights = runif(mcs*nc))
head(dat)

pacman::p_load(miceadds,
               dplyr,
               plyr,
               data.table,
               doParallel, foreach,
               Rcpp)

# stats::weighted.mean
df <- data.frame(idschool = sort(unique(dat$idschool)), wret = numeric(5))

# miceadds ----
GroupMean # rowsum(weights*x, group) / rowsum(weights, group)
# so kriegt man das auch in aggregate rein


# parallel ----
phys_cores <- detectCores(logical = FALSE)
cl <- makeCluster(phys_cores - 1, type = "PSOCK")
doParallel::registerDoParallel(cl)

# c++ ----

cppFunction('NumericVector group_meanC(NumericVector x, NumericVector w, NumericVector g) { 
               NumericVector ug = unique(g);
               int ng = ug.size();
               NumericVector out(ng);
               
               for(int gg = 0; gg < ng; gg++){
                 NumericVector subX = x[g == gg + 1];
                 NumericVector subW = w[g == gg + 1];
                 out[gg] = sum(subX * subW) / sum(subW);
               }
               
               return(out);
             }') 

# -> micro-BM ----

microbenchmark::microbenchmark(
  "for" = {  
    for(gg in 1:nrow(df)) {
      ind <- dat$idschool == gg
      df$wret[gg] <- weighted.mean(dat$Leistung[ind], dat$weights[ind])
    } 
  },
  
  "by" = {
    by(dat[, c("Leistung", "weights")], dat[, "idschool"], function(x) weighted.mean(x$Leistung, x$weights))
  },
  
  "parallel" = {
    foreach::foreach(gg = 1:nrow(df), .combine = rbind) %dopar% {
      ind <- dat$idschool == gg
      df$wret[gg] <- weighted.mean(dat$Leistung[ind], dat$weights[ind])
      df[gg,,drop = FALSE]
    }
  },
  
  "c++" = {
    df$wret = group_meanC(dat$Leistung, dat$weights, dat$idschool)
  },
  
  "rowsum" = {
    GroupMean(dat$Leistung, dat$idschool, weights = dat$weights)
  },
  
  "dt" = {
    DT <- data.table(dat)
    DT[, list(wret = weighted.mean(Leistung, weights)), by = idschool]
  },
  
  "dplyr" = {
    dat %>% group_by(idschool) %>% summarise(wrt = weighted.mean(Leistung, weights))
  },
  
  "plyr" = {
    ddply(dat, .(idschool), function(x) data.frame(wret = weighted.mean(x$Leistung, x$weights)))
  }
)

mcs*nc