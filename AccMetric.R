
FcstComp <- function(predicted, observed) {
  # predicted - predictions by different models
  # observed - actual observations
  
  library(forecast)
  
  dd1 <- NULL
  ACC <- NULL
  
  
  # OBTAIN PSEUDO R2 - CORRELATION.
  fitcorr <- format(cor(predicted, observed)^2, digits=4)
  # GENERATE REC CURVE
  m1 <- rminer::mmetric(observed, predicted, c('REC'))
  
  if (length(m1$rec) > 2) {
    m2 <- rminer::mmetric(observed, predicted, c('NAREC'))
    dd1 <- rbind(dd1, data.frame(tolerance = m1$rec[, 1], accuracy = m1$rec[, 2]))
  } else {
    m2 <- NA
    dd1 <- rbind(dd1, data.frame(tolerance = m1$rec[1], accuracy = m1$rec[2]))
  }
  
  mase   <- fmase  (predicted, observed)
  smape  <- fsmape (predicted, observed)
  nrows  <- fnrows (observed)
  
  temp <- data.frame(accuracy(observed, predicted),
                     MASE = mase, sMAPE = smape, NAREC= m2, R2 = fitcorr, NRows = nrows)
  ACC <- rbind(ACC, temp)
  
  res <- list(REC = dd1, ACC = ACC)
  
  res
}


fmase <- function(predicted, observed) {
  error <- 0
  
  if (length(observed) != length(predicted)) {
    return(NULL)
  } else if (length(observed) == 0 | length(predicted) == 0) {
    return(NULL)
  } else if (sum(as.numeric(predicted)) == 0 & sum(as.numeric(observed)) == 0) {
    error <- 0
  } else {
    denom = (sum(as.numeric(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)]))))/(length(observed) - 1)
    error = sum(as.numeric(abs(observed-predicted)) / denom)/length(observed)
  }
  return(error)
}

# fsmape <- function(predicted, observed) {
#   error <- 0
#   
#   if (length(observed) != length(predicted)) {
#     return(NULL)
#   } else if (length(observed) == 0 | length(predicted) == 0) {
#     return(NULL)
#   } else if (predicted == 0 & sum(as.numeric(observed)) == 0) {
#     error <- 0
#   } else {
#     error = sum((abs(observed-predicted)) / (observed+predicted)) / length(observed);
#     # denom = (sum(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)])))/(length(observed) - 1)
#     #  error = sum((abs(observed-predicted)) / denom)/length(observed);
#   }
#   return(100.0 * error)
# }

fsmape <- function(predicted, observed) {
  error <- 0
  temp <- 0
  
  if (length(observed) != length(predicted)) {
    return(NULL)
  } else if (length(observed) == 0 | length(predicted) == 0) {
    return(NULL)
  } else {
    for (i in 1:length(observed)) {
      if (predicted[i] == 0 & observed[i] == 0) {
        temp[i]  <- 0
      } else {
        temp[i] <- (abs(observed[i]-predicted[i])) / (observed[i]+predicted[i])
      }
    }
    error <- mean(temp)
  }
  return(100.0 * error)
}

# fsmape(c(1,1,0), c(1,1,0))

fnrows <- function(observed){
  return (length(observed))
}



ModelEvaluate <- function(predicted, observed, group = NULL) {
  library(foreach)
  
  if (length(predicted) != length(observed)) {
    stop('predicted values should have same length as observed!')
  }
  
  out <- list()
  out$overall <- FcstComp(predicted, observed)
  
  if(!is.null(group)) {
    if (length(predicted) != length(group)) {
      stop('group length should be same as predicted!')
    }
    for(g in unique(group)) {
      idx <- which(g == group)
      out[[g]] <- FcstComp(predicted[idx], observed[idx])
    }
  }
  
  temp1 <- lapply(out, function(x) x$ACC)
  temp2 <- lapply(out, function(x) x$REC)
  
  acc <- foreach(nn = names(temp1), .combine = 'rbind') %do% {
    cbind(Label = nn, temp1[[nn]])
  }
  
  rec <- foreach(nn = names(temp1), .combine = 'rbind') %do% {
    cbind(Label = nn, temp2[[nn]])
  }
  
  res <- list(ACC = acc, REC = rec)
}

RECPlot <- function(REC, nrow, ncol, filename = NULL) {
  RECList <- split(REC, f = REC$Label)
  
  first <- RECList[[1]]
  
  p <- ggplot(first, aes(x = tolerance, y = accuracy)) +
    geom_line(size = 0.75) + xlim(0, 20000)
  
  rs <- lapply(RECList[-1], function(x, p) { p %+% x }, p = p)
  
  rs <- c(list(p), rs)
  
  names(rs) <- names(RECList)
  
  for(nn in names(rs)) {
    rs[[nn]] <- rs[[nn]] + ggtitle(paste0(nn, ' REC'))
  }
  
  
  if (!is.null(filename)) {
    
    png(filename = filename, type = 'cairo', width = 6000, height = 4000, res = 300)
    do.call(gridExtra::grid.arrange, c(rs, list(nrow = nrow, ncol = ncol)))
    dev.off()
    
    print(paste0('REC Plot save to ', filename, ' successfully!'))
    
  } else {
    do.call(gridExtra::grid.arrange, c(rs, list(nrow = nrow, ncol = ncol)))
  }
}


# fmase(c(110,210),c(100,200))
# fmase(c(90,190), c(100,200))
# 
# fsmape(c(110,210),c(100,200))
# fsmape(c(90,190), c(100,200))
# 
# 
# fmase(c(100000,200000),c(100,200))
# fmase(c(0.1,0.2), c(100,200))
# 
# fsmape(c(100000,200000),c(100,200))
# fsmape(c(0.1,0.2), c(100,200))


# Showing multiple ggplots in single chart
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


