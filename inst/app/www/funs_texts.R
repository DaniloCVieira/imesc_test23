checkData<-function (x, method = c("xts", "zoo", "data.frame", "matrix",
                                   "vector"), na.rm = TRUE, quiet = TRUE, ...)
{
  method = method[1]
  switch(method, vector = {
    if (NCOL(x) > 1) {
      if (!quiet) warning("The data provided is not a vector or univariate time series.  Used only the first column")
      x = x[, 1]
    }
    if (na.rm) x = na.omit(x)
    x = as.vector(x)
  }, matrix = {
    x = as.matrix(x, ncol = NCOL(x))
  }, data.frame = {
    x = as.data.frame(x)
  }, zoo = {
    if (inherits(x, what = "zoo")) {
      x = as.zoo(x)
    } else {
      if (class(x) == "matrix" | class(x) == "data.frame") {
        x = zoo(x, order.by = as.POSIXct(rownames(x)))
      } else {
        if (class(x) == "numeric") {
          if (is.null(names(x))) x = zoo(matrix(x, ncol = NCOL(x))) else x = zoo(matrix(x,
                                                                                        ncol = NCOL(x)), order.by = as.POSIXct(names(x)))
        }
      }
    }
  }, xts = {
    if (is.xts(x)) return(x)
    if (!xtsible(x)) if (class(x) == "numeric") {
      x = zoo(matrix(x, ncol = NCOL(x)))
      if (!quiet) warning("The data cannot be converted into a time series.  Returning a 'zoo' object. ")
    } else stop("The data cannot be converted into a time series.  If you are trying to pass in names from a data object with one column, you should use the form 'data[rows, columns, drop = FALSE]'.  Rownames should have standard date formats, such as '1985-03-15'. ") else x = try.xts(x)
  })
  return(x)
}

chart.Correl<-function (R, histogram = F,barplot = TRUE, method = c("pearson", "kendall","spearman"),cex.cor=0.5,newcolhabs,palette,cex=1, ...) {

  x = checkData(R, method = "matrix")
  if (missing(method))
    method = method[1]
  cormeth <- method
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs",
                        method = cormeth, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    cex=cex.cor
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***",
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE,
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }

  bar.panel = function(x, ... = NULL) {
    #x<-x[,1]
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    #his <- barplot(table(x), plot=F)
    tabx<-table(x)
    maxn<-max(tabx, na.rm=T)
    breaks <-  seq(min(x), max(x), len=length(tabx))
    nB <- length(breaks)
    y <- table(x)
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = newcolhabs[[palette]](nB))
    # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
  }


  if (barplot){
    colors<-unlist(lapply(data.frame(x),function(x){
      #tabx<-table(x)
      newcolhabs[[palette]](length(x))#[as.factor(x)]
    }))

    graphics::pairs(x, gap = 0,
                    #lower.panel = panel.smooth,
                    upper.panel = panel.cor,
                    diag.panel = bar.panel,cex.cor=cex.cor, col=colors,  cex=cex,...)
  }


}

getglobal_model<-function(data,model='rf'){
  models<-lapply(attr(data,model), function (x) {
    if(class(x)[1]=='train'){x} else{ x[[1]]}
  })

  res<-caret_global_stats(models)
  res

}
getmodelist_names<-function(saved_data){
  x<-saved_data[[1]]

  nb=lapply(saved_data, function(x) names(attr(x,"nb")))
  rf=lapply(saved_data, function(x) names(attr(x,"rf")))
  som=lapply(saved_data, function(x) names(attr(x,"som")))
  sgboost=lapply(saved_data, function(x) names(attr(x,"sgboost")))
  svm=lapply(saved_data, function(x) names(attr(x,"svm")))
  knn=lapply(saved_data, function(x) names(attr(x,"knn")))

  reat<-lapply(list(som=som,nb=nb,svm=svm,knn=knn,rf=rf,sgboost=sgboost), function(x) unlist(x))


  model_names<-do.call(c,c(som,nb,svm,knn,rf,sgboost))
  res<-data.frame(Orig_Datalist=names(model_names),Attribute= rep(names(reat),unlist(lapply(reat,length))),model=model_names)


  res

}
getmodelist<-function(saved_data){
  nb=lapply(saved_data, function(x) attr(x,"nb"))
  rf=lapply(saved_data, function(x) attr(x,"rf"))
  som=lapply(saved_data, function(x) attr(x,"som"))
  sgboost=lapply(saved_data, function(x)attr(x,"sgboost"))
  svm=lapply(saved_data, function(x) attr(x,"svm"))
  knn=lapply(saved_data, function(x) attr(x,"knn"))
  res<-do.call(c,c(som,nb,svm,knn,rf,sgboost))

  pic<-unlist(lapply(res,function(x) class(x)[[1]]=="list"))
  if(length(which(pic))>0){
    re0<-unlist(res[pic],recursive = F)
    names(re0)<-names(res[pic])
    res[pic]<-re0}
  res

}
#' @export
shannon<-function (vect, base = 2)
{
  vect <- as.numeric(vect)
  if (all(vect <= 0))
    return(c(H = NA, J = NA))
  vect <- vect/sum(vect)
  vect <- vect * log(vect, base)
  h <- sum(vect[is.finite(vect)])
  hmax <- log(1/length(vect), base)
  res <- c(H = -h, J = h/hmax)
  attributes(res)$baseLog <- base
  res
}





#' @export
metaMDS1<-function (comm, distance = "bray", k = 2, try = 20, trymax = 20,
                    engine = c("monoMDS", "isoMDS"), autotransform = TRUE,
                    noshare = (engine == "isoMDS"), wascores = TRUE, expand = TRUE,
                    trace = 1, plot = FALSE, previous.best, ...)
{
  engine <- match.arg(engine)
  trymax <- max(trymax, try)
  commname <- deparse(substitute(comm), width.cutoff = 500L)
  if (length(commname) > 1L) {
    paste(commname, collapse = "", sep = "")
    commname <- gsub("[ ]{2,}", " ", commname)
  }
  if (any(autotransform, noshare > 0, wascores) && any(comm <
                                                       0, na.rm = TRUE)) {
    message("'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE")
    wascores <- FALSE
    autotransform <- FALSE
    noshare <- FALSE
  }
  if (inherits(comm, "dist")) {
    dis <- comm
    if (is.null(attr(dis, "method")))
      attr(dis, "method") <- "user supplied"
    wascores <- FALSE
  }
  else if ((is.matrix(comm) || is.data.frame(comm)) && isSymmetric(unname(as.matrix(comm)))) {
    dis <- as.dist(comm)
    attr(dis, "method") <- "user supplied"
    wascores <- FALSE
  }
  else {
    if (trace > 2)
      cat(">>> Calculation of dissimilarities\n")
    dis <- metaMDSdist(comm, distance = distance, autotransform = autotransform,
                       noshare = noshare, trace = trace, commname = commname,
                       ...)
  }
  if (missing(previous.best))
    previous.best <- NULL
  if (trace > 2)
    cat(">>> NMDS iterations\n")
  out <- metaMDSiter2(dis, k = k, try = try, trymax = trymax,
                     trace = trace, plot = plot, previous.best = previous.best,
                     engine = engine, ...)
  if (out$stress < 0.001) {
    warning("stress is (nearly) zero: you may have insufficient data")
  }
  if (trace > 2)
    cat(">>> Post-processing NMDS\n")
  points <- postMDS(out$points, dis, plot = max(0, plot - 1),
                    ...)
  if (engine == "monoMDS" && !is.null(scl <- attr(points,
                                                  "internalscaling"))) {
    out$dist <- out$dist/scl
    out$dhat <- out$dhat/scl
  }
  if (is.null(rownames(points)))
    rownames(points) <- rownames(comm)
  wa <- if (wascores) {
    comm <- attr(dis, "comm")
    wascores(points, comm, expand = expand)
  }
  else {
    NA
  }
  out$points <- points
  out$species <- wa
  out$call <- match.call()
  if (is.null(out$data))
    out$data <- commname
  class(out) <- c("metaMDS", class(out))
  out
}

metaMDSiter2<-function (dist, k = 2, try = 20, trymax = 20, trace = 1, plot = FALSE,
                        previous.best, engine = "monoMDS", maxit = 200, parallel = getOption("mc.cores"),
                        ...)
{
  engine <- match.arg(engine, c("monoMDS", "isoMDS"))
  EPS <- 0.05
  if (engine == "monoMDS")
    EPS <- EPS/100
  RESLIM <- 0.01
  RMSELIM <- 0.005
  SOL <- FALSE
  converged <- FALSE
  isotrace <- max(0, trace - 1)
  monotrace <- engine == "monoMDS" && trace > 1
  monomsg <- c("no. of iterations >= maxit", "stress < smin",
               "stress ratio > sratmax", "scale factor of the gradient < sfgrmin")
  monostop <- function(mod) {
    if (mod$maxits == 0)
      return(NULL)
    lab <- monomsg[mod$icause]
    cat("   ", mod$iters, "iterations: ", lab, "\n")
  }
  if (trace && engine == "monoMDS")
    stopcoz <- numeric(4)
  if (!missing(previous.best) && !is.null(previous.best)) {
    if (inherits(previous.best, "metaMDS") || is.list(previous.best) &&
        all(c("points", "stress") %in% names(previous.best))) {
      init <- previous.best$points
      nc <- NCOL(init)
      if (nc > k)
        init <- init[, 1:k, drop = FALSE]
      else if (nc < k)
        for (i in 1:(k - nc)) init <- cbind(init, runif(NROW(init),
                                                        -0.1, 0.1))
      if (trace)
        cat(sprintf("Starting from %d-dimensional configuration\n",
                    nc))
    }
    else {
      init <- as.matrix(previous.best)
    }
    s0 <- switch(engine, monoMDS = monoMDS(dist, y = init,
                                           k = k, maxit = 0, ...), isoMDS = isoMDS(dist, y = init,
                                                                                   k = k, maxit = 0))
    if (is.list(previous.best) && !is.null(previous.best$stress) &&
        !isTRUE(all.equal(previous.best$stress, s0$stress))) {
      if (trace)
        cat("Stress differs from 'previous.best': reset tries\n")
      if (inherits(previous.best, "metaMDS"))
        previous.best$tries <- 0
    }
  }
  else {
    s0 <- switch(engine, monoMDS = monoMDS(dist, y = cmdscale(dist,
                                                              k = k), k = k, maxit = maxit, ...), isoMDS = isoMDS(dist,
                                                                                                                  k = k, trace = isotrace, maxit = maxit))
  }
  if (trace)
    cat("Run 0 stress", s0$stress, "\n")
  if (monotrace)
    monostop(s0)
  tries <- 0


  if (is.null(parallel))
    parallel <- 1
  hasClus <- inherits(parallel, "cluster")
  isParal <- hasClus || parallel > 1
  isMulticore <- .Platform$OS.type == "unix" && !hasClus
  if (isParal && !isMulticore && !hasClus) {
    parallel <- makeCluster(parallel)
    clusterEvalQ(parallel, library(vegan))
  }
  if (inherits(parallel, "cluster")) {
    nclus <- length(parallel)}  else{ nclus <- parallel}
  withProgress(message = "Running ...",
               min = 1,
               max = try,
               {
                 while (tries < try || tries < trymax && !converged) {
                   init <- replicate(nclus, initMDS(dist, k = k))
                   if (nclus > 1)
                     isotrace <- FALSE
                   if (isParal) {
                     if (isMulticore) {
                       stry <- mclapply(1:nclus, function(i) switch(engine,
                                                                    monoMDS = monoMDS(dist, init[, , i], k = k,
                                                                                      maxit = maxit, ...), isoMDS = isoMDS(dist,
                                                                                                                           init[, , i], k = k, maxit = maxit, tol = 1e-07,
                                                                                                                           trace = isotrace)), mc.cores = parallel)
                     }
                     else {
                       stry <- parLapply(parallel, 1:nclus, function(i) switch(engine,
                                                                               monoMDS = monoMDS(dist, init[, , i], k = k,
                                                                                                 maxit = maxit, ...), isoMDS = isoMDS(dist,
                                                                                                                                      init[, , i], k = k, maxit = maxit, tol = 1e-07,
                                                                                                                                      trace = isotrace)))
                     }
                   }
                   else {
                     stry <- list(switch(engine, monoMDS = monoMDS(dist,
                                                                   init[, , 1], k = k, maxit = maxit), isoMDS = isoMDS(dist,
                                                                                                                       init[, , 1], k = k, maxit = maxit, tol = 1e-07,
                                                                                                                       trace = isotrace)))
                   }

                   for (i in 1:nclus) {
                     tries <- tries + 1
                     if (trace)
                       cat("Run", tries, "stress", stry[[i]]$stress,
                           "\n")
                     if (trace && engine == "monoMDS")
                       stopcoz[stry[[i]]$icause] <- stopcoz[stry[[i]]$icause] +
                         1L
                     if (monotrace)
                       monostop(stry[[i]])
                     if ((s0$stress - stry[[i]]$stress) > -EPS) {
                       pro <- procrustes(s0, stry[[i]], symmetric = TRUE)
                       if (plot && k > 1)
                         plot(pro)
                       if (stry[[i]]$stress < s0$stress) {
                         s0 <- stry[[i]]
                         converged <- FALSE
                         if (trace)
                           cat("... New best solution\n")
                       }
                       summ <- summary(pro)
                       if (trace)
                         cat("... Procrustes: rmse", summ$rmse, " max resid",
                             max(summ$resid), "\n")
                       if (summ$rmse < RMSELIM && max(summ$resid) <
                           RESLIM) {
                         if (trace)
                           cat("... Similar to previous best\n")
                         converged <- TRUE
                       }
                     }
                     flush.console()
                   }


                   incProgress(1)
                 }
               })



  if (trace) {
    if (converged)
      cat("*** Solution reached\n")
    else if (engine == "monoMDS") {
      cat(sprintf("*** No convergence -- %s stopping criteria:\n",
                  engine))
      for (i in seq_along(stopcoz)) if (stopcoz[i] > 0)
        cat(sprintf("%6d: %s\n", stopcoz[i], monomsg[i]))
    }
  }
  if (isParal && !isMulticore && !hasClus)
    stopCluster(parallel)
  if (!missing(previous.best) && inherits(previous.best, "metaMDS")) {
    tries <- tries + previous.best$tries
  }
  out <- s0
  out$ndim = k
  out$data <- attr(dist, "commname")
  out$distance <- attr(dist, "method")
  out$converged <- converged
  out$tries <- tries
  out$engine <- engine
  out
}




#' @export
js_tip<-function(mytips,id){
  paste0("
var mytips = [",mytips,"];
$('#",id,"').on('shown.bs.select', function() {
  var $lis = $($(this).data('selectpicker').selectpicker.current.elements);
  $lis.each(function(i) {
    $(this).attr('title', mytips[i]);
  });
});")
}




#' @export
NestedMenu <- function(
    label, items, trigger = "left",
    width = NULL, height = NULL, elementId = NULL
) {

  # forward options using x
  x = list(
    label = label,
    trigger = match.arg(trigger, c("left", "right", "hover")),
    items = items
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'NestedMenu',
    x,
    width = width,
    height = height,
    package = 'NestedMenu',
    elementId = elementId
  )
}

#' @export
widget_html.NestedMenu <- function(id, style, class, ...){
  tags$div(
    id = id, class = class, style = style,
    tags$span(class = "btn btn-primary")
  )
}

#' @export
NestedMenuOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'NestedMenu', width, height, package = 'NestedMenu')
}

#' @export
renderNestedMenu <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, NestedMenuOutput, env, quoted = TRUE)
}

#' @export
transf_data<-function(data,transf){
  data<-switch(
    transf,
    'log2'={decostand(data, "log", na.rm = T, logbase = 2)},
    'log10'={decostand(data, "log", na.rm = T, logbase = 10)},
    'total'={decostand(data, "total", na.rm = T)},
    'max'={decostand(data, "max", na.rm = T)},
    'frequency'={decostand(data, "frequency", na.rm = T)},
    'range'={decostand(data, "range", na.rm = T)},
    'pa'={decostand(data, "pa", na.rm = T)},
    'chi.square'={decostand(data, "chi.square", na.rm = T)},
    'hellinger'={decostand(data, "hellinger", na.rm = T)},
    'sqrt2'={sqrt(data)},
    'sqrt4'={sqrt(sqrt(data))},
    'log2(x+1)'={log2(data+1)},
    'log10(x+1)'={log10(data+1)},
    'BoxCox'={
      imp <- preProcess(data, method = "BoxCox")
      data <- predict(imp, data)
    },
    'YeoJohnson'={
      imp <- preProcess(data, method = "YeoJohnson")
      data <- predict(imp, data)
    },
    'expoTrans'={
      imp <- preProcess(data, method = "expoTrans")
      data <- predict(imp, data)
    }
  )
  return(data)
}


#' @export
get_faclevels<-function(factors)({
  l<-lapply(factors,levels)
  l2<-mapply(list, l, as.list(colnames(factors)), SIMPLIFY=FALSE)
  l2
})

#' @export
get_selfactors<-function(res,factors){
  x<-res[[1]]
  res1<-lapply(res, function(x){
    level=x[1]
    factor=attr(x,"ancestry")
    val=attr(x,"stselected")
    if(length(level)==1 & length(factor)==1){
      data.frame(factor,level,val=val)}
  })


  if(length(res1)>0){
  pic<-which(unlist(lapply(res1,function(x) !is.null(x))))
  dfsel<-do.call(rbind, res1[pic])
  ls2<-split(dfsel,dfsel$factor)

  ls2<-ls2[ which(names(ls2)%in%colnames(factors))]

  ls3<-mapply(list, ls2,as.list(factors[names(ls2)]) ,SIMPLIFY=FALSE)
lapply(ls3,function(x) length(x[[2]]))
  ls3<-do.call(cbind,lapply(ls3,function(x)
    x[[2]]%in%x[[1]]$level))
  ls3[ls3==F]<-NA
  factors2<-factors
  # colnames(factors2)%in%colnames(ls3)
dim(na.omit(ls3))
factors2<-factors2[,colnames(ls3)]
factors2[,colnames(ls3)]<-ls3
  rownames(na.omit(factors2))} else{
    NULL
  }
}
# Recursion function to get tree structure

#' @export
gettree<-function(a)
{

  # defining tree as list
  tree<-list()

  # Condition to identifly if selected column is not last column
  if(class(a)!="factor"&&length(a)>1)
  {

    # getting uniques list of names from the columns to create folder
    b<-unique(a[,1])

    # runnig file name in loop
    for(i in b)
    {
      # check if the flie name i not blank
      if(i!="")
      {
        # subset data for content of folder
        subdata<-a[which(a[,1]==i),]

        # if there is only one element for that item change icon as file
        if(length(subdata[,-1])==1)
        {tree[[i]]=structure("",sticon="file",stselected=TRUE)}
        else{
          # call the function recursively
          tree[[i]]<-gettree(subdata[,-1])
        }}

    }}

  # Change icon of last columns as file
  if(class(a)=="factor"||length(a)==1)
  {
    for(i in a)
    {
      tree[[i]]=structure("",sticon="file",stselected=TRUE)
    }

  }
  return(tree)
}

# Mac: xcode-select --install
#"Ubutu:  sudo apt install libgdal-dev"
#"Ubutu:  sudo apt install libudunits2-dev"

#' @export
plothists<-  function (data,newcolhabs, palette="viridis", len=10, type=c("quantiles","max","min","mean")){
  type=match.arg(type,c("quantiles","max","min","mean"))
  colsums<-colSums(data)
  rank<- order(colsums, decreasing=T)
  cols<-adjustcolor(getcolhabs(newcolhabs,palette,ncol(data)),0.5)
  df<-data.frame(rank=1:length(rank), colsum= colsums[rank],cols=cols[rank], row.names = colnames(data)[rank])


  hist(data[,1], breaks=30, xlim=range(data), col=cols[1], xlab="Values",
       ylab="Frequency", main="",border=NA, las=1)
  for(i in 2:ncol(data)){
    hist(  data[,i], breaks=30, xlim=range(data), col=cols[i], add=T,border=NA)
  }

  pic<-switch(type,
              "quantiles"=round(quantile(df$rank,  seq(0,1,len=len))),
              "max"=  df$rank[1:len],
              "min"=  df$rank[nrow(df):1][1:len],
  )



  leg<-df[ pic,]
  label=paste(leg$rank,
              paste0(rownames(leg)," (",round(leg$colsum,decimalplaces(  pretty(leg$colsum[len])[2])),")"),sep=" - ")
  legend("topr",legend=label, pch=15, col=leg$cols,title="Variable sum rank",title.adj=0, bty="n",cex=.7)


}


#registerDoParallel(cl)

#' @export
pwRDA2<-function (x.ord, y.ord, BPs, n.rand = 99)
{
  x.ord <- as.matrix(x.ord)
  y.ord <- as.matrix(y.ord)
  if (is.null(rownames(x.ord))) {
    rownames(x.ord) <- 1:nrow(x.ord)
  }
  if (is.null(rownames(y.ord))) {
    rownames(y.ord) <- 1:nrow(y.ord)
  }
  if (is.null(colnames(x.ord))) {
    colnames(x.ord) <- 1:ncol(x.ord)
  }
  if (is.null(colnames(y.ord))) {
    colnames(y.ord) <- 1:ncol(y.ord)
  }
  R.boot <- NULL
  pw.Models <- pwRDA.source(x.ord, y.ord, BPs)
  pw.obs <- pw.Models$summ
  obs <- pw.obs[2]
  rownames(x.ord) <- NULL
  rownames(y.ord) <- NULL

  withProgress(message = "Running...",
               min = 1,
               max = n.rand,
               {

                 for (b in 1:n.rand) {

                   sample <- sample(1:nrow(y.ord), replace = T)
                   suppressWarnings(comm.rand <- y.ord[sample, ])
                   suppressWarnings(new.x <- x.ord[sample, ])
                   R.boot[b] <- pwRDA.source(new.x, comm.rand, BPs)$summ[2]

                   incProgress(1)
                 }
               })

  p.value <- pnorm(obs, mean = mean(R.boot), sd = sd(R.boot),
                   lower.tail = F)
  summ <- rbind(c(pw.obs[1], anova(pw.Models$rda.0)[1, 4]),
                c(pw.obs[2], p.value), c(pw.obs[3], pw.obs[4]))
  summ <- round(summ, 10)
  rownames(summ) <- c("FULL", "PW", "F")
  colnames(summ) <- c("Statistic", "P.value")

  pw.Models[[1]] <- summ
  class(pw.Models) <- "pw"
  return(invisible(pw.Models))
}

#' @export
smw.root2<-function (yo, w=50, dist="bray")
{
  if (w%%2 == 1) {
    stop("window size should be even")
  }
  diss <- NULL
  yo <- data.frame(yo)
  nrow_yo=nrow(yo)
  yo<-data.frame(t(yo))
  i=1
  for (i in 1:(nrow_yo - w + 1)) {
    wy.ord <- yo[i:(i + (w - 1))]
    div<-length(wy.ord)/2
    half.a <- apply(wy.ord[1:(div)], 1, sum)
    half.b <- apply(wy.ord[-c(1:(div))], 1,sum)
    d <- vegdist(rbind(half.a, half.b), dist)
    diss[i] <- d

  }
  k <- (w/2)
  for (i in 1:((nrow_yo - w))) {
    k[i + 1] <- (w/2) + i
  }
  positions<-k

  result<-data.frame(positions =positions, sampleID = colnames(yo)[positions],
                     diss = diss)

  return(invisible(result))
}



#' @export
tiphelp<-function(text){
  tipify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),text)
}
#' @export
pophelp<-function(title,text, placement="right"){
  popify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),title,text, placement = placement, trigger="hover")
}

#' @export
get_change_attr<-function(data){
  transf=attr(data,"transf")
  column(12,
         span(inline(h5(actionLink("change_attr","Change-Attribute"), style="color: #05668D")),em(icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"Click to expand change history")),
         conditionalPanel("input.change_attr % 2",{

           if(!is.null(transf)) {
             column(12,
                    splitLayout(cellWidths = c('30%','70%'),
                                column(12,style="margin-top: 25px",
                                       div(strong(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'removed observations. If > 0 displays in the form of:: Original number of observations :: number of retained observations.',options=list(container="body"))
                                                  ,"Subobs"), style="margin: 7px"),
                                       div(strong(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'removed variables. If > 0 displays in the form of:: Original number of variables :: number of retained variables',options=list(container="body")),"Subvars"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"), 'Transformation option from the Transformation toolbox',options=list(container="body")),strong("Transf"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Variables removed by frequency/abundance',options=list(container="body")),strong("Removed"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Scale option from the Transformation toolbox',options=list(container="body")),strong("Scale"), style="margin: 7px"),
                                       div(
                                         tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Center option from the Transformation toolbox',options=list(container="body")),strong("Center"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'NA.omit  option from the Transformation toolbox',options=list(container="body")),strong("NA.omit"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Data-Attribute Imputation',options=list(container="body")),strong("Data_imp"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Factor-Attribute Imputation',options=list(container="body")),strong("Factor_imp"), style="margin: 7px")
                                ),
                                column(12,  style="margin-left: -20px;overflow-x: scroll",
                                       renderTable(
                                         transf, striped=T,rownames =F,spacing ="xs"
                                       )
                                )

                    )
             )
           }

         })


  )
}
#' @export
datalist_render<-function(datalist=NULL,bagdata=F)
{
  data=datalist
  factors=attr(data,"factors")
  datalist_name=attr(data,"datalist")

  coords=attr(data,"coords")
  base_shape=attr(data,"base_shape")
  layer_shape=attr(data,"layer_shape")
  data.factors=attr(data,"data.factors")
  transf=attr(data,"transf")
   div(
     column(12,style="background: #e6e6e6ff;color: black;border: 1px dashed black;margin: 10px 10px 10px 10px",
            h5(strong("Datalist:"),if(length(datalist_name)>0) {em(datalist_name, style="color: #05668D")})),
     column(12,
       if(length(transf)>0){
         splitLayout(
           absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
          get_change_attr(data),
           cellWidths = c("16%",'84%')
         )},


     ),
     column(12,
            if(length(data)>0) {
              splitLayout(
                absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                column(12,style="color:  gray",
                       p(h5(p("Data-Attribute"), style="color: #05668D")),
                       p(em("n.obs:",nrow(data), ";","nvar-numeric:", ncol(data), )),
                       if(length(data.factors>0)){
                         em("nvar-factors:", ncol(data.factors))
                       }


                ),
                cellWidths = c("16%",'84%')
              )}),
     column(12,
            if(length(factors)>0) {
              splitLayout(
                absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                column(12,style="color:  gray",
                       (h5(p("Factor-Attribute"), style="color: #05668D")),
                       em("n.obs:",nrow(factors), ";", "nvar:", ncol(factors))
                ),
                cellWidths = c("16%",'84%')
              )}
     ),
     column(12,
            if(length(coords)>0) {
              splitLayout(
                absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                column(12,style="color:  gray",
                       p(h5(p("Coords-Attribute"), style="color: #05668D")),
                       em("n.obs:",nrow(coords), ";", "nvar:", ncol(coords))
                ),
                cellWidths = c("16%",'84%')
              )}
     ),
     column(12,
            if(length(base_shape)>0) {
              splitLayout(
                absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                column(12,style="color:  gray",
                       p(h5(p("Base-shape-Attribute"), style="color: #05668D")),
                       em("sf object")
                ),
                cellWidths = c("16%",'84%')
              )}
     ),
     column(12,
            if(length(layer_shape)>0) {
              splitLayout(
                absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                column(12,style="color:  gray",
                       p(h5(p("Layer-shape-Attribute"), style="color: #05668D")),
                       em("sf object")
                ),
                cellWidths = c("16%",'84%')
              )}
     )


   )

}


#' @export
data_migrate0<-function(data,newdata, newname){
  {
    attr(newdata, "data.factor")=attr(data,"data.factor")
    attr(newdata, "datalist")=newname
    attr(newdata, "filename")=attr(data, "filename")
    attr(newdata, "factors")=attr(data, "factors")[rownames(data), , drop=FALSE]
    attr(newdata, "coords")= attr(data,"coords")
    attr(newdata, "base_shape")= attr(data,"base_shape")
    attr(newdata, "layer_shape")=attr(data,"layer_shape")
    transf<-attr(data, "transf")
    if(!is.null(transf)){
      colnames(transf)<-paste("Change",1:ncol(transf))
    }
    attr(newdata, "transf")=transf
    attr(newdata, "nobs_ori")=attr(data, "nobs_ori")
    attr(newdata, "nvar_ori")=attr(data, "nvar_ori")
     attr(newdata, "rf")=attr(data, "rf")
    attr(newdata, "nb")=attr(data, "nb")
    attr(newdata, "svm")=attr(data, "svm")
    attr(newdata, "som")=attr(data, "som")
    return(newdata)
  }
}

#' @export
data_migrate<-function(data,newdata, newname){
  {
    attr(newdata, "data.factor")=attr(data,"data.factor")
    attr(newdata, "datalist")=newname
    attr(newdata, "filename")=attr(data, "filename")
    attr(newdata, "factors")=attr(data, "factors")[rownames(data), , drop=FALSE]
    attr(newdata, "coords")= attr(data,"coords")
    attr(newdata, "base_shape")= attr(data,"base_shape")
    attr(newdata, "layer_shape")=attr(data,"layer_shape")
    attr(newdata, "extra_shape")=attr(data,"extra_shape")
    transf<-attr(data, "transf")
    if(!is.null(transf)){
      colnames(transf)<-paste("Change",1:ncol(transf))
    }
    attr(newdata, "transf")=transf
    attr(newdata, "nobs_ori")=attr(data, "nobs_ori")
    attr(newdata, "nvar_ori")=attr(data, "nvar_ori")
   # attr(newdata, "rf")=attr(data, "rf")
    #attr(newdata, "nb")=attr(data, "nb")
    #attr(newdata, "svm")=attr(data, "svm")
    #attr(newdata, "som")=attr(data, "som")
    return(newdata)
  }
}


#' @export
textrfs<-function(...){
  column(12,
         p('Liu, Yonggang, Robert H. Weisberg, and Christopher NK Mooers.  (2006). Performance evaluation of the self-organizing map for feature extraction',em("Journal of Geophysical Research"),": Oceans 111.C5."),
         p("Liu, Yonggang, Robert H. Weisberg, and Christopher NK Mooers. 'Self-organizing maps'",em(" Information Sciences. Berlin: Springer"),"30 (2001)"),
         p("Clark, S., Sisson, S. A., & Sharma, A. (2020). Tools for enhancing the application of self-organizing maps in water resources research and engineering. Advances in Water Resources, 143(July), 103676. https://doi.org/10.1016/j.advwatres.2020.103676")
         )
}

#' @export
textoffline<-function(...)({
  column(12,
        h4(strong("Running offline"), icon(verify_fa = FALSE,name=NULL,class="fab fa-windows"), icon(verify_fa = FALSE,name=NULL,class="fab fa-apple"), icon(verify_fa = FALSE,name=NULL,class="fab fa-linux")),
        p("The last version of ",span('iMESc',style="font-family: 'Alata', sans-serif;")," is also available in ",a("GitHub",href="https://github.com/DaniloCVieira/iMESc")),
        p("Follow the steps bellow to get started with ",span('iMESc',style="font-family: 'Alata', sans-serif;"),":"),
       column(12,style="background: white",
              p(style="margin-top:20px; ",
                strong("1."),"Install",a("R", href="https://cran.r-project.org/")," and ",a("RStudio",href="https://www.rstudio.com/products/rstudio/download/")," if you haven't done so already;"),
              p(strong("2.")," If you are a Mac or Linux user, please follow the commands in the section ",strong("'Shell commands'")," before proceeding;"),
              p(strong("3.")," Initate a new script;"),
              p(strong("4.")," Install shiny package if it is not already installed;"),
              p(strong("5.")," Run the functions below;")),
        fluidRow(style="margin-left: 40px;margin-right: 40px;",
               column(12,style="background: white;font-family: Lucida Console;border: 1px solid SeaGreen; margin: 10 10 10 10",
                      p(style="margin-top:20px",
                        code(
                        'library(shiny)')),
                      p(code("runGitHub('iMESc','DaniloCVieira', ref='main')")))),
        p(style="margin-top:20px; ","The app will automatically install the required packages, and may take some time if this is your first time using the application. The next time, it shouldn't take more than a few seconds."),

        p(style="border-top: 1px solid",
          h4(style="margin-top:20px",
           strong("Shell commands: Mac OS and Linux systems"), icon(verify_fa = FALSE,name=NULL,class="fab fa-apple"), icon(verify_fa = FALSE,name=NULL,class="fab fa-linux"))),
        p("Mac and Linux Users may require some additional commands before installing or running the application for the first time;"),

       column(12,style="background: white; margin-bottom: 20px",
              p(style="margin-top:20px; ",
                icon(verify_fa = FALSE,name=NULL,class="fab fa-apple"),
                strong("If you are a Mac User:"), style="color: SeaGreen"),
              p(style="margin-top:20px;",
                strong("1.")," Open a new terminal;"),
              p(strong("2.")," Type the following into your terminal:"),
              p(code("xcode-select -p"), "it will open a prompt"),
              p(strong("3.")," Install the Command Line Tools from the prompt;"),
              p(strong("4.")," Try run'", code("runGitHub('iMESc','DaniloCVieira', ref='main')"),"again"),
              p(strong("5."),"Click in 'Open in Browser' on the top left corner of the window for better visualization"),
              em('For other issues, please contact the author.')
              ),

       column(12,style="background: white",
              p(style="margin-top:20px; margin-left: 10px;",
                icon(verify_fa = FALSE,name=NULL,class="fab fa-linux"),
                strong("If you are a Linux User:"), style="color: SeaGreen"),
              p(strong("1.")," Open a new terminal;"),
              p(strong("2.")," Type the following into your terminal:"),
              p(code("sudo apt install libgdal-dev")),
              p(strong("3.")," Once the above installation is completed, type the next command into your terminal: "),
              p(code("sudo apt install libudunits2-dev")),
              p(strong("4.")," Try run'", code("runGitHub('iMESc','DaniloCVieira', ref='main')"),"again;"),
              em('For other issues, please contact the author.'))
  )


})



#' @export
textpackages<-function(...)
{
  div(

    h5("Packages"),
    p('shinydashboard'),
    p('shinydashboardPlus'),
    p('shinyjs'),
    p('shiny'),
    p('readxl'),
    p('vegan'),
    p('caret'),
    p('viridisLite'),
    p('aweSOM'),
    p('sp'),
    p('raster'),
    p('rasterVis'),
    p('rgdal'),
    p('gstat'),
    p('ggspatial'),
    p('ggplot2'),
    p('sf'),
    p('class'),
    p('shinyWidgets'),
    p('randomForestExplainer')
  )
}


#' @export
texttask<-function(...)
{
  div(
    h5("tasks:"),
    p("- define the final tabs,"),
    p("- set/define selector inputs,"),
    p("- implement help pages/tool tips"),
    p("- implement download results/Rfile,"),
    p("- implement more graphics options,"),
    p("- finish random forest, decision tree")

  )
}

#' @export
textabout<-function(...)
{
  div(
    h4("Requirements"),
    h5(strong("data input")),
    p('...'),
    h5(strong("running the app in the R-shiny server")),
    p('...'),
    h5(strong("running the app offline")),
    p('...'),

  )
}




#' @export
psummary<-function(data)
{
  nas=sum(is.na(unlist(data)))

  n=data.frame(rbind(Param=paste('Missing values:', nas)))
  a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))

  c<-data.frame(Param=
               c("max:", "min:", "mean:","median:","var:", "sd:"),
             Value=c(max(data,na.rm = T), min(data,na.rm = T), mean(unlist(data),na.rm = T), median(unlist(data),na.rm = T), var(unlist(data),na.rm = T), sd(unlist(data),na.rm = T)))
  c$Value<-unlist(lapply(c[,2],round,3))
  ppsummary("-------------------")
  ppsummary(n)
  ppsummary("-------------------")
  ppsummary(a)
  ppsummary("-------------------")
  ppsummary(c)
  ppsummary("-------------------")
}

#' @export
ppsummary <- function(m){
  write.table(format(m, justify="left", trim=T),
              row.names=F, col.names=F, quote=F)
}
#' @export
textnclust<-function(...)
{
 div(
   h3("Determining the best number of clusters"),
   h4(strong("elbow")),
   p(" Generate elbow plot? **** this may take a while to run..."),
   br(),
   br(),
   h4(strong("rfloop")),
   p("Perform random forest analysis with increasing number of clusters"),
   h4(strong("cv")),
   p("number of folds for cross-validation"),
   h4(strong("repeats")),
   p("the number of complete sets of folds to compute")


 )
  }


#' @export
texthc<-function(...){
  div(
    h4("Hierarchical clustering"),
    br(),


  )
}



#' @export
textintro<-function(...)
{
  column(12,

        column(12,
               br(),
               h4(strong("Welcome to",span('iMESc',style="font-family: 'Alata', sans-serif;"),style="color: #05668D")),
               p("This app is intended to dynamically integragate a range of algorithms for data pre-processing, analysis, and visualization. To optimize learning from the data, iMESc allows the adjustment of the analytical parameters of each ML algorithm  by means of a context-aware dynamic interface.  Throughout the app, data input and results output are organized in modules enabling the creation of multiple ML pipelines  . iMESc also offers users classical statistical approaches, such as principal component analysis and non-metric multidimensional scale, calculation of traditional biodiversity indexes and the spatialization of the data on the geographical coordinate plan."),
               p("iMESc allows users to create a savepoint, a single R object that can be reloaded later to restore analysis output."),
               p("Get started by creating a Datalist. Use the",icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"),"button."),
               p("To ensure that iMESc content fits nicely on the screen, we recommend a landscape minimum resolution  of 1377 x 768 pixels. "))


      )

}
#' @export
textrefs<-function(...)
{
  div(
    h4(strong("References")),
    p('Kohonen, T. (2001). Self-Organizing Maps (Vol. 30). ',em("Springer Berlin Heidelberg"),a(" https://doi.org/10.1007/978-3-642-56927-2")),
    p('Liu, Y., Weisberg, R. H., & Mooers, C. N. K. (2006). Performance evaluation of the self-organizing map for feature extraction.', em('Journal of Geophysical Research'), '111(C5), C05018. ',a("https://doi.org/10.1029/2005JC003117",href="https://doi.org/10.1029/2005JC003117")),
    p('Chon, T.-S. (2011). Self-Organizing Maps applied to ecological sciences.', em('Ecological Informatics'), '6(1), 50-61.' ,a("https://doi.org/10.1016/j.ecoinf.2010.11.002",ref="https://doi.org/10.1016/j.ecoinf.2010.11.002")),
   p(' Lucas, T. C. D. (2020). A translucent box: interpretable machine learning in ecology. ',em('Ecological Monographs'),' 90(4). ',a("https://doi.org/10.1002/ecm.1422",href="https://doi.org/10.1002/ecm.1422")),
    p("De'ath, G., & Fabricius., K. E. (2000). Classification and regression trees: a powerful yet simple technique for ecological data analysis. ',em('Ecology'),' 81(11), 3178-3192." ,a("https://doi.org/https://doi.org/10.1890/0012-9658(2000)081[3178:CARTAP]2.0.CO;2",href="https://doi.org/https://doi.org/10.1890/0012-9658(2000)081[3178:CARTAP]2.0.CO;2")),
    p('Cutler, D. R., Edwards Jr, T. C., Beard, K. H., Cutler, A., Hess, K. T., Gibson, J., & Lawler, J. J. (2007). Random forests for classification in ecology. ',em('Ecology'),' 88(11), 2783-2792.' ,a("https://doi.org/https://doi.org/10.1890/07-0539.1Ci",href="https://doi.org/https://doi.org/10.1890/07-0539.1Ci")),
    p('Giraudel, J. L., & Lek, S. (2001). A comparison of self-organizing map algorithm and some conventional statistical methods for ecological community ordination.', em('Ecological Modelling'),' 146(1-3), 329-339.' ,a("https://doi.org/10.1016/S0304-3800(01)00324-6",href="https://doi.org/10.1016/S0304-3800(01)00324-6")),
   p(' Clark, S., Sisson, S. A., & Sharma, A. (2020). Tools for enhancing the application of self-organizing maps in water resources research and engineering.', em('Advances in Water Resources'), '143(July), 103676. ',a("https://doi.org/10.1016/j.advwatres.2020.103676",href="https://doi.org/10.1016/j.advwatres.2020.103676")),

  p("Vieira, D. C., Brustolin, M. C., Ferreira, F. C., & Fonseca, G. (2019). segRDA: An <scp>r</scp> package for performing piecewise redundancy analysis.",em(" Methods in Ecology and Evolution"),", 10(12), 2189-2194.",a("https://doi.org/10.1111/2041-210X.13300",href="https://doi.org/10.1111/2041-210X.13300")),
  p("Checon, H. H., Vieira, D. C., Corte, G. N., Sousa, E. C. P. M., Fonseca, G., & Amaral, A. C. Z. (2018). Defining soft bottom habitats and potential indicator species as tools for monitoring coastal systems: A case study in a subtropical bay. ",em("Ocean & Coastal Management"),", 164, 68-78.",a(" https://doi.org/10.1016/j.ocecoaman.2018.03.035",href=" https://doi.org/10.1016/j.ocecoaman.2018.03.035")),
  p("Corte, G. N., Checon, H. H., Fonseca, G., Vieira, D. C., Gallucci, F., Domenico, M. Di, & Amaral, A. C. Z. (2017). Cross-taxon congruence in benthic communities: Searching for surrogates in marine sediments. ",em("Ecological Indicators"),", 78, 173-182.",a("https://doi.org/10.1016/j.ecolind.2017.03.031",href="https://doi.org/10.1016/j.ecolind.2017.03.031"))
  )
}
#' @export
textProperty<-function(...){
  div(
    p("Show areas for high values (red) and low values (blue) for the selected variable")
  )
}
#' @export

textauthors<-function(...)
{
  column(
    12,style="background: white; margin-left: 20px",
    h5(strong("Author")),
    column(12,
           p("Danilo C Vieira")),

    h5(strong("Contributors")),
    column(12,
           p("Fabiana S. Paula"),
           p("Daiane Faller"),
           p("Juliane Castro Carneiro"),
           p("Julio Cezar F. Moreira"),
           p("Dr. Gustavo Fonseca"))
  )

}
#' @export

Read_Shapefile <- function(shp_path) {
  infiles <- shp_path$datapath # get the location of files
  dir <- unique(dirname(infiles)) # get the directory
  outfiles <- file.path(dir, shp_path$name) # create new path name
  name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
  x <- read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
  return(x)
}
#' @export

Read_savepoint <- function(input_id) {
  infiles <- input_id$datapath # get the location of files
  lapply(infiles, function(x) read)

  dir <- unique(dirname(infiles)) # get the directory
  outfiles <- file.path(dir, input_id$name) # create new path name
  name <- strsplit(input_id$name[1], "\\.")[[1]][1] # strip name
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
  x <- readRDS(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
  return(x)
}
#' @export

textpretreat<-function(...)
{
  div(
    h3("Pre-treatment"),
    br(),
    br(),
    h4(strong("Transformation")),
    p("Type of transformation"),
    h4(strong("Remove rares")),
    p("removes..."))
}
#' @export



textscreeplot<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    strong(" A  key question is how many clusters (k) should we define?"),
    br(),
    p("The scree plot panel helps you to determine the optimal number of clusters.  It is a heuristic graphic method that consists of:"),
    p(strong("a)"), "plotting an internal measure of the clustering performace againt the number of clusters and"),
    p(strong("b)"),"inspecting the shape of the resulting  curve  in  order  to  detect  the  point  at  which  the curve changes drastically."),
    p("Generally, you want to choose a number of clusters so that adding another cluster doesn't improve much better the internal measure of the clustering performace (i.e the steep slope),"),
    p("The panel",code("Scree plot"),"implements two tools (",code("WSS")," and ",code("RFaccu"),") for creating a scree plot "),
    p(code("WSS"), "method uses the ratio of number of clusters vs. Within Sum of Squares (WSS)", style="margin-left: 10px;"),
    p(code("RFaccu"), "method uses the number of clusters vs. Accuracies from Random Forest analysis", style="margin-left: 10px;"),
    p("Finding the 'elbow' may not be intuitive. So, the user can apply the ",code("split moving window analysis"),", which may help to identify the break in the elbow relationship, The split moving window analysis toolbox appears once the scree plot is generated and helds more details about this analyis.")
  )

}
#' @export

textclustering<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    br(),
                    p("This step is intend to cluster the map units into classes, allowing to find the structure underlying the values associated with the map units after training. At the end of this procedure each observation belongs to a map unit, and each map unit belongs to a class (cluster)."),
                    p("In the", code("Hierarchical clustering"),"panel you can define the number of clusters, perform the Hierarchical clustering analysis and then inspect the subpanels",code("Dendogram"), "and", code("BMU clusters"))


                    )))
}
#' @export



temptext<-function(...)
{
  p(br(),"The app implements two tools to help the user define the number of clusters, both based on the elbow method. One using the ratio of Number of clusters vs. Total Sum of Squares, and the other looking at Accuracies vs. the number of clusters. F")
}
#' @export

textupload<-function(...){
  as.character(

    "csv file where rows are the observations, columns are the variables  The first column must contain the observation labels. Columns containing characters are initially omitted and can later be included as binary columns by factor level."

  )}
#' @export

textlab<-function(...)
{
  as.character(
    "csv file containg the factors for your data, which will be used for labeling, grouping and viewing the results. It can contain as many factors as you want."
  )
}
#' @export

textinput<-function(...){
  paste(
    "Create Datalists and upload your data and their attributes. All analyses available at ",strong('iMESc',style="font-family: 'Alata', sans-serif;")," will require a Datalist created by the user, either uploaded or using example data."


  )
}
#' @export


texttransf<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                  p("This functionality uses the",code("decostand"),

                    "function from the", actionLink("vegan","vegan"),"package."),
                  p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),"The ",em('Transformation')," methods are described in the function's ", actionLink("decostand","help page"),),



                  ),
           column(12,
                  htmlOutput("decostand")
                  )))
}
#' @export



textpreProcess<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    column(12,
           p("Pre-processing transformation (centering, scaling etc.) can be estimated from the training data and applied to any data set with the same variables."),
           p("Please see ",strong("Detail", style="color: SeaGreen")," section in",actionLink("preProcessh","preProcess"),'page for more Details')



    ),
    column(12,
           htmlOutput("preProcesshelp")
    ))
}
#' @export

textscale<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      actionLink("scalehelph","scale") ,
                      "function from the", "R base.")
    ),
    column(12,
           htmlOutput("scalehelp_out")
    )))
}
#' @export


textmdshelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      code("metaMDS") ,
                      "function from the 'vegan' package"),
                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),code("distance")," argument is described  in 'Detail' section of the ",actionLink("mdshelph","help page")," of the function;")
    ),
    column(12,
           htmlOutput("mdshelphelp")
    )))
}
#' @export
textpcahelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the"
                      ,actionLink("pcahelph","prcomp") ,
                      "function from R base")
    ),
    column(12,
           htmlOutput("pcahelphelp")
    )))
}
#' @export

textvotes<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
  )
}
#' @export



textsupersom<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('som')," function from the",
                    actionLink("kohonen","kohonen"),"package."),
                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink("supersomh","help page")," of the function;")
    ),
    column(12,
           htmlOutput("supersomhelp")
    )))
}
#' @export


textmap<-function(...)
{
  paste0("To vizualize the ",em('BMUs')," on the map, choose ",code('bmu')," To visualize a ",em('variable'),"  on the map (from your uploaded and pretreated data), choose ",code("variable"),"  and then select the target variable")



}
#' @export

textinterp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      actionLink("interph","idw") ,
                      "function from the 'gstat' package.")
    ),
    column(12,
           htmlOutput("interphelp")
    )))
}
#' @export
texthclust<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      code("hclust") ,
                      "function from the 'stats'"),
                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),code("methods"),"are described 'Detail section of the ",actionLink("hclusth","help page")," of the function;")
    ),
    column(12,
           htmlOutput("hclusthelp")
    )))
}
#' @export
textrf<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    "In essence the Random Forest is based on generating a large number of decision trees, each constructed using a different subset of your training set. These subsets are selected by sampling at random and with replacement from the original data set. The decision trees are then used to identify a classification consensus by selecting the most common output (mode).",
                    p("This functionality uses the",
                      code("train"),"and",code("trainControl"),
                      "functions from the 'caret' package"),

                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),"Please visit their respective help pages (",actionLink("rfh","train"),"and",actionLink("rfh2","trainControl"),") for more details about the parameters available in",span('iMESc',style="font-family: 'Alata', sans-serif;"))
    ),
    column(12,
           htmlOutput("rfhelp")
    )))
}
#' @export


textctree<-function(...)
{

  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    "Decision Trees are a non-parametric supervised learning method used for classification and regression. The goal is to create a model that predicts the value of a target variable by learning simple decision rules inferred from the data features.",
                    p("This functionality uses the",
                      code("ctree"),"and",code("ctree_control"),
                      "functions from the 'caret' package"),

                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),"Please visit their respective help pages (",actionLink("ctreeh","ctree"),"and",actionLink("ctreeh2","ctree_control"),") for more details about the parameters")
    ),
    column(12,
           htmlOutput("ctreehelp")
    )))

}
#' @export

textseed<-function(...){
  "A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the analysis."
}
#' @export
textinterpbmu<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      actionLink("interpbmuh","knn") ,
                      "function from the 'class' package.")
    ),
    column(12,
           htmlOutput("interpbmuhelp")
    )))
}
#' @export

textres<-function(...)
{
  "The resolution of the interpolation"
}
#' @export

textdisplayas<-function(...)
{

  paste0("The option ",code('discrete')," generate the map using only the provided coordinates, whereas the option ",code('interpolation')," provide some tools for generating an interpolated surface on the map.")





}
#' @export



textcoords<-function(...){
  paste(
    em('Required only for the spatial tools menu:'),
    "csv file with the longitudes and latitudes of the observations. The first column must contain the name of the observations. The second and third columns must contain the longitude and latitude respectively"


  )
}
#' @export



textbase<-function(...)
{
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
    ,column(12,
            "A single R file containing the shape to be used to clip interpolated surfaces when generating maps(e. g an oceanic basin shape). To create this file, use the ",actionLink("shp_tool","Shp Toolbox"),", or in R, run the following code:",
            verbatimTextOutput("codechunk_base"),
            column(12,"Upload the generated 'my_base_shape_shape' file"),
            column(12,
                   htmlOutput("basehelp"))

    ))
}
#' @export

codebase<-function(...)
{
  cat(c("library('df')","\n",
  "my_base_shape_shape<-st_read('directory_containing_the_shapefiles/shape_name.shp')","\n",
  "save(my_base_shape_shape,
       file='directory_to_save_the_base_shapeshape/my_base_shape_shape')"))
}
#' @export

codelayer<-function(...)
{
  cat(c("library('df')","\n",
        "my_layer_shape_shape<-st_read('directory_containing_the_shapefiles/shape_name.shp')","\n",
        "save(my_layer_shape_shape,
       file='directory_to_save_the_layer_shapeshape/my_layer_shape_shape')"))
}
#' @export


textlayer<-function(...)
{
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
    ,column(12,
            "A single R file containing the shape to be used to be used as an additional layer (e.g. a continent shape). To create this file, use the ",actionLink("shp_tool2","Shp Toolbox"),", or in R, run the following code:",
            verbatimTextOutput("codechunk_layer"),
            column(12,"Upload the generated 'my_layer_shape_shape' file"),
            column(12,
                   htmlOutput("layerhelp"))

    ))
}
#' @export




textremove<-function(...)
{
  column(12,
         p(code('singleton'),"Remove variables occurring only once.",icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"), "Requires a counting data"),
         p(code("pctRare"),"Remove variables with an abundance less than pctRare% of total abundance."),
         p(code("pctPrev"),"Remove species occurring in less than  pctPrev% of the samples")
  )

}
#' @export



texttraining<-function(...)
{div(
  br(),
  br(),
  h3("Parametrization"),
  br(),
  h4(strong("Topology")),
  p("choose between a hexagonal or rectangular topology"),
  h4(strong("xdim, ydim")),
  p("dimensions of the grid."),
  h4(strong("Distance method")),
  p("Distance functions to be used for the data"),
  h4(strong('rlen')),
  p("The number of times the complete data set will be presented to the network."),
  h4(strong("seed")),
  p("A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")
)}
#' @export


text_som_fine_tuning<-function(...){

  div(

    h3('finetuning'),
    br(),
    br(),
    h4(strong('a1, a2')),
    p("learning rate, indicating the amount of change. Default is to decline linearly from 0.05 to 0.01 over rlen updates. Not used for the batch algorithm."),
    h4(strong('neigh.fct')),
    p("choose between bubble and gaussian neighbourhoods when training a SOM."),


    h4(strong('r1, r2')),
    p("the radius of the neighbourhood, either given as a single number or a vector (start, stop). If it is given as a single number the radius will change linearly from radius to zero; as soon as the neighbourhood gets smaller than one only the winning unit will be updated."),
    h4(strong('mode')),
    p("type of learning algorithm")
  )


}
#' @export

datastr<-function(data){
  data_structure<-list( n.obs=nrow(data),
                        n.vars=ncol(data),
                        type.vars=table(unlist(lapply(data,function(x) class(x)))))
  return(data_structure)
}
#' @export

pfactors<-function(data.factors, palette="FantasticFox1",newcolhabs)
{

  for(i in 1:ncol(data.factors)){
    dt<-table(data.factors)
    par(mar=c(1,2,1,0.5))
    barplot(as.matrix(dt), beside=F,legend.text = names(dt), col=getcolhabs(newcolhabs,palette,length(dt)), main=colnames(data.factors)[i],las=1)
  }
  plofac<-recordPlot()
  return(plofac)

}
#' @export


pfac<-function(data.factors)
{
  m<-matrix(c(3:(ncol(data.factors)+2)), ncol=2)
  m<-matrix(c(3:(length(as.vector(m))+2))
            ,ncol=2, byrow = T)
  m<-cbind(2,m)
  m<-(rbind(1,m))
  layout(m, heights = c(2,rep(5,nrow(m)-1)), widths = c(.1))
  par(mar=c(0,0,0,0))
  plot.new()
  text(.5,.5,"", cex=2)
  plot.new()
  text(.5,.5,"number of observations",srt=90, cex=2)
  for(i in 1:ncol(data.factors)){
    dt<-table(data.factors[i])
    par(mar=c(1,2,1,0.5))
    barplot(as.matrix(dt), beside=F,legend.text = names(dt), col=rainbow(length(dt),s = .3), main=colnames(data.factors)[i],las=1)
  }
  plofac<-recordPlot()
  return(plofac)

}
#' @export

pmds<-function(mds_data,keytext=NULL,key=NULL,points=T, text=F,palette="black", cex.points=1, cex.text=1, pch=16, textcolor="gray",newcolhabs, pos=2, offset=0)
{
 if(!is.null(key)) {
  colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
  col<-colkey[key]} else{col= getcolhabs(newcolhabs,palette, nrow(mds_data$points)) }
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  par(mar=c(5,5,4,1))
  plot(mds_data$points, pch=pch,  las=1, type="n", main="Multidimensional scaling")
  legend("topr",legend=c(paste("Stress:",round(mds_data$stress,2)), paste0("Dissimilarity:", "'",mds_data$distmethod,"'")),cex=.8, bty="n")
  if(isTRUE(points)){ points(mds_data$points, pch=pch, col=col, cex=cex.points)}

  if(isTRUE(text)){
    colkey2<-getcolhabs(newcolhabs,textcolor, nlevels(keytext))
    col2<-colkey2[keytext]
    text(mds_data$points, col=col2, labels=keytext, cex=cex.text, pos=pos, offset=offset)}
  if(!is.null(key)){
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    legend("center",pch=pch,col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }
  on.exit(par(opar),add=TRUE,after=FALSE)
return(mds_data)


}
#' @export
ppca<-function(pca,key=NULL,keytext=NULL,points=T, text=NULL,palette="black", cex.points=1, cex.text=1, pch=16,textcolor="gray", biplot=T,newcolhabs, pos=2, offset=0) {

  {

    PCA = pca

    comps<-summary(PCA)

    exp_pc1<-paste("PC I (",round(comps$importance[2,1]*100,2),"%", ")", sep="")
    exp_pc2<-paste("PC II (",round(comps$importance[2,2]*100,2),"%", ")", sep="")

    choices = 1:2
    scale = 1
    scores= PCA$x
    lam = PCA$sdev[choices]
    n = nrow(scores)
    lam = lam * sqrt(n)
    x = t(t(scores[,choices])/ lam)
    y = t(t(PCA$rotation[,choices]) * lam)
    n = nrow(x)
    p = nrow(y)
    xlabs = 1L:n
    xlabs = as.character(xlabs)
    dimnames(x) = list(xlabs, dimnames(x)[[2L]])
    ylabs = dimnames(y)[[1L]]
    ylabs = as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
    unsigned.range = function(x) c(-abs(min(x, na.rm = TRUE)),
                                   abs(max(x, na.rm = TRUE)))
    rangx1 = unsigned.range(x[, 1L])
    rangx2 = unsigned.range(x[, 2L])
    rangy1 = unsigned.range(y[, 1L])
    rangy2 = unsigned.range(y[, 2L])
    xlim = ylim = rangx1 = rangx2 = range(rangx1, rangx2)
    ratio = max(rangy1/rangx1, rangy2/rangx2)



  }



  if(!is.null(key)) {
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    col<-colkey[key]} else{col= getcolhabs(newcolhabs,palette, nrow(x)) }
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  par(pty = "s",mar=c(5,5,5,1))
  plot(x, type = "n", xlim = xlim, ylim = ylim, las=1, xlab=exp_pc1, ylab=exp_pc2, main="Principal Component Analysis",col.sub="black", tck=0)
  abline(v=0, lty=2, col="gray")
  abline(h=0, lty=2, col="gray")

  if(isTRUE(points)){
    points(x, pch=pch, col=col, cex=cex.points)
  }
  if(isTRUE(text)){
    colkey2<-getcolhabs(newcolhabs,textcolor, nlevels(keytext))
    col2<-colkey2[keytext]
    text(x, col=col2, labels=keytext, cex=cex.text, pos=pos, offset=offset)
  }
if(isTRUE(biplot)){

    par(new = TRUE)
    xlim = xlim * ratio*2
    ylim = ylim * ratio
    plot(y, axes = FALSE, type = "n",
         xlim = xlim,
         ylim = ylim, xlab = "", ylab = "")
    axis(3,padj=1, tck=-0.01); axis(4, las=1)
    boxtext(x =y[,1], y = y[,2], labels = rownames(y), col.bg = adjustcolor("white", 0.2),  cex=1, border.bg  ="gray80", pos=3)

    PCA$rotation[,1]*10
    #text(y, labels = ylabs, font=2, cex=.8, col=)
    arrow.len = 0.1
    arrows(0, 0, y[, 1L] * 0.8, y[, 2L] * 0.8,
           length = arrow.len, col = 2, lwd=1.5)

  }

  if(!is.null(key)) {
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    legend("center",pch=pch,col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }

  on.exit(par(opar),add=TRUE,after=FALSE)
  return(PCA)
}
.checkSelect<-function (select, scores) {
  if (is.logical(select) && !isTRUE(all.equal(length(select),
                                              NROW(scores)))) {
    warning("length of 'select' does not match the number of scores: ignoring 'select'")
  }
  else {
    scores <- if (is.matrix(scores)) {
      scores[select, , drop = FALSE]
    }
    else {
      scores[select]
    }
  }
  scores
}

labels.cca<-function (object, display, ...) {
  if (is.null(object$CCA))
    CCA <- "CA"
  else CCA <- "CCA"
  switch(display, sp = , species = rownames(object[[CCA]]$v),
         wa = , sites = , lc = rownames(object[[CCA]]$u), reg = colnames(object[[CCA]]$QR$qr),
         bp = rownames(object[[CCA]]$biplot), cn = {
           cn <- rownames(object[[CCA]]$centroids)
           bp <- rownames(object[[CCA]]$biplot)
           c(cn, bp[!(bp %in% cn)])
         })
}
#' @export
#'
text.cca<-function (x, display = "sites", labels, choices = c(1,2), scaling = "species", arrow.mul, head.arrow = 0.05,select, const, axis.bp = FALSE, correlation = FALSE, hill = FALSE,...) {
  if (length(display) > 1)
    stop("only one 'display' item can be added in one command")
  pts <- scores(x, choices = choices, display = display, scaling = scaling,
                const, correlation = correlation, hill = hill, tidy = FALSE)
  cnam <- rownames(pts)
  if (missing(labels))
    labels <- labels.cca(x, display)
  if (!missing(select)) {
    pts <- .checkSelect(select, pts)
    labels <- labels[select]
  }
  if (display == "cn") {
    if (!is.null(nrow(pts))) {
      cnlabs <- seq_len(nrow(pts))
      text(pts, labels = labels[cnlabs], ...)
    }
    else {
      cnlabs <- NULL
    }
    pts <- scores(x, choices = choices, display = "bp",
                  scaling = scaling, const, correlation = correlation,
                  hill = hill, tidy = FALSE)
    bnam <- rownames(pts)
    pts <- pts[!(bnam %in% cnam), , drop = FALSE]
    if (nrow(pts) == 0)
      return(invisible())
    else {
      display <- "bp"
      if (!is.null(cnlabs))
        labels <- labels[-cnlabs]
    }
  }
  if (display %in% c("bp", "reg", "re", "r")) {
    if (missing(arrow.mul)) {
      arrow.mul <- ordiArrowMul(pts)
    }
    pts <- pts * arrow.mul
    arrows(0, 0, pts[, 1], pts[, 2], length = head.arrow,
           ...)
    pts <- ordiArrowTextXY(pts, labels, rescale = FALSE,
                           ...)
    if (axis.bp) {
      axis(side = 3, at = c(-arrow.mul, 0, arrow.mul),
           labels = rep("", 3))
      axis(side = 4, at = c(-arrow.mul, 0, arrow.mul),
           labels = c(-1, 0, 1))
    }
  }
  text(pts, labels = labels, ...)
  invisible()
}
#'
#' @export
#'
points.cca<-function (x, display = "sites", choices = c(1, 2), scaling = "species",
          arrow.mul, head.arrow = 0.05, select, const, axis.bp = FALSE,
          correlation = FALSE, hill = FALSE, ...) {
  if (length(display) > 1)
    stop("only one 'display' item can be added in one command")
  pts <- scores(x, choices = choices, display = display, scaling = scaling,
                const, correlation = correlation, hill = hill, tidy = FALSE)
  if (!missing(select))
    pts <- .checkSelect(select, pts)
  if (display == "cn") {
    cnam <- rownames(pts)
    points(pts, ...)
    pts <- scores(x, choices = choices, display = "bp",
                  scaling = scaling, const, correlation = correlation,
                  hill = hill)
    bnam <- rownames(pts)
    pts <- pts[!(bnam %in% cnam), , drop = FALSE]
    if (nrow(pts) == 0)
      return(invisible())
    else display <- "bp"
  }
  if (display %in% c("bp", "reg", "re", "r")) {
    if (missing(arrow.mul)) {
      arrow.mul <- ordiArrowMul(pts)
    }
    pts <- pts * arrow.mul
    arrows(0, 0, pts[, 1], pts[, 2], length = head.arrow,
           ...)
    pts <- pts * 1.1
    if (axis.bp) {
      axis(3, at = c(-arrow.mul, 0, arrow.mul), labels = rep("",
                                                             3))
      axis(4, at = c(-arrow.mul, 0, arrow.mul), labels = c(-1,
                                                           0, 1))
    }
    return(invisible())
  }
  points(pts, ...)
  invisible()
}
linestack<-function (x, labels, cex = 0.8, side = "right", hoff = 2,
          air = 1.1, at = 0, add = FALSE, axis = FALSE, ...)
{
  if (length(at) > 1 || length(hoff) > 1 || length(air) > 1 ||
      length(cex) > 1)
    stop("only one value accepted for arguments 'cex', 'hoff', 'air' and 'at'")
  x <- drop(x)
  n <- length(x)
  misslab <- missing(labels)
  if (misslab) {
    labels <- names(x)
  }
  if (!is.expression(labels) && !is.character(labels)) {
    labels <- as.character(labels)
  }
  nlab <- length(labels)
  if (!misslab && n != nlab) {
    stop(gettextf("wrong number of supplied 'labels: expected %d, got %d",
                  n, nlab))
  }
  side <- match.arg(side, c("right", "left"))
  op <- par(xpd = TRUE)
  on.exit(par(op))
  ord <- order(x)
  x <- x[ord]
  labels <- labels[ord]
  pos <- numeric(n)
  if (!add) {
    plot(pos, x, type = "n", axes = FALSE, xlab = "",
         ylab = "", ...)
  }
  hoff <- hoff * strwidth("m")
  ht <- air * strheight(labels, cex = cex)
  mid <- (n + 1)%/%2
  pos[mid] <- x[mid]
  if (n > 1) {
    for (i in (mid + 1):n) {
      pos[i] <- max(x[i], pos[i - 1] + ht[i])
    }
  }
  if (n > 2) {
    for (i in (mid - 1):1) {
      pos[i] <- min(x[i], pos[i + 1] - ht[i])
    }
  }
  segments(at, x[1], at, x[n])
  if (side == "right") {
    text(at + hoff, pos, labels, pos = 4, cex = cex, offset = 0.2,
         ...)
    segments(at, x, at + hoff, pos)
  }
  else if (side == "left") {
    text(at - hoff, pos, labels, pos = 2, cex = cex, offset = 0.2,
         ...)
    segments(at, x, at - hoff, pos)
  }
  if (axis)
    axis(if (side == "right")
      2
      else 4, pos = at, las = 2)
  invisible(pos[order(ord)])
}
#' @export
#'
plot.cca<-function (x, choices = c(1, 2), display = c("sp", "wa", "cn"), scaling = "species", type, xlim, ylim,const, correlation = FALSE, hill = FALSE, ...) {
  TYPES <- c("text", "points", "none")
  if (any(display %in% c("c", "cn")))
    display <- c(display, "bp")
  g <- scores(x, choices, display, scaling, const, correlation = correlation,
              hill = hill, tidy = FALSE)
  if (length(g) == 0 || all(is.na(g)))
    stop("nothing to plot: requested scores do not exist")
  if (!is.list(g))
    g <- list(default = g)
  for (i in seq_along(g)) {
    if (length(dim(g[[i]])) > 1)
      rownames(g[[i]]) <- rownames(g[[i]], do.NULL = FALSE,
                                   prefix = substr(names(g)[i], 1, 3))
  }
  if (!is.null(g$centroids)) {
    if (is.null(g$biplot))
      g$biplot <- scores(x, choices, "bp", scaling)
    bipnam <- rownames(g$biplot)
    cntnam <- rownames(g$centroids)
    g$biplot <- g$biplot[!(bipnam %in% cntnam), , drop = FALSE]
    if (nrow(g$biplot) == 0)
      g$biplot <- NULL
  }
  if (missing(type)) {
    nitlimit <- 80
    nit <- max(nrow(g$spe), nrow(g$sit), nrow(g$con), nrow(g$def))
    if (nit > nitlimit)
      type <- "points"
    else type <- "text"
  }
  else type <- match.arg(type, TYPES)
  if (length(choices) == 1) {
    if (length(g) == 1)
      pl <- linestack(g[[1]], ...)
    else {
      hasSpec <- names(g)[1] == "species"
      ylim <- range(c(g[[1]], g[[2]]), na.rm = TRUE)
      pl <- linestack(g[[1]], ylim = ylim, side = ifelse(hasSpec,
                                                         "left", "right"), ...)
      linestack(g[[2]], ylim = ylim, side = ifelse(hasSpec,
                                                   "right", "left"), add = TRUE, ...)
    }
    return(invisible(pl))
  }
  if (missing(xlim)) {
    xlim <- range(g$species[, 1], g$sites[, 1], g$constraints[,
                                                              1], g$biplot[, 1], if (length(g$centroids) > 0 &&
                                                                                     all(is.na(g$centroids))) NA else g$centroids[, 1],
                  g$default[, 1], na.rm = TRUE)
  }
  if (!any(is.finite(xlim)))
    stop("no finite scores to plot")
  if (missing(ylim)) {
    ylim <- range(g$species[, 2], g$sites[, 2], g$constraints[,
                                                              2], g$biplot[, 2], if (length(g$centroids) > 0 &&
                                                                                     all(is.na(g$centroids))) NA else g$centroids[, 2],
                  g$default[, 2], na.rm = TRUE)
  }
  plot(g[[1]], xlim = xlim, ylim = ylim, type = "n",
       asp = 1, ...)
  abline(h = 0, lty = 3)
  abline(v = 0, lty = 3)
  if (!is.null(g$species)) {
    if (type == "text")
      text(g$species, rownames(g$species), col = "red",
           cex = 0.7)
    else if (type == "points")
      points(g$species, pch = "+", col = "red",
             cex = 0.7)
  }
  if (!is.null(g$sites)) {
    if (type == "text")
      text(g$sites, rownames(g$sites), cex = 0.7)
    else if (type == "points")
      points(g$sites, pch = 1, cex = 0.7)
  }
  if (!is.null(g$constraints)) {
    if (type == "text")
      text(g$constraints, rownames(g$constraints), cex = 0.7,
           col = "darkgreen")
    else if (type == "points")
      points(g$constraints, pch = 2, cex = 0.7, col = "darkgreen")
  }
  if (!is.null(g$biplot) && nrow(g$biplot) > 0 && type != "none") {
    if (length(display) > 1) {
      mul <- ordiArrowMul(g$biplot)
    }
    else mul <- 1
    attr(g$biplot, "arrow.mul") <- mul
    arrows(0, 0, mul * g$biplot[, 1], mul * g$biplot[, 2],
           length = 0.05, col = "blue")
    biplabs <- vegan::ordiArrowTextXY(mul * g$biplot, rownames(g$biplot))
    text(biplabs, rownames(g$biplot), col = "blue")
  }
  if (!is.null(g$regression) && nrow(g$regression > 0) && type !=
      "none") {
    rcol <- "purple4"
    if (length(display) > 1) {
      mul <- ordiArrowMul(g$regression)
    }
    else mul <- 1
    attr(g$regression, "arrow.mul") <- mul
    arrows(0, 0, mul * g$regression[, 1], mul * g$regression[,
                                                             2], length = 0.05, col = rcol)
    biplabs <- ordiArrowTextXY(mul * g$regression, rownames(g$regression))
    text(biplabs, rownames(g$regression), col = rcol)
  }
  if (!is.null(g$centroids) && !anyNA(g$centroids) && type !=
      "none") {
    if (type == "text")
      text(g$centroids, rownames(g$centroids), col = "blue")
    else if (type == "points")
      points(g$centroids, pch = "x", col = "blue")
  }
  if (!is.null(g$default) && type != "none") {
    if (type == "text")
      text(g$default, rownames(g$default), cex = 0.7)
    else if (type == "points")
      points(g$default, pch = 1, cex = 0.7)
  }
  class(g) <- "ordiplot"
  invisible(g)
}
#' @export
#'
pwRDA.source<-function (x.ord, y.ord, BPs) {
  y.ord <- as.matrix(y.ord)
  x.ord <- as.matrix(x.ord)
  n <- nrow(x.ord)
  k <- ncol(x.ord)
  bks <- c(0, BPs, nrow(x.ord))
  nBPs <- length(bks) - 1
  Xb <- matrix(0, ncol = nBPs * k, nrow = n)
  for (i in 1:(nBPs)) {
    Xb[(bks[i] + 1):bks[i + 1], ((k * (i - 1)) + 1):((k *
                                                        (i - 1)) + k)] <- x.ord[(bks[i] + 1):bks[i + 1],
                                                        ]
  }
  Xb <- jitter(Xb)
  Xbc = scale(Xb, center = T, scale = F)
  rda.0 <- vegan::rda(data.frame(y.ord) ~ ., data.frame(x.ord))
  rda.pw <- vegan::rda(data.frame(y.ord) ~ ., data.frame(Xb))
  Yc = scale(y.ord, center = T, scale = F)
  Y.avg = matrix(rep(apply(Yc, 2, mean), times = nrow(Yc)),
                 ncol = ncol(Yc), byrow = T)
  B.pw = solve(t(Xbc) %*% Xbc) %*% (t(Xbc) %*% Yc)
  coord <- rda.pw$CCA$biplot
  bew.bp <- t(cor(coord, t(cor(x.ord, Xb))))
  rda.pw$CCA$biplot <- bew.bp
  Ypred.pw = Xbc %*% B.pw
  Yres <- Yc - Ypred.pw
  TSS.pw = sum((Yc)^2)
  RSS.pw = sum((Ypred.pw - Y.avg)^2)
  r2.pw <- RSS.pw/TSS.pw
  n.pw <- nrow(Xbc)
  k.pw <- ncol(Xbc)
  Radj.pw <- 1 - ((1 - r2.pw) * ((n.pw - 1)/(n.pw - k.pw -
                                               1)))
  Xc = scale(jitter(x.ord), center = T, scale = F)
  B.full = solve(t(Xc) %*% Xc) %*% (t(Xc) %*% Yc)
  Ypred.full = Xc %*% B.full
  Yres <- Yc - Ypred.full
  TSS.full = sum((Yc)^2)
  RSS.full = sum((Ypred.full - Y.avg)^2)
  r2.full <- RSS.full/TSS.full
  n.full <- nrow(Xc)
  k.full <- ncol(Xc)
  Radj.full <- 1 - ((1 - r2.full) * ((n.full - 1)/(n.full -
                                                     k.full - 1)))
  F.stat <- ((RSS.full - RSS.pw)/(k.full - k.pw))/(RSS.full/(n.pw -
                                                               k.full))
  dg1 <- k.pw - k.full
  dg2 <- n.pw - k.pw
  F.stat <- ((RSS.pw - RSS.full)/(dg1))/(RSS.pw/(dg2))
  p.value <- 1 - pf(F.stat, dg1, dg2, lower.tail = T)
  summ <- c(Radj.full = Radj.full, Radj.pw = Radj.pw, F.stat = F.stat,
            p.value = p.value)
  pw <- list(summ = summ, rda.0 = rda.0, rda.pw = rda.pw)
  class(pw) <- "pw"
  return(invisible(pw))
}
#' @export
prda<-function(model,pch=c(16,3),key=NULL,keytext=NULL,points=T,text=F,palette="black",col.sp="red",col.arrow='steelblue', show.sp=T, scaling=1, constr=FALSE, cex.bp=1,col.lab="red",cex.text=.8,cex.points=1,textcolor="gray", biplot=T, n.sp=NULL, pch.sp=3, sp.display="Label",newcolhabs=newcolhabs, pos=2, offset=0) {
  opar<-par(no.readonly=TRUE)
  if(!is.null(key)) {
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    col<-colkey[key]
    layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  } else{col= getcolhabs(newcolhabs,palette, nrow(model$CA$Xbar))}

  #par(mar=c(5,3,1,1))
  un<-round(summary(model)$cont$importance[2,1:2]*100,2)
  con<-round(summary(model)$concont$importance[2,1:2]*100,2)
  if(isFALSE(constr)){labs<-un} else {labs<-con}
  xlab<-paste("RDA I -"," (",labs[1]," %", ")*", sep="")
  ylab<-paste("RDA II -"," (",labs[2]," %", ")*", sep="")
  #xlab<-paste("RDA I")
  #ylab<-paste("RDA II")
  si<-scores(model,display="sites")
  bp<-scores(model,display="bp")
  sp<-scores(model,display="sp")
  plotrda<-plot.cca(model,pch=pch[1],xlab=xlab,ylab=ylab,col=col, las=1, scaling=scaling, type="n")
  if(isTRUE(points)){points.cca(model, display="sites", cex=cex.points, col=col, pch=pch[1], scaling=scaling)}
  if(isTRUE(text)){
    colkey2<-getcolhabs(newcolhabs,textcolor, nlevels(keytext))
    col2<-colkey2[keytext]
    text.cca(model, display="sites",labels=keytext, cex=cex.text,  col=col2, scaling=scaling, pos=pos, offset=offset)}
  sum_rda <- summary(model)
  splen <- sort(sqrt(rowSums(sum_rda$sp[,1:2]^2)), decreasing=T)
  if(is.null(n.sp)){n.sp<-length(splen) }
  which.sp<-names(splen)[1:n.sp]
  if(isTRUE(show.sp)){
    if(sp.display=="Label"){
      text.cca(sp[which.sp,], col=col.sp, scaling=scaling, labels=which.sp)
    } else{
      points.cca(sp[which.sp,], pch=pch.sp,col=col.sp, scaling=scaling)
    }
  }
  if(isTRUE(biplot)){
    if(length(bp)>0){
      points.cca(model, display="bp", axis.bp =T, scaling=scaling)
      text.cca(model,display="bp", axis.bp =T, scaling=scaling)
    }}
  if(!is.null(key)) {
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(newcolhabs,palette, nlevels(key))
    legend("center",pch=pch[1],col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }
  rda<-recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(rda)
}


#' @export
str_factors<-function(factors, palette="viridis",newcolhabs)
{


  m<-matrix(1:(ncol(factors)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(.3,.3,.3))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable")
  plot.new()
  text(0.5,.5,"Levels")
  plot.new()
  text(.5,.5,"Distribution")





  for(i in 1: ncol(factors))
  {
    plot.new()
    text(.5,.5,colnames(factors)[i])
  }
  for(i in 1: ncol(factors))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=nlevels(factors[,i])),.5,summary(factors[,i]))
  }
  for(i in 1:ncol(factors))
  {

    barplot(as.matrix(table(x[,i])), horiz=T,ann=F, axes=F, col=getcolhabs(newcolhabs,"viridis", nlevels(x[,i])))

  }


  lapply(factors, function(x){})

}



#' @export
str_numerics<-function(numerics, cextext=1)
{

  par(mar=c(0,0,0,0), cex=2)
  m<-matrix(1:(ncol(numerics)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(.2,.35,.35))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable", cex=cextext)
  plot.new()
  text(  seq(0.1,0.9, length.out=6),.5,c('Min.','1st Qu.','Median','Mean','3rd Qu. ',' Max.'), cex=cextext)
  plot.new()
  text(.5,.5,"Histogram", cex=cextext)

  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i], cex=cextext)
  }
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=6),.5,round(summary(numerics[,i]),3), cex=cextext)
  }

  for(i in 1: ncol(numerics))
  {

    hist(numerics[,i], ann=F, axes=F)
  }



}

#' @export
str_numerics2<-function(numerics,obs=NULL, confs=NULL)
{

  par(mar=c(0,0,0,0), cex=2)
  m<-matrix(1:(ncol(numerics)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(.2,.35,.35))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable")
  plot.new()
  text(  seq(0.1,0.9, length.out=6),.5,c('Min.','1st Qu.','Median','Mean','3rd Qu. ',' Max.'))
  plot.new()
  text(.5,.5,"Histogram")

  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i])
  }
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=6),.5,round(summary(numerics[,i]),3))
  }

  i=1
  for(i in 1: ncol(numerics))
  {

    h<-hist(numerics[,i], ann=F, axes=F, xlim=range(c(numerics[,i],obs[i])))

    if(!is.null(obs)){
      abline(v=obs[i], col= 'red')
    }

    x=confs[i,]
   y=range(h$counts)

   polygon(c(rep(unlist(x), each=2)), c(unlist(y),rev(unlist(y))))

    if(!is.null(confs)){
      abline(v=confs[i,], col= 'blue', lty=2)
    }
  }
}

#' @export
str_numerics3<-function(numerics,obs=NULL, pred_interval=NULL,
                        col.conf="#FCC7C7", col.obs="darkblue", col.pred="#CDCCD9",q_class,col.mean="SeaGreen", cex=2)
{
  q_class[,c(1:3)]<-round(q_class[,c(1:3)],5)

  par(mar=c(0,0,0,0), cex=cex)
  m<-matrix(1:((ncol(numerics)*6)+6), ncol=6)
  confs<-c(pred_interval/2,   1-(pred_interval/2))
  layout(m, widths = c(200,100,100,100,100,300))
  opar<-par(no.readonly=TRUE)
  plot.new()
  par(mar=c(0,0,0,0))
  text(.5,.5,"ID", cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i], cex=cex)
  }
  plot.new()
  text(.5,.5,"Observed", cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,1], cex=cex)
  }
  plot.new()
  text(.5,.5,paste0("q",confs[[1]]), cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,2], cex=cex)
  }
  plot.new()
  text(.5,.5,paste0("q",confs[[2]]), cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,3], cex=cex)
  }
  plot.new()
  text(.5,.5,"q_class", cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,4], cex=cex)
  }

  plot.new()
  text(.5,.5,"Density plot", cex=cex)

i=2

     for(i in 1: ncol(numerics))
  {
    confcurve(
      vec=numerics[,i],
      ob=obs[i],
      pred_interval=pred_interval,
      col.conf=col.conf, col.obs=col.obs, col.pred=col.pred,col.mean=col.mean
    )
  }



}
#' @export
confcurve<-function(vec,ob,pred_interval, col.conf="#FCC7C7", col.obs="darkblue", col.pred="#CDCCD9",col.mean="SeaGreen"){

  confs<-c(pred_interval/2,   1-(pred_interval/2))

  dens <- density(vec)
  plot(dens, xlim=range(c(ob,dens$x)),yaxt="n",xaxt="n", ann=F, type="n", bty="n")
  #plot(dens, xlim=range(c(ob,vec)))

  x1_sup <- min(which(dens$x >= quantile(vec, confs[2]))) # quantile() ftw!
  x2_sup <- max(which(dens$x <  max(dens$x)))
  with(dens, polygon(x=c(x[c(x1_sup,x1_sup:x2_sup,x2_sup)]), y= c(0, y[x1_sup:x2_sup], 0),
                     col=col.conf, border=NA
  ))

  x1_inf <- 1 # quantile() ftw!
  x2_inf <- max(which(!dens$x >= quantile(vec, confs[1])))
  with(dens, polygon(x=c(x[c(x1_inf,x1_inf:x2_inf,x2_inf)]), y= c(0, y[x1_inf:x2_inf], 0),
                     col=col.conf, border=NA
  ))

  x1 <- x1_sup
  x2 <- x2_inf
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0),
                     col=col.pred, border=NA
  ))
  abline(v=ob, lty=2, col=col.obs, lwd=2)
  abline(v=mean(vec), lty=2, col=col.mean, lwd=2)

}


#' @export
getclassmat<-function(data.factors)
{
  res<-lapply(data.factors,classvec2classmat)
  namesfac<-names(res)
  colnamesfac<-lapply(res,colnames)
  names<-list()
  for( i in 1:length(namesfac))
  {
    names[[i]]<-paste(namesfac[i], colnamesfac[[i]], sep="_")
  }
  classmat<-do.call("cbind",res)
  colnames(classmat)<-unlist(names)
  return(classmat)
}




#' @export
getHelp <- function(fun){
  temp = tools::Rd2HTML(gbRd::Rd_fun(fun),out = tempfile("docs"),dynamic=T)
  content = readr::read_file(temp)
  file.remove(temp)
  content
}
#' @export

textrfloop<-function(...)
{
  div(
    p("Compute the accuracies using random forest algorithm for increasing number of clusters (k):"),
    p(style="margin-left: 5px;",code("*"),"For each k, calculate the accuracy;"),
    p(style="margin-left: 10px;",code("*"),"Plot the curve of accuracies according to the number of clusters k;"),
    p(style="margin-left: 5px;",code("*"),"The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.")
  )
}
#' @export
textelbow<-function(...)
{

  div(
    p("Compute the Within-Sum-of-Squares (WSS) for increasing number of clusters (k):"),
    p(style="margin-left: 5px;",code("*"),"For each k, calculate the WSS;"),
    p(style="margin-left: 5px;",code("*"),"Plot the curve of WSS according to the number of clusters k;"),
    p(style="margin-left: 5px;",code("*"),"The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.")
  )
}
#' @export










textsmw<-function(...)
{
  "Performs the split moving window analysis for several windows sizes, calculates the mean dissimilarity profiles, and considers as significant the dissimilarities that exceed mean plus one standard deviation. Click for more information"

}
#' @export

textrfbphelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p(strong("Dissimilarity profile of the moving split-window analysis:")),
                    p(strong("1."),"Placing a window of even-numbered size at the beginning of the data series"),
                    div(style="margin-left: 5px;",
                        p(strong("2."),"splitting the window into two equal halves"),
                        p(strong("3."),"computing a dissimilarity  between the two halves"),
                        p(strong("4."),"shifting window one position along the series, and"),
                        p(strong("5.")," repeating the procedure till the end of the data series  (Cornelius & Reynolds 1991)")
                    )




    )
    ))
}
#' @export


textbphelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p(strong("Dissimilarity profile of the moving split-window analysis:")),
                    p(strong("1."),"Placing a window of even-numbered size at the beginning of the data series"),
                    div(style="margin-left: 5px;",
                        p(strong("2."),"splitting the window into two equal halves"),
                        p(strong("3."),"computing a dissimilarity  between the two halves"),
                        p(strong("4."),"shifting window one position along the series, and"),
                        p(strong("5.")," repeating the procedure till the end of the data series  (Cornelius & Reynolds 1991)")
                    )




    )
    ))
}
#' @export





#' @export
divI<-function(abund,choices=c("N","S","margalef","D","H","J'","Dom_rel","Skewness")){
  res=list()
  if("N"%in%choices){res$N<-rowSums(abund)}
  if("S"%in%choices){res$S<-vegan::specnumber(abund)}
  if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
  if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
  if("H"%in%choices){res$H<-vegan::diversity(abund)}
  if("J'"%in%choices){
    H<-vegan::diversity(abund)
    S<-vegan::specnumber(abund)
    res$J <- H/log(S)}
  if("Dom_rel"%in%choices){res$Dom_rel<-apply(decostand(abund, "total"),1,sort,T)[1,]}
  if("Skewness"%in%choices){res$Skewness=apply(decostand(abund,"log"),1,skewness)}
  return(data.frame(do.call(cbind,res)))
}

#' @export
plot_by_class<-function(data,palette='viridis', fac,newcolhabs){

  data$class<-fac
  col<-getcolhabs(newcolhabs,palette,nlevels(data$class))

  res<-data.frame(cbind(data[,10], utils::stack(lapply(data[1:9], as.character))))



  res<-res[,c(1,3,2)]
  colnames(res)<-c("class",'x','y')
  res$num<-as.numeric(res$y)

  str(res)
  colnames(res)
  ggplot(res,aes(x=num, y=class, height = num, group = class, fill=class)) +
    geom_density_ridges()


  ggplot(res,
         aes(
           x = num,
           y = fct_reorder(class, num, .fun = sum),
           fill = fct_reorder(class, num, .fun = sum)
         )) +
    geom_density_ridges() +
    theme(legend.position = "none")


  data<-res
  gg<-data %>%
    #gather(x, y, colnames(data)[1]:colnames(data)[ncol(data)-1]) %>%
    ggplot(aes(x = y, y = class, color = class, fill = class)) +
    scale_color_manual(values=col)+
    scale_fill_manual(values=col)+
    facet_wrap( ~ x, scale = "free", ncol = 3) +
    geom_density_ridges(alpha = 0.8) +
    guides(fill = FALSE, color = FALSE)
  gg
}
#' @export
sortImp2<-function (object, top)
{
  if (object$calledFrom == "varImp") {
    best <- switch(object$model, pam = "maxabs", "max")
  }
  else {
    best <- "max"
  }
  featureRank <- switch(best, max = rank(-apply(object$importance,
                                                1, max, na.rm = TRUE)), min = rank(apply(object$importance,
                                                                                         1, min, na.rm = TRUE)), maxabs = rank(-apply(abs(object$importance),
                                                                                                                                      1, max, na.rm = TRUE)))
  tiedRanks <- as.numeric(names(table(featureRank)[table(featureRank) >
                                                     1]))
  if (length(tiedRanks) > 0) {
    for (i in seq(along = tiedRanks)) {
      tmp <- featureRank[featureRank == tiedRanks[i]]
      featureRank[featureRank == tiedRanks[i]] <- tmp +
        runif(length(tmp), min = 0.001, max = 0.999)
    }
  }
  featureOrder <- order(featureRank)
  out <- object$importance[featureOrder, , drop = FALSE]
  out <- out[1:top, , drop = FALSE]
  out
}
#' @export

plot_varimport<-function(m,palette='viridis',newcolhabs,position="dodge",type="gg",nvars=10, cex.axes=13,cex.lab=13,cex.main=15,cex.sub=13,cex.leg=13){

  X <- varImp(m)

  #colnames(X$importance)<- m$levels
   if(type=="default"){

     #nvars=10
     DF<-data.frame(variable=rownames(X$importance), Importance=X$importance)
     colnames(DF)<-c("variable","Importance")
     col<-colsbyprof(DF$Importance, cols=getcolhabs(newcolhabs,palette,100))

     pic<-order(DF$Importance, decreasing=T)[1:nvars]
     DF<-DF[pic,]


     gg<-ggplot(DF, aes(x=reorder(variable,Importance), y=Importance,fill=Importance))+
       geom_bar(stat="identity")+ coord_flip()+
       ylab("Variable Importance")+
       xlab("")+
       ggtitle("Feature Importance")+


       scale_fill_gradientn(colours=getcolhabs(newcolhabs,palette,nrow(X$importance)),breaks=pretty(X$importance[pic,]), limits=range(pretty(X$importance[pic,])))+
       theme_minimal() +
       theme(
         panel.ontop = TRUE,
             panel.background=element_rect(fill=NA, color="white"),
         panel.grid.major=element_line(color=NA),
         panel.grid.minor=element_line(color="gray",linetype=2),
             panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),

             axis.line=element_line(),
             axis.text=element_text(size=cex.axes),
             axis.title=element_text(size=cex.lab,face="bold"),
             plot.title=element_text(size=cex.main),
             plot.subtitle=element_text(size=cex.sub),
             legend.text=element_text(size=cex.leg),
             legend.title=element_text(size=cex.leg))
gg



    # gg<-plot(X,nvars)
   } else{
     plotObj <- sortImp2(X, top=nvars)
     if (ncol(plotObj) == 2) {
       plotObj <- plotObj[, 1, drop = FALSE]
       names(plotObj) <- "Importance"
     }

     featureNames <- rownames(plotObj)
     outcomeNames <- colnames(plotObj)
     df<- data.frame(as.table(as.matrix(X$importance[featureNames,outcomeNames])))
     colnames(df)[2]<-"Classes"
     df$Var1<-ordered(df$Var1,levels=names(sort(unlist(lapply(split(df,df$Var1),function(x) sum(x[,3]))),decreasing  =F)))
     col<-getcolhabs(newcolhabs,palette,nlevels(df$Classes))
     gg<-df %>%
       ggplot(aes(x=Var1,y=Freq, fill=Classes)) +
       geom_bar(position = position,stat = "identity") +
       scale_fill_manual(values=col)+
       coord_flip() +
       labs(x="Variables", y="Importance")
  }

  gg

}
#' @export


plot_florest_class<-function(res_forest,newcolhabs, palette="viridis", ylab="N of Trees"){
  obs<-attr(res_forest,'obs')
  df<- data.frame(as.table(as.matrix(res_forest)))
  df$Obs<-obs[df$Var1,]
  df$obs_x<-attr(res_forest,'ntree')+3
  Obs<-obs[df$Var1,]
  colnames(df)[2]<-"Classes"

  col<-getcolhabs(newcolhabs,palette,nlevels(df$Classes))
  gg<-df %>%
    ggplot(aes(x=Var1,y=Freq, fill=Classes)) +
    geom_bar(position = 'stack',stat = "identity") +
    scale_fill_manual(values=col)+
    scale_y_continuous(limits = c(0,  attr(res_forest,'ntree')+5))+
    geom_point(data=df, aes(Var1, obs_x, col=factor(Obs)), col=col[Obs],show.legend=F) +
    coord_flip() +
    labs(x="Observations", y=ylab)

  gg

}
#' @export

retype<-function(data){
  suppressWarnings({
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    num<-data
    ints<-names(which(unlist(lapply(num, function(x) sum(is.wholenumber(x))==nrow(data)))))

    num[ints]<-lapply(data[ints],function(x) as.integer(x))

    return(num)


  })
}
#' @export


plot_ridges<-function(data,fac,palette,newcolhabs){
  col<-getcolhabs(newcolhabs,palette,nlevels(data$class))
  x<-seq_along(colnames(data)[-ncol(data)])[1]
  resss<-lapply(seq_along(colnames(data)[-ncol(data)]), function(x){
    var<-colnames(data)[x]
    df<-data[x]
    df<-data.frame(x=df,y=data$class)
    colnames(df)<-c("x","y")
    ggplot(df, aes(x = x, y = y)) +
      geom_density_ridges(aes(fill = y),show.legend = T) +
      scale_fill_manual(values = c(col))+
      ggtitle(NULL)+
      xlab(var) + ylab(NULL)+ guides(fill=guide_legend(title=fac))
  })
  ggarrange(plotlist=resss, ncol=3, nrow=3, common.legend = TRUE, legend="top")

}

