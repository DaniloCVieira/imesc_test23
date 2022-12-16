
#' @export
distsom<-function(m){
  switch(m$dist.fcts,
         "BrayCurtis"={vegdist(m$codes[[1]])},
         "euclidean"={dist(m$codes[[1]])},
         "sumofsquares"={dist(m$codes[[1]])^2},
         "manhattan"={dist(m$codes[[1]],method="manhattan")}
  )
}
#' @export
som_quality_class<-function(m,newdata,somC){
  #somC<-readRDS("somC_araca.rds")
  #newdata<-vals$saved_data[["nema_c1_mean"]]
  neu_hc<-as.factor(somC$som.hc)
  obs_hc<-somC$somC
  i=2
  lev_hc<-levels(neu_hc)
  res<-do.call(rbind,lapply(1:somC$groups, function(i){
    som_temp<-m
    ob<-which(obs_hc==lev_hc[i])
    ne<-which(neu_hc==lev_hc[i])
    som_temp$grid[[1]]<-m$grid[[1]][ne,]
    som_temp$unit.classif<-m$unit.classif[ob]
    som_temp$codes[[1]]<-som_temp$codes[[1]][ne,]
    names(som_temp$unit.classif)<-paste0("V",som_temp$unit.classif)
    traindat=newdata[ob,]
    som<-som_temp

    somQuality2(som, traindat )
  }))
  rownames(res)<-as.factor(levels(somC$som.hc))
  res

}

#' @export
somQuality2<-function (som, traindat) {
  if (is.null(som))
    return(NULL)
  ok.dist <- som_dist2(som)

  bmu <- som$unit.classif
  dim(som$codes[[1]])
  sqdist <- rowSums((traindat - som$codes[[1]][names(bmu), ])^2, na.rm = TRUE)
  err.quant <- mean(sqdist)
  totalvar <- mean(rowSums(t(t(traindat) - colMeans(traindat,
                                                    na.rm = TRUE))^2, na.rm = TRUE))
  err.varratio <- 100 - round(100 * err.quant/totalvar, 2)

  for(i in 1:4){
    rownames(ok.dist[[i]])<-colnames(ok.dist[[i]])<-  rownames(som$codes[[1]])
  }

  row=1
  bmu2 <- apply(traindat, 1, function(row) {

    dist <- colMeans((t(som$codes[[1]]) - row)^2, na.rm = TRUE)
    names(dist)[order(dist)[2]]

  })


  err.topo <- mean(!ok.dist$neigh.matrix[cbind(names(bmu), bmu2)])

  reskaski<-e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)
  for(i in 1:length(reskaski)){
    rownames(reskaski[[i]])<-colnames(reskaski[[i]])<-rownames(som$codes[[1]])
  }

  err.kaski <- reskaski$length[cbind(names(bmu),
                                     bmu2)]
  err.kaski <- mean(err.kaski + sqrt(sqdist))
  cellpop <- table(factor(som$unit.classif, levels = 1:nrow(som$grid$pts)))
  res <- c(err.quant = err.quant, err.varratio = err.varratio,
           err.topo = err.topo, err.kaski = err.kaski, cellpop = cellpop)
  res4<-mapply(c,err.quant = err.quant, err.varratio = err.varratio,
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)

  res4[[1]]

}
#' @export
somQuality3<-function (som, traindat, somC)
{
  if (is.null(som))
    return(NULL)
  ok.dist <- somDist(som)
  bmu <- som$unit.classif
  sqdist <- rowSums((traindat - som$codes[[1]][bmu, ])^2, na.rm = TRUE)


  err.quant <- tapply(sqdist,as.factor(somC$somC[names(sqdist)]),mean)
  tot_vars<-rowSums(t(t(traindat) - colMeans(traindat,
                                             na.rm = TRUE))^2, na.rm = TRUE)
  totalvar <- tapply(tot_vars,as.factor(somC$somC[names(sqdist)]),mean)


  err.varratio <- lapply(1:length(totalvar),function(x){
    100 - round(100 * err.quant[[x]]/totalvar[[x]], 2)
  })
  bmu2 <- apply(traindat, 1, function(row) {
    dist <- colMeans((t(som$codes[[1]]) - row)^2, na.rm = TRUE)
    order(dist)[2]
  })


  err.topo <-   tapply(!ok.dist$neigh.matrix[cbind(bmu, bmu2)],as.factor(somC$somC[names(sqdist)]),mean)

  err.kaski <- e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu,
                                                                                   bmu2)]

  err.kaski <-  tapply(err.kaski + sqrt(sqdist),as.factor(somC$somC),mean)


  res4<-mapply(c,err.quant = err.quant, err.varratio = unlist(err.varratio),
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)
  do.call(rbind,res4)

}
#' @export
som_dist2<-function (som) {

  if (is.null(som))
    return(NULL)
  proto.gridspace.dist <-as.matrix(dist(som$grid[[1]]))
  proto.dataspace.dist <- as.matrix(distsom(som))
  neigh <- round(proto.gridspace.dist, 3) == 1
  proto.dataspace.dist.neigh <- proto.dataspace.dist
  proto.dataspace.dist.neigh[!neigh] <- NA
  list(proto.grid.dist = proto.gridspace.dist, neigh.matrix = neigh,
       proto.data.dist = proto.dataspace.dist, proto.data.dist.neigh = proto.dataspace.dist.neigh)
}

#' @export
som_dist<-function (som) {

  if (is.null(som))
    return(NULL)
  proto.gridspace.dist <- kohonen::unit.distances(som$grid,
                                                  F)
  proto.dataspace.dist <- as.matrix(kohonen::object.distances(som,"codes"))
  neigh <- round(proto.gridspace.dist, 3) == 1
  proto.dataspace.dist.neigh <- proto.dataspace.dist
  proto.dataspace.dist.neigh[!neigh] <- NA
  list(proto.grid.dist = proto.gridspace.dist, neigh.matrix = neigh,
       proto.data.dist = proto.dataspace.dist, proto.data.dist.neigh = proto.dataspace.dist.neigh)
}
#' @export
Dcodesamples<-function(m,newdata){
  start_time <- Sys.time()
  codes<-kohonen::getCodes(m)
  data<-newdata
  res<-apply(data,1,function(x){
    apply(codes,1,function(xx){
      switch(m$dist.fcts,
             "BrayCurtis"={vegdist(t(data.frame(x,xx)))},
             "euclidean"={dist(t(data.frame(x,xx)))},
             "sumofsquares"={dist(t(data.frame(x,xx)))^2},
             "manhattan"={dist(t(data.frame(x,xx)),method="manhattan")}
      )
    })
  })



  return(res)
}

#' @export
quality_som<-function(som, som_pred) {

  traindat=som_pred$predictions
  if (is.null(som))
    return(NULL)
  ok.dist <- som_dist(som)


  bmu <- som_pred$unit.classif
  codes=som$codes


  res<-mapply(list,codes=codes,traindat=traindat, SIMPLIFY = F)
  sqdist <-lapply(res,function(x){
    rowSums((x[[2]] -x[[1]][bmu, ])^2,
            na.rm = TRUE)
  })
  err.quant<-lapply(sqdist,mean)
  x<-traindat[[1]]

  totalvar <- lapply(traindat,function(x){
    mean(rowSums(t(t(x) - colMeans(x,
                                   na.rm = TRUE))^2, na.rm = TRUE))
  })

  res2<-mapply(c,err.quant,totalvar, SIMPLIFY = F)

  err.varratio <-  lapply(res2,function(x){
    100 - round(100 * x[[1]]/x[[2]], 2)
  })

  bmu2 <- lapply(res,function(x){
    apply(x$traindat, 1, function(row) {
      dist <- colMeans((t(x$codes) - row)^2, na.rm = TRUE)
      order(dist)[2]
    })
  })


  err.topo <- lapply(bmu2,function(x){
    mean(!ok.dist$neigh.matrix[cbind(bmu, x)])
  })

  err.kaski <-lapply(bmu2,function(x){
    e1071::allShortestPaths(ok.dist$proto.data.dist.neigh)$length[cbind(bmu,
                                                                        x)]
  })

  res3<-mapply(list,sqdist=sqdist,err.kaski=err.kaski, SIMPLIFY = F)



  err.kaski <- lapply(res3,function(x){
    mean(x$err.kaski + sqrt(x$sqdist))
  })

  cellpop <- table(factor(som$unit.classif, levels = 1:nrow(som$grid$pts)))

  res4<-mapply(c,err.quant = err.quant, err.varratio = err.varratio,
               err.topo = err.topo, err.kaski = err.kaski, SIMPLIFY = F)

  res5<-do.call(rbind,res4)
  if(nrow(res5)==2){
    rownames(res5)<-c("X","Y")
  } else{ c("X")}

  res5
}


#' @export
fun_correctsom<-function(newdata,m){
  newdata=as.matrix(newdata)
  pred_som<-predict(m,newdata=newdata, whatmap = 1)
  res<-get_correct_predsom(m,pred_som,newdata)
  res
}
#' @export
fun_errors_somX<-function(correctsom,newdata){
  predX<- correctsom$predX
  observed<-newdata
  res0<-data.frame(get_unit_errors(data.frame(predX),observed))
  res0
}
#' @export
fun_errors_somY<-function(correctsom,newdata){
  predX<- correctsom$predY
  observed<-newdata
  res0<-data.frame(get_unit_errors(data.frame(predX),observed))
  res0
}
#' @export
fun_errors_somX_sp<-function(correctsom,newdata){
  predX<- correctsom$predX
  observed<-newdata

  rmse_res<- mse_res<-mape_res<-mae_res<-list()
  for(i in 1:ncol(observed))
  {
    mape_res[[i]]<-mape(observed[,i],predX[,i])
    mse_res[[i]]<-mse(observed[,i],predX[,i])
    rmse_res[[i]]<-rmse(observed[,i],predX[,i])
    mae_res[[i]]<-mae(observed[,i], predX[,i])
  }
  rmse<-data.frame(RMSE=do.call(c,rmse_res))
  mae<-data.frame(MAE=do.call(c,mae_res))
  mape<-data.frame(MAPE=do.call(c,mape_res))
  mse<-data.frame(MSE=do.call(c,mse_res))
  rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
  data.frame(mae=mae,mse=mse,rmse=rmse)

}
#' @export
fun_errors_somY_sp<-function(correctsom,newdata_somY){
  predY<- correctsom$predY
  observed<-newdata_somY
  rmse_res<- mse_res<-mape_res<-mae_res<-list()
  for(i in 1:ncol(observed))
  {
    mape_res[[i]]<-mape(observed[,i],predY[,i])
    mse_res[[i]]<-mse(observed[,i],predY[,i])
    rmse_res[[i]]<-rmse(observed[,i],predY[,i])
    mae_res[[i]]<-mae(observed[,i], predY[,i])
  }
  rmse<-data.frame(RMSE=do.call(c,rmse_res))
  mae<-data.frame(MAE=do.call(c,mae_res))
  mape<-data.frame(MAPE=do.call(c,mape_res))
  mse<-data.frame(MSE=do.call(c,mse_res))
  rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
  data.frame(mae=mae,mse=mse,rmse=rmse)

}

#' @export
get_somERR<-function(predsom_results=c('BMUs',
                                       'Data predictions (X)',
                                       'Neuron predictions (X)',
                                       'Data predictions (Y)',
                                       "Neuron predictions (Y)",
                                       "Obs errors (X)",
                                       "Obs errors (Y)",
                                       "Var errors (X)",
                                       "Var errors (Y)","SOM quality"),m,newdata,newdataY,som_pred){
  correctsom<-fun_correctsom(newdata, m)
  predX<- correctsom$predX
  predY<- correctsom$predY
  res<-switch(predsom_results,
              'BMUs'={
                bmu<-data.frame(bmu_test=som_pred$unit.classif)
                rownames(bmu)<-rownames(predX)
                bmu
              },
              'Data predictions (X)'={predX},
              'Neuron predictions (X)'={som_pred$unit.predictions[[1]]},
              'Data predictions (Y)'={predY},
              "Neuron predictions (Y)"={ neuY<-data.frame(do.call(cbind,som_pred$unit.predictions[-1]))},
              "Obs errors (X)"={
                data.frame(fun_errors_somX(correctsom,newdata))
              },
              "Obs errors (Y)"={
                try(data.frame(fun_errors_somY(correctsom,newdata)),silent =T)
              },
              "Var errors (X)"={
                data.frame(fun_errors_somX_sp(correctsom,newdata))
              },
              "Var errors (Y)"={
                try(data.frame(fun_errors_somY_sp(correctsom,newdataY)),silent =T)
              },
              "Quality measures"={data.frame(quality_som(som=m,som_pred=som_pred))}

  )
  return(res)
}

#' @export
match_col<-function(x,ref)
{
  sum(colnames(x)%in%colnames(ref))==ncol(x)
}


#' @export
cutdata1<-function(data,  method.hc="ward.D2", dist="bray", col=NULL)
{
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  d<-vegdist(data, method=dist)
  hc<-hclust(d,method=method.hc)
  hc

  return(hc)
}

#' @export
cutm<-function(m,  method.hc="ward.D2")
{
  codes<-kohonen::getCodes(m)
  d=kohonen::object.distances(m,"codes")
  hc<-hclust(d,method=method.hc)
  return(hc)
}




#' @export
cutsom<-function(m,groups, members=NULL, method.hc="ward.D2", palette="turbo",newcolhabs, dataX)
{


  codes<-m$codes[[1]]
  weights=table(factor(m$unit.classif, levels=1:nrow(codes)))
  if(is.null(members)==F){members=weights}
  d=kohonen::object.distances(m,"codes")
  hc<-hclust(d,method=method.hc,members=members)

  hcut<-cutree( hc, groups)
  newclass<-m$unit.classif
  for(i in 1:length(hcut)) {newclass[newclass==i]<-rep(hcut[i],sum(  newclass==i))}


  names(newclass)<-rownames(dataX)
  pred<-newclass
  colhabs<-getcolhabs(newcolhabs,palette,groups)

  col_vector2=c(colors_bmu(m))
  res<-list(groups,pred,result=NULL, hcut )
  somC<-list(somC=pred, colhabs=colhabs,som.model=m,som.hc=hcut, groups=groups, colunits=col_vector2,cluster.result=NULL, hc.object=hc)
  class(somC)<-"somC"
  return(somC)

}

#' @export
cutdata2<-function(data, groups, method.hc="ward.D2", dist="bray", palette="turbo",newcolhabs)
{
  #validate(need(anyNA(data)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
  d<-vegdist(data, method=dist)
  hc<-hclust(d,method=method.hc)
  pred<-hcut<-cutree( hc, groups)
  #names(pred)<-rownames(data)
  colhabs<-getcolhabs(newcolhabs,palette,groups)
  m=list()
  m[[1]]<-data
  somC<-list(somC=pred, colhabs=colhabs,som.model=m,som.hc=hcut, groups=groups,hc.object=hc)
  return(somC)
}


#' @export
hc_plot<-function(somC, col=NULL, labels=NULL)
{

  opar<-par(no.readonly=TRUE)

  hc <- as.hclust(as.dendrogram(somC$hc.object))

  hc.dendo <- as.dendrogram(somC$hc.object)
  my_5_cluster <-stats::cutree(hc, k=somC$groups)
  clust.cutree <- dendextend::cutree(hc.dendo, k=somC$groups, order_clusters_as_data = FALSE)

  idx <- order(as.vector(names(clust.cutree)))
  clust.cutree <- clust.cutree[idx]
  df.merge <- merge(my_5_cluster,clust.cutree,by='row.names')
  df.merge.sorted <- df.merge[order(df.merge$y),]
  lbls<-unique(df.merge.sorted$x)
  if(is.null(col)){ color=somC$colhabs[lbls]
  } else {color=col}

  dend1 <- color_branches(hc.dendo, col=color,k = somC$groups, groupLabels = lbls)
  if(is.null(col)){
    colors_dend1<- somC$colhabs[my_5_cluster[labels(dend1)]]
  } else {colors_dend1="gray80" }

  if(!is.null(labels)){
    lab_dend<-labels(dend1)
    names(labels)<-names(somC[[1]])
    labels(dend1)<-labels[labels(dend1)]
  }
  labels_colors(dend1)<-colors_dend1
  plot(dend1,main="Cluster Dendogram", ylab="Height")

  phc <- recordPlot()
  on.exit(par(opar),add=TRUE,after=FALSE)
  return(phc)

}



#' @export
Kdata<-function(x){
  res<-ceiling((5*sqrt(nrow(x)))/3)
  if(res>15){res<-15}
  res
}



#' @export
myKmodel<-function(x){
  res<-ceiling(nrow(getCodes(x))/3)
  if(res>15){res<-15}
  res
}



#' @export
topology_back<-function(df,dist="BrayCurtis",topo=T) {
  if(isTRUE(topo)){

    df=data.frame(na.omit(df))
    rem<-which(colSums(df)==0)
    if(length(rem)>0){df<-df[,-rem]}

    if(length(attr(df,"scaled:scale"))<1)
    {
      df=scale(df)
    }
    #m<-supersom(as.matrix(df), grid=somgrid(2,2), rlen=1, dist.fcts=dist)
    df<-data.frame(df)*100
    N=floor(5*sqrt(nrow(df)))
    #ev <- eigen(object.distances(m,"data"))
    ev <- eigen(cov(as.matrix(df)))
    ratio_y<-ev$values[order(ev$values, decreasing = T)[2]]/abs(ev$values[order(ev$values, decreasing = T)[1]])
    ratio_x<-ev$values[order(ev$values, decreasing = T)[1]]/ev$values[order(ev$values, decreasing = T)[2]]
    y=floor(sqrt(N/ratio_y))
    x=ceiling(sqrt(N/ratio_x))
    res<-c(units=N,x=y,y=x)
    input<-res
    units<-input[1]
    gridtopo=input[2:3]
    Ydoble<-input[3]*2
    names(gridtopo)<-NULL
    prop<-gridtopo[1]/gridtopo[2]


  } else{return( res<-c(units=25,x=5,y=5))}
  res
}
#' @export
topology<-function(df,dist="BrayCurtis",topo=T) {

  pic<-which(unlist(lapply(data.frame(df),function(x) var(x)))==0)
  if(length(pic)>0){
    df<-df[,-pic]
  }

  df=data.frame(na.omit(df))
  rem<-which(colSums(df)==0)
  if(length(rem)>0){df<-df[,-rem]}

  if(length(attr(df,"scaled:scale"))<1)
  {
    df=scale(df)
  }
  N=floor(5*sqrt(nrow(df)))
  pr<-prcomp(df)
  ratio_y<-pr$x[order(pr$x, decreasing = T)[2]]/abs(pr$x[order(pr$x, decreasing = T)[1]])
  ratio_x<-pr$x[order(pr$x, decreasing = T)[1]]/pr$x[order(pr$x, decreasing = T)[2]]
  y=floor(sqrt(N/ratio_y))
  x=ceiling(sqrt(N/ratio_x))
  res<-c(units=N,x=y,y=x)
  input<-res
  units<-input[1]
  gridtopo=input[2:3]
  Ydoble<-input[3]*2
  names(gridtopo)<-NULL
  prop<-gridtopo[1]/gridtopo[2]
  res
}



#' @export
weighted.correlation <- function(v,w,grille)
{

  x <- grille$grid$pts[,"x"]
  y <- grille$grid$pts[,"y"]
  mx <- weighted.mean(x,w)
  my <- weighted.mean(y,w)
  mv <- weighted.mean(v,w)
  numx <- sum(w*(x-mx)*(v-mv))
  denomx <- sqrt(sum(w*(x-mx)^2))*sqrt(sum(w*(v-mv)^2))
  numy <- sum(w*(y-my)*(v-mv))
  denomy <- sqrt(sum(w*(y-my)^2))*sqrt(sum(w*(v-mv)^2)) #correlation for the two axes
  res <- c(numx/denomx,numy/denomy)
  return(res)
}



#' @export
colors_bmu<-function(m)
{
  fun_xy <- function(x, y){
    R <- (x+1)/2
    G <- (1-x)/2
    B <- (y+1)/2
    A <- 1- 0.5*exp(-(x^2+y^2)/0.2)
    rgb(R, G, B, A)}
  z <- outer(seq(-1,1,length=m$grid$xdim), seq(-1,1,length=m$grid$ydim), FUN = fun_xy)
  return(z)
}




#' @export
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                    border.bg = NA, adj = 1, pos = 4, offset = 0,
                    padding = c(0.5, 0.5), cex = 1, font = par('font')){

  ## The Character expansion factro to be used:
  theCex <- par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }
  strheight
  ## Width and height of text
  textHeight <- strheight(labels, cex = theCex, font = font)
  textWidth <- strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  rect(xleft = xMid - rectWidth/2,
       ybottom = yMid - rectHeight/2,
       xright = xMid + rectWidth/2,
       ytop = yMid + rectHeight/2,
       col = col.bg, border = border.bg)

  ## Place the text:
  text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
       adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}


#' @export
train.summary_fun<- function(m){
  {

    traindata <- data.frame(m$data[[1]])
    mean = round(mean(unlist(traindata)), 2)
    n.obs = nrow(traindata)
    n.variables = ncol(traindata)
    summ <- m$grid[-1]
    summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
    summ <- do.call(rbind, summ)

    mode <-attr(m,"mode")
    alpha0 = m$alpha[1]
    alpha1 =m$alpha[2]
    radius0 =m$radius[1]
    radius1 =m$radius[2]
    user.weights = m$user.weights
    maxNA.fraction = m$maxNA.fraction
    dist.fcts = m$dist.fcts[[1]]

    summ[,1]<-tolower(summ[,1])

    Parameters <- rbind(
        errors_som(m),
        n.obs,
        n.variables,
        summ,
        alpha0,
        alpha1,
        radius0,
        radius1,

        maxNA.fraction,
        dist.fcts,
        mode

      )

    data.frame(Parameters)

  }
}


