
cordata_filter<-function(data,cor_method="pearson",cor_cutoff=0.75,cor_use='everything'){
  met<-match.arg(cor_method,c("pearson", "kendall", "spearman"))

  pic<-which(apply(data,2,function(x) var(x,na.rm=T))==0)
  if(length(pic)>0){
    datatemp<-data
    #datatemp[is.na(datatemp)]<-0
    datatemp[colnames(data)[pic]]<-NULL
    data<-datatemp
  }


  if(cor_cutoff==1){
    cordata<-cor(data, use=cor_use,method =met)
    return(cordata)
  }

  cordata<-cor(data, use=cor_use,method =met)
  tmp <- cordata
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data.new<-data[,-which(colnames(data)%in%unique(rownames(which(abs(tmp)>cor_cutoff,arr.ind = T))))]
  cordata<-cor(data.new, use=cor_use,method =met)
  cordata

}
i_corplot<-function(cordata,newcolhabs,cor_palette,cor_sepwidth_a,
                    cor_sepwidth_b,cor_notecex,cor_noteco,cor_na.color,
                    cor_sepcolor,cor_dendogram,
                    cor_scale,cor_Rowv,cor_Colv,cor_revC,
                    cor_na.rm,cor_labRow,cor_labCol,cor_cellnote,cor_density.info) {
  sepwidth=c(cor_sepwidth_a,cor_sepwidth_b)

  hmet=match.arg(cor_hclust_method,c('ward.D','ward.D2','single','complete','average','mcquitty','median','centroid'))
  hdist<-match.arg(cor_distance,c('euclidean','bray','jaccard','hellinger'))
  dend<-match.arg(cor_dendogram,c("both","row","column","none"))
  sca_de<-match.arg(cor_scale,c("none","row", "column"))
  Rowv<-as.logical(match.arg(cor_Rowv,c('TRUE','FALSE')))
  Colv<-match.arg(cor_Colv,c('Rowv',T,F))
  revC<-as.logical(match.arg(cor_revC,c('TRUE','FALSE')))
  na.rm<-as.logical(match.arg(cor_na.rm,c('TRUE','FALSE')))

  labRow<-as.logical(match.arg(cor_labRow,c('TRUE','FALSE')))
  labCol<-as.logical(match.arg(cor_labCol,c('TRUE','FALSE')))

  labRow<-if(isTRUE(labRow)){
    labRow<-NULL
  } else{
    labRow<-NA
  }
  labCol<-if(isTRUE(labCol)){
    labCol<-NULL
  } else{
    labCol<-NA
  }
  cellnote<-as.logical(match.arg(cor_cellnote,c('TRUE','FALSE')))

  if(isTRUE(cellnote)){
    cellnote<-round(cordata,2)
  } else{
    cellnote<-matrix(rep("",length(cordata)), nrow(cordata), ncol(cordata))
  }


  #x11()


  ncex=cor_notecex*20/nrow(cordata)
  heatmap.2(cordata,
            Rowv=Rowv,
            Colv=Colv,
            na.rm=na.rm,
            revC=revC,
            dendrogram = dend,
            col=newcolhabs[[cor_palette]],
            labRow=labRow,
            labCol=labCol,
            sepcolor=cor_sepcolor,
            sepwidth=sepwidth,
            cellnote=cellnote,
            notecex=ncex,
            notecol=cor_noteco,
            na.color=cor_na.color,trace='none',
            density.info=cor_density.info,
            key.title = "Correlation",
            denscol = "black",
            linecol = "black")

}

i_alone_scores<-function(newdata,obc,reps,modelist,weis,accu, scale,en_method,top.features, root=NULL, inter_method,progress=T, ms=NULL,type="modelist", feaimp=F, error=F){
  m<-modelist[[1]]
  score<-ifelse(m$modelType=="Classification","Accuracy",'Rsquared')
  message<-if(inter_method=="Paired"){
    "Calculating Root..."
  } else{
    "Calculating permutated importance..."
  }


  tops<-gettop(modelist,scale,en_method,weis,top.features)
  if(!is.null(root)){
    pic<- which(colnames(newdata)%in%root)
  }else {
    pic<-which(colnames(newdata)%in%tops)}
  cols_run<-colnames(newdata)[pic]
  if(isTRUE(progress)){
    session=getDefaultReactiveDomain()
  } else{
    session<-MockShinySession$new()
  }
  j=1
  nmax<-reps*length(cols_run)
  withProgress(min=0, max=nmax,message=message,session=session,{
    performaces0<-lapply(1:length(cols_run),function(i){
      re<-lapply(1:reps, i=i,function (j,i){
        temp<-newdata
        cols<-as.character(cols_run[i])
        temp[,cols]<-sample(newdata[,cols])
        pred<-if(type=="modelist"){
          getpreds(modelist,temp,weis)
        }else{predict(ms,temp)}
        res<-postResample(pred,obc)


        cat("\n Calculating scoreloss;", paste0("rep",j,"/var:",cols))
        incProgress(1, session=session, message=paste0("rep_",j,"; var_",cols))
        res[score]
      })

      reee<-matrix(unlist(re),nrow=1, dimnames = list(cols_run[i],paste0("Score",1:reps)))
      reee
    })
    perfms0<-data.frame(do.call(rbind,performaces0))
  })
  perfms0
}
i_both_scores<-function(newdata,obc,reps,modelist,weis,accu,scale=T,method, top.features=10, root=NULL, inter_method=c("Paired","Forward","Backward","Tops"),type="modelist", progress=T){
  m<-modelist[[1]]

  parvar<-getparvar(inter_method,newdata,modelist,top.features,scale,weis,root)
  perfms<-i_loop_both(parvar,reps,newdata,modelist,weis,obc, type, ms=ms, progress=progress)
  perfms
}
i_loop_both<-function(parvar,reps,newdata,modelist,weis,obc, type="modelist", ms=NULL,progress=T){

  score<-ifelse(modelist[[1]]$modelType=="Classification","Accuracy",'Rsquared')

  nloop<-if(is.data.frame(parvar)){
    nrow(parvar) } else{length(parvar)}
  parloop<-if(is.data.frame(parvar)){
    as.list(data.frame(t(parvar)))
  } else{
    parloop<-parvar
  }
  nsample<-nrow(newdata)
  if(isTRUE(progress)){
    session=getDefaultReactiveDomain()
  } else{
    session<-MockShinySession$new()
  }
  withProgress(min=0, max=length(parloop)*reps,message="Running....",session=session,{
    i<-j<-1

    performaces<- lapply(1:length(parloop), function(i){
      re<-lapply(1:reps,i=i,function(j,i){

        temp<-newdata
        cols<-as.character(parloop[[i]])
        cat(paste0("\n Rep", j, "; Var_",paste0(cols,collapse = "+")))
        temp[cols]<-lapply(as.list(newdata[,cols, drop=F]),sample)
        pred<-if(type=="modelist"){
          getpreds(modelist,temp,weis)
        }else{predict(ms,temp)}
        res<-postResample(pred,obc)
        incProgress(1, message=paste0("inter_",i), session=session)
        res[score]
      })
      reee<-data.frame(Var1=parloop[[i]][[1]],Var2=parloop[[i]][[2]],matrix(c(unlist(re)),nrow=1, dimnames = list(paste0(parloop[[i]],collapse="::"),c(paste0("Score",1:reps)))))
      reee
    })

  })
  perfms<-data.frame(do.call(rbind,performaces))
  perfms
}
interact_ml<-function(newdata,predtab,weis,obc,pred,reps,modelist,sig=0.05,top.features=10, scale=T, en_method="weighted", root=NULL, weitype, inter_method=c("Paired","Forward","Backward"),progress=T){
  inter_method=match.arg(inter_method,c("Paired","Forward","Backward"))

  m<-modelist[[1]]
  predtrain<-pred
  accu<-postResample(predtrain,obc)
  accu<-if(m$modelType=="Classification"){accu["Accuracy"]
  } else{accu['Rsquared']}
  tops<-gettop(modelist,scale,method,weis,top.features)
  perfms0<-i_alone_scores(newdata,obc,reps,modelist,weis,accu, scale,en_method,top.features, root=tops,inter_method=inter_method,progress)
  inters0<-i_both_scores(newdata,obc,reps,modelist,weis,accu,scale,en_method, top.features, root=root,inter_method=inter_method, type="modelist", progress)

  i_scores=list(score_alone=perfms0,score_both= inters0, accu=accu)
  i_scores
}

i_importance<-function(i_scores){
  accu<-i_scores$accu
  score_alone=i_scores$score_alone
  score_both=i_scores$score_both[,-c(1:2)]
  score_vars=i_scores$score_both[c(1:2)]
  df<-do.call(rbind,lapply(1:nrow(score_vars),function(i){
    v1<-score_vars[i,"Var1"]
    v2<-score_vars[i,"Var2"]
    imp_i<-accu-score_alone[v1,]
    imp_j<-accu-score_alone[v2,]
    imp_ij<- accu-score_both[i,]
    ai=accu-imp_i-imp_ij
    aj=accu-imp_j-imp_ij
    aij = accu - imp_i - imp_j - imp_ij
    imp_ij = accu - ai - aj + aij
    imp_ij
  }))
  df<-data.frame(df)
  rownames(df)<-rownames(score_vars)
  df
}
i_t.test<-function(i_scores,i_result, sig){
  accu<-i_scores$accu
  score_alone=i_scores$score_alone
  score_both=i_scores$score_both[,-c(1:2)]
  score_vars=i_scores$score_both[c(1:2)]
  i_result5<-data.frame(do.call(rbind,lapply(1:nrow(score_vars),function(i){
    v1<-score_vars[i,"Var1"]
    v2<-score_vars[i,"Var2"]
    imp_i<-accu-score_alone[v1,]
    imp_j<-accu-score_alone[v2,]
    imp_ij<-i_result[i,]
    i_result3<-data.frame(t(rbind(imp_i,imp_j,imp_ij)))
    i_result4<-reshape2::melt(data.frame(id=rownames(i_result3),i_result3),'id')
    res<-pairwise.t.test(i_result4$value,i_result4$variable, alternative="greater", paired=T)
    #res$p.value[2,]<sig
    siggg<-sum(res$p.value[2,]<sig)==2
    re<-data.frame(res$p.value[2,1],as.logical(siggg))
    colnames(re)<-c("p_value","Sig_both")
    re
  })))

  rownames(i_result5)<-rownames(score_vars)
  i_result5
}


mergedatacol<-function(datalist,rm_dup=T){
  {

    to_merge<-datalist

    to_merge_fac<-lapply(datalist,function(x) attr(x,"factors"))
    mx <- which.max(do.call(c,lapply(to_merge,nrow)))
    newmerge<-data.frame(id=rownames(to_merge[[mx]]))
    rownames(newmerge)<-newmerge$id
    l1<-unlist(lapply(to_merge,function(x){
      x[rownames(newmerge),, drop=F]
    }),
    recursive = F)
    newdata<-data.frame(l1)

    rownames(newdata)<-rownames(to_merge[[mx]])
    colnames(newdata)<-c(do.call(c,lapply(to_merge,colnames)))

    if(isTRUE(rm_dup)) {
      if(any(duplicated(colnames(newdata)))){
        dup<-which(duplicated(colnames(newdata)))
        keep<-which.max(do.call(c,lapply(lapply(newdata[dup],na.omit),length)))
        fall<-dup[-keep]
        newdata<-newdata[colnames(newdata)[-fall]]
      }
    }



    mxfac <- which.max(do.call(c,lapply(to_merge_fac,nrow)))
    newmerge_fac<-data.frame(id=rownames(to_merge_fac[[mxfac]]))
    rownames(newmerge_fac)<-newmerge_fac$id
    l2<-unlist(lapply(to_merge_fac,function(x){
      x[rownames(newmerge_fac),, drop=F]
    }),
    recursive = F)
    newfac<-data.frame(l2)

    rownames(newfac)<-rownames(to_merge_fac[[mx]])
    colnames(newfac)<-c(do.call(c,lapply(to_merge_fac,colnames)))
    if(isTRUE(rm_dup)){
      if(any(duplicated(colnames(newfac)))){
        dup<-which(duplicated(colnames(newfac)))
        keep<-which.max(do.call(c,lapply(lapply(newfac[dup],na.omit),length)))
        fall<-dup[-keep]
        newfac<-newfac[colnames(newfac)[-fall]]
      }
    }

    newdata<-data_migrate(to_merge[[mx]],newdata,"")
    attr(newdata, "transf")=NULL
    attr(newdata,"factors")<-newfac[rownames(newdata),]
    newdata
  }
}
getcol_missing<-function(data){
  res0<-res<-which(is.na(data), arr.ind=TRUE)

  for(i in 1:nrow(res)){
    res0[i,1]<-rownames(data)[res[i,1]]
    res0[i,2]<-colnames(data)[res[i,2]]
  }
  colnames(res0)<-c("ID","Variable")
  rownames(res0)<-NULL
  res<-data.frame( table(res0[,2]))
  colnames(res)<-c("Variable","Missing")
  rownames(res)<-res[,1]
  res
}


getrow_missing<-function(data){
  res0<-res<-which(is.na(data), arr.ind=TRUE)

  for(i in 1:ncol(res)){
    res0[i,1]<-rownames(data)[res[i,1]]
    res0[i,2]<-colnames(data)[res[i,2]]
  }
  colnames(res0)<-c("ID","Variable")

  res<-data.frame( table(rownames(res0)))
  colnames(res)<-c("Variable","Missing")
  rownames(res)<-res[,1]
  res
}



getroot<-function(newdata,obc,reps,modelist,weis,accu, scale,en_method,top.features, root=NULL, inter_method,progress=T, ms=NULL,type="modelist", feaimp=F, error=F){

  m<-modelist[[1]]
  score<-ifelse(m$modelType=="Classification","Accuracy",'Rsquared')
  message<-if(inter_method=="Paired"){
    "Calculating Root..."
  } else{
    "Calculating permutated importance..."
  }


  tops<-gettop(modelist,scale,en_method,weis,top.features)
  if(!is.null(root)){
    pic<- which(colnames(newdata)%in%root)
  }else {
    pic<-which(colnames(newdata)%in%tops)}
  cols_run<-colnames(newdata)[pic]
  if(isTRUE(progress)){
    session=getDefaultReactiveDomain()
  } else{
    session<-MockShinySession$new()
  }
  nmax<-reps*length(cols_run)
  withProgress(min=0, max=nmax,message=message,session=session,{
    performaces0<-lapply(1:reps, function(j){
      testes<-list()
      for(i in 1:length(cols_run)) {
        temp<-newdata
        cols<-as.character(cols_run[i])
        temp[,cols]<-sample(newdata[,cols])
        pred<-if(type=="modelist"){
          getpreds(modelist,temp,weis)
        }else{predict(ms,temp)}
        if(isFALSE(feaimp)){
        res<-postResample(pred,obc)
        testes[[i]]<-res[score]
        }
        cat("\n Calculating scoreloss;", paste0("rep",j,"/var:",cols))
        incProgress(1, session=session, message=paste0("rep_",j,"; var_",cols))
        names(testes)[i]<-cols[i]
      }

      do.call(c,testes)
    } )

    perfms0<-data.frame(do.call(cbind,performaces0))
    colnames(perfms0)<-paste0("score_loss", 1:ncol(perfms0))
    rownames(perfms0)<-cols_run

  })


  if(isTRUE(error)){
    e<-1-accu
    p<-1-perfms0
    perfms0<-p-e
  } else{
    perfms0<-accu-perfms0
    perfms0
  }




}
getinter<-function(newdata,obc,reps,modelist,weis,accu,scale=T,method, top.features=10, root=NULL, inter_method=c("Paired","Forward","Backward","Tops"),type="modelist", progress=T){
  m<-modelist[[1]]

  parvar<-getparvar(inter_method,newdata,modelist,top.features,scale,weis,root)
  perfms<-get_perfloop(parvar,reps,newdata,modelist,weis,obc, type, ms=ms, progress=progress)
  seeds=attr(perfms,"seeds")


  if(is.data.frame(parvar)){
    inters<-data.frame(parvar,accu-perfms)
    rownames(inters)<-apply(inters[,1:2],1,function(x) paste(x,collapse = "::"))
  }else{
    if(length(parvar[[2]])>1){
      Var1<-do.call(c,lapply(parvar,function(x){paste(x[length(x)]) }))
      inters<-data.frame(expand.grid(root,Var1),accu-perfms)
      rownames(inters)<-paste0("...+",inters[,2])
    } else{
      Var2<-do.call(c,lapply(parvar,function(x){paste(x[length(x)]) }))
      inters<-data.frame(Var1=attr(parvar,"varnames"),Var2,accu-perfms)

      rownames(inters)<-inters[,2]
    }
  }
  attr(inters,"seeds")<-seeds
  inters
}


get_Acculoss<-function(newdata,modelist,scale,obc,weis,root,accu,sig,reps,type="modelist", ms=NULL,top.features=10){
  parvar<-getparvar("Tops",newdata,modelist,top.features=top.features,scale=scale,weis,root=root)
  acc<-get_perfloop(parvar,reps,newdata,modelist,weis, obc,type=type,ms=ms)
  acc2<-acc
  pics<-which(acc2>accu,T)
  acc2[pics]<-accu
  accures<-accu-acc2
  accumean<-apply(accures,1,mean)


  df<-reshape2::melt(data.frame(id=rownames(t(accures)),t(accures)), id="id")
  res<-pairwise.t.test(df$value,df$variable, alternative="less",paired=F)
  pic<-which(as.vector(apply(res$p.value,1,function(x) x<=sig))==T)
  res<-data.frame(res$p.value)
  sigs<-which(res<=sig, arr.ind = T)
  reds<-data.frame(
    pvalue=res[sigs],
    Var1=rownames(res)[sigs[,1]],
    Var2=colnames(res)[sigs[,2]]
  )
  sigvars<-unique(reds[,3])
  attr(accumean,"sigvars")<-sigvars

  accumean

}
get_Acculoss<-function(newdata,modelist,scale,obc,weis,root,accu,reps,type="modelist", ms=NULL,top.features=10){
  parvar<-getparvar("Tops",newdata,modelist,top.features=top.features,scale=scale,weis,root=root)
  acc<-get_perfloop(parvar,reps,newdata,modelist,weis, obc,type=type,ms=ms)
  acc}

getsig_aculoss_old<-function(acc, accu=NULL,sig){
  acc2<-acc

  if(is.null(accu)){
    accures<-acc2
  } else {
    pics<-which(acc2>accu,T)
    acc2[pics]<-accu
    accures<-accu-acc2
  }
  accumean<-apply(accures,1,mean)


  df<-reshape2::melt(data.frame(id=rownames(t(accures)),t(accures)), id="id")
  res<-pairwise.t.test(df$value,df$variable, alternative="greater",paired=F)
  pic<-which(as.vector(apply(res$p.value,1,function(x) x<=sig))==T)
  res<-data.frame(res$p.value)
  sigs<-which(res<=sig, arr.ind = T)
  reds<-data.frame(
    pvalue=res[sigs],
    Var1=rownames(res)[sigs[,1]],
    Var2=colnames(res)[sigs[,2]]
  )
  sigvars<-unique(reds[,3])
  attr(accumean,"sigvars")<-sigvars

  accumean
}
getsig_aculoss<-function(acc, accu=NULL,sig,test="greater"){
  accures<-acc
  acc<-data.frame(acc)
  accumean<-apply(accures,1,mean)
  i=20
  ps<-lapply(1:nrow(acc),function(i){
    rownames(acc)
    remaining<-acc[-i,]
    mean(unlist(acc[i,]))
    mean(unlist(remaining))
    re<-t.test(unlist(acc[i,]),unlist(remaining), alternative=test, paired=F)
    re
  })
  st<-unlist(lapply(ps,function(x) x$statistic))
  ps<-unlist(lapply(ps,function(x) x$p.value))
  sigvars<-rownames(acc)[which(ps<=sig)]

  attr(accumean,"sigvars")<-sigvars

  accumean
}

getsigs2_loss<-function(inter_res0,sig=0.05){
  inters=inter_res0$inters
  perfms0=inter_res0$perfms0
  root=perfms0[inters[,1],]
  root_inter<-list(loss_root=root,loss_inter=inters[,-c(1:2)])
  rownames(root_inter$loss_inter)<-apply(inters[,1:2],1,function(x) paste(x,collapse=':'))
  ro<-root_inter[[2]]
  data=reshape2::melt(data.frame(id=rownames(ro),ro), id="id")
  res<-lm(value~as.factor(id),data=data)
  res<-pairwise.t.test(data$value,data$id,  p.adjust.method="bonferroni",alternative="greater")
  pic<-which(as.vector(apply(res$p.value,1,function(x) x<=0.05))==T)
  res<-data.frame(res$p.value)
  sigs<-which(res<=sig, arr.ind = T)
  reds<-data.frame(
    pvalue=res[sigs],
    Var1=rownames(res)[sigs[,1]],
    Var2=colnames(res)[sigs[,2]]
  )
  reds
}
getReg_wei<-function(modelist,newdata,obc, en_method,weitype){
#req(is.numeric(obc))
  weis<-rep(1,length(modelist))
  if(en_method%in%c("non-weighted")){
    return(weis)
  }
  m<-modelist[[1]]
  class=which(colnames(m$trainingData)==".outcome")
  traindata<-data.frame(m$trainingData[-class])
  #req(sum(colnames(newdata)%in%colnames(colnames(m$trainingData)[-class])==ncol(newdata)))
  #req(sum(names(obc)%in%rownames(newdata))==length(obc))

  if(en_method=="accu_weighted") {
    if(weitype=="training"){
      weis<-do.call(rbind,lapply(modelist,function(m){
        postResample(m$pred$pred,m$pred$obs)
      }))
      weis<-if(m$modelType=="Classification"){weis[,"Accuracy"]
      } else{weis[,'Rsquared']}
    }
    if(weitype=="test") {
      newdata<-newdata[names(obc),colnames(m$trainingData)[-class]]
      req(nrow(newdata)==length(obc))

      weis<-unlist(do.call(data.frame,lapply(modelist,function(m){
        pred<-suppressWarnings(predict(m,newdata))
        req(length(pred)==length(obc))
        res<-postResample(pred,obc)
        res<-if(m$modelType=="Classification"){res["Accuracy"]
        } else{res['Rsquared']}
        res
      }
      )))
    }
    weis<-weis/sum(weis)
    weis
    #weis<-getAccu_wei(predtab,obc)

  }
}



getClass_wei<-function(predtab,obc,modelist, en_method, weitype="test",newdata){
 #req(is.factor(obc))
  req(length(predtab)>0)
  req(ncol(predtab)>1)
  req(length(weitype)==1)

  weis<-rep(1,ncol(predtab))
  if(en_method%in%c("non-weighted")){
    names(weis)<-names(modelist)
    return(weis)
  }
  m<-modelist[[1]]
  class=which(colnames(m$trainingData)==".outcome")
  traindata<-data.frame(m$trainingData[-class])
  if(weitype=="training"){
    newdata<-traindata
    predtab<-predcomb_table(modelist,newdata)
    obc_train<-m$trainingData[[class]]
    names(obc_train)<-rownames(traindata)
    obc<-obc_train

  }
  newdata<-newdata[,colnames(m$trainingData)[-class]]
  tab1<-apply(predtab, 2,function(x){
    as.numeric(x==obc)
  })
  if(en_method=="weighted") {
    for(i in 1:nrow(tab1)){
      for(j in 1:length(weis)){
        ai=sum(tab1[i,]==0)/ncol(tab1)
        weis[j]<-if(tab1[i,j]==1){weis[j]+ai} else {weis[j]}
      }
    }
    names(weis)<-names(modelist)
    return(weis)
  }

  if(en_method=="accu_weighted") {
    if(weitype=="training"){
      weis<-do.call(rbind,lapply(modelist,function(m){
        postResample(m$pred$pred,m$pred$obs)
      }))
      weis<-if(m$modelType=="Classification"){weis[,"Accuracy"]
      } else{weis[,'Rsquared']}
      weis<-weis/sum(weis)
      names(weis)<-names(modelist)
      return(weis)
    }
    if(weitype=="test") {
      weis<-unlist(do.call(data.frame,lapply(modelist,function(m){
        pred<-suppressWarnings(predict(m,newdata=newdata))
        res<-postResample(pred,obc)
        res<-if(m$modelType=="Classification"){res["Accuracy"]
        } else{res['Rsquared']}
        res
      }
      )))
      weis<-weis/sum(weis)
      names(weis)<-names(modelist)
      return(weis)
    }
    #weis<-getAccu_wei(predtab,obc)

  }


  weis
}
getAccu_wei<-function(predtab,obc){
  model_perfomaces<-data.frame(t(apply(predtab,2,function(i){
    postResample(i,obc)
  })))
  weis<-model_perfomaces$Accuracy
  names(weis)<-colnames(predtab)
  weis
}



getweis<-function(modelist,predtab, obc, en_method, weitype,newdata){
  if(modelist[[1]]$modelType=="Classification") {
    weis<-getClass_wei(predtab,obc,modelist, en_method, weitype,newdata)}else {
      weis<-getReg_wei(modelist,newdata,obc, en_method,weitype)}
}



getinteraction<-function(newdata,predtab,weis,obc,pred,reps,modelist,sig=0.05,top.features=10, scale=T, en_method="weighted", root=NULL, weitype, inter_method=c("Paired","Forward","Backward")){
  inter_method=match.arg(inter_method,c("Paired","Forward","Backward"))

  m<-modelist[[1]]
  predtrain<-pred
  accu<-postResample(predtrain,obc)
  accu<-if(m$modelType=="Classification"){accu["Accuracy"]
  } else{accu['Rsquared']}
  perfms0<-getroot(newdata,obc,reps,modelist,weis,accu, scale,en_method,top.features, root=root,inter_method=inter_method,progress)
  inters0<-getinter(newdata,obc,reps,modelist,weis,accu,scale,en_method, top.features, root=root,inter_method=inter_method, type="modelist", progress=F)
  inters=list(perfms0=perfms0,inters=inters0)
  inters
}



get_ensemble_pred<-function(predtab,modelist, en_method=c("weighted","non-weighted"), obc=NULL, weitype,newdata,weis){


  type<-modelist[[1]]$modelType
  if(type=="Regression"){
    if(en_method=="non-weighted"){
      get_mavoting(predtab,modelist)
    } else{
      get_weimean(predtab,weis)
    }

  } else{
    if(en_method=="non-weighted"){
      get_mavoting(predtab,modelist)
    } else{
      get_weivoting(predtab,weis,modelist)
    }
  }

}

get_acculoss_comb<-function(modelist,predtab, newdata,obc,reps=5,sig=0.01,en_method=c("weighted","non-weighted"),weitype){
  m<-modelist[[1]]
  acculist<-list()
  # get the table of predictions


  if(m$modelType=="Classification"){
    weis<-if(en_method=="non-weighted"){
      rep(1,ncol(predtab))

    } else { getClass_wei(predtab,obc,modelist, en_method,weitype,newdata)}
    newpredictions<-get_weivoting(predtab,weis,modelist)
  } else {
    weis<-if(en_method=="non-weighted"){
      rep(1,ncol(predtab))

    } else{getReg_wei(modelist,newdata,obc, en_method,weitype)}
    newpredictions<-get_weimean(predtab,weis)
  }


  class=which(colnames(m$trainingData)==".outcome")
  X<-data.frame(m$trainingData[-class])
  Y<-m$trainingData[,class]
  accu=1
  kmax<-ncol(X)*reps
  predtrain<-newpredictions
  accu<-postResample(predtrain,obc)
  withProgress(min=0, max=ncol(X)*reps,message="Running...",{
    init<-1
    metric_loss<-for(i in 1:ncol(X)) {

    accus<-lapply(1:reps, function(...){
      temp<-newdata

      temp[,i]<-sample(temp[,i])
      predtab_i<-predcomb_table2(modelist,newdata=temp)
      if(m$modelType=="Classification"){
        pred<-get_weivoting(predtab_i,weis,modelist)} else{
          pred<-get_weimean(predtab_i,weis)
        }
      res<-accu-caret::postResample(pred,obc)
      incProgress(1)
      init<-init+1
      cat("\n",init,"/",kmax)
      #cat("\n",ii+i,"/",kmax)
      res
    })

    res_temp<- do.call(rbind,accus )
    accu_temp<-if(m$modelType=="Classification"){res_temp[,"Accuracy"]
    } else{res_temp[,'Rsquared']}
    acculist[[i]]<-accu_temp
    incProgress(1)
  }
  })


  names(acculist)<-colnames(newdata)
  accu_loss<-apply(do.call(rbind,acculist),1,function(i)  mean(i))
  # accu_loss[which(accu_loss>=sig)]
  accu_loss
}


get_wei_percetages<-function(predtab,
                             modelist,
                             obc,
                             en_method, weitype, newdata){

  weis<-getweis(modelist,predtab, obc, en_method, weitype,newdata)
  res_forest<-get_weivoting_models(predtab,weis,modelist)
  res_forest
}



get_options_show<-function(res_forest, sortby,obc,  nhist_tree,data_factors=NULL){


  ord<-get_qclass_ord(res_forest,obc, sortby, data_factors)
  data=t(res_forest[ord,])
  d=1:ncol(data)
  res<-split(d, ceiling(seq_along(d)/nhist_tree))
  options_num<-lapply(res,function (x) range(x))
  options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
  list(options_num=options_num,options_show=options_show)
}

get_qclass_ord<-function(res_forest,obc, sortby, data_factors=NULL){
  if(is.factor(obc)){
    result<-get_wei_errors_class(res_forest,obc)
    res1<-"Accuracy"
    res2<-"Error"
  } else{
    result<-data.frame(res_forest)
    res1="RMSE"
    res2="MAE"
  }
  if(sortby%in%c("Accuracy","Error")) {
    ord<-switch (sortby,
                 "Accuracy" =order(result[[res1]], decreasing = T),
                 "Error"=order(result[[res2]], decreasing = T))
  } else {
    sortby<-data_factors[rownames(result),sortby, drop=F]
    df<-cbind(result,sortby)
    ord<-order(df[,ncol(df)])
  }
  ord<-rownames(result)[ord]
}


getshow_qclass<-function(res_forest, sortby,predtab,obc, splitdata_trees,nhist_tree,data_factors){
  res<-get_options_show(res_forest, sortby,obc,  nhist_tree,data_factors)
  options_num<-res$options_num
  options_show<-res$options_show
  options_num_sel<-options_num[[which(options_show==splitdata_trees)]]

  options_num_sel
}






get_obs_erros_ensemble_class<-function(resforest,predtab,modelist,obc,pred, round, en_method){
  if(en_method=="non-weighted"){

    res<-getobs_errors_class(predtab,obc)
  } else{  res<-get_wei_errors_class(resforest,obc)*100}
  res<-data.frame(res)
  res<-round(res,round)
  res$obs<-unlist(obc)

  res$pred<-unlist(pred)
  res
}
get_wei_errors_class<-function(res_forest,obc){
  df0<-df<-res_forest
  df$obs<-obc
  Result<-data.frame(Accuracy=apply(df,1,function(x) {
    as.numeric(x[which(names(x)[-length(x)]==x[length(x)])])
  }))
  Result$Error<-1-Result$Accuracy
  Result

}




get_weivoting_models<-function(predtab,weis,modelist){

  predtab<-data.frame(predtab)
  colnames(predtab)<-names(modelist)
  levels_obc<-modelist[[1]]$levels
  names(weis)<-names(modelist)
  inst<-9
  lev<-levels_obc[1]
  res<-do.call(rbind,
               lapply(1:nrow(predtab),function(inst){
                 i<-predtab[inst,, drop=F]
                 win<-unlist(
                   lapply(levels_obc,function(lev){
                     pic<-i[which(i==lev)]
                     weis
                     sum(weis[names(pic)])
                   })
                 )


               })
  )
  result<-data.frame(res)
  rownames(result)<-rownames(predtab)
  colnames(result)<-levels_obc
  result<-decostand(result,"total")
  result

}

plotforest_qclass<-function(
    result_forest,pred_res, palette="turbo", newcolhabs,ylab="Observations",xlab="Percentage of classifications", leg="Model predictions" ,leg2="Final prediction",cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13,textcolor="white", main="", obc,leg3="Correct observation",size_points=3){
  df0<-df<-result_forest
  pred=pred_res
  obs<-obc



  df<-data.frame(t(apply(df,1,function(x) x/sum(x))))
  colnames(df)<-colnames(df0)
  df$id=rownames(df)
  df$final_pred<-pred[rownames(df),]
  df$order<-1:nrow(df)
  df$obs<-obs[rownames(df)]
#  df$final_pred<-as.factor(sample(c(2,3,4),6,replace = T))

  df3<-reshape2::melt(df,id=c('id','order','obs',"final_pred"))
  colnames(df3)<-c("id",'order','obs',"final_pred","pred","value")
  a<-df3[,c("id","obs","final_pred")]
  df4<-reshape2::melt(a,id="id")

  df5<-df4<-df3
  df4$y<-1.1
  df4$val<-df4$final_pred
  df5$y<-1.2
  df5$val<-df5$obs
  df5$fac<-"observed"
  df4$fac<-"predicted"

  df6<-rbind(df4,df5)

  col<-getcolhabs(newcolhabs,palette, nlevels(pred[,1]))
  p<-ggplot(df3, aes(x=reorder(id,order), y=value, fill=pred)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=ifelse(value >= 0.07, paste(round(value*100,2),"%"),""),y=value),position=position_stack(vjust=0.5), colour=getcolhabs(newcolhabs,textcolor,1)) +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(y="", x="")+scale_fill_manual(name=leg,values=col)



  col2<-getcolhabs(newcolhabs,palette, nlevels(pred[,1]))[which(levels(pred[,1])%in%c(df4$val) )]


  p<-p+geom_point(aes(x=id, y=y, col=val, shape=fac),data=df6,size=size_points)+scale_color_manual(name=leg2,values=col2)+guides(

      fill = guide_legend(override.aes = list(shape = NA))
    )+scale_shape_manual(name="",values = c(16,15))



  p<-p+ylab(xlab)+
    xlab(ylab)+theme(
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main),
      strip.text.x=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg))



  p<-p+ggtitle(main)
  p
}


get_accu<-function(modelist,newpredictions,obc){
  m<-modelist[[1]]
  accu<-postResample(unlist(newpredictions),obc)
  accu<-if(m$modelType=="Classification"){accu["Accuracy"]
  } else{accu['Rsquared']}

}
getpreds<-function(modelist,newdata,weis){
  m<-modelist[[1]]
  predtab_i<-predcomb_table2(modelist,newdata)
  if(m$modelType=="Classification"){
    pred<-get_weivoting(predtab_i,weis,modelist)} else{
      pred<-get_weimean(predtab_i,weis)
    }
  pred[,1]<-factor(pred[,1],levels=m$levels)
pred
  }

comb_feaimp<-function(modelist,  scale=T, progress=T){
  m<-modelist[[1]]
  if(isTRUE(progress)){
    withProgress(min=0, max=length(modelist),message="Running...",{
      imptable<-lapply(modelist,function(m){

        if(m$modelType=="Classification"){
          imp<-varImp(m,scale)
          imp2<-apply(imp$importance,1,mean)
          if(isTRUE(scale)){imp2<-scales::rescale(imp2,c(0,100))
          }
          incProgress(1)
          imp2
        } else{
          imp<-varImp(m,scale)
          imp2<-imp$importance
          incProgress(1)
          imp2
        }
      })
    })
  } else{

    imptable<-lapply(modelist,function(m){

      if(m$modelType=="Classification"){
        imp<-varImp(m,scale)
        imp2<-apply(imp$importance,1,mean)
        if(isTRUE(scale)){imp2<-scales::rescale(imp2,c(0,100))
        }

        imp2
      } else{
        imp<-varImp(m,scale)
        imp2<-imp$importance

        imp2
      }
    })

  }



  res<-do.call(cbind,imptable)
  colnames(res)<-names(modelist)
  res

}


getparvar<-function(inter_method,newdata,modelist,top.features,scale,weis, root){
  tops<-gettop(modelist,scale,method,weis,top.features)
  inter_method=match.arg(inter_method,c("Paired","Forward","Backward","Tops"))


  ###

  pic<-if(!is.null(root)){which(colnames(newdata)%in%root)
  } else{which(colnames(newdata)%in%tops)}
  if(inter_method=="Paired"){
    parvar<-expand.grid( colnames(newdata)[pic] ,tops, stringsAsFactors = F)
    parvar<-data.frame(parvar[-which(apply(apply(parvar,1,duplicated),2,sum)==1),])
    attr(parvar,"varnames")<-apply(parvar,1,function(x)paste0(x,collapse="::"))

    }
  if(inter_method=="Forward"){
    parvar<-list()
    n<-1
    for(i in 1:(length(tops)-1)){
      parvar[[i]]<-tops[n]
      n<-c(n,length(n)+1)
    }
    attr(parvar,"varnames")<-unlist(lapply(parvar,function(x)paste0("...+",x[length(x)])))
  }
  if(inter_method=="Backward"){
    parvar<-list()
    n<-1
    for(i in (length(tops)-1):1){
      parvar[[i]]<-tops[n]
      n<-c(n,length(n)+1)
    }

    attr(parvar,"varnames")<-unlist(lapply(parvar,function(x)paste0("...-",x[length(x)])))
  }
  if(inter_method=="Tops"){
    parvar<-as.list(colnames(newdata))
    attr(parvar,"varnames")<-unlist(parvar)
  }
  return(parvar)
}

get_perfloop<-function(parvar,reps,newdata,modelist,weis,obc, type="modelist", ms=NULL,progress=T){

  score<-ifelse(modelist[[1]]$modelType=="Classification","Accuracy",'Rsquared')


  nloop<-if(is.data.frame(parvar)){
    nrow(parvar) } else{length(parvar)}

  parloop<-if(is.data.frame(parvar)){
    as.list(data.frame(t(parvar)))
  } else{
    parloop<-parvar

  }

  nsample<-nrow(newdata)
  if(isTRUE(progress)){
    session=getDefaultReactiveDomain()
  } else{
    session<-MockShinySession$new()
  }
  withProgress(min=0, max=length(parloop)*reps,message="Running....",session=session,{
i<-j<-1
  testes<-vector('list',length(parloop))
  performaces<-vector('list',reps)
  for(j in 1:reps){
    a<-1
    for(i in parloop) {
      cat(paste0("\n Rep", j, "; Var_",paste0(i,collapse = "+")))

      temp<-newdata

      temp[i]<-lapply(as.list(newdata[,i, drop=F]),function(x) x[sample(1:nsample)])

      pred<-if(type=="modelist"){
        getpreds(modelist,temp,weis)
      }else{predict(ms,temp)}
      res<-postResample(pred,obc)
      testes[[a]]<-res[score]
      a<-a+1
     incProgress(1, message=paste0("inter_",i), session=session)
    }
    #

    res<-do.call(c,testes)

    performaces[[j]]<-res
  }
   })
  perfms<-data.frame(do.call(cbind,performaces))
  colnames(perfms)<-paste0("score_loss", 1:ncol(perfms))
  rownames(perfms)<-attr(parvar,"varnames")
  perfms
}




getsigs_loss2<-function(inter_res0,sig=0.05){
  accures<-rbind(inter_res0[[1]],inter_res0[[2]][,-c(1:2)])
  df<-reshape2::melt(data.frame(id=rownames(t(accures)),t(accures)), id="id")
  res<-pairwise.t.test(df$value,df$variable, alternative="less",paired=F)
  pic<-which(as.vector(apply(res$p.value,1,function(x) x<=sig))==T)
  res<-data.frame(res$p.value)
  sigs<-which(res<=sig, arr.ind = T)
  reds<-data.frame(
    pvalue=res[sigs],
    Var1=rownames(res)[sigs[,1]],
    Var2=colnames(res)[sigs[,2]]
  )
  testes<-res
  sigvars<-unique(reds[,3])
  statistic=unlist(lapply(testes,function(res){
    if(is.numeric(res)){"NULL"}else{
      res$statistic}
  }))

  list(sigs=sigvars,teste=data.frame(p.value,statistic))

}


getsigs_loss<-function(root_inter,reps,inters,sig=0.05, inter_method){


  testes<-lapply(1:nrow(root_inter[[2]]),function(i){
    if(inter_method=="Paired"){
      x=unlist(root_inter[[1]][i,])
      y=unlist(root_inter[[2]][i,])
      }

    if(inter_method=="Forward"){
      if(i+1<=nrow(root_inter[[2]])){
        x=unlist(root_inter[[2]][i,])
        y=unlist(root_inter[[2]][i+1,])
      } else{
        x=1
        y=1
      }
    }
    if(inter_method=="Backward"){
      if((nrow(root_inter[[2]])-i)!=0){
        x=unlist(root_inter[[2]][nrow(root_inter[[2]]),])
        y=unlist(root_inter[[2]][nrow(root_inter[[2]])-i,])
      } else{
        x=1
        y=1
      }
    }

    if(identical(x,y)|(mean(x)==0)+(mean(y)==0)==1){
      1
    } else{
      data=data.frame(x=c(x,y),
                      fac=c(rep('r',reps),rep('i',reps)))

      try({
        res<-t.test(x,y,data=data, alternative="two.sided",paired=T)
        res
      })
    }
  })


  p.value=unlist(lapply(testes,function(res){
    if(is.numeric(res)){res}else{
      res$p.value}
  }))
  statistic=unlist(lapply(testes,function(res){
    if(is.numeric(res)){"NULL"}else{
      res$statistic}
  }))

  names(p.value)<-rownames(root_inter[[2]])

  pic<-which(p.value<=0.01)

  res<-inters[pic,1:2]
  rownames(res)<-rownames(root_inter$mean_loss_both_vars)[pic]
  list(sigs=res,teste=data.frame(p.value,statistic))

}
gettop<-function(modelist,scale,en_method,weis,top.features=10) {
  allimportance<-comb_feaimp(modelist,  scale=scale, progress=F)
  mean_importance<-get_weig_faimp(allimportance, en_method="non-weighted", weis=weis)
  names(mean_importance)[order(mean_importance,decreasing=T)[1:top.features]]

}

getdiffs_inter<-function(inter_res){
  means<-do.call(data.frame,lapply(inter_res[1:2],function(x){
    apply(x,1,function(xx) mean(xx))
  }))
  rownames(means)<-rownames(inter_res$mean_loss_both_vars)
  diffs<-means[2]-means[1]
  sds<-do.call(data.frame,lapply(inter_res[1:2],function(x){
    sd=apply(x,1,function(xx) sd(xx))
    sd
  }))

  mean_loss_diff=means[2]-means[1]
  sd_loss_diff<-data.frame(mean_root=means[1],mean_inter=means[2],sd_loss_diff=apply(as.matrix(inter_res[[2]])-as.matrix(inter_res[[1]]),1,sd))
  res<-data.frame(mean_loss_diff=unlist(mean_loss_diff),sd_loss_diff)
  rownames(res)<-rownames(mean_loss_diff)
  res
}
#inter_res0<-vals$inter_res0


get_inter_results<-function(inter_res0, reps=5,sig=0.05,inter_method){
  inters=inter_res0$inters
  perfms0=inter_res0$perfms0
  root=perfms0[inters[,1],]
  root_inter<-list(mean_loss_Var1=root,mean_loss_both_vars=inters[,-c(1:2)])
  if(inter_method=="Paired"){
  rownames(root_inter$mean_loss_both_vars)<-apply(inters[,1:2],1,function(x) paste(x,collapse=':'))
  } else{

  }

  rownames(root_inter$mean_loss_both_vars)<-rownames(inters)
  teste<-getsigs_loss(root_inter,reps,inters,sig,inter_method)
  #teste<-getsigs_loss2(inter_res0,sig)
  root_inter$sig_inters<-teste$sigs
  root_inter$diff_to_root<-getdiffs_inter(root_inter)
  root_inter$diff_to_root$statistic<-teste$teste$statistic
  root_inter$diff_to_root$p.value<-teste$teste$p.value

  nsigs<-nrow(root_inter$sig_inters)
  root_inter$diff_to_root$sig<-""
  root_inter$diff_to_root[rownames(root_inter$sig_inters),"sig"]<-rep("*",nsigs)

  interaction_results<-data.frame(root_inter$diff_to_root)
  cols<-c('mean_loss_Var1','mean_loss_both_vars','mean_loss_diff','sd_loss_diff','statistic','p.value','sig')
  interaction_results<-interaction_results[cols]

  root_inter$diff_to_root<-interaction_results
  root_inter$mean_loss_Var1_unique<-perfms0
  root_inter
}
get_interplot<-function(inter_res,palette="turbo",newcolhabs,cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13, leg="", main="",xlab="Interactions",ylab="permutated importance (%)"){

  means<-do.call(data.frame,lapply(inter_res[1:2],function(x){
    apply(x,1,function(xx) mean(xx))
  }))
  rownames(means)<-rownames(inter_res$mean_loss_both_vars)
  diffs<-means[2]-means[1]
  sds<-do.call(data.frame,lapply(inter_res[1:2],function(x){
    sd=apply(x,1,function(xx) sd(xx))
    sd
  }))
  df_root<-data.frame(id=rownames(inter_res$mean_loss_both_vars),mean=means[,1],sd=sds[,1],fac="root")
  df_inter<-data.frame(id=rownames(inter_res$mean_loss_both_vars),mean=means[,2],sd=sds[,2],fac="inter")


  df3<-rbind(df_root,df_inter)
  pic<-rownames(inter_res$sig_inters)
  df3<-df3[which(df3[,1]%in%pic),]
  colnames(df3)[2]<-"value"

  p<-ggplot(df3, aes(x=reorder(id,value, mean,decreasing =F), y=value)) +
    geom_point(aes(y=value, color=fac)) +
    geom_errorbar(aes(ymin=value-sd, ymax=value+sd,color =fac), width=.2)
  p<-p+ scale_color_manual(name=leg,values=getcolhabs(newcolhabs,palette,2))+
    xlab(xlab)+
    ylab(ylab)


  p<-p+
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    coord_flip()+
    theme(
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg))

  p+ggtitle(main)

}
get_interplot2<-function(inter_res,palette="turbo",newcolhabs,cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13, leg="", main="",xlab="Interactions",ylab="Mean score gain (%)"){
  df<-inter_res$diff_to_root
  df$sig[df$sig==""]<-paste0("non-sig")
  df$sig[df$sig=="*"]<-paste0("sig")
  df$mean_loss_diff[df$mean_loss_diff<0]<-0
  df$mean_loss_diff<-df$mean_loss_diff
  df3<-data.frame(id=rownames(df),df)
  df3$fac=1
  p<-ggplot(df3, aes(x=reorder(id,mean_loss_diff , mean,decreasing =F), y=mean_loss_diff, fill=sig))+
    geom_bar(stat="identity",position=position_dodge())
    #geom_point(aes(y=mean_loss_diff , color=sig)) +
    #geom_errorbar(aes(ymin=mean_loss_diff, ymax=mean_loss_diff+sd_loss_diff   ,color =sig), width=.2)
 # p<-p+geom_hline(yintercept = df3$mean_loss_Var1)

  p<-p+ scale_fill_manual(name=leg,values=getcolhabs(newcolhabs,palette,2))+
    xlab(xlab)+
    ylab(ylab)


  p<-p+
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    coord_flip()+
    theme(
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg))

  p+ggtitle(main)
}



predcomb_table<-function(modelist,newdata, progress=T){
  if(isTRUE(progress)){
    session=getDefaultReactiveDomain()
  } else{
    session<-MockShinySession$new()
  }

  withProgress(min=0, max=length(modelist),message="Running...",session=session,{
    res_compre<-lapply(modelist,newdata=newdata,function(x,newdata){
      try({
        if(isTRUE(progress)){   incProgress(1)}

        trainingData<-x$trainingData
        trainingData<-trainingData[-which(colnames(trainingData)==".outcome")]
        res<-suppressWarnings(predict(x,newdata= newdata[which(colnames(newdata)%in%colnames(trainingData))]))
        res
      })
    })
  })

  pic<-which(unlist(lapply(res_compre,function(x)class(x)=="try-error")))
  if(length(pic)>0){
    res_compre<-res_compre[-pic]
  }
  res_compre<-data.frame(res_compre)
  rownames(res_compre)<-rownames(newdata)
  colnames(res_compre)<-names(modelist)
  attr(res_compre,"rownames")<-rownames(newdata)
  res_compre


}
predcomb_table2<-function(modelist,newdata){

  res_compre<-lapply(modelist,newdata=newdata,function(x,newdata){
    try({

      trainingData<-x$trainingData
      trainingData<-trainingData[-which(colnames(trainingData)==".outcome")]
      res<-suppressWarnings(predict(x,newdata= newdata[which(colnames(newdata)%in%colnames(trainingData))]))
      res
    })
  })


  pic<-which(unlist(lapply(res_compre,function(x)class(x)=="try-error")))
  if(length(pic)>0){
    res_compre<-res_compre[-pic]
  }
  res_compre<-data.frame(res_compre)
  rownames(res_compre)<-rownames(newdata)
  attr(res_compre,"rownames")<-rownames(newdata)
  res_compre


}



get_perf_by_model<-function(modelist){
  result<-lapply(modelist, function(x){
    res<-x$results[ rownames(x$bestTune),  ]
    if(x$modelType=="Classification"){
      res[,"Accuracy"]
    } else{
      res[,'Rsquared']
    }
  })
  result<-do.call(c,result)

  result
}
get_perfm_global<-function(modelist){


  validate(need(sum(duplicated(lapply(modelist,function(x){
    attr(x,"Datalist")
  }))==F)==1,"Incompatible training data across the models"))
  m<-modelist[[1]]
  trainingData<-m$trainingData
  trainingData<-trainingData[-which(colnames(trainingData)==".outcome")]
  predtab<-predcomb_table(modelist,newdata=trainingData)

  predvote<-getpredvote(modelist,predtab)
  perf<-postResample(predvote, obc)
  accu<-if("Accuracy"%in%names(perf)){perf['Accuracy']} else {perf['Rsquared']}
  accu
}
getpredvote<-function(modelist,predtab){
  obs_class<-do.call(data.frame,lapply(modelist,function(x){
    x$trainingData[".outcome"]
  }))
  validate(need(sum(duplicated(t(obs_class))==F )==1,"Incompatible Y across the models"))
  obc<-obs_class[,1]

  if(is.factor(obc)){
    predvote<-apply(predtab,1,function(x){
      tabx<-table(x)
      names(tabx)[  which.max(tabx)]
    })
    predvote<-factor(predvote, levels=levels(obc), labels=levels(obc))

  } else {
    predvote<-apply(predtab,1,function(x){
      res<-data.frame(apply(do.call(data.frame,predtab),1,function(x) mean(x)))

    })
  }
  predvote
}


get_weivoting<-function(predtab,weis,modelist){
levels_obc<-modelist[[1]]$levels
req(length(names(weis))>0)
names(weis)<-names(modelist)
colnames(predtab)<-names(modelist)
  res<-unlist(
    lapply(1:nrow(predtab),function(inst){
      i<-predtab[inst,, drop=F]
      win<-which.max(
        unlist(
          lapply(levels_obc,function(lev){
            pic<-i[which(i==lev)]
            sum(weis[names(pic)])
          })
        )
      )
      levels_obc[win]

    })
  )
  names(res)<-rownames(predtab)
  res<-data.frame(res)
  colnames(res)<-"Majority weighted votes"
  res

}
get_mavoting<-function(predtab,modelist){
  type=modelist[[1]]$modelType
  if(is.factor(predtab[[1]])){
    col_name<-c("Majority votes")
    res<-data.frame(as.factor(names(unlist(lapply(apply(do.call(data.frame,predtab),1,function(x) table(x)), function(x) which.max(x))))))

  }else{
    col_name<-c("Mean")
    res<-data.frame(apply(do.call(data.frame,predtab),1,function(x) mean(x)))
  }
  rownames(res)<-  rownames(predtab)
  colnames(res)<-col_name
  res
}

get_acculoss<-function(m, newdata,obc,reps=5,sig=0.01){
  class=which(colnames(m$trainingData)==".outcome")
  X<-data.frame(m$trainingData[-class])
  Y<-m$trainingData[,class]
  accu=1
  kmax<-ncol(X)*reps
  accu<-postResample(caret::predict.train(m,newdata=newdata),obc)
  acculist<-list()
  metric_loss<-for(i in 1:ncol(X)) {

    accus<-lapply(1:reps, function(...){
      temp<-newdata

      temp[,i]<-sample(temp[,i])
      pred<-caret::predict.train(m,newdata=temp)
      res<-accu-caret::postResample(pred,obc)
      res
      #init<-init+1
      #cat("\n",init,"/",kmax)
      #cat("\n",ii+i,"/",kmax)
      res
    })

    res_temp<- do.call(rbind,accus )
    accu_temp<-if(m$modelType=="Classification"){res_temp[,"Accuracy"]
    } else{res_temp[,'Rsquared']}
    acculist[[i]]<-accu_temp
  }
  names(acculist)<-colnames(newdata)
  accu_loss<-apply(do.call(rbind,acculist),1,mean)

  #accu_loss[which(accu_loss>=sig)]
  accu_loss
}



get_weimean<-function(predtab,weis){
  req(length(weis)==ncol(predtab))
  res<-data.frame(
    apply(predtab,1,function(x){
      weighted.mean(x, weis)

    })
  )
  colnames(res)<-"Weighted_mean"
  res
}
getobs_errors_class<-function(predtab,obc){

  res<-data.frame(do.call(rbind,lapply(1:length(obc),function(i){
    x<-factor(predtab[i,],levels=levels(obc))
    postResample(x,rep(obc[i],length(x)))[1]
  })))
  res$Error<-1-res[,1]
  rownames(res)<-names(obc)
  res*100
}

get_weig_faimp<-function(allimportance, en_method="weighted", weis=NULL){
  if(en_method=="non-weighted"){
    apply(allimportance,1,mean)
  } else{
    req(!is.null(weis))
    get_weimean(allimportance,weis)
  }

}


plot_model_features<-function(moldels_importance, palette="turbo", newcolhabs,ylab="Variables",xlab="Importance", colby=c("models",'acculoss',"acc"), aculoss=NULL,facet_grid=T,nvars=10, sig=0.01, leg=if(colby=="models") {'Model'}else{"Score decrease (%)"},cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13,sigvars=NULL,size.sig=1.5, acc=NULL,title=NULL,modelist=NULL,scale=T){
  if(is.null(aculoss)){
    accucopy<-T
    aculoss<-rep(1,nrow(moldels_importance))
  } else{
    accucopy<-F
  }


  if(colby=="models"){
    #validate(need(nvars<=nrow(moldels_importance),paste0("number of variables must be less than or equal to the number of variables in the model:",nrow(moldels_importance))))
    df<-data.frame(reshape2::melt(data.frame(id=rownames(moldels_importance),moldels_importance), id=c("id")))
    colnames(df)<-c("variable","model","value")
    df$acculoss<-aculoss

     } else   if(colby=="acculoss"){
    req(length(aculoss)==length(rep(rownames(moldels_importance),each=ncol(moldels_importance))))
    names(aculoss)<-rep(rownames(moldels_importance),each=ncol(moldels_importance))
    accu_table<-data.frame(matrix(aculoss,nrow=nrow(moldels_importance),dimnames =list(rownames(moldels_importance),colnames(moldels_importance))))


    df<-data.frame(reshape2::melt(data.frame(id=rownames(moldels_importance),moldels_importance), id=c("id")))
    colnames(df)<-c("variable","model","value")
    df$acculoss<-aculoss
  } else  if(colby=="acc"){
    #acc<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$models_resampling
    if(class(acc)[[1]]=="list"){
      df<-do.call(rbind,lapply(1:length(acc),function(x){
        acctemp<-acc[[x]]
        acctemp$acculoss<-names(modelist)[x]
        acctemp$id<-rownames(acctemp)
        acctemp
      }))
      acc<-data.frame(df)
      re<-data.frame(reshape2::melt(acc,c("id","acculoss")))
      df<-re[order(re$value, decreasing=T),]
      colnames(df)<-c("variable","acculoss","rep","value")

    } else{
      re<-data.frame(reshape2::melt(data.frame(id=rownames(acc),acc),"id"))
      df<-re[order(re$value, decreasing=T),]
      colnames(df)<-c("variable","model","value")
      df$acculoss<-df$value
    }


  }
  req(is.data.frame(df))

  if(!is.null(acc)){
    colnames(df)[2]<-"model"}

  if(length(sigvars)>0){

    if(class(sigvars)=="list"){
      names(sigvars)<-names(modelist)
      newres<-split(df,df$model)
      x<-names(newres)[1]
      df<-data.frame(do.call(rbind,lapply(names(newres),function(x){
        temp<-newres[[x]]
        temp$sig<-NA
        temp$sig[which(temp$variable%in%sigvars[[x]])]<-"1"
        temp
      })))
      df$sig<-factor(df$sig)

    }else{
      df$sig<-NA
      df$sig[df$variable%in%sigvars]<-"1"
      df$sig<-factor(df$sig)
    }

  }



  df$sig_pos<--5
  df<-df[order(df$sig),]

  mm<-max(df$value)

  if(scale==T){
    lisss<-if(!is.null(acc)){
      if(!is.null(modelist)){
        df$acculoss<-df$value
        split(df,list(df$model,df$rep))
      } else{
        df$acculoss<-df$value
        split(df,list(df$model))
      }



    } else{
      split(df,df$model)
    }
    df<-data.frame(
      do.call(rbind,lapply(lisss,function(x){


        x$value<- x$value/mm*100
        x$acculoss <-x$acculoss/mm*100
        x
      }))
    )

  }
  if(is.null(nvars)){
    nvars<-nrow(moldels_importance)
  }





  mean_vals<-tapply(df$value,df$variable, mean)
  col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
  picvar<-which(df$variable%in%col_names)



  if(colby=="models"){
    col= getcolhabs(newcolhabs,palette,nrow(df))
    p <- ggplot(df[picvar,], aes(reorder(variable,value, mean), value, fill=model))+  scale_fill_manual(name=leg,values=getcolhabs(newcolhabs,palette, ncol(moldels_importance)))
    p<-p + geom_bar(stat = "identity", position="dodge")+coord_flip()
    } else{

    if(!is.null(modelist)){
      df_naomit<-na.omit(df)
      df$variable<-factor(df$variable)
      df2<-data.frame(aggregate(df[,c('value','acculoss')],df[c("variable","model")], sum))
      df2<-data.frame(
        do.call(rbind,lapply(split(df2,df2$model),function(x){
          x$value<-scales::rescale(x$value,c(0,100))
          x$value<-scales::rescale(x$acculoss,c(0,100))
          x
        }))
      )
      df2$value<-scales::rescale(df2$value,c(0,100))
      df2$value<-scales::rescale(df2$acculoss,c(0,100))
      df2$model<-factor(df2$model)
      #df<-df[order(df$sig, decreasing=T),]
      mean_vals<-tapply(df2$acculoss,df2$variable, mean)
      col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
      picvar<-which(df2$variable%in%col_names)

      p<-ggplot(df2[picvar,], aes(reorder(variable,acculoss, mean), value, fill=model))+
        geom_bar(stat = "identity")+ scale_fill_manual(name=leg,values=getcolhabs(newcolhabs,palette, nlevels(df2$model)))+coord_flip()



    if(length(sigvars)>0){
      p<-p+geom_point(aes(y=sig_pos, shape=sig), data=df_naomit)+scale_shape_manual(name="",values =8, labels=c(paste0("p.value","\u2264",sig)), breaks=1)+
        guides(fill = guide_legend(override.aes = list(colour = NA)))+
        labs(x=xlab, y=ylab, shape=NULL)+scale_color_manual(guide="none", values=getcolhabs(newcolhabs,palette, nlevels(df2$model)))


      }
    } else{
      if(isTRUE(accucopy)){
        df$acculoss<-df$value
      }
      df$variable<-factor(df$variable)
      df2<-data.frame(aggregate(df[,c('value','acculoss')],df["variable"], sum))

      df2$value<-scales::rescale(df2$value,c(0,100))
      df2$value<-scales::rescale(df2$acculoss,c(0,100))

      mean_vals<-tapply(df2$acculoss,df2$variable, mean)
      col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
      picvar<-which(df2$variable%in%col_names)
      p0<-ggplot(df2[picvar,], aes(reorder(variable,acculoss, mean), value))+
        geom_bar(stat = "identity", fill=getcolhabs(newcolhabs,palette,1))+coord_flip()
      p<-p0
      p
      }
  }
  # p + geom_bar(stat = "identity",aes(color=sig))+coord_flip()+scale_color_manual(values=c("red","blue"))
  if(is.null(modelist)){
    if(length(sigvars)>0){
    df_naomit<-na.omit(df)
    p<-p+geom_point(aes(y=sig_pos, shape=sig),size=1, data=df_naomit)+scale_shape_manual(name="",values =8, labels=c(paste0("p.value","\u2264",sig)), breaks=1)
  }
  }


  # Use vars() to supply variables from the dataset:
  if(isTRUE(facet_grid)){
    if(is.null(modelist)){
    p<-p + facet_grid(~factor(model, levels=colnames(moldels_importance)))+xlab(xlab)+ylab(ylab)} else{
      p<-p + facet_grid(~factor(model, levels=names(modelist)))+xlab(xlab)+ylab(ylab)
    }

  }
  p<-p+xlab(xlab)+ylab(ylab)+ggtitle(title)
  p<-p+
    theme(
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main),
      strip.text.x=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg))
p
}

plot_model_features2<-function(moldels_importance, palette="turbo", newcolhabs,ylab="Variables",xlab="Importance", colby=c("models",'aculoss'), aculoss=NULL,facet_grid=T,nvars=10, sig=0.01, leg=if(colby=="models") {'Model'}else{"Score decrease (%)"},cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13,sigvars=NULL){
  if(is.null(aculoss)){aculoss<-rep(0,nrow(moldels_importance))}


  if(is.null(nvars)){
    nvars<-nrow(moldels_importance)
  }


  if(colby=="models"){
    validate(need(nvars<=nrow(moldels_importance),paste0("number of variables must be less than or equal to the number of variables in the model:",nrow(moldels_importance))))

  pic<-order(apply(moldels_importance,1,mean),decreasing = T)[1:nvars]

  col_names<-rownames(moldels_importance)[pic]
  moldels_importance<-moldels_importance[col_names,,drop=F]
  } else{
    req(length(aculoss)==length(rep(rownames(moldels_importance),each=ncol(moldels_importance))))
    names(aculoss)<-rep(rownames(moldels_importance),each=ncol(moldels_importance))
    if(!is.null(sigvars)){
      pic<-sigvars
    } else{
      pic<-which(aculoss>=sig)
    }


   accu_table<-data.frame(matrix(aculoss,nrow=nrow(moldels_importance),dimnames =list(rownames(moldels_importance),colnames(moldels_importance))))
   if(is.null(sigvars)){
   col_names<-names(which(apply(accu_table,1,function(x) any( x>=sig))))}else{
     col_names<-sigvars
   }

      moldels_importance<-moldels_importance[col_names,,drop=F]
      aculoss<-unlist(accu_table[col_names,,drop=F])
  }
  df<-reshape2::melt(data.frame(id=rownames(moldels_importance),moldels_importance), id=c("id"))

  colnames(df)<-c("variable","model","value")
  df$acculoss<-aculoss

  if(colby=="models"){
    col= getcolhabs(newcolhabs,palette,nrow(df))

    mean_vals<-tapply(df$value,df$variable, mean)
    col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
    picvar<-which(df$variable%in%col_names)

    p <- ggplot(df[picvar,], aes(reorder(variable,value, mean), value, fill=model))+  scale_fill_manual(name=leg,values=getcolhabs(newcolhabs,palette, ncol(moldels_importance)))
  } else{
    mean_vals<-tapply(df$value,df$variable, mean)
    col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
    picvar<-which(df$variable%in%col_names)


    p <- ggplot(df[picvar,], aes(reorder(variable,value, mean), value, fill=acculoss))+scale_fill_gradientn(name=leg,colors=getcolhabs(newcolhabs,palette,100))


  }


  p<-p + geom_bar(stat = "identity")+coord_flip()




  # Use vars() to supply variables from the dataset:
  if(isTRUE(facet_grid)){
    p<-p + facet_grid(cols = vars(model))+xlab(xlab)+ylab(ylab)

  }
  p<-p+
    theme(
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main),
      strip.text.x=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg))
  return(p)
}



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
               p("iMESc is a shiny-based application that allows the performance of end-to-end machine learning workflows. The available resources meet several needs of environmental workflows, but it is not restricted to. iMESc includes tools for data pre-processing, to perform descriptive statistics, supervised and unsupervised machine learning algorithms and interactive data visualization by means of graphs, maps, and tables. Throughout the app, data input and output are organized in modules enabling the creation of multiple ML pipelines. Additionally, it allows saving the workspace in a single file, contributing to the best practices in data-sharing and analysis reproducibility. The app is entirely written in the R programming language and thus it is free."),
               p("iMESc allows users to create a savepoint, a single R object that can be reloaded later to restore analysis output."),
               p("Get started by creating a Datalist. Use the",icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"),"button."),
               p("To ensure that iMESc content fits nicely on the screen, we recommend a landscape", strong("minimum resolution  of 1377 x 768 pixels.")))


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
                        col.conf="#FCC7C7", col.obs="darkblue", col.pred="#CDCCD9",q_class,col.mean="SeaGreen", cex=2,pred_ensemble=NULL)
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
      col.conf=col.conf, col.obs=col.obs, col.pred=col.pred,col.mean=col.mean,
      pred_ensemble=  if(!is.null(pred_ensemble)){pred_ensemble[i]}
    )
  }



}
#' @export
confcurve<-function(vec,ob,pred_interval, col.conf="#FCC7C7", col.obs="darkblue", col.pred="#CDCCD9",col.mean="SeaGreen",pred_ensemble=NULL){

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
  if(!is.null(pred_ensemble)){
    abline(v=pred_ensemble, lty=2, col=col.mean, lwd=2)
  } else{
    abline(v=mean(vec), lty=2, col=col.mean, lwd=2)
  }


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


textworkflow<-reactive({
  div(style="padding-top: 15px",
    p(strong("1. Preparation of the Datalists according to the analytical method")),
    p(tags$li('For unsupervised methods, only the Numeric Attribute (X) is needed.')),
    p(tags$li('For classification model, the Numeric-Attribute* (X) and the Factor-Attribute (Y) are needed. X and Y can come from the same Datalist or from different Datalists.')),
    p(tags$li('For regression models, X and Y are Numeric-Attributes, and therefore different Datalists')),
    p(em("*For the Naive Bayes Algorithm, Y is always the Factor-Attribute, and X can be either Factor-Attribute or Numeric-Attribute.")),

    p(strong("2. Pre-processing")),
    p('This section contemplates several tools to prepare the data for the analysis. These tools aims at:'),
    p(tags$li('Filling the missing values, if any (Data imputation tool)')),
    p(tags$li('Transforming the data (e.g., scale, center, log), this is particularly recommended for distance-based methods (e.g., PCA, SOM).')),
    p(tags$li('Partitioning the Y data between training and testing for supervised machine learning methods. This action creates a column in the Factor-Attribute indicating the partitioning.')),


    p(strong("3. Save changes and models")),
    p('Saving data changes or trained models is a recurring step throughout iMESc.'),
    p('After a transformation, for example, the user should save the changes as a new Datalist or overwrite an existing one. During this process, the Factor, Coords and Shapes attributes (if any) are transferred automatically to the new Datalist. Models previously saved in a Datalist (e.g.: RF-Attribute) are not transferred to the newer version.'),
    p(strong("Important:"),'The saved trained models are placed in the Datalist used as a predictor (X).'),
    p('After training the user can save it as a new model or overwrite an existing one. This action creates a new attribute within the Datalist (e.g., RF-Attribute for Random Forest models). A summary table with all saved models in a Datalist can be viewed in the Data Bank menu.'),

    p(strong("4. Download a Savepoint")),
    p('Save the workspace by creating a Savepoint. This file is a single R object that can be downloaded and reloaded later to restore all the files and analysis outputs.To restore a workspace, upload one or multiple Savepoints at once. When uploading multiple Savepoints, Datalists with the same name in different Savepoints will get a integer suffix.')
  )
})
