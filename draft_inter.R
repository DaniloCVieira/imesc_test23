
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
        #incProgress(1, message=paste0("inter_",i), session=session)
        res[score]
      })
      reee<-data.frame(Var1=parloop[[i]][[1]],Var2=parloop[[i]][[2]],matrix(c(unlist(re)),nrow=1, dimnames = list(paste0(parloop[[i]],collapse="::"),c(paste0("Score",1:reps)))))
      reee
    })

  })
  perfms<-data.frame(do.call(rbind,performaces))
  perfms
}
interact_ml<-function(newdata,predtab,weis,obc,pred,reps,modelist,sig=0.05,top.features=10, scale=T, en_method="weighted", root=NULL, weitype, inter_method=c("Paired","Forward","Backward")){
  inter_method=match.arg(inter_method,c("Paired","Forward","Backward"))

  m<-modelist[[1]]
  predtrain<-pred
  accu<-postResample(predtrain,obc)
  accu<-if(m$modelType=="Classification"){accu["Accuracy"]
  } else{accu['Rsquared']}
  tops<-gettop(modelist,scale,method,weis,top.features)
  perfms0<-i_alone_scores(newdata,obc,reps,modelist,weis,accu, scale,en_method,top.features, root=tops,inter_method=inter_method,progress)
  inters0<-i_both_scores(newdata,obc,reps,modelist,weis,accu,scale,en_method, top.features, root=root,inter_method=inter_method, type="modelist", progress)

  inters=list(score_alone=perfms0,score_both= inters0)
  inters
}




i_scores<-get_inter_res0()
i_result<-i_importance(i_scores)
i_sig<-i_t.test(i_scores,i_result, sig=input$inter_sig)
importances_alone<-i_scores$score_alone
importances_both<-i_result
isig<-rownames(i_sig)[which(i_sig$Sig_both)]


args<-list(
  moldels_importance=importances_both, palette="turbo", newcolhabs=vals$newcolhabs,ylab="Variables",xlab="Importance", colby='acc', aculoss=NULL,facet_grid=F,nvars=10, sig=0.01, leg="Importance",cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13,sigvars=isig,size.sig=1.5, acc=importances_both,title=NULL,modelist=NULL,scale=T
)

attach(args)
do.call(plot_model_features,args)
