vals<-readRDS("savepoint.rds")


codes<-read.csv("tabelao_bentos.csv", sep=";", row.names=1)
data<-codes[,-c(1:5)]


newcolhabs<-vals$newcolhabs




cor_palette="turbo"
cor_sepwidth_a=0.05
cor_sepwidth_b=0.05
cor_notecex=1.0
cor_noteco="black"
cor_na.color=par("bg")
cor_sepcolor="white"


cor_method = c("pearson", "kendall", "spearman")
cor_hclust_method=c('ward.D','ward.D2','single','complete','average','mcquitty','median','centroid')
cor_distance=c('euclidean','bray','jaccard','hellinger')
cor_dendogram = c("both","row","column","none")
cor_scale = c("none","row", "column")
cor_Rowv = c('TRUE','FALSE')
cor_Colv=c('Rowv',T,F)
cor_revC=c('TRUE','FALSE')
cor_na.rm=c('TRUE','FALSE')
cor_labRow=c('TRUE','FALSE')
cor_labCol=c('TRUE','FALSE')
cor_cellnote=c('TRUE','FALSE')

args<-readRDS('args.rds')

args$cordata

which(apply(data,2,sum, na.rm=T)==0)
cor_cutoff=0.7

nearZeroVar(data)
cordata_filter<-function(data,cor_method="pearson",cor_cutoff=0.75){

  pic<-which(apply(data,2,function(x) var(x,na.rm=T))==0)
  if(length(pic)>0){
    datatemp<-data
    datatemp[is.na(datatemp)]<-0
    datatemp[colnames(data)[pic]]<-NULL
    data<-datatemp
  }
  met<-match.arg(cor_method,c("pearson", "kendall", "spearman"))
  cordata<-cor(data, use="na.or.complete",method =met)
  tmp <- cordata
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data.new<-data[,-which(colnames(data)%in%unique(rownames(which(abs(tmp)>cor_cutoff,arr.ind = T))))]
  cordata<-cor(data.new, use="complete.obs",method =met)
  cordata

}



cordata_filter(data=getdata_upload0(),cor_method=input$cor_method,cor_cutoff=input$cor_cutoff)


cordata<-get_cordata(data)

args<-list(cordata=input$cordata,
          cor_cutoff=input$cor_cutoff,
          newcolhabs=input$newcolhabs,
          cor_palette=input$cor_palette,
          cor_sepwidth_a=input$cor_sepwidth_a,
          cor_sepwidth_b=input$cor_sepwidth_b,
          cor_notecex=input$cor_notecex,
          cor_noteco=input$cor_noteco,
          cor_na.color=input$cor_na.color,
          cor_sepcolor=input$cor_sepcolor,
          cor_hclust_method=input$cor_hclust_method,
          cor_distance=input$cor_distance,
          cor_dendogram=input$cor_dendogram,
          cor_scale=input$cor_scale,
          cor_Rowv=input$cor_Rowv,
          cor_Colv=input$cor_Colv,
          cor_revC=input$cor_revC,
          cor_na.rm=input$cor_na.rm,
          cor_labRow=input$cor_labRow,
          cor_labCol=input$cor_labCol,
          cor_cellnote=cor_cellnote)

do.call(i_corplot,args)
i_corplot<-function(cordata,cor_cutoffnewcolhabs,cor_palette,cor_sepwidth_a,
                    cor_sepwidth_b,cor_notecex,cor_noteco,cor_na.color,
                    cor_sepcolor,cor_hclust_method,cor_distance,cor_dendogram,
                    cor_scale,cor_Rowv,cor_Colv,cor_revC,
                    cor_na.rm,cor_labRow,cor_labCol,cor_cellnote) {
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
  }
  labCol<-if(isTRUE(labCol)){
    labCol<-NULL
  }
  cellnote<-as.logical(match.arg(cor_cellnote,c('TRUE','FALSE')))

  if(isTRUE(cellnote)){
    cellnote<-round(cordata,2)
  } else{
    cellnote<-matrix(rep("",length(cordata)), nrow(cordata), ncol(cordata))
  }


  #x11()
  hclust_fun<-function(...){
    hclust(dist(cordata),hmet)
  }

  ncex=cor_notecex*20/nrow(cordata)
  heatmap.2(cordata,
            hclustfun = hclust_fun,
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
            na.color=cor_na.color,trace='none')

}





args<-list(cordata=cordata,
           cor_cutoff=cor_cutoff,
           newcolhabs=newcolhabs,
           cor_palette=NULL,
           cor_sepwidth_a=cor_sepwidth_a,
           cor_sepwidth_b=cor_sepwidth_b,
           cor_notecex=cor_notecex,
           cor_noteco=cor_noteco,
           cor_na.color=cor_na.color,
           cor_sepcolor=cor_sepcolor,
           cor_hclust_method=cor_hclust_method,
           cor_distance=cor_distance,
           cor_dendogram=cor_dendogram,
           cor_scale=cor_scale,
           cor_Rowv=cor_Rowv,
           cor_Colv=cor_Colv,
           cor_revC=cor_revC,
           cor_na.rm=cor_na.rm,
           cor_labRow=cor_labRow,
           cor_labCol=cor_labCol,
           cor_cellnote=cor_cellnote)




args$cor_palette[[1]]<-NULL
do.call(i_corplot,args)
