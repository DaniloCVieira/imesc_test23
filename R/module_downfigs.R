#' @export
module_ui_figs<- function(id){
  ns <- NS(id)


}

#' @export
module_server_figs <- function (input, output, session,vals ){
  ns <- session$ns

  fn_download <- function()
  {

    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)

    if(input$fformat=="pdf"|input$fformat=="svg") fheight <- round(fheight*0.3937,2)
    if(input$fformat=="pdf"|input$fformat=="svg") fwidth <- round(fwidth*0.3937,2)

    if(input$fformat=="png") png(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
    if(input$fformat=="tiff") tiff(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",compression="lzw", pointsize = input$pointsize)
    if(input$fformat=="jpeg") jpeg(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",quality=100, pointsize = input$pointsize)
    if(input$fformat=="pdf") pdf(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)
    if(input$fformat=="svg") svg(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)



    switch (
      vals$hand_plot,
      'Correlation Plot'=replayPlot(vals$plot_correlation),
      'Missing plot'=plot(vals$missing_plot),
      'Scatter plot'=replayPlot(vals$scatter_plot),
      'Corr plot'=replayPlot(vals$corplot),
      "Download plot - Model comparation"=plot(vals$comp_plot),
      "k-means (pca reprentation)"= plot(vals$kmeans_plot_data),
      "k-means (codebook)"=plot(vals$psom_plot),
      "Dendrogram"=replayPlot(vals$hc_tab1_plot),
      "Scree plot"=replayPlot(vals$hc_tab2_plot),
      "Hcut"=replayPlot(vals$hc_tab3_plot),
      "codebook clusters"=plot(vals$hc_tab4_plot),
      "mapcodes predictions"=replayPlot(vals$hc_tab5_plot),
      "Ridge plot"=plot(vals$rid_plot),
      "variable summary"={
        replayPlot(vals$varplot)
      },
      "factor summary"={replayPlot(vals$factorsplot)},
      "histogram"={replayPlot(vals$phist)},
      "boxplot"={replayPlot(vals$pbox_plot)},
      "pca"=   {replayPlot(vals$ppca_plot)},
      "mds"= {replayPlot(vals$pmds_plot)},
      "rda"={replayPlot(vals$rda_plot)},
      "dp"={replayPlot(vals$plot_dp)},
      "we"={replayPlot(vals$plot_we)},
      "segrda"={replayPlot(vals$seg_rda_plot)},

      "Confusion Matrix RF"={plot(vals$cm_rf)},
      "Confusion Matrix - test RF"={plot(vals$conf_rf)},
      "Minimal Depth distribution"={plot(vals$rfd_res)},
      "Multi-way importance"={plot(vals$rfm_res)},
      "RF interactions"={plot(vals$rf_inter)},
      "RF - Partial Dependence"=plot(vals$rfbiplot1),
      "RF ranking comparations"=plot(rf_rank$df),
      "RF measure comparations"=plot(rf_rel$df),
      "RF - Partial Dependence (classes)"=plot(vals$rfbiplot2),
      "florest plot"=replayPlot(vals$plot_florest),
      'florest plot (class)'=plot(vals$plot_florest_class),


      "NB - Confusion Matrix"={plot(vals$cm_nb)},
      "NB - Variable Importance plot"=plot(vals$nb_varImp_plot),
      "NB - Confusion Matrix (predictions)"=plot(vals$nb_cm_pred),
      "NB - DM plot"=replayPlot(vals$nb_densplot),

      "SVM - Confusion Matrix"={plot(vals$cm_svm)},
      "SVM - Variable Importance plot"=plot(vals$svm_varImp_plot),
      "SVM - Confusion Matrix (predictions)"=plot(vals$svm_cm_pred),
      "SVM - DM plot"=replayPlot(vals$svm_densplot),

      "knn - Confusion Matrix"={plot(vals$cm_knn)},
      "knn - Variable Importance plot"=plot(vals$knn_varImp_plot),
      "knn - Confusion Matrix (predictions)"=plot(vals$knn_cm_pred),
      "knn - DM plot"=replayPlot(vals$knn_densplot),

      "sgboost - Confusion Matrix"={plot(vals$cm_sgboost)},
      "sgboost - Variable Importance plot"=plot(vals$sgboost_varImp_plot),
      "sgboost - Confusion Matrix (predictions)"=plot(vals$sgboost_cm_pred),
      "sgboost - DM plot"=replayPlot(vals$sgboost_densplot),

      "Training plot"={pchanges(vals$som_results)},
      "Couting plot"={  pcounts(vals$som_results)},
      "uMatrix"={   pUmatrix(vals$som_results)},
      "BMUs"={plot(vals$bmus_plot)},
      "BMUs predictions"={replayPlot(vals$bmus_pred_plot)},
      "property plot"={replayPlot(vals$pprop_plot)},
      "Confusion Matrix SOM"={plot(vals$conf_som)},

      "feature_importance_models"={plot(vals$plot_ensemble)},
      "ensemble_confusion_plot"={plot(vals$plot_ensemble)},
      "ensemble_predictions_class_plot"={plot(vals$plot_ensemble)},
      "ensemble_predictions_reg_plot"={plot(vals$plot_ensemble)},
      "ensemble_feature_importance_plot"={plot(vals$plot_ensemble)},
      "ensemble_interactions_plot"={plot(vals$plot_ensemble)}

    )


  }


  output$plotoutput <- renderImage({

    fheight <- input$fheight
    fwidth <- input$fwidth
    fres <- as.numeric(input$fres)
    png(paste0(vals$hand_plot,".png",sep=""), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
    switch(
      vals$hand_plot,
      'Correlation Plot'=replayPlot(vals$plot_correlation),
      'Missing plot'=plot(vals$missing_plot),
      'Scatter plot'=replayPlot(vals$scatter_plot),
      'Corr plot'=replayPlot(vals$corplot),
      "Download plot - Model comparation"=plot(vals$comp_plot),
      "k-means (pca reprentation)"= plot(vals$kmeans_plot_data),
      "k-means (codebook)"=plot(vals$psom_plot),
      "Dendrogram"=replayPlot(vals$hc_tab1_plot),
      "Scree plot"=replayPlot(vals$hc_tab2_plot),
      "Hcut"=replayPlot(vals$hc_tab3_plot),
      "codebook clusters"=plot(vals$hc_tab4_plot),
      "mapcodes predictions"=replayPlot(vals$hc_tab5_plot),
      "Ridge plot"=plot(vals$rid_plot),
      "variable summary"={
        replayPlot(vals$varplot)
      },
      "factor summary"={replayPlot(vals$factorsplot)},
      "histogram"={replayPlot(vals$phist)},
      "boxplot"={replayPlot(vals$pbox_plot)},
      "pca"=   {replayPlot(vals$ppca_plot)},
      "mds"= {replayPlot(vals$pmds_plot)},
      "rda"={replayPlot(vals$rda_plot)},
      "dp"={replayPlot(vals$plot_dp)},
      "we"={replayPlot(vals$plot_we)},
      "segrda"={replayPlot(vals$seg_rda_plot)},

      "Confusion Matrix RF"={plot(vals$cm_rf)},
      "Confusion Matrix - test RF"={plot(vals$conf_rf)},
      "Minimal Depth distribution"={plot(vals$rfd_res)},
      "Multi-way importance"={plot(vals$rfm_res)},
      "RF interactions"={plot(vals$rf_inter)},
      "RF - Partial Dependence"=plot(vals$rfbiplot1),
      "RF ranking comparations"=plot(rf_rank$df),
      "RF measure comparations"=plot(rf_rel$df),
      "RF - Partial Dependence (classes)"=plot(vals$rfbiplot2),
      "florest plot"=replayPlot(vals$plot_florest),
      'florest plot (class)'=plot(vals$plot_florest_class),


      "NB - Confusion Matrix"={plot(vals$cm_nb)},
      "NB - Variable Importance plot"=plot(vals$nb_varImp_plot),
      "NB - Confusion Matrix (predictions)"=plot(vals$nb_cm_pred),
      "NB - DM plot"=replayPlot(vals$nb_densplot),

      "SVM - Confusion Matrix"={plot(vals$cm_svm)},
      "SVM - Variable Importance plot"=plot(vals$svm_varImp_plot),
      "SVM - Confusion Matrix (predictions)"=plot(vals$svm_cm_pred),
      "SVM - DM plot"=replayPlot(vals$svm_densplot),

      "knn - Confusion Matrix"={plot(vals$cm_knn)},
      "knn - Variable Importance plot"=plot(vals$knn_varImp_plot),
      "knn - Confusion Matrix (predictions)"=plot(vals$knn_cm_pred),
      "knn - DM plot"=replayPlot(vals$knn_densplot),

      "sgboost - Confusion Matrix"={plot(vals$cm_sgboost)},
      "sgboost - Variable Importance plot"=plot(vals$sgboost_varImp_plot),
      "sgboost - Confusion Matrix (predictions)"=plot(vals$sgboost_cm_pred),
      "sgboost - DM plot"=replayPlot(vals$sgboost_densplot),


      "Training plot"={pchanges(vals$som_results)},
      "Couting plot"={  pcounts(vals$som_results)},
      "uMatrix"={   pUmatrix(vals$som_results)},
      "BMUs"={plot(vals$bmus_plot)},
      "BMUs predictions"={replayPlot(vals$bmus_pred_plot)},
      "property plot"={replayPlot(vals$pprop_plot)},
      "Confusion Matrix SOM"={plot(vals$conf_som)},


      "feature_importance_models"={plot(vals$plot_ensemble)},
      "ensemble_confusion_plot"={plot(vals$plot_ensemble)},
      "ensemble_predictions_class_plot"={plot(vals$plot_ensemble)},
      "ensemble_predictions_reg_plot"={plot(vals$plot_ensemble)},
      "ensemble_feature_importance_plot"={plot(vals$plot_ensemble)},
      "ensemble_interactions_plot"={plot(vals$plot_ensemble)}

    )


    dev.off()

    return(list(src = paste0(vals$hand_plot,".png",sep=""),
                contentType = "image/png",
                width = round((input$fwidth*as.numeric(input$fres))/2.54, 0),
                height = round((input$fheight*as.numeric(input$fres))/2.54, 0),
                alt = "plot"))
  },deleteFile=TRUE)
  output$bn_download <- downloadHandler(
    filename = fn_downloadf,
    content = function(file) {
      fn_download()
      dev.off()
      file.copy(fn_downloadf(), file, overwrite=T)
    }
  )
  fn_downloadf <- reactive({

    if(input$fformat=="png") filename <- paste0(vals$cur_data,"_",vals$hand_plot,".png",sep="")
    if(input$fformat=="tiff") filename <- paste0(vals$cur_data,"_",vals$hand_plot,".tif",sep="")
    if(input$fformat=="jpeg") filename <- paste0(vals$cur_data,"_",vals$hand_plot,".jpg",sep="")
    if(input$fformat=="pdf") filename <- paste0(vals$cur_data,"_",vals$hand_plot,".pdf",sep="")
    if(input$fformat=="svg") filename <- paste0(vals$cur_data,"_",vals$hand_plot,".svg",sep="")
    return(filename)
  })

  observeEvent(input$fheight,{
    vals$fheight<-input$fheight
  })
  observeEvent(input$fwidth,{
    vals$fwidth<-input$fwidth
  })
  observeEvent(input$fres,{
    vals$fres<-input$fres
  })
  observeEvent(input$pointsize,{
    vals$pointsize<-input$pointsize
  })
  observeEvent(input$fformat,{
    vals$fformat<-input$fformat
  })




  showModal({
    if(is.null(vals$fheight)){vals$fheight<-15}
    if(is.null(vals$fwidth)){vals$fwidth<-20}
    if(is.null(vals$fres)){vals$fres<-"100"}
    if(is.null(vals$pointsize)){vals$pointsize<-12}
    if(is.null(vals$fformat)){vals$fformat<-"pdf"}
    modalDialog(



      column(12,
             splitLayout(
               numericInput(ns("fheight"), "Height (cm)", min=2, max=15, step=1, value = vals$fheight),
               numericInput(ns("fwidth"), "Width (cm)", min=2, max=15, step=1, value = vals$fwidth),
               selectInput(ns("fres"), "Res", choices=c("100","200","300"), selected = vals$fres),
               numericInput(ns("pointsize"), "pointsize",value=vals$pointsize,step=1),
               selectInput(ns("fformat"), "File type", choices=c("png","tiff","jpeg","pdf","svg"), selected =vals$fformat, multiple = FALSE, selectize = TRUE),
               div(style="margin-top: 25px",downloadButton(ns("bn_download"),NULL, style="button_active")),

             ),

             column(12, withSpinner(type=8,color="SeaGreen",imageOutput(ns("plotoutput"))))
      ),
      title=p(strong("action:"),"download", vals$hand_plot),
      easyClose = T

    )
  })

}
