
#' @export
module_ui_kmeans <- function(id){
  ns<-NS(id)
  column(12,
         inline( actionButton(ns("teste_comb"),"SAVE")),
         div(span(
           inline(uiOutput(ns('kmeans_inputs')))
         )) ,
         uiOutput(ns('kmeans_out'))
  )

}


#' @export
module_server_kmeans <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns
  ns_kmeans <- NS('kmeans')

  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol <- data.frame(val = c(16,15,17,18,8,1,5,3))
  for(i in 1:length(symbols)) {
    symbol1<-base64enc::dataURI(file = paste0('inst/app/www/pch',i,".png"), mime = "image/png")
    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}
  observeEvent(input$teste_comb,{
    savereac()
  })

  output$psom_side<-renderUI({
    # req(input$model_or_data=="som codebook")
    div(class="map_control_style",style="color: #05668D",
        div(
          # uiOutput(ns('psom_bgpalette')),
          # uiOutput(ns("psom_display")),
          # uiOutput(ns("psom_fac_control")),
          # uiOutput(ns("psom_obscol")),
          # uiOutput(ns("psom_bgalpha")),
          # uiOutput(ns("psom_symbsize")),
          # uiOutput(ns("psom_shape")),
          # uiOutput(ns('psom_legcontrol')),
          #  uiOutput(ns('psom_create_codeook'))
        ))
    tagList(module_ui_somplot(ns("kmeans")))
  })

  output$psom_out<-renderUI({
    #req(input$model_or_data=="som codebook")
    #plotOutput(ns("psom_code"))
    #
    req(!is.null(vals$k_means_results))
    k_means_results<-vals$k_means_results

    #saveRDS(vals$k_means_results,"k_means_results.rds")
   # vals<-readRDS("savepoint_kmeans.rds")
     #input<-readRDS("input_kmeans.rds")
     #k_means_results<-readRDS("k_means_results.rds")

    hc=as.factor(k_means_results[[1]])
    callModule(module_server_somplot,
               "kmeans",
               vals=vals,
               data_target=input$data_kmeans,
               som_model=input$som_kmeans,
               background_type="hc",
               property=NULL,
               hc=hc,
               df_symbol=df_symbol
    )

    #vals<-readRDS('vals.rds')

   # saveRDS(vals[[ns_kmeans("somplot_args")]],"somplot_args.rds")


#vals$som_kmeans
    #valsk$k_means_results[[1]]
   # valsk$kmeans_result
    renderPlot({
      #vals<-readRDS("savepoint.rds")
     # vals<-readRDS("savepoint_kmeans.rds")
     # input<-readRDS("input_kmeans.rds")
      args<-vals[[ns(ns_kmeans("somplot_args"))]]
      req(length(args)>0)
      vals$psom_plot<-do.call(bmu_plot,args)
      vals$psom_plot


    })




  })

  output$choiceskmeans<-renderUI({
    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(style="padding-left: 5px",
        div("Clustering target:"),
        radioButtons(ns("model_or_data"), NULL, choiceValues  = choices_kmeans(), choiceNames=choices_kmeans_names(),selected=c(choices_kmeans()[length(choices_kmeans())]))

    )
  })
  choices_kmeans_names <- reactive({
    req(input$data_kmeans)
    a <- if (length(   names(vals$saved_data) > 0)) {
      "Numeric-Attribute"
    } else {
      NULL
    }

    b <-    if(length(attr(vals$saved_data[[input$data_kmeans]],"som"))>0){"SOM-codebook"}else{NULL}
    res <- c(a, b)
    res
  })
  choices_kmeans <- reactive({
    req(input$data_kmeans)
    a <- if (length(   names(vals$saved_data) > 0)) {
      "data"
    } else {
      NULL
    }

    b <-    if(length(attr(vals$saved_data[[input$data_kmeans]],"som"))>0){"som codebook"}else{NULL}
    res <- c(a, b)
    res
  })
  output$kmeans_inputs<-renderUI({
    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(12,class="well3",
           div(
             class="choosechannel",
             div(
               style="height: 85px;vertical-align: text-top; display: table",
               inline(
                 div(class="align_hc",
                     inline(div(style="padding: 5px",strong("X:"))),
                     inline(uiOutput(ns("data_kmeans"))))
               ),
               inline(
                 div(class="align_hc",inline(uiOutput(ns("choiceskmeans"))))
               ),
               inline(
                 div( class="align_hc",uiOutput(ns("somkmeans")))
               ),
               inline(
                 div(class="align_hc",uiOutput(ns("kmeans_centers")))
               ),

               inline(
                 div(class="align_hc",uiOutput(ns("oc_kmax")))
               ),
               inline(
                 div(class="align_hc",uiOutput(ns("oc_boot")))
               ),

               inline(
                 div(class="align_hc",uiOutput(ns("kmeans_intermax")))
               ),
               inline(
                 div(class="align_hc",uiOutput(ns("kmeans_nstarts")))
               ),
               inline(
                 div(class="align_hc",uiOutput(ns("kmeans_alg")))
               ),
               inline(
                 div(class="align_hc",
                     div("Seed"),
                     numericInput(ns("kmeans_seed"),NULL,value=NA,width="80px"))
               ),
               uiOutput(ns("kmeans_runs"))




             )

           )

    )
  })
  output$kmeans_runs<-renderUI({
    switch (input$kmeans_tab,
            'kmeans_tab1' =inline(actionButton(ns("kmeans_run"),"RUN")),
            'kmeans_tab2' =div(inline(uiOutput(ns('kmeans_oclusters'))),inline(actionButton(ns("oc_run"),"RUN")))

    )
  })

  observeEvent(input$kmeans_tab,{
    vals$kmeans_tab<-input$kmeans_tab
  })
  output$kmeans_out<-renderUI({

    validate(need(length(vals$saved_data)>0,"No Datalist found"))

    tabsetPanel(
      id=ns("kmeans_tab"), selected=vals$kmeans_tab,
      tabPanel("1. K-means clustering", value="kmeans_tab1",
               uiOutput(ns("kmeans_tab1_out"))),
      tabPanel("2. Optimal number of clusters",value='kmeans_tab2',
               uiOutput(ns("kmeans_tab2_out")))
    )

  })

  output$kmeans_tab2_out<-renderUI({

    div(
      sidebarLayout(
        sidebarPanel(
          div(class="map_control_style",style="color: #05668D",
              uiOutput(ns("kmeans_tab2_side"))
          )
        ),
        mainPanel(uiOutput(ns("oc_plots")))
      )
    )
  })

  output$kmeans_tab2_side<-renderUI({
    req(input$kmeans_oc)
    div(

      uiOutput(ns("oc_gap")),
      uiOutput(ns("oc_sil")),
      uiOutput(ns("oc_ss"))
    )
  })
  observeEvent(input$k.max,{
    vals$k.max<-input$k.max
  })
  observeEvent(input$oc_boot,{
    vals$oc_boot<-input$oc_boot
  })


  output$oc_boot<-renderUI({
    if(is.null(vals$k.max)){vals$oc_boot<-50}
    req(input$kmeans_oc)
    req(input$kmeans_tab=='kmeans_tab2')
    div(
      div("oc_boot:"),
      numericInput(ns("oc_boot"),NULL,vals$oc_boot,step=1,width='100px'),

    )
  })
  output$oc_kmax<-renderUI({
    if(is.null(vals$k.max)){vals$k.max<-24}
    req(input$kmeans_oc)
    req(input$kmeans_tab=='kmeans_tab2')
    div(
      div("K.max:"),
      numericInput(ns("k.max"),NULL,vals$k.max,step=1,width='100px'),

    )
  })
  observeEvent(input$oc_run,{
    req(input$kmeans_oc=='Elbow')
    output$oc_elbow_out<-renderUI({
      req(input$k.max)
      renderPlot({
        p<-fviz_nbclust(getdata_kmeans(), kmeans, method = "wss", k.max = input$k.max,iter.max = input$km_itermax, nstart = input$km_nstart,algorithm = km_alg()) + theme_minimal() + ggtitle("the Elbow Method")
        vals$kmeans_elbow<-p
        vals$kmeans_elbow
      })
    })
  })

  km_alg<-reactive({
    km_alg<-input$km_alg
    if(km_alg=="Forgy/Lloyd"){km_alg='Lloyd'}
    km_alg
  })

  observeEvent(input$oc_run,{
    req(input$kmeans_oc=='Silhouette')
    output$oc_elbow_out<-renderUI({
      req(input$k.max)
      renderPlot({
        p<-fviz_nbclust(getdata_kmeans(), kmeans, method = "silhouette", k.max = input$k.max,iter.max = input$km_itermax, nstart = input$km_nstart,algorithm = km_alg()) + theme_minimal() + ggtitle("the Silhouette Method")
        vals$kmeans_elbow<-p
        vals$kmeans_elbow
      })
    })
  })


  observeEvent(input$oc_run,{
    req(input$kmeans_oc=='Gap Statistic')
    output$oc_elbow_out<-renderUI({
      req(input$k.max)
      renderPlot({
        gap_stat <- suppressWarnings(clusGap(getdata_kmeans(), FUN = kmeans, K.max = input$k.max,iter.max = input$km_itermax, nstart = input$km_nstart,algorithm = km_alg(), B = input$oc_boot,verbose=F))
        vals$gap_stat<-gap_stat
        p<-fviz_gap_stat(vals$gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")
        vals$kmeans_elbow<-p
        vals$kmeans_elbow
      })
    })
  })

  observeEvent(input$oc_run,{
    req(input$kmeans_oc=='Gap Statistic')
    req(input$k.max)
    suppressWarnings(
      try({})

    )



  })

  output$oc_plots<-renderUI({
    req(input$kmeans_oc)
    div(
      uiOutput(ns("oc_elbow_out")),
      uiOutput(ns("oc_gap_out")),
      uiOutput(ns("oc_sil_out")),
      uiOutput(ns("oc_ss_out"))
    )
  })

  output$kmeans_tab1_out<-renderUI({


    sidebarLayout(
      sidebarPanel(
        span(
          inline(
            div(id="save_kmeansbtn",class="save_changes",
                tipify(uiOutput(ns("save_kmeans")),"Save K-Means clusters in the Factor-Attribute")
            )
          )
        ),
        uiOutput(ns('psom_side')),
        uiOutput(ns("kmeans_down_plot")),
        uiOutput(ns("kmeans_down_model"))

      ),
      mainPanel(
        uiOutput(ns('pdata_out')),
        uiOutput(ns('psom_out')),
        inline( DT::dataTableOutput(ns("k_mean_res")))
      )
    )
  })

  output$kmeans_down_model<-renderUI({
    req(!is.null(vals$k_means_results))
    div(id="kmeans_side",
        class="map_control_style",style="color: #05668D",
        #uiOutput(ns("kmeans_result_side")),
        tipify(
          downloadLink(ns("downModel_kmeans"),"+ Download model"),placement = "bottom",
          HTML(paste0(
            div(HTML(paste0("Download model as ",em('rds')," file"))),
            div(HTML(paste0("to be read in R as"))),
            div(HTML(paste0(em('base::readRDS(file.rds)'))))
          ))
        )
    )
  })
  output$kmeans_down_plot<-renderUI({
    req(!is.null(vals$k_means_results))
    actionLink(ns("downplot_kmeans"),"+ Download plot")
  })
  observeEvent(input$downplot_kmeans,{
    vals$hand_plot<-switch(input$model_or_data,
                           "data"="k-means (pca reprentation)",
                           "som codebook"="k-means (codebook)")
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  output$downModel_kmeans <- {
    downloadHandler(
      filename = function() {
        paste0("kmeans_model","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$k_means_results,file)
      })
  }
  ### psom module


  output$psom_create_codeook<-renderUI({
    req(input$model_or_data=="som codebook")
    actionLink(ns('psom_create_codebook'),"+ Create Datalist with the Codebook and kmeans class")
  })

  output$pdata_out<-renderUI({
    req(input$model_or_data=="data")
    req(input$data_kmeans)
    data<-vals$saved_data[[input$data_kmeans]]
    if(ncol(data)==1){
      plotOutput(ns("pdata_plot_box"))
    } else{
      plotOutput(ns("pdata_plot"))
    }

  })

  output$pdata_plot_box<-  renderPlot({
    data<-vals$saved_data[[input$data_kmeans]]
    clu<-as.factor(vals$k_means_results$cluster)
    pbox(data.frame(Cluster=clu,data), newcolhabs = vals$newcolhabs, palette = "turbo")
    colors<-vals$newcolhabs[['turbo']] (nlevels(clu))[clu]
    points(data.frame(Cluster=clu,data),col="black",bg=colors, pch=21)

  })
  output$pdata_plot<-renderPlot({
    req(!is.null(vals$k_means_results))
    #library("factoextra")
    res.km<-vals$k_means_results
    data<-vals$saved_data[[input$data_kmeans]]
    psom_factors<-psom_factors()
    psom_points<-psom_points()
    pic<- which(unlist(lapply(data,var))==0)
    if(length(pic)>0){
      data<-data[,-pic]
    }
    res.pca<-prcomp(data,center=T)
    cols<-getcolhabs(vals$newcolhabs, input$psom_facpalette, nrow(res.km$centers))
    summ<-summary(res.pca)
    lab<-paste0("(",round(summ$importance[,1:2][2,]*100,2),"%)")
    xlab<-paste("Dim I",lab[1])
    ylab<-paste("Dim II",lab[2])
    # Coordinates of individuals
    ind.coord <- as.data.frame(res.pca$x[,1:2])
    # Add clusters obtained using the K-means algorithm
    ind.coord$cluster <- factor(res.km$cluster)
    colnames(ind.coord)<-c("Dim.1","Dim.2","cluster")
    variance.percent <- round(summ$importance[,1:2][2,]*100,2)
    shape<-if(is.null(input$psom_symbol)){16} else{as.numeric(input$psom_symbol)}


    p<-ggscatter(
      ind.coord, x = "Dim.1", y = "Dim.2",
      color = "cluster", ellipse = TRUE, ellipse.type = "convex",
      shape=shape,
      size = input$psom_symbol_size,  legend = "right", ggtheme = theme_bw(base_size = input$psom_symbol_size+11),show.legend.text=F,
      xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
      ylab = paste0("Dim 2 (", variance.percent[2], "% )" ),
      label=NULL, point=psom_points
    )

    if(input$dot_label_clus=='labels'){
      p<-p  +geom_text( data=ind.coord, aes(x=Dim.1, y=Dim.2, label=psom_factors,),size=input$psom_symbol_size, color=cols[ind.coord$cluster])

    }
    vals$kmeans_plot_data<-p+scale_color_manual(values =  cols )
    vals$kmeans_plot_data

  })


  output$psom_bgpalette <- renderUI({
    req(input$model_or_data=="som codebook")
    div(class="map_control_style",style="color: #05668D",
        div(
          div(class="palette",
              "+ Palette:",
              pickerInput(inputId = ns("psom_data_palette"),
                          label = NULL,
                          choices = vals$colors_img$val,
                          choicesOpt=list(content=vals$colors_img$img),
                          selected=vals$psom_data_palette,
                          width="100px"))
        )
    )
  })
  observeEvent(input$psom_data_palette,{vals$psom_data_palette<-input$psom_data_palette})

  psom_factors<- reactive({
    if (length(input$psom_factors)>0) {
      if(input$psom_factors=="rownames"){
        rownames(vals$saved_data[[input$data_kmeans]])
      } else{
        as.factor(attr(vals$saved_data[[input$data_kmeans]],"factors")[rownames(vals$saved_data[[input$data_kmeans]]), input$psom_factors])
      }

    } else{NULL}
  })
  psom_points<-reactive({
    req(input$dot_label_clus)
    if(input$dot_label_clus == 'symbols'){ T} else {F}

  })
  output$psom_code<-renderPlot({
    req(!is.null(vals$k_means_results))
    req(input$psom_facpalette)
    data=vals$saved_data[[input$data_kmeans]]
    m<-attr(data,"som")[[input$som_kmeans]]

    somC<-list(
      som.hc=as.factor(vals$k_means_results[[1]]),
      groups=input$km_centers,
      som.model= m
    )
    pclus(
      somC=somC,
      cex = as.numeric(input$psom_symbol_size),
      factor.pal = as.character(input$psom_facpalette),
      labels.ind = psom_factors(),
      pch=as.numeric(input$psom_symbol),
      points=psom_points(),
      bg_palette=input$psom_data_palette,
      ncol=input$psom_ncol,
      insetx=input$psom_insertx,
      insety=input$psom_inserty,
      alpha.legend=input$psom_bgleg,
      newcolhabs=vals$newcolhabs,
      bgalpha=input$psom_bgalpha
    )
    vals$psom_plot<-recordPlot()
  })

  output$psom_obscol<-renderUI({
    if(is.null(vals$psom_facpalette)){vals$psom_facpalette<-vals$colors_img$val[2]}
    div(span("+ Obs color",inline(
      tipify(pickerInput(inputId = ns("psom_facpalette"),
                         label =NULL,
                         choices = vals$colors_img$val,
                         choicesOpt = list(content = vals$colors_img$img),
                         selected=vals$psom_facpalette,
                         options=list(container="body"), width="75px"),
             "Symbol colors. Choose a gradient to color observations by a factor"
      )
    )))
  })

  savecodebook<-reactive({
    data=vals$saved_data[[input$data_kmeans]]
    m<-attr(data,"som")[[input$som_kmeans]]
    codes<-data.frame(do.call(cbind,m$codes))
    factors<-data.frame(vals$k_means_results[[1]])
    rownames(factors)<-rownames(codes)<-paste0("unit_",1:nrow(codes))
    colnames(factors)<-paste0("Kmeans_",input$km_centers)

    attr(codes,"factors")<-factors
    temp<-codes
    temp<-data_migrate(data,temp,"new")
    attr(temp,"data.factor")<-NULL
    attr(temp,"factors")<-factors
    attr(temp,"datalist")<-NULL
    attr(temp,"filename")<-NULL
    attr(temp,"coords")<-NULL
    attr(temp,"base_shape")<-NULL
    attr(temp,"layer_shape")<-NULL
    attr(temp,"transf")<-NULL
    attr(temp,"nobs_ori")<-NULL
    if(input$hand_save=="create"){
      vals$saved_data[[input$newdatalist]]<-temp
      #  vals$cur_data<-input$data_newname
    } else{

      vals$saved_data[[input$over_datalist]]<-temp
      # vals$cur_data<-input$data_over

    }
  })
  observeEvent(input$psom_insertx,{
    vals$psom_insertx<-input$psom_insertx
  })
  observeEvent(input$psom_inserty,{
    vals$psom_inserty<-input$psom_inserty
  })
  observeEvent(input$psom_ncol,{
    vals$psom_ncol<-input$psom_ncol
  })
  observeEvent(input$psom_bgleg,{
    vals$psom_bgleg<-input$psom_bgleg
  })
  output$psom_legcontrol<-renderUI({
    req(input$model_or_data=="som codebook")
    req(input$psom_facpalette)
    col<-getcolhabs(vals$newcolhabs,input$psom_facpalette,2)
    req(col[1]!=col[2])
    if(is.null(vals$psom_insertx)){vals$psom_insertx<-0}
    if(is.null(vals$psom_inserty)){vals$psom_inserty<-0.4}
    if(is.null(vals$psom_ncol)){vals$psom_ncol<-1}
    if(is.null(vals$psom_bgleg)){vals$psom_bgleg<-0.85}
    {
      div(
        div(span("+ leg x:",tipify(numericInput(ns("psom_insertx"),NULL,value=vals$psom_insertx,step=0.05, width="75px"),"legend position relative to the x location"))),
        div(span("+ leg y:",tipify(numericInput(ns("psom_inserty"),NULL,value=vals$psom_inserty,step=0.05, width="75px"),"legend position relative to the y location"))),
        div(span("+ ncol leg:",tipify(numericInput(ns("psom_ncol"),NULL,value=vals$psom_ncol,step=1, width="75px"),"the number of columns in which to set the legend items"))),
        div (span("+ bg leg:",tipify(numericInput(ns("psom_bgleg"),NULL,value=vals$psom_bgleg,step=0.05, max=1, width="75px"),"Legend background transparency")))
      )
    }
  })
  observeEvent(input$psom_symbol,{
    vals$psom_symbol<-input$psom_symbol
  })
  output$psom_shape<-renderUI({

    req(input$dot_label_clus=='symbols')
    div(span("+ Shape",
             tipify(pickerInput(inputId = ns("psom_symbol"),
                                label = NULL,
                                choices = df_symbol$val,
                                choicesOpt = list(content = df_symbol$img),
                                options=list(container="body"), width="100px",
                                selected=vals$psom_symbol)
                    ,"symbol shape")))
  })
  observeEvent(input$psom_symbol_size,{
    vals$psom_symbol_size<-input$psom_symbol_size
  })
  output$psom_symbsize<-renderUI({
    if(is.null(vals$psom_symbol_size)){vals$psom_symbol_size<-1}
    div(span(
      "+ size",inline(
        tipify(numericInput(ns("psom_symbol_size"),NULL,value = vals$psom_symbol_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    ))
  })
  observeEvent(input$psom_bgalpha,vals$psom_bgalpha<-input$psom_bgalpha)
  output$psom_bgalpha<-renderUI({
    if(is.null(vals$psom_bgalpha)){vals$psom_bgalpha<-0.5}
    div(span(
      "+ Background transparency",inline(
        tipify(numericInput(ns("psom_bgalpha"),NULL,value = vals$psom_bgalpha,min = 0,max = 1,step = .1, width="75px"),"symbol size")
      )
    ))
  })
  output$psom_display<-renderUI({
    div(span("+ Display:",inline(
      radioButtons(ns("dot_label_clus"), NULL, choices = c("symbols","labels"), inline=T, width="100px", selected=vals$dot_label_clus)
    )))
  })
  observeEvent(input$dot_label_clus,{
    vals$dot_label_clus<-input$dot_label_clus
  })
  observeEvent(input$psom_facpalette,{
    vals$psom_facpalette<-input$psom_facpalette
  })
  observeEvent(input$psom_factors,{vals$psom_factors<-input$psom_factors})
  output$psom_fac_control<-renderUI({
    req(input$psom_facpalette)
    col<-getcolhabs(vals$newcolhabs,input$psom_facpalette,2)
    req(input$dot_label_clus == 'labels'|col[1]!=col[2])

    if(input$model_or_data=='som codebook'){
      choices<-c(colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
    } else{
      choices<-c("rownames",colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
    }
    span("+ Labels:", inline(
      pickerInput(ns("psom_factors"),NULL,
                  choices = c(choices),selected=vals$psom_factors,width="150px")
    ))


  })


  ###




  observeEvent(list(input$data_kmeans,input$km_centers,input$km_itermax,input$km_nstart,input$km_alg,input$model_or_data, input$k.max),{
    vals$kmeans_result<-NULL
    vals$k_means_results<-NULL
    vals$kmeans_elbow<-NULL

  })


  output$save_kmeans<-renderUI({
    req(vals$kmeans_result)
    req(class(vals$kmeans_result)=="ikmeans")
    div(class="border_alert",
        bsButton(ns("tools_savekmeans"), icon("fas fa-save"),style  = "button_active", type="action",value=FALSE)
    )
  })

  observeEvent(input$tools_savekmeans,{
    if(input$tools_savekmeans %% 2) {
      vals$hand_save<-"Save K-means Clusters"
      vals$hand_save2<-p(p(em(input$data_kmeans,style="color: gray"),strong("::"),em("Factor-Attribute",style="color: gray"), strong("::")))
      vals$hand_save3<-NULL
      if(input$model_or_data=='som codebook'){
        vals$hand_save3<-"This action classifies the observations according to the clustering of neurons"
      }
      showModal(module_kmeans())
    }
  })

  observeEvent(input$psom_create_codebook,{
    if(input$psom_create_codebook %% 2) {
      vals$hand_save<-"create_codebook_kmeans"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
      showModal(module_kmeans())
    }
  })



  output$kmeans_result_side<-renderUI({
    req(vals$kmeans_result)
    req(class(vals$kmeans_result)=="ikmeans")
    pickerInput(ns('kmeans_res'),"show result:",c('cluster','centers','totss','withinss','tot.withinss','betweenss','size','inter','ifault'), width="120px")
  })



  observeEvent(input$kmeans_run,{
    try({
      if (!is.na(input$kmeans_seed)) {set.seed(input$kmeans_seed)}
      centers<-input$km_centers
      data<-getdata_kmeans()
      kmeans_result<-kmeans(data, centers, iter.max = input$km_itermax, nstart = input$km_nstart,algorithm = km_alg())
      vals$k_means_results<-kmeans_result
      #newclass<-m$unit.classif
      #for(i in 1:length(hcut)) {newclass[newclass==i]<-rep(hcut[i],sum(  newclass==i))}


      # kmeans_result<-readRDS("kmeans.rds")
      kmeans_result<-lapply(kmeans_result, function(x)data.frame(x))
      onecol<-which(unlist(lapply(kmeans_result,ncol))==1)
      for(i in names(onecol)){
        colnames(kmeans_result[[i]])<-i
      }
      class(kmeans_result)<-"ikmeans"
      vals$kmeans_result<-kmeans_result


    })
  })

  output$k_mean_res<-DT::renderDataTable({
    req(input$kmeans_res)
    vals$kmeans_result[[input$kmeans_res]]
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')


  observeEvent(input$kmeans_alg,{ vals$kmeans_alg<-input$kmeans_alg})
  output$kmeans_alg<-renderUI({
    req(input$model_or_data)
    div(
      div('Algorithm:'),
      div(pickerInput(ns('km_alg'),NULL,c("Hartigan-Wong", "Forgy/Lloyd",
                                          "MacQueen"), width="150px", selected=vals$km_alg))
    )
  })



  observeEvent(input$km_nstarts,{ vals$km_nstarts<-input$km_nstarts})
  output$kmeans_nstarts<-renderUI({
    req(input$model_or_data)
    if(is.null(vals$km_nstart)){vals$km_nstart<-1}
    km_centers<-input$km_centers
    req(length(km_centers)==1)
    div(
      div(span("nstart",tiphelp("if centers is a number, how many random sets should be chosen?"))),
      numericInput(ns('km_nstart'),NULL, width="120px", value=vals$km_nstart, step=1)
    )
  })

  observeEvent(input$km_itermax,{ vals$km_itermax<-input$km_itermax})
  output$kmeans_intermax<-renderUI({
    req(input$model_or_data)
    if(is.null(vals$km_itermax)){vals$km_itermax<-10}
    div(
      div(span("iter.max",tiphelp("the maximum number of iterations allowed"))),
      numericInput(ns('km_itermax'),NULL, width="120px", value=vals$km_itermax,step=1)
    )
  })

  observeEvent(input$km_centers,{ vals$km_centers<-input$km_centers})

  output$kmeans_oclusters<-renderUI({
    req(input$kmeans_tab=='kmeans_tab2')
    inline(
      pickerInput(ns("kmeans_oc"),"Method",c("Elbow","Gap Statistic","Silhouette"),width='150px')
    )
  })

  output$kmeans_centers<-renderUI({
    # req(input$kmeans_tab=='kmeans_tab1')
    req(input$model_or_data)
    if(is.null(vals$km_centers)){vals$km_centers<-5}
    div(
      div(span("centers",tiphelp("the number of clusters: a random set of (distinct) rows in data is chosen as the initial centres"))),
      numericInput(ns('km_centers'),NULL, width="100px", value=vals$km_centers, step=1)
    )
  })
  observeEvent(input$data_kmeans,{ vals$cur_data<-input$data_kmeans})
  output$data_kmeans<-renderUI({
    tags$div(style="margin: 0px; padding: 0px",
             div("Training Datalist:"),
             div(
               pickerInput(ns("data_kmeans"),
                           NULL,
                           choices =    names(vals$saved_data),
                           width="250px", selected=vals$cur_data)
             )
    )
  })
  observeEvent(input$som_kmeans,{ vals$som_kmeans<-input$som_kmeans})
  output$somkmeans<-renderUI({
    req(input$model_or_data)
    req(input$model_or_data)
    req(input$model_or_data=='som codebook')
    data=vals$saved_data[[input$data_kmeans]]
    div(div("Som codebook:"),
        div(pickerInput(ns("som_kmeans"),NULL,choices = names(attr(data,"som")),selected=vals$som_kmeans,
                        width="150px")))
  })


  getdata_kmeans<-reactive({
    req(input$data_kmeans)
    data=vals$saved_data[[input$data_kmeans]]
    #validate(need(length(data)>0,"no data found"))
    res<-if(input$model_or_data=="data"){
      data
    } else{
      req(length(input$som_kmeans)>0)
      m<-attr(data,"som")[[input$som_kmeans]]
      codes<-getCodes(m)
      codes
    }
    res

  })
  savekmeans<-reactive({
    temp <- vals$kmeans_result[[1]][,1]
    if(input$model_or_data=="som codebook"){
      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]
      newclass<-m$unit.classif
      fac<-temp
      for(i in 1:length(fac)) {newclass[newclass==i]<-rep(fac[i],sum(  newclass==i))}
      names(newclass)<-rownames(data)
      temp<-newclass
    }
    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_kmeans]],"factors")[input$newdatalist]<-as.factor(temp)
    } else{
      attr(vals$saved_data[[input$data_kmeans]],"factors")[input$over_datalist]<-as.factor(temp)
    }
    removeClass('save_kmeansbtn','save_changes')
  })

  observeEvent(input$kmeans_run,{
    addClass('save_kmeansbtn','save_changes')
  })

  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      "Save K-means Clusters"= { name_kmeans_clusters()},
      "create_codebook_kmeans"={name_kmeans_codebook()}
    )})
  name_kmeans_clusters<-reactive({
    centers<-input$km_centers
    if(length(centers)==1){bag0=centers}else{bag0=length(centers)}
    name0<-"kmeans"
    name0<-paste(name0,bag0)
    bag=1
    name1<-paste0(name0," (",bag,")")
    if(name1%in%colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%colnames(attr(vals$saved_data[[input$data_kmeans]],"factors"))) break
      }
    }
    paste0(name0," (",bag,")")
  })
  name_kmeans_codebook<-reactive({
    name0<-paste0("Coodebook+Kmeans:",input$km_centers)
    bag=1
    name1<-paste0(name0," (",bag,")")
    if(name1%in%colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%colnames(attr(vals$saved_data[[input$data_kmeans]],"factors"))) break
      }
    }
    paste0(name0," (",bag,")")
  })
  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    choices=switch (vals$hand_save,
                    'Save K-means Clusters' = colnames(attr(vals$saved_data[[input$data_kmeans]],'factors')),
                    "create_codebook_kmeans"=names(vals$saved_data)
    )



    res<-pickerInput(ns("over_datalist"), NULL,choices, width="350px")
    data_overwritte$df<-T
    inline(res)
  })
  output$data_create<-renderUI({
    req(newname$df!=0)
    data_store$df<-F
    req(input$hand_save=="create")
    res<-textInput(ns("newdatalist"), NULL, newname$df, width="350px")
    data_store$df<-T
    inline(res)
  })
  observeEvent( input$data_confirm,{
    req(!is.null(vals$hand_save))
    switch(
      vals$hand_save,
      "Save K-means Clusters"= {savekmeans()},
      "create_codebook_kmeans"=savecodebook()
    )
    vals$hand_save<-NULL
    removeModal()

  })
  module_kmeans <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    uiOutput(ns('saverf_teste')),
                    fluidRow(modalButton(strong("cancel")),
                             inline(uiOutput(ns("save_confirm")))
                    )
      ),

      easyClose = T
    )

  }
  output$databank_storage<-renderUI({
    req(!is.null(vals$hand_save))
    newname$df<-0
    get_newname()
    div(
      column(12,
             div(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")),
             div(vals$hand_save2,style="color: gray"),
             div(vals$hand_save3)),
      column(12,style="margin-top: 20px",
             radioButtons(ns("hand_save"),NULL,
                          choiceNames= list(div(style="height: 40px",span("Create", style="margin-right: 15px"), inline(uiOutput(ns("data_create")))),
                                            div(style="height: 40px",span("Overwrite", style="margin-right: 15px"), inline(uiOutput(ns("data_over"))))),
                          choiceValues=list('create',"over"), width="800px")
      )


    )
  })
  output$save_confirm<-renderUI({
    req(isTRUE(data_store$df)|isTRUE(data_overwritte$df))
    actionButton(ns("data_confirm"),strong("confirm"))
  })

  savereac<-reactive({

    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    saveRDS(tosave,"savepoint_kmeans.rds")
    saveRDS(reactiveValuesToList(input),"input_kmeans.rds")

    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })
}
