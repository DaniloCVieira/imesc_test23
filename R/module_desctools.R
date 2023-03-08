
#button(
#input(
#link(
##buttons(

#' @export
module_ui_desctools<-function(id){

  ns<-NS(id)
  tagList(
    column(12,style="background: white",
           uiOutput(ns("upload_selection")),
           uiOutput(ns("panel_main"))
           )
  )

}

# Server
#' @export
module_server_desctools<-function (input,output,session,vals,df_colors,newcolhabs,df_symbol ){
  ns<-session$ns
   pw_icon <- base64enc::dataURI(file = "inst/app/www/pwrda_icon.png", mime = "image/png")
   smw_icon <- base64enc::dataURI(file = "inst/app/www/smw_icon.png", mime = "image/png")


  box_y_cur<-reactiveValues(df=1)

  filter_box2_cur<-reactiveValues(df=1)
  filter_box1_cur<-reactiveValues(df=1)
  box_factor_cur<-reactiveValues(df=1)
  bag_smw<-reactiveValues(df=F)
  aggreg_reac<-reactiveValues(df=0)
  updp<-reactiveValues(df=F)
  updp0<-reactiveValues(df=F)
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })

  observeEvent(input$desc_options,{
    if(input$desc_options%in%c('tab_scatter','tab_segrda','tab_rda',"tab_omi")){
      shinyjs::hide('data_upload0')
    }
    if(!input$desc_options%in%c('tab_scatter','tab_segrda','tab_rda',"tab_omi")){
      shinyjs::show('data_upload0')
    }
  })
  output$upload_selection<-renderUI({

    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(12,
           pickerInput(ns("data_upload0"),NULL,choices=names(vals$saved_data),  selected=vals$cur_data, options=list(container="body","style-base" = "form-control", style = "")))

  })

  observeEvent(input$desc_options,{
    vals$cur_desc_options<-input$desc_options
  })


  output$panel_main<-renderUI({
    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    req(input$data_upload0)
    # border_alert<-border_alert()
    data<-vals$saved_data[[input$data_upload0]]
    if(is.null(vals$cur_desc_options)){vals$cur_desc_options<-'Summaries'}
    column(12,
           tabsetPanel(id=ns('desc_options'),selected = vals$cur_desc_options,
                       tabPanel('Summaries',
                                uiOutput(ns('summaries_out'))),
                      # tabPanel('Missing data',uiOutput(ns('missing_data'))),
                       tabPanel('Ridges',
                                uiOutput(ns('rid_panel'))),
                       tabPanel('Scatter',value="tab_scatter",
                                uiOutput(ns('scatter_out'))),
                       tabPanel('Histogram',
                                uiOutput(ns("stats_phist"))),
                       tabPanel('Boxplot',
                                column(12,uiOutput(ns("stats_cbox")),
                                       div(
                                         uiOutput(ns('boxplot_out'))
                                       ))),
                       tabPanel('MDS',
                                div(
                                  sidebarLayout(
                                    sidebarPanel(
                                      fluidRow(
                                        class="map_control_style",
                                        style="color: #05668D",
                                        uiOutput(ns('omds_dist')),
                                        uiOutput(ns('mds_options'))

                                      )
                                    ),
                                    mainPanel(
                                      uiOutput(ns("stats_cmds")),
                                      uiOutput(ns("stats_pmds"))
                                    )
                                  )
                                )
                       ),
                      # tabPanel('Correlation plot',div(sidebarLayout(sidebarPanel(fluidRow(class="map_control_style",style="color: #05668D",uiOutput(ns('side_corplot')))),mainPanel(uiOutput(ns("corr_plot")))))),
                       tabPanel('PCA',
                                div(
                                  sidebarLayout(
                                    sidebarPanel(
                                      fluidRow(
                                        class="map_control_style",
                                        style="color: #05668D",
                                        uiOutput(ns('opca_biplot')),
                                        uiOutput(ns('pca_options_plot')),
                                        uiOutput(ns('pca_summary'))

                                      )
                                    ),

                                    mainPanel(
                                      uiOutput(ns("stats_cpca")),
                                      tabsetPanel(id=ns('pca_options'),selected=vals$pca_options,
                                        tabPanel("Plot",
                                                 value="pca_plot",
                                                 uiOutput(ns("stats_ppca"))),
                                        tabPanel("Summary",
                                                 value="pca_summary",
                                                 inline(DT::dataTableOutput(ns("summary_pca"))))
                                      )
                                    )
                                  )
                                )
                       ),
                       tabPanel('RDA',value="tab_rda",
                                div(
                                  div(uiOutput(ns("stats_crda"))),
                                  sidebarLayout(
                                    sidebarPanel(
                                      fluidRow(
                                        class="map_control_style",
                                        style="color: #05668D",
                                        uiOutput(ns('orda_options')),
                                        uiOutput(ns('rda_options'))

                                      )
                                    ),
                                    mainPanel(
                                      uiOutput(ns("stats_rda"))
                                    )
                                  )
                                )
                       ),
                       tabPanel('segRDA',value="tab_segrda",
                                div(
                                  style="background: white",
                                  p(strong("Segmented Redundancy Analysis")),
                                  span(
                                    inline(
                                      span(style="width: 150px",

                                           inline( pickerInput(ns("segrda_X"),span("Y Data", tiphelp("Predictors")), choices=names(vals$saved_data), selected=vals$cur_segrda_X))
                                      )
                                    ),
                                    inline( pickerInput(ns("segrda_Y"),span("~ X Data", tiphelp("Response data")), choices=names(vals$saved_data), selected=vals$cur_segrda_Y))
                                  )

                                ),
                                uiOutput(ns('segrda_panels'))
                       )
           )
    )

  })


  getmissing<-reactive({
    vals<-readRDS("savepoint.rds")
    data<-vals$saved_data$zeu
    req(is.data.frame(vals$saved_data[[input$data_upload0]]))
    data=vals$saved_data[[input$data_upload0]]

    image(as.matrix(data))

    res0<-res<-which(is.na(data), arr.ind=TRUE)
    if(length(res0)>0){
      for(i in 1:nrow(res)){
        res0[i,1]<-rownames(data)[res[i,1]]
        res0[i,2]<-colnames(data)[res[i,2]]
      }
      colnames(res0)<-c("ID","Variable")
      rownames(res0)<-NULL
      res<-data.frame( table(res0[,2]))
      colnames(res)<-c("Variable","Missing")
      rownames(res)<-res[,1]
      pic<-colnames(vals$saved_data[[input$data_upload0]])[which(colnames(vals$saved_data[[input$data_upload0]])%in%res[,1])]
      res[,1]<-NULL
      if(length(pic)>0)
        res[pic,, drop=F]
    }



  })


  get_dataord<-reactive({
    req(input$missing_reorder!="N missing")
    data=vals$saved_data[[input$data_upload0]]
    dataord<-if(input$missing_reorder=="Factor"){
      attr(data,"factors")    } else{data}
    dataord
  })



  observeEvent(input$missing_id1,{
    vals$missing_id1<-input$missing_id1
  })
  observeEvent(input$missing_id2,{
    vals$missing_id2<-input$missing_id2
  })
  observeEvent(input$missing_var1,{
    vals$missing_var1<-input$missing_var1
  })
  observeEvent(input$missing_var2,{
    vals$missing_var2<-input$missing_var2
  })
  observeEvent(input$missing_reorder,{
    vals$missing_reorder<-input$missing_reorder
  })

  observeEvent(input$missing_ord,{
    vals$missing_ord<-input$missing_ord
  })


  output$missing_data<-renderUI({
    sidebarLayout(
      sidebarPanel(uiOutput(ns('missing_side'))),
      mainPanel(uiOutput(ns('missing_plot')))
    )
  })

  output$missing_side<-renderUI({
    data=vals$saved_data[[input$data_upload0]]
    if(is.null(vals$missing_id1)){
      ob1<-rownames(data)[c(1,nrow(data))]
      va1<-colnames(data)[c(1,ncol(data))]
      vals$missing_id1<-ob1[1]
      vals$missing_id2<-ob1[2]
      vals$missing_var1<-va1[1]
      vals$missing_var2<-va1[2]
    }



    div(class="map_control_style",style="color: #05668D",
      div("Row:",
          inline(pickerInput(ns("missing_id1"), NULL,rownames(data), selected=vals$missing_id1, width="100px")), strong("to"),inline(
            pickerInput(ns("missing_id2"), NULL,rownames(data), selected=vals$missing_id2, width="100px")
          )
      ),
      div("Col:",
          inline(pickerInput(ns("missing_var1"), NULL,colnames(data), selected=vals$missing_var1, width="100px")), strong("to"),inline(
            pickerInput(ns("missing_var2"), NULL,colnames(data), selected=vals$missing_var2, width="100px")
          )
      ),
      div("Reorder",
          inline(pickerInput(ns("missing_reorder"), NULL,c(
            "N missing","Variable","Factor"
          ), selected=vals$missing_reorder, width="100px")),
          inline(uiOutput(ns("missing_ord")))
      ),
      div("+ Palette",
        pickerInput(inputId=ns("missing_palette"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px')
      ),
      div(
        actionLink(ns('split_missing'),"+ Split into missing and non-missing", style="button_active")
      ),
      div(
        actionLink(ns('missing_downp'),"+ Download plot", style="button_active")
      ),

      actionButton(ns("save_teste"),"SAVE")
    )

  })

  observeEvent(input$split_missing,{

    #vals<-readRDS("savepoint.rds")
    #input<-readRDS('input.rds')
    data=vals$saved_data[[input$data_upload0]]
    req(any(is.na(data)))
    factors<-attr(data,"factors")
    coords<-attr(data,"coords")
    colmiss<-getcol_missing(data)[,1]
    comi<-which(colnames(data)%in%as.character(colmiss))
    romi<-which(rownames(data)%in%as.character(getrow_missing(data)[,1]))

    ylist<-list()
    for(i in 1:length(comi)){
      romipic<-which(is.na(data[,comi[i]]))
      X=data[-romipic,-comi, drop=F]
      attr(X,"factors")<-factors[rownames(X),]
      attr(X,"coords")<-coords[rownames(X),]

      Y=data[-romipic,comi[i], drop=F]
      fac<-factors[rownames(Y),]
      n_sample<-round(nrow(Y)*20/100)
      part<-sample(1:nrow(Y),n_sample)
      name0<-paste0("Partition_",colnames(Y))
      name1<-make.unique(c(colnames(factors),name0), sep="_")
      name_part<-name1[ncol(factors)+1]
      fac[name_part]<-NA
      fac[rownames(Y)[as.vector(part)] ,name_part]<-"test"
      fac[rownames(Y)[-as.vector(part)] ,name_part]<-"training"
      fac[name_part]<-factor(fac[,name_part])
      attr(Y,"factors")<-fac
      attr(Y,"coords")<-coords[rownames(Y),]
      ylist[[i]]<-Y

      newdata=data[romipic,-comi, drop=F]
      attr(newdata,"factors")<-factors[rownames(newdata),]
      attr(newdata,"coords")<-coords[rownames(newdata),]


      name0<-paste0(input$data_upload0,"_COMP_X_to_", colnames(Y))
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      namemissing<-name1[length(vals$saved_data)+1]
      vals$saved_data[[namemissing]]<-X

      #name0<-paste0(input$data_upload0,"_COMP_Y_", colnames(Y))
      # name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      #namemissing<-name1[length(vals$saved_data)+1]
      #vals$saved_data[[namemissing]]<-Y

      name0<-paste0(input$data_upload0,"_MISS_newX_to_",colnames(Y))
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      namemissing<-name1[length(vals$saved_data)+1]
      vals$saved_data[[namemissing]]<-newdata
    }
    datY<-mergedatacol(ylist)
    name0<-paste0(input$data_upload0,"_COMP_Y_")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    namemissing<-name1[length(vals$saved_data)+1]
    vals$saved_data[[namemissing]]<-datY
  })




  observeEvent(input$missing_downp,{

    vals$hand_plot<-"Missing plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

  })

  output$missing_ord<-renderUI({
    choices<-colnames(get_dataord())
    pickerInput(ns("missing_ord"), NULL,choices, selected=vals$missing_ord, width="100px")
  })




  output$missing_plot<-renderUI({
    req(input$missing_reorder)
    data=vals$saved_data[[input$data_upload0]]


    ob1<-c(input$missing_id1,input$missing_id2)
    obs<-which(rownames(data)%in%ob1)
    va1<-c(input$missing_var1,input$missing_var2)
    var<-which(colnames(data)%in%va1)
    pic_var<-seq(var[1],var[2])
    pic_obs<-seq(obs[1],obs[2])
    data<-data[pic_obs,pic_var]
    renderPlot({
      df<-data.frame(data)
      df$nmissing<-apply(data,1,function(x) sum(is.na(x)))

      if(input$missing_reorder=='N missing'){
        a<-reshape2::melt(data.frame(id=rownames(df),df), c("id","nmissing"))
        p<-ggplot(a,aes(reorder(variable,nmissing),reorder(id,nmissing)))+  geom_tile(aes(fill=value), color="black")+scale_fill_gradientn(colours=  vals$newcolhabs[[input$missing_palette]](100),na.value="black")
      } else {
        df<-data.frame(data)
        df$nmissing<-apply(data,1,function(x) sum(is.na(x)))

        req(input$missing_reorder)
        dataord<-get_dataord()
        ordvar<-dataord[,input$missing_ord]
        df$ordvar<-ordvar
        a<-reshape2::melt(data.frame(id=rownames(df),df), c("id","nmissing","ordvar"))

        p<-ggplot(a,aes(reorder(variable,ordvar),reorder(id,ordvar)))+  geom_tile(aes(fill=value), color="black")+scale_fill_gradientn(colours=  vals$newcolhabs[[input$missing_palette]](100),na.value="black")
      }
      p<-p+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Variables")+ylab("Observations")

      vals$missing_plot<-p
      vals$missing_plot
    })
  })



observeEvent(input$save_teste,{
  saveRDS(reactiveValuesToList(vals),"savepoint.rds")
  saveRDS(reactiveValuesToList(input),"input.rds")
  beep()
  #vals<-readRDS("vals.rds")
  #input<-readRDS('input.rds')

})



output$side_corplot<-renderUI({
  div(
    div(pickerInput(ns("cor_method"), "+ Method", choices=c("pearson", "kendall",
                                                            "spearman"))),
    div(pickerInput(ns("corplot_type"), "+ plot type", choices=c("barplot", 'histogram'))),
    div(class="palette",span("+ Palette:",inline(
      pickerInput(inputId=ns("cor_palette"),
                  label = NULL,
                  choices = vals$colors_img$val,
                  choicesOpt = list(content = vals$colors_img$img), width="120px", selected=vals$box_palette)
    ))),
    div(span("+ Shape:",
             inline(pickerInput(inputId=ns("cor_pch"),
                                label = NULL,
                                choices = df_symbol$val,
                                options=list(container="body"),
                                choicesOpt = list(content = df_symbol$img), width='75px')))),
    div(
      span("+ Size (symb):",
           inline(numericInput(ns("cor_cex_symb"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='100px')
           ))
    ),
    div(
      span("+ Size (corr):",
           inline(numericInput(ns("cor_cex_cor"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='100px')
           ))
    ),
    div(
      span("+ Size (axes):",
           inline(numericInput(ns("cor_cex_axes"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='100px')
           ))
    ),
    div(
      span("+ Size (labels):",
           inline(numericInput(ns("cor_cex_lab"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='100px')
           ))
    ),
    div(actionButton(ns("cor_go"),"RUN", style="button_active")),
    uiOutput(ns("cor_downplot"))
  )
})

output$cor_downplot<-renderUI({
  req(!is.null(vals$corplot))
  div(
    actionLink(ns('cordown'),"+ Download plot", style="button_active")
  )
})

observeEvent(input$data_upload0,{
  vals$corplot<-NULL
})

observeEvent(input$cor_go,{
  output$corr_plot<-renderUI({
    method=input$cor_method
    palette=input$cor_palette
    pch=input$cor_pch
    cex.cor=input$cor_cex_cor
    cex.axis=input$cor_cex_axes
    cex.labels=input$cor_cex_lab
    cex=input$cor_cex_symb
    if(input$corplot_type=="barplot"){barplot=T} else{barplot=F}
    if(input$corplot_type=="histogram"){histogram=T} else{histogram=F}
    tab<-getdata_upload0()
    renderPlot({
      chart.Correl(tab, method=method, histogram=histogram,barplot=barplot,
                   pch=as.numeric(pch), cex.cor=cex.cor, newcolhabs=vals$newcolhabs, palette=palette,cex=cex, cex.labels=cex.labels, cex.axis=cex.axis)
      vals$corplot<-recordPlot()
    })
  })
})
observeEvent(input$cordown,{

  vals$hand_plot<-"Corr plot"
  module_ui_figs("downfigs")
  mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

})


pic_pca_results<-reactive({
  req(input$show_pca_results)
  switch (input$show_pca_results,
          'Standard deviations' = 'sdev',
          'Rotation'='rotation',
          'Centering'='center',
          'Scaling'='scale',
          'Scores'='x',
          'Importance'='importance',
  )
})

  output$pca_summary<-renderUI({
    req(input$pca_options=="pca_summary")
    div(
      tags$div(
        pickerInput(ns("show_pca_results"),"Show result:", c('Importance','Scores',"Standard deviations","Rotation","Centering","Scaling")),style="width: var(--parentHeight);"
      ),
      actionLink(
        ns('down_pca_results'),span("+ Download",icon("fas fa-table")))
    )


  })

  observeEvent(input$down_pca_results,{
    vals$hand_down<-"PCA result"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })

  observeEvent(input$pca_options,{
    vals$pca_options<-input$pca_options
  })
  observeEvent(input$scatter_y_datalist,{
    vals$scatter_y_datalist<-input$scatter_y_datalist
  })
  output$scatter_y_datalist<-renderUI({
    pickerInput(ns("scatter_y_datalist"),'Datalist Y', choices=names(vals$saved_data), selected=vals$scatter_y_datalist)
  })
  observeEvent(input$scatter_x_datalist,{
    vals$scatter_x_datalist<-input$scatter_x_datalist
  })
  output$scatter_x_datalist<-renderUI({
    pickerInput(ns("scatter_x_datalist"),'Datalist X', choices=names(vals$saved_data), selected=vals$scatter_x_datalist)
  })
  output$scatter_out<-renderUI({
    div(style="background: white",
        p(strong("Scatter plot")),

        sidebarLayout(
          sidebarPanel(
            div( class="map_control_style",
                 style="color: #05668D",

              div(inline(uiOutput(ns("scatter_x_datalist"))),
                  inline(uiOutput(ns("scatter_x")))),

              div(
                inline(uiOutput(ns("scatter_y_datalist"))),
                inline(uiOutput(ns("scatter_y_input")))
              ),
              uiOutput(ns('scatter_side'))
            )
          ),
          mainPanel(uiOutput(ns("scatter_plot"))))
    )
  })

  output$scatter_side<-renderUI({
    req(input$scatter_x)
    req(input$scatter_y)
    div(


      div(span("+ Shape:",
               inline(pickerInput(inputId=ns("scatter_symbol"),
                                  label = NULL,
                                  choices = df_symbol$val,
                                  options=list(container="body"),
                                  choicesOpt = list(content = df_symbol$img), width='75px')))),
      div(
        span("+ Size:",
             inline(numericInput(ns("scatter_cexpoint"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
             ))
      ),
      div(span('+ X label:',
               inline(
                 textInput(ns("scatter_xlab"),NULL , value=input$scatter_x, width="120px")
               )
      )),
      div(span('+ Y label:',
               inline(
                 textInput(ns("scatter_ylab"),NULL , value=input$scatter_y, width="120px"))
      )),
      div(
        actionLink(ns('scatter_downplot'),"+ Download plot", style="button_active")
      )

    )
  })


  observeEvent(input$scatter_downplot,{

    vals$hand_plot<-"Scatter plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

  })
  output$scatter_plot<-renderUI({
    datax<-vals$saved_data[[input$scatter_x_datalist]]
    datay<-vals$saved_data[[input$scatter_y_datalist]]
    renderPlot({
      plot(datax[,input$scatter_x],datay[,input$scatter_y], pch=as.numeric(input$scatter_symbol), cex=input$scatter_cexpoint, xlab=input$scatter_xlab, ylab=input$scatter_ylab)
      vals$scatter_plot<-recordPlot()
    })
  })

  output$scatter_y_input<-renderUI({
    req(input$scatter_y_datalist)
    data<-vals$saved_data[[input$scatter_y_datalist]]
    div(

      pickerInput( ns("scatter_y"),'Y',choices =colnames(data),selected= vals$scatter_y, width="200px"
      ))


  })

  output$scatter_x<-renderUI({

    req(input$scatter_x_datalist)
    data<-vals$saved_data[[input$scatter_x_datalist]]
    div(

      pickerInput( ns("scatter_x"),"X",choices = colnames(data),selected= vals$scatter_x, width="200px"
      ))



  })

  observeEvent(input$scatter_x,{
    vals$scatter_x<-input$scatter_x
  })
  observeEvent(input$scatter_y,{
    vals$scatter_y<-input$scatter_y
  })
  output$rid_out<-renderUI({
    req(input$rid_split)
    req(input$rid_fac)
    req(input$data_upload0)
    data=vals$saved_data[[input$data_upload0]]
    factors<-attr(data,"factors")
    #input$rid_fac<-'depth.area'
    fac<-factors[,input$rid_fac]
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/9))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$rid_split)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    #savereac()
    data$class<-fac

    vals$rid_plot<-   plot_ridges(data=data,
                                  fac=input$rid_fac,
                                  input$rid_col,
                                  newcolhabs=vals$newcolhabs)

    renderPlot(vals$rid_plot)
  })
  output$title_boxplot<-renderUI({
    textInput(ns("titlebox"),NULL , value=titlebox(), width="150px")
  })
  output$editbox<-renderUI({



    req(input$box_factor)
    req(input$filter_box1)
    res<-na.omit(getbox())
    if(is.null(vals$boxmar_b)){
      vals$showout<-F
      vals$boxmar_b=5
      vals$boxmar_l<-5
      vals$boxmar_t<-5
      vals$boxmar_r=5
      vals$box_horizF
      vals$show_outs=T
      vals$varwidth=F
      vals$box_lwd=1.2
      vals$box_xlab_size=1
      vals$box_xlab_font='plain'
      vals$box_xlab_adj=2
      vals$box_xsize=1
      vals$box_srt=0
      vals$box_xlab.adj=0
      vals$box_ylab_size=1
      vals$box_ylab_font='plain'
      vals$box_ylab_adj=3
      vals$box_ysize=1
      vals$box_tick=-0.02
      vals$showboxleg=F
      vals$cex_pbox=1
    }


    prettyvec<-pretty(res[,2])
    ymin_box=prettyvec[1]
    ymax_box=prettyvec[length(prettyvec)]

    fluidRow(class="map_control_style",style="color: #05668D",
             div(span('+ Title:',
                      inline(
                        uiOutput(ns("title_boxplot"))
                      )
             )),
             div(class="palette",span("+ Palette:",inline(
               pickerInput(inputId=ns("box_palette"),
                           label = NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img), width="120px", selected=vals$box_palette)
             ))),

             div(span("+ Overall size:",inline(
               numericInput(ns("cex_pbox"),NULL,value=vals$cex_pbox,step=0.1, width="75px")
             ))),
             div(span("+ Margins:",
                      tipify(numericInput(ns("boxmar_b"),NULL,value=vals$boxmar_b,step=0.1, width="60px"),"bottom", placement = "right"),
                      tipify(numericInput(ns("boxmar_l"),NULL,value=vals$boxmar_l,step=0.1, width="60px"),"left", placement = "right"),
                      tipify(numericInput(ns("boxmar_t"),NULL,value=vals$boxmar_t,step=0.1, width="60px"),"top", placement = "right"),
                      tipify(numericInput(ns("boxmar_r"),NULL,value=vals$boxmar_r,step=0.1, width="60px"),"right", placement = "right"))),

             div(span("+",inline(
               checkboxInput(ns("box_horiz"),"Horizontal",vals$box_horiz, width="95px")
             ))),
             div(span("+",inline(
               checkboxInput(ns("show_outs"),"Outliers:",vals$show_outs, width="95px")
             ))),
             uiOutput(ns("show_outs_on")),

             div(span("+",inline(
               checkboxInput(ns("varwidth"),"Varwidth",vals$varwidth, width="95px")
             ),tipify(icon("fas fa-question-circle"),"Drawn boxes with widths proportional to the square-roots of the number of observations in the groups", options =list(container="body")))),

             div(span("+ Line width:",inline(
               numericInput(ns("box_lwd"),NULL,value=vals$box_lwd,step=0.1, width="75px")
             ))),
             column(12,style="border-top: 1px solid #05668D;border-bottom: 1px solid #05668D",
                    fluidRow(
                      div(uiOutput(ns("box_xaxis"))),
                      column(12,id=ns("pbox_xaxis"),
                             div(span('+ Label:',inline(textInput(ns("box_xlab_text"),NULL , value=input$box_factor, width="120px")))),
                             #div(span("+ Label size:",inline(numericInput(ns("box_xlab_size"),NULL,value=vals$box_xlab_size,step=.1, width="75px")))),
                             #div(span("+ Label font:",inline(pickerInput(ns("box_xlab_font"),NULL,choices=c("plain","bold","italic","bold_italic"), width="80px", selected=vals$box_xlab_font)))),
                             #div(span("+ Label adjust:",inline(numericInput(ns("box_xlab_adj"),NULL,value=vals$box_xlab_adj,step=.1, width="75px")))),
                            # div(span("+ Axis size:",inline(numericInput(ns("box_xsize"),NULL,value=vals$box_xsize,step=.1, width="75px")))),
                             #div(span("+ Axis rotation:",inline(numericInput(ns("box_srt"),NULL,value=vals$box_srt,step=1, width="75px")))),
                            # div(span("+ Axis lab-adjust:",inline(numericInput(ns("box_xlab.adj"),NULL,value=0,step=1, width="55px"))))
                            )
                    )
             ),

             column(12,
                    fluidRow(
                      div(uiOutput(ns('box_yaxis'))),
                      column(12,
                             div(span('+ Label:',
                                      inline(
                                        textInput(ns("box_ylab_text"),NULL , value=input$box_y, width="120px")
                                      )
                             )),
                             #div(span("+ Label size:",inline(numericInput(ns("box_ylab_size"),NULL,value=vals$box_ylab_size,step=.1, width="75px")))),
                             #div(span("+ Label-font:",inline(pickerInput(ns("box_ylab_font"),NULL,choices=c("plain","bold","italic","bold_italic"), width="80px", selected=vals$box_ylab_font)))),
                             #div(span("+ Label adjust:",inline(numericInput(ns("box_ylab_adj"),NULL,value=0,step=.1, width="75px")))),
                             #div(span("+ Axis size:",inline(numericInput(ns("box_ysize"),NULL,value=vals$box_ysize,step=.1, width="75px")))),
                            # uiOutput(ns("box_y_rotation")),
                             div(span("+ min-value:",inline(numericInput(ns("ymin_box"),NULL,value=ymin_box,step=0.1, width="75px")))),
                             div(span("+ max-value:",inline(numericInput(ns("ymax_box"),NULL,value=ymax_box,step=0.5, width="75px"))))
                      )
                    )
             ),

             column(12,style="border-top: 1px solid #05668D;",

                    span("+ Tick size:",inline(
                      numericInput(ns("box_tick"),NULL,value=vals$box_tick,step=0.01, width="75px")
                    ))),
             column(12,style="border-top: 1px solid #05668D;",
                    fluidRow(
                      div(

                        span("+",inline(
                          checkboxInput(ns("showboxleg"),"Show legend:",vals$showboxleg, width="95px")
                        ))),
                      uiOutput(ns("showboxleg_out"))
                    )),
             column(12,style="border-top: 1px solid #05668D;",
                    fluidRow(
                      actionLink(
                        ns("downp_box"),span("+ Download",icon("fas fa-download")), style="button_active"
                      )

                    ))


    )

  })



  observeEvent(input$cex_pbox,{vals$cex_pbox<-input$cex_pbox})
  observeEvent(input$box_srt_y,{vals$box_srt_y<-input$box_srt_y})
  observeEvent(input$showout,{vals$showout<-input$showout})
  observeEvent(input$out_label,{vals$out_label<-input$out_label})
  observeEvent(input$box_palette,{vals$box_palette<-input$box_palette})
  observeEvent(input$boxmar_b,{vals$boxmar_b<-input$boxmar_b})
  observeEvent(input$boxmar_l,{vals$boxmar_l<-input$boxmar_l})
  observeEvent(input$boxmar_t,{vals$boxmar_t<-input$boxmar_t})
  observeEvent(input$boxmar_r,{vals$boxmar_r<-input$boxmar_r})
  observeEvent(input$box_horizF,{vals$box_horizF<-input$box_horizF})
  observeEvent(input$show_outs,{vals$show_outs<-input$show_outs})
  observeEvent(input$varwidth,{vals$varwidth<-input$varwidth})
  observeEvent(input$box_lwd,{vals$box_lwd<-input$box_lwd})
  observeEvent(input$box_xlab_size,{vals$box_xlab_size<-input$box_xlab_size})
  observeEvent(input$box_xlab_font,{vals$box_xlab_font<-input$box_xlab_font})
  observeEvent(input$box_xlab_adj,{vals$box_xlab_adj<-input$box_xlab_adj})
  observeEvent(input$box_xsize,{vals$box_xsize<-input$box_xsize})
  observeEvent(input$box_srt,{vals$box_srt<-input$box_srt})
  observeEvent(input$box_xlab.adj,{vals$box_xlab.adj<-input$box_xlab.adj})
  observeEvent(input$box_ylab_size,{vals$box_ylab_size<-input$box_ylab_size})
  observeEvent(input$box_ylab_font,{vals$box_ylab_font<-input$box_ylab_font})
  observeEvent(input$box_ylab_adj,{vals$box_ylab_adj<-input$box_ylab_adj})
  observeEvent(input$box_ysize,{vals$box_ysize<-input$box_ysize})
  observeEvent(input$ymin_box,{vals$ymin_box<-input$ymin_box})
  observeEvent(input$ymax_box,{vals$ymax_box<-input$ymax_box})
  observeEvent(input$box_tick,{vals$box_tick<-input$box_tick})
  observeEvent(input$showboxleg,{vals$showboxleg<-input$showboxleg})


  output$showboxleg_out<-renderUI({
    req(isTRUE(input$showboxleg ))
    div(style="margin-left: 5px",
        div(span("+ leg y:",inline(
          numericInput(ns("insety"),NULL,value=0,step=0.1, width="75px")
        ))),
        div(span("+ leg x:",inline(
          numericInput(ns("insetx"),NULL,value=0,step=0.1, width="75px")
        )))
    )
  })
  observeEvent(input$show_outs,{
    req(isTRUE(input$show_outs))
    output$show_outs_on<-renderUI({
      column(12,span("+",inline(
        checkboxInput(ns("showout"),"Labels:",vals$showout, width="95px")
      ),inline(
        pickerInput(ns("out_label"),NULL,choices=colnames(attr(getdata_upload0(),"factors")), width="95px", selected=vals$out_label)
      )))

    })
  })
  output$box_y_rotation<-renderUI({
    req(isFALSE(input$box_horiz))
    div(span("+ Axis rotation:",inline(
      pickerInput(ns("box_srt_y"),NULL,choices=c("horizontal","vertical"), width="75px", selected=vals$box_srt_y)
    )))

  })
  output$box_xaxis<-renderUI({
    if(isFALSE(input$box_horiz)){
      span("+ X-axis:")} else{
        span("+ Y-axis:")
      }
  })
  output$box_yaxis<-renderUI({
    if(isFALSE(input$box_horiz)){
      span("+ Y-axis:")} else{
        span("+ X-axis:")
      }
  })




  output$omds_dist<-renderUI({
    if(is.null(vals$cur_dist_mds)){vals$cur_dist_mds="Choose one"}
    div(
      span("+ Distance:",
           inline(
             pickerInput(ns("distance"),NULL,choices = c("Choose one" = "", c('bray', "euclidean", 'jaccard')), selected=vals$cur_dist_mds, width="125px")
           )
      )
    )
  })
  observeEvent(input$distance,{
    vals$cur_dist_mds<-input$distance
  })

  output$opca_biplot<-renderUI({
    req(input$pca_options=="pca_plot")
    div(
      span("+",
           inline(
             checkboxInput(ns("biplot"), span("Biplot", pophelp(NULL,"show biplot arrows")), T, width="75px")
           )
      )
    )
  })


  output$mds_options<-renderUI({
    req(input$distance %in%c("bray","euclidean","jaccard"))
    div(
      div(
        span("+",
             inline(checkboxInput(ns("mds_show_symbols"),"Symbol" ,T, width='75px')))),
      uiOutput(ns("mds_show_symbols_out")),

      div(span("+",
               inline(checkboxInput(ns("mds_show_labels"),"Labels",F)
               ))),
      uiOutput(ns("mds_show_labels_out")),


      div(
        actionLink(
          ns('mds_downp'),span("+ Download",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })

  output$mds_show_labels_out<-renderUI({
    req(isTRUE(input$mds_show_labels ))

    div(style="margin-left: 5px",
        div(span("+ Factor:",
                 inline(tipify(pickerInput(ns("mds_labfactor"),NULL,choices = colnames(attr(getdata_upload0(),"factors")), width="125px"),  "label classification factor")
                 ))),
        div(span("+ Lab Color:",
                 inline(tipify(
                   pickerInput(
                     inputId=ns("mds_labcolor"),
                     label = NULL,
                     selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                     options=list(container="body")
                   ),  "label classification factor"
                 )
                 ))),
        div(span("+ Lab adj:",
                 inline(
                   tipify(pickerInput(ns("mds_labadj"),NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                 ))),
        div(span("+ Lab offset:",
                 inline(
                   tipify(numericInput(ns("mds_offset"),NULL,value = 0,step = .1, width="75px"),  "this value controls the distance ('offset') of the text label from the specified coordinate in fractions of a character width.")
                 ))),
        div(span("+ Size:",
                 numericInput(ns("mds_cextext"),NULL,value = 1,min = 0.1,max = 3,step = .1)))
    )

  })
  output$mds_show_symbols_out<-renderUI({
    req(isTRUE(input$mds_show_symbols))
    div(style="margin-left: 5px",
        div(
          span("+ Shape:",
               inline(pickerInput(inputId=ns("mds_symbol"),
                                  label = NULL,
                                  choices = df_symbol$val,
                                  options=list(container="body"),
                                  choicesOpt = list(content = df_symbol$img), width='75px')))),
        div(
          span("+ Size:",
               inline(numericInput(ns("mds_cexpoint"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
               ))
        ),
        div(class="palette",
            span("+ Color:",
                 inline(
                   tipify(
                     pickerInput(inputId=ns("mds_colpalette"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
        uiOutput(ns("mds_fac_palette"))
    )
  })
  output$pca_show_labels_out<-renderUI({
    req(isTRUE(input$pca_show_labels))

    div(style="margin-left: 5px",
        div(span("+ Factor:",
                 inline(tipify(pickerInput(ns("pca_labfactor"),NULL,choices = colnames(attr(getdata_upload0(),"factors")), width="125px"),  "label classification factor")
                 ))),
        div(span("+ Lab Color:",
                 inline(tipify(
                   pickerInput(
                     inputId=ns("pca_labcolor"),
                     label = NULL,
                     selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                     options=list(container="body")
                   ),  "label classification factor"
                 )
                 ))),
        div(span("+ Lab adj:",
                 inline(
                   tipify(pickerInput(ns("pca_labadj"),NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                 ))),
        div(span("+ Lab offset:",
                 inline(
                   tipify(numericInput(ns("pca_offset"),NULL,value = 0,step = .1, width="75px"),  "this value controls the distance ('offset') of the text label from the specified coordinate in fractions of a character width.")
                 ))),
        div(span("+ Size:",
                 inline(
                   tipify(numericInput(ns("pca_cextext"),NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                 )))
    )

  })
  output$mds_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$mds_colpalette,2)
    req(col[1]!=col[2])
    div(
      span("+ Factor:",
           inline(tipify(pickerInput(ns("mds_symbol_factor"),NULL,choices = rev(colnames(attr(getdata_upload0(),"factors"))), width='125px'), "symbol classification factor"))))
  })
  output$pca_options_plot<-renderUI({
    req(input$pca_options=="pca_plot")
    div(
      div(
        span("+",
             inline(checkboxInput(ns("pca_show_symbols"),"Symbol" ,T, width='75px')))),
      uiOutput(ns("pca_show_symbols_out")),

      div(span("+",
               inline(checkboxInput(ns("pca_show_labels"),"Labels",F)
               ))),
      uiOutput(ns("pca_show_labels_out")),


      div(
        actionLink(
          ns('pca_downp'),span("+ Download",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  output$pca_show_symbols_out<-renderUI({
    req(isTRUE(input$pca_show_symbols))
    div(style="margin-left: 5px",
        div(
          span("+ Shape:",
               inline(pickerInput(inputId=ns("pca_symbol"),
                                  label = NULL,
                                  choices = df_symbol$val,
                                  options=list(container="body"),
                                  choicesOpt = list(content = df_symbol$img), width='75px')))),
        div(
          span("+ Size:",
               inline(numericInput(ns("pca_cexpoint"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
               ))
        ),
        div(class="palette",
            span("+ Color:",
                 inline(
                   tipify(
                     pickerInput(inputId=ns("pca_colpalette"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
        uiOutput(ns("pca_fac_palette"))
    )
  })
  output$pca_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$pca_colpalette,2)
    req(col[1]!=col[2])
    div(
      span("+ Factor:",
           inline(tipify(pickerInput(ns("pca_symbol_factor"),NULL,choices = rev(colnames(attr(getdata_upload0(),"factors"))), width='125px'), "symbol classification factor"))))
  })



  observeEvent(input$downp_summ_num,{
    vals$hand_plot<-"variable summary"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_stats_fac,{
    vals$hand_plot<-"factor summary"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_hist,{
    vals$hand_plot<-"histogram"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$mds_downp,{
    vals$hand_plot<-"mds"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$pca_downp,{
    vals$hand_plot<-"pca"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })



  output$summaries_out<-renderUI({
    if(is.null(vals$curview_summ_options)){vals$curview_summ_options<-'Data'}
    column(12,splitLayout(
      cellWidths = c('10%','90%'),
      column(12,
             fluidRow(style="margin-top: 15px",
                      radioGroupButtons(ns("summ_options"),NULL,choices=c("Data","Variables","Factors","Datalist"), status  ="button_active",justified =F,direction="vertical", selected=vals$curview_summ_options))),
      uiOutput(ns("summ_out"))

      ))
  })
  output$stats_phist<-renderUI({
    fluidRow(
      column(12,actionButton(ns("downp_hist"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
      column(12,renderPlot({

        vals$phist<-phist(getdata_upload0())
        vals$phist
      }))
    )
  })
  observeEvent(input$cextext,{
    vals$cextext<-input$cextext
  })
  output$stats_var<-renderUI({
    if(is.null(vals$cextext)){vals$cextext<-1}
    data=getdata_upload0()
    factors<-data[,unlist(lapply(data,is.factor))]
    numerics<-data[,unlist(lapply(data,is.numeric))]

    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/50))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))

    fluidRow(
      column(12,
             splitLayout(cellWidths = c("10%","45%","45%"),
                         column(12,actionButton(ns('downp_summ_num'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active")),
                         column(12,selectInput(ns("splitdata"),"Variables:", choices=options_show)
                         ),
                         column(12,numericInput(ns("cextext"),"Text size:", value= vals$cextext, step=1, width="100px")))
      ),
      column(12,plotOutput(ns("summ_num"), height = '700px'))
    )



  })
  output$stats_fac<-renderUI({
    column(12,
           column(12,

                  h5(strong("Factors:")),
                  h5(strong("Structure:")),
                  verbatimTextOutput(ns('strlabels')),


           ),
           column(12,
                  column(12,actionButton(ns("downp_stats_fac"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                  column(12,plotOutput(ns("factorsplot")))))
  })
  output$psummary<-renderPrint({
    data=getdata_upload0()
    withProgress(message = "Calculating Numeric-Attribute summary ... Please, wait!",
                 min = 1,
                 max = 13,
                 {

                   nas=sum(is.na(unlist(data)))
                   incProgress(1)

                   n=data.frame(rbind(Param=paste('Missing values:', nas)))
                   incProgress(1)
                   a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))
                   incProgress(1)


                   ppsummary("-------------------")
                   incProgress(1)
                   ppsummary(n)
                   ppsummary("-------------------")
                   incProgress(1)
                   ppsummary(a)
                   ppsummary("-------------------")
                   incProgress(1)


                 })


  })
  output$summ_num<-renderPlot({
    data=getdata_upload0()
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/50))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))


    options_num_sel<-options_num[[which(options_show==input$splitdata)]]


    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    str_numerics(data, cextext=input$cextext)
    vals$varplot<-recordPlot()
  })


  output$pca_fiz<-renderUI({
    renderPlot({
      pca_symbol_factor<-pca_symbol_factor()
      pca<-prcomp(getdata_upload0())


      {
        col_pts=getcolhabs(vals$newcolhabs,input$pca_colpalette,nlevels(pca_symbol_factor))
        gg<-fviz_pca_biplot(pca,geom="points", label="var",habillage=pca_symbol_factor,col.var ='red')
        data2<-data.frame(id=rownames(pca$x),factor=pca_symbol_factor,pca$x[,1:2])
        if(isTRUE(input$pca_show_symbols )){
          gg<-gg+geom_point(data=data2,aes(x = PC1, y = PC2, col=factor), pch=as.numeric(input$pca_symbol), size=input$pca_cexpoint)}
        if(isTRUE(input$pca_show_labels )){
          gg<-gg+geom_text(data=data2,aes(x = PC1, y = PC2, label = factor, col=factor))
        }
        gg

      }
      colorFAC<-  data.frame(prev_fac=levels(pca_symbol_factor),col_pts, levels=1:nlevels(pca_symbol_factor))
      gg<-gg+scale_color_manual(name=input$pca_labfactor,labels =  colorFAC$prev_fac,values =  colorFAC$col_pts,drop=F )
      if(isFALSE(input$biplot)){
        gg$layers<-c(gg$layers[-3])
      } else{
        gg$layers<-c(gg$layers[-3],gg$layers[3])
      }

      gg

    })
  })



  output$summary_pca<-DT::renderDataTable({


    req(!is.null(vals$pca))
    res<- summary(vals$pca)
    center<-res$center
    scale<-res$scale
    sdev<-res$sdev
    res$center<-data.frame(center)
    res$scale<-data.frame(scale)
    res$sdev<-data.frame(sdev)
    res<-lapply(res, data.frame)
    vals$pca_out<-res[[pic_pca_results()]]
    vals$pca_out
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = T,class ='cell-border compact stripe')


  observeEvent(input$data_upload0,{
    req(input$desc_options=='PCA')
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    X   = as.matrix(getdata_upload0())
    vals$pca<-prcomp(X)
  })




  output$mdscustom<-renderPlot({plot_mds()})
  output$stats_ppca<-renderUI({
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    column(      12,
      #uiOutput(ns('pca_fiz')),
      renderPlot({
        req(!is.null(vals$pca))
        suppressWarnings({
          ppca(
            vals$pca,
            key = pca_symbol_factor(),
            points = input$pca_show_symbols,
            text = input$pca_show_labels,
            palette = input$pca_colpalette,
            cex.points = input$pca_cexpoint,
            cex.text = input$pca_cextext,
            pch=pca_symbol(),
            keytext=pca_text_factor(),
            biplot=input$biplot,
            newcolhabs=vals$newcolhabs,
            textcolor=input$pca_labcolor,
            pos=input$pca_labadj,
            offset=input$pca_offset
          )
        })
        vals$ppca_plot<-recordPlot()


      })

    )

  })
  output$stats_pmds<-renderUI({
    validate(need(!anyNA(getdata_upload0()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    res<-list(
      column(12,
             column(12,plotOutput(ns("mdscustom"))))
    )

    res
  })
  output$factorsplot<-renderPlot({
    vals$factorsplot<-pfac(res_pfac())
    vals$factorsplot
  })
  output$stats_data<-renderUI({
    column(
      12, style = "background: white;",
      fluidRow(

        column(12,
               h5(strong(
                 "numeric variables:"
               ))),
        column(6, verbatimTextOutput(ns("psummary"))),
        column(12,uiOutput(ns("Locate_NA")))




      )
    )
  })
  output$Locate_NA<-renderUI({
    req(anyNA(unlist(getdata_upload0())))
    div(
      column(12,strong("Missing Values:")),
      column(12,
             div(  tags$style('#missing_values td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                   tags$style('#missing_values th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),

                   inline(
                     DT::dataTableOutput(ns("missing_values"))
                   )
             ))
    )
  })
  output$missing_values<-DT::renderDataTable({
    data=getdata_upload0()
    res0<-res<-which(is.na(data), arr.ind=TRUE)
    for(i in 1:nrow(res)){
      res0[i,1]<-rownames(data)[res[i,1]]
      res0[i,2]<-colnames(data)[res[i,2]]
    }
    colnames(res0)<-c("ID","Variable")
    rownames(res0)<-NULL
    res0
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = F,class ='cell-border compact stripe')
  output$summ_out<-renderUI({
    column(12,switch (input$summ_options,
                      "Data" = uiOutput(ns("stats_data")),
                      "Variables" = uiOutput(ns("stats_var")),
                      "Factors" = uiOutput(ns("stats_fac")),
                      "Datalist"=datalist_render(getdata_upload0())
    )
    )


  })






  output$strlabels<-renderPrint({

    ppsummary("----------------")
    ppsummary(paste("Missing values:",sum(is.na(attr(getdata_upload0(),"factors")))))
    ppsummary("----------------")
    str(attr(getdata_upload0(),"factors")[rownames(getdata_upload0()),,drop=F])
  })
  ##
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })



  output$save_breakpoints<-renderUI({
    if(is.null(vals$bag_smw)){
      class="novo"
    } else{ class="save_changes"}

    if(!isFALSE(vals$splitBP)){
      if(!any(unlist(lapply(attr(vals$saved_data[[input$segrda_X]],"factors"), function (x) identical(x,as.vector(vals$splitBP)))))){
        popify(
          div(class=class,id=ns("save_changes_bp"),
              bsButton(ns('tools_saveBP'),icon("fas fa-save"),style='save_button')
          ),"Save breakpoints from DP",
          "this action divides the observations according to the breakpoints and assigns a factor to each split"
        )

      }
    }

  })









  output$rid_panel<-renderUI({
    sidebarLayout(
      sidebarPanel(
        class="map_control_style",
        style="color: #05668D",
        uiOutput(ns('rid_side'))),
      mainPanel(uiOutput(ns('rid_out')))
    )
  })
  output$rid_side<-renderUI({
    req(input$data_upload0)
    data<-vals$saved_data[[input$data_upload0]]
    factors<-attr(data,"factors")
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/9))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))

    div(
      div(
        div("+ Show:",
            inline(pickerInput(ns("rid_split"),NULL, choices=options_show, width="200px")))
      ),
      div(class="palette",
          span("+ Palette:",
               inline(
                 pickerInput(inputId=ns("rid_col"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px')))),
      div(
        span("+ Factor:",
             inline(
               pickerInput(inputId=ns("rid_fac"),label = NULL,choices =rev(colnames(factors)),selected=vals$rid_fac,width='120px')))
      ),
      div(
        actionLink(ns("rid_downp"),"Download plot", style="button_active")
      )

    )
  })
  output$boxplot_out<-renderUI({
    sidebarLayout(
      sidebarPanel(uiOutput(ns("editbox"))),
      mainPanel(uiOutput(ns("stats_pbox")))
    )
  })
  output$stats_cbox<-renderUI({
    div(style="background: white",
        p(strong("Box plot")),
        inline(uiOutput(ns("box_y_input"))),
        inline(uiOutput(ns("box_factor"))),
        inline(uiOutput(ns("filter_box1"))),
        inline(uiOutput(ns("filter_box2")))



    )
  })
  output$box_y_input<-renderUI({
    data<-getdata_upload0()
    div(
      div(tipify(
        strong("Y ~"),
        " y is the data values to be split into groups according to the grouping variable", options = list(container="body")
      )),
      pickerInput( ns("box_y"),NULL,choices = colnames(data),selected= vals$box_y, width="200px"
      ))

  })

  output$box_factor<-renderUI({
    div(
      div(strong("Factor:")),
      pickerInput(ns("box_factor"),NULL,
                  choices =rev(colnames(attr(vals$saved_data[[input$data_upload0]],
                                      "factors"))),
                  selected=vals$box_factor, width="200px")
    )
  })
  observeEvent(input$box_factor,{vals$box_y<-input$box_y})
  observeEvent(input$box_factor,{vals$box_factor<-input$box_factor})
  output$filter_box1<-renderUI({
    div(
      div(strong("Filter:")),
      pickerInput(ns("filter_box1"),NULL,choices = c("none", colnames(attr(getdata_upload0(),"factors"))),selected=filter_box1_cur$df, width="200px"))
  })
  output$filter_box2<-renderUI({
    req(input$filter_box1)
    if (input$filter_box1 != "none") {
      data = getdata_upload0()
      labels<-attr(data,"factors")[rownames(data), input$filter_box1]
      div(
        div(strong("Class:")),
        pickerInput(ns("filter_box2"),
                    NULL,
                    choices = c(levels(as.factor(labels))),
                    selected=filter_box2_cur$df, width="200px")
      )
    }
  })
  output$stats_pbox<-renderUI({
    req(input$filter_box1)
    if(input$filter_box1 != "none"){
      req(input$filter_box2)
    }
    div(
      column(12,renderPlot({

        res<-getbox()

        req(length(res)>0)
        req(input$box_palette)
        boxp<-pbox(res=res,
                   cex_pbox=input$cex_pbox,
                   palette=input$box_palette,
                   varwidth=input$varwidth,
                   coefic=1.5,
                   lab_out=labbox(),
                   show_outs=input$show_outs,
                   lwd=as.numeric(input$box_lwd),
                   ylim=c(input$ymin_box, input$ymax_box),
                   main=input$titlebox,
                   insetx=input$insetx,insety=input$insety,
                   xlab.adj=input$box_xlab.adj,
                   srt=input$box_srt,
                   srty=input$box_srt_y,
                   newcolhabs=vals$newcolhabs,
                   showleg=input$showboxleg,
                   mar_b=input$boxmar_b,
                   mar_l=input$boxmar_l,
                   mar_t=input$boxmar_t,
                   mar_r=input$boxmar_r,
                   xsize=input$box_xsize,
                   ysize=input$box_ysize,
                   tck=input$box_tick,
                   ylab_text=input$box_ylab_text,
                   lineylab=input$box_ylab_adj,
                   cex_ylab=input$box_ylab_size,
                   font_ylab=input$box_ylab_font,
                   xlab_text=input$box_xlab_text,
                   linexlab=input$box_xlab_adj,
                   cex_xlab=input$box_xlab_size,
                   font_xlab=input$box_xlab_font,
                   horizontal=input$box_horiz
        )

        vals$pbox_plot<-recordPlot()
        boxp

      }))
    )
  })



  pca_symbol<-reactive({
    if(isFALSE(input$pca_show_symbols)){NA}else{as.numeric(input$pca_symbol)}
  })


  getdata_upload0<-reactive({
    req(input$data_upload0)
    vals$saved_data[[input$data_upload0]]})


  labbox<-reactive({
    if(isTRUE(input$showout)){data.frame(attr(getdata_upload0(),"factors")[as.character(input$out_label)]
    )} else{
      NULL
    }
  })
  titlebox<-reactive({
    if(isFALSE(input$box_horiz)){
      y<-input$box_y
      x<-input$box_factor
    } else{
      x<-input$box_y
      y<-input$box_factor

    }
    a<-paste(y,"~",x)
    if(input$filter_box1!='none'){b=paste0("[",input$filter_box1,"::",input$filter_box2,"]")} else{b=NULL}
    paste(a,b)
  })
  getbox<-reactive({
    req(input$box_factor)
    req(input$box_y)
    req(input$filter_box1)
    data=getdata_upload0()
    labels<-attr(data,"factors")
    pic<-1:nrow(data)
    x<-labels[input$box_factor]
    y<-data[input$box_y]

    if (input$filter_box1 != "none") {
      filtro<-as.character(input$filter_box1)
      filtro2<-as.character(input$filter_box2)
      pic<-which(as.character(labels[, filtro]) == filtro2)


    }
    res = data.frame(x,y)[pic,]
    res[,1]<-res[,1]
    res
   # saveRDS(res,"res.rds")
    #re<-readRDS('res.rds')
  # levels(re$Consenso)
   #vals<-readRDS("savepoint.rds")
   # levels(attr(vals$saved_data[["SANTOS_C1"]],"factors")$Consenso)
  })


  mds_symbol_factor<-reactive({
    req(input$mds_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$mds_colpalette,2)
    if(col[1]!=col[2]){
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$mds_symbol_factor]
    }else{NULL}
  })
  mds_text_factor<-reactive({
    if(isFALSE(input$mds_show_labels)){NULL} else{
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$mds_labfactor]}
  })
  mds_symbol<-reactive({
    if(isFALSE(input$mds_show_symbols)){NA}else{as.numeric(input$mds_symbol)}
  })
  pca_symbol_factor<-reactive({
    req(input$pca_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$pca_colpalette,2)
    if(col[1]!=col[2]){
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$pca_symbol_factor]
    }else{NULL}
  })
  pca_text_factor<-reactive({
    if(isFALSE(input$pca_show_labels)){NULL} else{
      data = getdata_upload0()
      attr(data,"factors")[rownames(data), input$pca_labfactor]}

  })
  plot_mds<-reactive({
    validate(need(input$distance!='', "Select a distance measure for the mds"))
    mds_data = vals$mds
    if (exists("mds_data")) {
      pmds(
        mds_data = mds_data,
        key = mds_symbol_factor(),
        points =input$mds_show_symbols,
        text = input$mds_show_labels,
        palette = input$mds_colpalette,
        cex.points = input$mds_cexpoint,
        cex.text = input$mds_cextext,
        pch=mds_symbol(),
        keytext=mds_text_factor(),
        newcolhabs=vals$newcolhabs,
        textcolor=input$mds_labcolor,
        pos=input$mds_labadj,
        offset=input$mds_offset
      )
    }
    vals$pmds_plot<-recordPlot()
    res




  })
  res_pfac<-reactive({
    attr(getdata_upload0(),"factors")[rownames(getdata_upload0()),,drop=F]
  })


  observeEvent(input$data_upload0,{
    req(length(vals$saved_data)>0)
    vals$cur_data<-input$data_upload0
  })
  observeEvent(input$rid_downp,{
    vals$hand_plot<-"Ridge plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals)})
  observeEvent(input$rid_fac,
               vals$rid_fac<-input$rid_fac)
  observeEvent(input$box_y,{
    box_y_cur$df<-input$box_y
  })
  observeEvent(input$filter_box2,{
    filter_box2_cur$df<-input$filter_box2
  })
  observeEvent(input$filter_box1,{
    filter_box1_cur$df<-input$filter_box1
  })
  observeEvent(input$box_factor,{
    box_factor_cur$df<-input$box_factor
  })
  observeEvent(input$box_horiz,{
    if(isTRUE(input$box_horiz)){

      #updateNumericInputIcon(session,"box_xlab.adj", value=200)
      updateNumericInputIcon(session,"box_xlab_adj", value=5)
      updateNumericInputIcon(session,"boxmar_l", value=7)

    }
  })
  observeEvent(input$downp_box,{
    vals$hand_plot<-'boxplot'
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(list(input$distance,getdata_upload0()),{
    req (input$distance %in%c("bray","euclidean","jaccard"))
    vals$mds<-metaMDS1(getdata_upload0(), distance = input$distance)

  })




  observeEvent(input$downcenter_rda,{
    vals$hand_down<-"rda"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })


  ##################
  ###  SEGRDA    ###
  ### ###############
  output$segrda_header<-renderUI({
    div()
  })
  output$segrda_panels<-renderUI({
    req(input$segrda_X)
    req(input$segrda_Y)

    column(12,

           tabsetPanel(id=ns("segrda_panels"),
                       selected=vals$segrda_panels,

                       tabPanel("SMW",
                                uiOutput(ns("segrda_smw"))

                       ),
                       tabPanel("DP",
                                uiOutput(ns("segrda_dp"))

                       ),

                       tabPanel("pwRDA",
                                p(strong("Piecewise RDA")),
                                uiOutput(ns("pw_out")))))
  })


  observeEvent(input$segrda_Y,{
    vals$cur_segrda_Y<-input$segrda_Y})


  observeEvent(input$segrda_X,{
    vals$cur_segrda_X<-input$segrda_X})

  output$databank_storage<-renderUI({

    div(
      column(12,
             div(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")),
             div(vals$hand_save2,style="color: gray"),
             div(vals$hand_save3))

    )
  })
  output$save_confirm<-renderUI({
    actionButton(ns("data_confirm"),strong("confirm"))
  })
  output$ord_side<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(
               span("+",
                    checkboxInput(ns("segrda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T, width = "100px")
               )
             ),

             div(
               span("+",
                    inline(
                      checkboxInput(ns("segrda_ord"),strong("Axis ordination",pophelp(NULL,"Both the SMW and pwRDA analyses depend on ordered datasets. Defaults to ordering both response and explanatory matrices using one of the axes of the RDA model. If unchecked,please make sure all your inputs are already ordered.")), value=T)
                    ),
                    inline(uiOutput(ns("ord_check")))
               )
             ),
             uiOutput(ns("ord_sure"))


    )
  })
  output$ord_check<-renderUI({
    req(isTRUE(input$segrda_ord))
    inline(numericInput(ns("axis_ord_segrda"),NULL, value=1, step=1, width="75px"))
  })
  output$ord_sure<-renderUI({
    req(isFALSE(input$segrda_ord))
    div(style='white-space: normal;',
        strong("Wargning:", style="color: SeaGreen"),"Make sure both X and Y are previously ordered")
  })
  output$ordplot_matrix<-renderUI({
    req(isTRUE(input$segrda_ord))

    #mybreaks<-vals$window_pool

    fluidRow(
      renderPlot({
        sim1o<-getord()
        #sim1o<-readRDS('sim1o.rds')
        mybreaks<-vals$window_pool
        #mybreaks<-c(2,50,141)
        xo<-sim1o$xo ## ordered explanatory matrix.
        yo<-sim1o$yo ## ordered community matrix (untransformed).
        x<-sim1o$y
        par(mfrow = c(1, 2), mgp = c(1, 1, 0), cex = 0.9)
        image(x, main = "Original response data", col = topo.colors(100), axes = F,

              xlab = "Observations", ylab = "Variable values")
       # abline(v=scales::rescale(mybreaks,c(0,1)), col="red")


        image(yo, main = "Ordered response data", col = topo.colors(100), axes = F,
              xlab = "Observations", ylab = "Variable values")
        #abline(v=scales::rescale(mybreaks,c(0,1)), col="red")


      })
    )
  })
  output$segrda_smw<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(uiOutput(ns('side_smw')),
                     br(),
                     div(style="margin-left: 20px",
                         actionButton(ns("go_smw"),strong( img(src=smw_icon,height='20',width='20'),"run SMW"), style="button_active")
                     )
        ),
        mainPanel(
          tabsetPanel(id=ns("smw_panels"),
                      tabPanel("Data ordination",
                               value="swm_1",
                               uiOutput(ns("ordplot_matrix"))),
                      tabPanel("SMW results",
                               value="swm_2",
                               uiOutput(ns("go_smw")))
          )
        )
      )
    )
  })
  output$ord_windows<-renderUI({
    div(
      tipify(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breakpoints (comma delimited, within the data range)"),
      "+ Windows",
      textInput(ns('custom_windows'), NULL, paste0(get_windows(),collapse=", "), width="200px"),

    )
  })

  get_windows<-reactive({
    req(input$segrda_X)
    data<-vals$saved_data[[input$segrda_X]]
    req(nrow(data)>0)
    w<-1:(nrow(data)/2)
    w<-  w[which(( w %% 2) == 0)]
    w<-round(seq(10,w[length(w)], length.out=5))
    w[which(( w %% 2) != 0)]<-w[which(( w %% 2) != 0)]+1
    w
  })
  is.even<-function(x){ x %% 2 == 0}
  getpool<-reactive({
    mybreaks<-NULL
    req(input$segrda_X)
    req(length(input$custom_windows)>0)
    req(!is.na(input$custom_windows))
    mybreaks<-as.numeric(unlist(strsplit(input$custom_windows,",")))
    data<-vals$saved_data[[input$segrda_X]]
    cond0<-length(mybreaks)>0
    validate(need(cond0,"The windows vector is empty"))
    cond1<-sum(sapply(mybreaks,is.even))==length(mybreaks)
    cond2<-min(mybreaks)>=2
    cond2_res<-paste("The maximum window size cannot exceed the number of observations:", nrow(data))
    validate(need(cond1,"Window sizes must be even"))
    validate(need(cond2,"The minimum allowed size of the windows is 2"))
    validate(need(max(mybreaks)<=nrow(data),cond2_res))
    mybreaks

  })
  output$smw_tuning<-renderUI({
    div(
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"Dissimilarity index"),"+ Distance:"),
             inline(
               pickerInput(ns("smw_dist"),NULL, choices=c("bray","euclidean","manhattan","jaccard"), width="100px")
             )
        )
      ),
      div(
        span(span(tipify(actionLink(ns("smw_rand_help"),icon("fas fa-question-circle")),"The type of randomization for significance computation. Click for details"),"+ Randomization:"),
             inline(
               pickerInput(ns("smw_rand"),NULL, choices=c("shift","plot"), width="70px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The number of randomizations"),"+ n.rand:"),
             inline(
               numericInput(ns("smw_nrand"),NULL, value=10, width="100px")
             )
        )
      ),
      uiOutput(ns("down_we_out"))
    )
  })
  output$down_we_out<-renderUI({
    req(length(vals$smw_dp)>0)
    tipify(
      actionLink(
        ns('downp_we'),span("+ Download",icon("fas fa-download")), style="button_active"
      ),
      "Download plot"
    )
  })

  output$side_smw<-renderUI({
    fluidRow( class="map_control_style",style="color: #05668D",
              uiOutput(ns("ord_side")),
              uiOutput(ns("ord_windows")),
              uiOutput(ns("smw_tuning"))
    )
  })
  output$go_smw<-renderUI({
    getpool()
    validate(need(!is.null(vals$window_pool),"The window pool is empty. Use the arrow button to include the window sizes"))
    validate(need(length(vals$smw_dp)>0,"Please click 'run SMW' button"))



    fluidRow(
      column(12,

             renderPlot({



               if(length(vals$window_pool)>1){w.effect=TRUE
               main="Window size effect"} else{w.effect=F
               main="Dissimilary Profile"}

               suppressWarnings(plot( vals$smw_dp, w.effect =w.effect  , main=main))
               vals$plot_we<-recordPlot()

             })
      )
    )
  })

  observeEvent(vals$max_seq,{

    req(!is.null(vals$max_seq))
    vals$dp_seq.sig<-attr(max_seq(),"min")
  })

  output$plot_dp<-renderPlot({
    req(input$dp_view=='Plot')
    req(input$dp_seq.sig)
    req(input$dp_cex)
    req(input$dp_BPs)
    max_seq<-max_seq()
    validate(need(input$dp_seq.sig<=max_seq,paste(
      "DP shows '", sum(DP_smw()[,5]!="ns"),"' significant dissimilarity values but no breakpoint could be determined for seq.sig='",input$dp_seq.sig,"The maximum value for this input must be","max_seq"
    )))
    smw<- vals$smw_dp
    getDP()
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    par(cex=input$dp_cex)
    suppressWarnings(
      suppressMessages(
        plot(smw,w=dp_w, sig=input$dp_sig, z=input$dp_z,   BPs=dp_BPs,
             seq.sig=input$dp_seq.sig, bg= getcolhabs(vals$newcolhabs,input$dp_palette,nlevels(as.factor(vals$splitBP[,1]))),bg_alpha=input$dp_bg,cols=c(getcolhabs(vals$newcolhabs,input$dp_dcol,1),getcolhabs(vals$newcolhabs,input$dp_scol,1),getcolhabs(vals$newcolhabs,input$dp_bcol,1)))
      )
    )

    if(isTRUE(updp$df)){
      updateTabsetPanel(session,'segrda_panels','pwRDA')
      updp$df<-F
    }

    vals$plot_dp<-recordPlot()
  })
  output$dp_extract<-renderUI({
    req(input$dp_view=='DP results')
    dp<-getDP()
    fluidRow(
      column(12,renderPrint({dp}))
      #renderPrint({vals$splitBP})
    )
  })
  output$dp_view_dp<-renderUI({
    req(input$dp_view=='DP results')
    tipify(
      actionLink(
        ns('downcenter_dp_smw'),span("+ Download",icon("fas fa-table")), style="button_active"
      ),
      "Download DP results"
    )
  })
  observeEvent(input$dp_seq.sig,{
    vals$dp_seq.sig<-input$dp_seq.sig
  })
  output$side_dp<-renderUI({
    if(is.null(vals$dp_seq.sig)){
      vals$dp_seq.sig<-3
    }

    validate(need(length(vals$smw_dp)>0,"You need to run SMW analysis first"))
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"A target window size from which results will be extracted. If empty return z-scores averaged over the set of window sizes"),'+ w:'),
             inline(
               numericInput(ns("dp_w"),NULL, value=NULL, width="75px")
             )
        )
      ),
      div(
        span(span(actionLink(ns("dp_index_help"),tipify(icon("fas fa-question-circle"),"The result to be extracted. Click for details")),'+ index:'),
             inline(
               pickerInput(ns("dp_index"),NULL,choices=c("dp","rdp","md","sd","oem","osd","params"), width="75px")
             )
        )
      ),
      div(
        span(span(actionLink(ns("dp_sig_help"),tipify(icon("fas fa-question-circle"),"Significance test for detecting dissimilarity values that differs significantly from those appearing in a random pattern. Click for details")),'+ sig'),
             inline(
               pickerInput(ns("dp_sig"),NULL,choices=c("z","sd","sd2","tail1"), width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The critical value for the significance of z-values"),"+ z:"),
             inline(
               numericInput(ns("dp_z"),NULL, value=1.85,step=0.01, width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"Defines if the breakpoints should be chosen as those sample positions corresponding to the maximum dissimilarity in a sequence of significant values (max) or as those sample positions corresponding to the median position of the sequence (median). Defaults to BPs=max. If empty the breakpoints are not computed"),"+ BPs:"),
             inline(
               pickerInput(ns("dp_BPs"),NULL,choices=c("","max","median"), selected = "max", width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The maximum length of consecutive, significant values of dissimilarity that will be considered in defining the community breakpoints"),"+ seq.sig:"),
             inline(
               numericInput(ns("dp_seq.sig"),NULL, value=vals$dp_seq.sig,step=1, width="75px", min=1)
             )
        )
      ),

      div(uiOutput(ns("dp_view_dp"))),
      div(uiOutput(ns("dp_view_plot")))


    )

  })
  output$dp_view_plot<-renderUI({
    req(input$dp_view=='Plot')
    div(
      div(class="palette",
          span("+ Palette",
               inline(
                 pickerInput(inputId=ns("dp_palette"),
                             label = NULL,
                             choices =     vals$colors_img$val[getgrad_col()],
                             choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), width="75px")
               )
          )
      ),

      div(
        span("+ size",
             inline(
               numericInput(ns("dp_cex"), NULL,value=1,min=0.1,step=0.1, width="75px")
             )
        )
      ),

      div(
        span("+ diss col",
             inline(
               pickerInput(inputId=ns("dp_dcol"),
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), width="75px")
             )
        )
      ),
      div(
        span("+ sig col",
             inline(
               pickerInput(inputId=ns("dp_scol"),
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), selected=  vals$colors_img$val[getsolid_col()][4], width="75px")
             )
        )
      ),
      div(
        span("+ bp col",
             inline(
               pickerInput(inputId=ns("dp_bcol"),
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), selected=  vals$colors_img$val[getsolid_col()][3], width="75px")
             )
        )
      ),

      div(
        span("+ bg",
             inline(numericInput(ns("dp_bg"), NULL,value=0.5,min=0,max=1,step=0.05, width="75px"))
        )
      ),
      tipify(
        actionLink(
          ns('downp_dp'),span("+ Download",icon("fas fa-download"))
        ),
        "Download plot"
      )
    )
  })
  output$segrda_dp<-renderUI({
    validate(need(length(vals$smw_dp)>0,"You need to run SMW analysis first"))
    fluidRow(
      column(12,
             div(strong("Dissimilary Profile"),
                 inline(uiOutput(ns("save_breakpoints")))),),
      sidebarLayout(
        sidebarPanel(uiOutput(
          ns("side_dp")
        )),
        mainPanel(
          tabsetPanel(id=ns("dp_view"),selected=vals$dp_view,
                      tabPanel("Plot", value="Plot",
                               plotOutput(ns("plot_dp"))),
                      tabPanel("DP results", value="DP results",
                               uiOutput(ns("dp_extract"))))


        )
      )
    )
  })
  observeEvent(input$dp_view,
               vals$dp_view<-input$dp_view)
  get_breaks_from_factor<-reactive({
    x<-vals$saved_data[[input$segrda_X]]
    y<-vals$saved_data[[input$segrda_Y]]
    bp<-attr(x,"factors")[,input$bp_column]
    xord<-x[order(as.numeric(bp)),]
    yord<-y[order(as.numeric(bp)),]
    breaks<-bp[order(as.numeric(bp))]
    breaks<-which(diff(as.numeric(breaks))==1)
    pw_in<-list(
      breaks=breaks,
      xord=xord,
      yord=yord
    )
  })

  observeEvent(input$segrda_X,{
    x<-attr(vals$saved_data[[input$segrda_X]],"factors")
  })

  choices_bp_column<-reactive({
    req(input$segrda_X)
    x<-attr(vals$saved_data[[input$segrda_X]],"factors")
    colnames(x)
  })

  observeEvent(input$bp_column,
               vals$bp_column<-input$bp_column)

  output$user_bp<-renderPrint(vals$bag_user_bp)
  output$getDP<-renderPrint({
    req(!isFALSE(vals$splitBP))
    suppressWarnings(bp(getDP()))})

  output$pw_out<-renderUI({

    div(
      span(
        inline(
          div(
            tipify(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breakpoints (comma delimited, within the data range)"),
            "+ Split reference [Y Datalist]",
            pickerInput(ns('bp_column'), NULL,choices_bp_column() , width="200px", selected=vals$bp_column)
          )
        ),
        inline(numericInput(ns("pw_nrand"), 'n.rand', value=99, width="75px")),
        inline(actionButton(ns("run_pwrda"),strong(img(src=pw_icon,height='20',width='20'),"run pwRDA"), style="button_active")),

        inline(uiOutput(ns('pwrda_models_out'))),

      ),
      uiOutput(ns("pwRDA_out"))
    )
  })
  output$pwrda_models_out<-renderUI({
    choices<-names(attr(vals$saved_data[[input$segrda_X]],"pwrda"))
    req(length(choices)>0)
    div(
      inline(pickerInput(ns('pwrda_models'),"Results",choices, width="200px", selected=vals$pwrda_models)),
      inline(uiOutput(ns('save_pw'))),
      inline(actionButton(ns("delete_pwrda_models"),icon("fas fa-trash-alt")))
    )
  })

  observeEvent(input$delete_pwrda_models,{
    attr(vals$saved_data[[input$segrda_X]],"pwrda")[[input$pwrda_models]]<-NULL
  })
  output$save_pw<-renderUI({
    if(is.null(vals$bag_pw)){
      class="novo"
    } else{ class="save_changes"}
    div(class=class,
        actionButton(ns("save_pwrda"),icon("fas fa-save"), style="button_active")
    )
  })
  observeEvent(input$pwrda_models,
               vals$pwrda_models<-input$pwrda_models)
  observeEvent(input$save_pwrda,{
    vals$hand_save<-"Save pwRDA model in"
    vals$hand_save2<-div(span("Target:",em(input$segrda_X,style="color: SeaGreen")))
    vals$hand_save3<-NULL
    showModal(module_desctools())
  })
  observeEvent(input$disegrda_sp_summ,
               vals$disegrda_sp_summ<-input$disegrda_sp_summ)

  output$segrda_view_out<-renderUI({
    req(input$segrda_view=='Summary')

    div(
      div(
        span(
          "+ Results:",
          inline(
            pickerInput(ns("disegrda_sp_summ"),NULL,choices=c('Summary stats','Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot'), selected=vals$disegrda_sp_summ,  options=list(container="body"), width="200px")
          )
        )
      ),
      div(
        span("+ Axes",
             inline(
               numericInput(ns("segrda_axes"),NULL, value=2)
             )
        )
      ),
      div(
        tipify(
          actionLink(
            ns('downcenter_segrda'),span("+ Download",icon("fas fa-download")), style="button_active"
          ),
          "Download selected results"
        )

      )
    )

  })
  output$side_pw<-renderUI({
    #req(length(vals$segrda_model)>0)
    #req(length(vals$segrda_model$rda.pw)>0)
    sidebarPanel(
      fluidRow(class="map_control_style",style="color: #05668D",
               uiOutput(ns("segrda_view_out")),
               uiOutput(ns("segrda_view_plot"))


      )
    )
  })
  observeEvent(input$segrda_view,
               vals$segrda_view<-input$segrda_view)
  output$segrda_sp_shape<-renderUI({
    req(input$segrda_sp_display=='Shape')

    inline(pickerInput(
      inputId=ns("segrda_spshape"),
      label = NULL,
      choices = df_symbol$val,
      options=list(container="body"),
      selected=df_symbol$val[8],
      choicesOpt = list(content = df_symbol$img),width='35px'
    ))

  })
  output$segrda_sp_on<-renderUI({
    req(isTRUE(input$segrda_sp))

    div(
      div(
        '+ Number:',
        inline(numericInput(ns("segrda_spnum"),NULL, 10, step=1)),tipify(icon("fas fa-question-circle",style="color: gray"),"Show N variables with the highest scores", placement = "bottom")
      ),
      div(
        "+ Display:",
        span(
          inline(
            pickerInput(ns("segrda_sp_display"),NULL,choices=c("Label","Shape"), width = "75px")
          ),
          uiOutput(ns("segrda_sp_shape"))
        )
      ),



      div(
        span("+ Variable Color:",
             inline(
               tipify(
                 pickerInput(
                   inputId=ns("segrda_spcolor"),label = NULL,selected=  vals$colors_img$val[getsolid_col()][4],choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),       options=list(container="body")), "Variable Color.")))
      )
    )

  })
  output$segrda_show_symbol_on<-renderUI({
    req(isTRUE(input$segrda_show_symbols))

    div(style="margin-left: 5px",
        div(
          span("+ Shape:",
               inline(pickerInput(inputId=ns("segrda_symbol"),
                                  label = NULL,
                                  choices = df_symbol$val,
                                  options=list(container="body"),
                                  choicesOpt = list(content = df_symbol$img), width='75px')))),
        div(
          span("+ Size:",
               inline(numericInput(ns("segrda_cexpoint"),NULL,value = 1,min = 0.1,max = 3,step = .1)
               ))
        ),
        div(class="palette",
            span("+ Color:",
                 inline(
                   tipify(
                     pickerInput(inputId=ns("segrda_colpalette"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1]), "Symbol palette. Choose a gradient to color observations by a factor")))),
        uiOutput(ns("segrda_fac_palette"))
    )

  })
  output$segrda_show_labels_on<-renderUI({
    req(isTRUE(input$segrda_show_labels))

    div(style="margin-left: 5px",
        div(span("+ Factor:",
                 inline(tipify(pickerInput(ns("segrda_labfactor"),NULL,choices = colnames(attr(vals$saved_data[[input$segrda_X]],"factors")), width="125px"),  "label classification factor")
                 ))),

        div(span("+ Lab Color:",
                 inline(tipify(
                   pickerInput(
                     inputId=ns("segrda_labcolor"),
                     label = NULL,
                     selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                     options=list(container="body")
                   ),  "label classification factor"
                 )
                 ))),
        div(span("+ Lab adj:",
                 inline(
                   tipify(pickerInput(ns("segrda_labadj"),NULL,choices=c(1:4),  options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                 ))),
        div(span("+ Lab offset:",
                 inline(
                   tipify(numericInput(ns("segrda_offset"),NULL,value = 0,step = .1),  "this value controls the distance ('offset') of the text label from the specified coordinate in fractions of a character width.")
                 ))),
        div(span("+ Size:",
                 inline(
                   tipify(numericInput(ns("segrda_cextext"),NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                 )))

    )

  })
  output$segrda_view_plot<-renderUI({
    req(input$segrda_view=='Plot')

    div(
      div(
        '+ Scaling:',
        inline(numericInput(ns("segrda_scaling"),NULL, 2, step=1, min=0, max=3)),tipify(icon("fas fa-question-circle",style="color: gray"),"Scaling for species and site scores. Either species (2) or site (1) scores are scaled by eigenvalues, and the other set of scores is left unscaled, or with 3 both are scaled symmetrically by square root of eigenvalues.", placement = "bottom")
      ),
      div(span("+",checkboxInput(ns("segrda_biplot"), span("Biplot", pophelp(NULL,"show biplot arrows")), T))),
      div(span("+",checkboxInput(ns("segrda_sp"), span("Variables", pophelp(NULL,"Variables")), T))),
      uiOutput(ns("segrda_sp_on")),
      div(
        span("+",
             inline(checkboxInput(ns("segrda_show_symbols"),"Symbol" ,T)))),
      uiOutput(ns("segrda_show_symbol_on")),
      div(span("+",
               inline(checkboxInput(ns("segrda_show_labels"),"Labels",F)
               ))),
      uiOutput(ns("segrda_show_labels_on")),


      div(
        tipify(
          actionLink(
            ns('downp_pw'),span("+ Download",icon("fas fa-download")), style="button_active"
          ), "Download plot"
        ))
    )

  })
  output$segrda_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$segrda_colpalette,2)
    req(col[1]!=col[2])

    div(
      span("+ Factor:",
           inline(tipify(pickerInput(ns("segrda_symbol_factor"),NULL,choices = rev(colnames(attr(vals$saved_data[[input$segrda_X]],"factors"))), width='125px'), "symbol classification factor"))))
  })
  output$segrda_print<-renderPrint({
    req(input$segrda_view=='Summary')
    segrda_summary()})
  output$pwRDA_out<-renderUI({
    validate(need(length(getBP())>0, "No breakpoints found"))
    fluidRow(
      sidebarLayout(
        uiOutput(ns("side_pw")),
        mainPanel(
          tabsetPanel(id=ns("segrda_view"),selected=vals$segrda_view,
                      tabPanel("Plot", value="Plot",
                               plotOutput(ns("ppwRDA"))),
                      tabPanel("Summary", value="Summary",
                               verbatimTextOutput(ns("segrda_print"))))


        )

      )
    )
  })

  max_seq<-reactive({
    validate(need(any(DP_smw()[,5]!='ns'),"DP without any significant dissimilarity value: no breakpoint could be determined."))

    df<-DP_smw()[,c(1,5)]
    colnames(df)<-c("a","b")
    new=transform(df, Counter = ave(a, rleid(b), FUN = seq_along))
    max_seq<-max(new[new[,2]=="*",3])
    attr(max_seq,"min")<-if(max_seq!=1){
      min( new[new[,2]=="*",3][new[new[,2]=="*",3]!=1])} else{
        min(new[new[,2]=="*",3])
      }



    vals$max_seq<-max_seq
  })
  DP_smw<-reactive({
    smw<- vals$smw_dp
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    res<-suppressWarnings(
      suppressMessages(
        extract(smw,w=dp_w,
                index=input$dp_index,
                sig=input$dp_sig,
                z=input$dp_z,
                BPs=dp_BPs,
                seq.sig=input$dp_seq.sig)
      )
    )
    vals$dp_smw<-res
    vals$dp_smw
  })
  getDP<-reactive({
    smw<- vals$smw_dp
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    dp<-DP_smw()
    colnames(dp)[2]<-"SampleID"

    sim1o<-getord()
    yo<-data.frame(sim1o$yo)
    bps<-suppressWarnings(bp(dp))

    to_split<-c(1,rep(1:(length(bps)+1),
                      diff(c(1,bps, nrow(yo)))))
    splits<-lapply(split(yo,to_split),function(x) rownames(x))
    data_empty<-data.frame(attr(vals$saved_data[[input$segrda_X]],"factors")[,1,drop=F])
    data_empty[,1]<-as.numeric(data_empty[,1])
    for(i in 1:length(splits)){
      data_empty[splits[[i]],1]<-i
    }


    vals$splitBP<- data_empty
    dp
  })
  getord<-reactive({
    req(input$segrda_Y)
    req(input$segrda_X)
    req(input$axis_ord_segrda)
    x<-as.matrix(vals$saved_data[[input$segrda_X]])
    colnames(x)<-colnames(vals$saved_data[[input$segrda_X]])
    y<-na.omit(as.matrix(vals$saved_data[[input$segrda_Y]][rownames(x),,drop=F]))
    colnames(y)<-colnames(vals$saved_data[[input$segrda_Y]])
    x<-na.omit(x[rownames(y),])
    if(isTRUE(input$segrda_ord)){
      sim1o<-OrdData(x=y,y=x, axis=input$axis_ord_segrda,scale=input$segrda_scale)} else{
        sim1o<-list()
        sim1o$xo<-y
        sim1o$yo<-x
      }
    sim1o

  })


  save_bpfac<-reactive({
    vals$bagbp0<-vals$bagbp0+1
    dp<-getDP()
    newfac<-vals$splitBP
    factors<-attr(vals$saved_data[[input$segrda_X]],"factors")

    if(input$hand_save=="create") {
      attr(vals$saved_data[[input$segrda_X]],"factors")[rownames(vals$splitBP),input$newdatalist]<-as.factor(vals$splitBP[,1])
    } else{
      attr(vals$saved_data[[input$segrda_X]],"factors")[rownames(vals$splitBP),input$over_datalist]<-as.factor(vals$splitBP[,1])
    }

    vals$bag_smw<-NULL

  })


  getBP<-reactive({
    pw_in<-get_breaks_from_factor()
    breaks=pw_in$breaks
    breaks
  })
  segrda_summary<-reactive({
    res<-summary(vals$segrda_model$rda.pw)
    res<-switch(input$disegrda_sp_summ,
                "Summary stats"= vals$segrda_model$summ,
                "Variable scores"=res$species,
                "Observation scores"=res$sites,
                "Linear constraints"=res$constraints,
                "Biplot"=res$biplot,

                "Importance (unconstrained)"=res$cont$importance,
                "Importance (constrained)"=res$concont$importance

    )
    vals$segrda_summary<-res[,1:input$segrda_axes]
    vals$segrda_summary

  })


  observeEvent(input$segrda_panels,{
    vals$segrda_panels<-input$segrda_panels
  })



  observeEvent(input$dp_index_help,{
    showModal(
      modalDialog(
        column(12,
               p(strong("dp:"),span("The dissimilarity profile (DP) table containing significant discontinuities and suggested breakpoints")),
               p(strong("rdp:"),span("data frame containing the randomized DP;")),
               p(strong("md:"),span("mean dissimilarity of the randomized DP")),
               p(strong("sd:"),span("standard deviation for each sample position")),
               p(strong("ospan:"),span("overall expected mean dissimilarity;")),
               p(strong("osd:"),span("average standard deviation for the dissimilarities;")),
               p(strong("params:"),span("list with input arguments"))
        ),
        easyClose = T,
        size="m",
        title="index for extracting SMW results"
      )
    )

  })
  observeEvent(input$dp_sig_help,{
    showModal(
      modalDialog(
        column(12,
               p(strong("z:"),span("consider normalized dissimilarity (z-scores) discontinuities that exceed a z critical value")),
               p(strong("sd:"),span("consider dissimilarity discontinuities that exceed mean plus one standard deviation")),
               p(strong("sd2:"),span("consider dissimilarity discontinuities that exceed mean plus two standard deviation")),
               p(strong("tail1:"),span("Consider dissimilarity discontinuities that exceed 95 percent confidence limits"))

        ),
        easyClose = T,
        size="m",
        title="Significance test fort the SMW results"
      )
    )

  })
  observeEvent(input$inc_bp,{    vals$bag_user_bp<-c(vals$bag_user_bp,input$pw_user)
  })
  observeEvent(input$remove_breaks,{    vals$bag_user_bp<-NULL
  })
  observeEvent(input$downcenter_segrda,{
    vals$hand_down<-"segRDA"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  observeEvent(input$go_smw,{
    vals$window_pool<-getpool()
    updateTabsetPanel(session,"smw_panels",selected='swm_2')
    vals$bag_smw<-T
    bag_smw$df<-T

    req(length(vals$window_pool)>0)
    sim1o<-getord()

    xo<-sim1o$xo ## ordered explanatory matrix.
    yo<-sim1o$yo ## ordered community matrix (untransformed)
    y=yo;ws=vals$window_pool; dist=input$smw_dist;rand=input$smw_rand;n.rand=input$smw_nrand
    if (n.rand < 2) {
      stop("number of randomizations not alowed")
    }
    if (any(ws%%2 == 1)) {
      stop("all Window sizes must be enven")
    }
    rand<-match.arg(rand, c("shift", "plot"))
    argg<-c(as.list(environment()), list())
    smw<-list()


    withProgress(message = paste0("SMW analysis (", 1, "/", length(ws), "); w =",
                                  ws[1]),
                 min = 1,
                 max = length(ws),
                 {
                   for (j in 1:length(ws)) {
                     w1<-ws[j]
                     DPtable<-smw.root2(yo, w1, dist)
                     OB<-DPtable[, 3]
                     rdp<-data.frame(rep(NA, length(OB)))
                     seq_yo<-1:nrow(yo)
                     withProgress(message="randomizing",min = 1,
                                  max = n.rand,{
                                    for (b in 1:n.rand) {
                                      if (rand == "shift") {

                                        comm.rand<-apply(yo, 2, function(sp) sp[sample(seq_yo)])
                                        rdp[b]<-smw.root2(data.frame(comm.rand),
                                                          w1, dist)[3]

                                      } else if (rand == "plot") {

                                        comm.rand<-t(apply(yo, 1, function(sp) sp[sample(seq_yo)]))
                                        rdp[b]<-smw.root2(data.frame(comm.rand),
                                                          w1, dist)[3]
                                      }

                                      incProgress(1)
                                    }
                                  })

                     rownames(rdp)<-DPtable[,1]
                     Dmean<-apply(rdp, 1, mean)
                     SD<-apply(rdp, 1, sd)
                     oem<-sum(Dmean)/(nrow(yo) - w1)
                     osd<-sum(SD)/(nrow(yo) - w1)
                     Dz<-(OB - oem)/osd
                     DPtable$zscore<-Dz
                     smw[[j]]<-list(dp = data.frame(DPtable), rdp = matrix(rdp),
                                    md = Dmean, sd = SD, oem = oem, osd = osd, params = argg)
                     class(smw[[j]])<-c("smw")
                     incProgress(1, message=paste0("SMW analysis (", j+1, "/", length(ws), "); w =",
                                                   ws[j+1]))
                   }
                 })


    names(smw)<-paste("w", ws, sep = "")
    class(smw)<-c("smw")
    vals$smw_dp<-isolate(smw)


  })
  observeEvent(input$downcenter_dp_smw,{
    vals$hand_down<-"DP smw"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$tools_saveBP,{
    vals$hand_save<-"Create factor using breakpoints from the dissimilarity profile"
    vals$hand_save2<-div(span("Target:",em(input$segrda_X,style="color: SeaGreen")))
    vals$hand_save3<-NULL
    showModal(module_desctools())
  })
  savenames<-reactive({
    switch(
      vals$hand_save,
      "Create factor using breakpoints from the dissimilarity profile"= {c(paste0("BP_"),nlevels(as.factor(vals$splitBP[,1])))},
      "Save pwRDA model in"={
        paste0(input$segrda_X,"~",input$segrda_Y,"[factor:",input$bp_column,"]")
      })
  })

  observeEvent( input$data_confirm,{
    req(!is.null(vals$hand_save))
    switch(
      vals$hand_save,
      "Create factor using breakpoints from the dissimilarity profile"= {save_bpfac()},
      "Save pwRDA model in"={
        save_pwrda()
      })
    removeModal()

  })
  save_pwrda<-reactive({
    attr(vals$saved_data[[input$segrda_X]],"pwrda")[[input$newdatalist]]<-vals$segrda_model
    attr(vals$saved_data[[input$segrda_X]],"pwrda")[["pwRda (unsaved)"]]<-NULL
    updatePickerInput(session,'pwrda_models', selected=input$newdatalist)
    vals$bag_pw<-NULL
  })
  observeEvent(input$downp_we,{
    vals$hand_plot<-"we"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_dp,{

    vals$hand_plot<-"dp"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_pw,{

    vals$hand_plot<-"segrda"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$pwrda_models)
    names(attr(vals$saved_data[[input$segrda_X]],"pwrda"))
    vals$segrda_model<-attr(vals$saved_data[[input$segrda_X]],"pwrda")[[input$pwrda_models]]
  })
  observeEvent(input$run_pwrda,{
    vals$bag_pw<-T
    pw_in<-get_breaks_from_factor()
    breaks=pw_in$breaks
    sim1o<-list()
    sim1o$xo<-pw_in$yord
    sim1o$yo<-pw_in$xord

    model=suppressMessages(
      try(
        pwRDA2(sim1o$xo,sim1o$yo , BPs=breaks,n.rand = input$pw_nrand)
      )
    )
    if(is.null(attr(vals$saved_data[[input$segrda_X]],"pwrda"))){
      attr(vals$saved_data[[input$segrda_X]],"pwrda")<-list()
    }
    attr(vals$saved_data[[input$segrda_X]],"pwrda")[["pwRda (unsaved)"]]<-model
  })

  savereac<-reactive({
    vals<-reactiveValuesToList(vals)
    vals2<-vals[-c(which(names(vals)=='saved_data'))]
    vals3<-vals2[-which(unlist(
      lapply(vals2,function(x) object.size(x)))>=50000)]
    vals3$saved_data<-vals$saved_data
    attr(vals3,"imesc")<-'imesc'
    saveRDS(vals3,'savepoint.rds')
    saveRDS(reactiveValuesToList(input),'input.rds')
    beep(10)
  })

  segrda_symbol_factor<-reactive({
    req(input$segrda_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$segrda_colpalette,2)
    symbol_factor<-if(col[1]!=col[2]){
      data = vals$saved_data[[input$segrda_X]]
      res<-attr(data,"factors")[rownames(data), input$segrda_symbol_factor]
      names(res)<-rownames(data)
      res
    }else{NULL}
    symbol_factor
  })

  segrda_text_factor<-reactive({
    text_factor<-if(isFALSE(input$segrda_show_labels)){NULL} else{
      data = vals$saved_data[[input$segrda_X]]
      res<-attr(data,"factors")[, input$segrda_labfactor]
      names(res)<-rownames(data)
      res
    }
    text_factor
  })
  segrda_symbol<-reactive({
    rda_symbol<-if(isFALSE(input$segrda_show_symbols)){NA}else{as.numeric(input$segrda_symbol)}

  })
  output$ppwRDA<-renderPlot({

    symbol_factor<-segrda_symbol_factor()
    rda_symbol<-segrda_symbol()
    text_factor<-segrda_text_factor()
    text_factor<-text_factor[rownames(scores(vals$segrda_model$rda.pw)$sites)]
    symbol_factor<-symbol_factor[rownames(scores(vals$segrda_model$rda.pw)$sites)]

    segrda<-prda(vals$segrda_model$rda.pw,
                 key = symbol_factor,
                 points =input$segrda_show_symbols,
                 text = input$segrda_show_labels,
                 palette = input$segrda_colpalette,
                 cex.points = input$segrda_cexpoint,
                 cex.text = input$segrda_cextext,
                 pch=c(rda_symbol,3),
                 keytext=text_factor,
                 biplot=input$segrda_biplot,
                 show.sp=input$segrda_sp,
                 n.sp=input$segrda_spnum,
                 sp.display=input$segrda_sp_display,
                 pch.sp=as.numeric(input$segrda_spshape),

                 col.sp=getcolhabs(vals$newcolhabs,input$segrda_spcolor,1),
                 textcolor=input$segrda_labcolor,
                 scaling=input$segrda_scaling,
                 newcolhabs=vals$newcolhabs,
                 pos=input$segrda_labadj,
                 offset=input$segrda_offset
    )
    vals$seg_rda_plot<-recordPlot()
    segrda

  })





  #####
  ## RDA
  ##
  ##


  output$rda_fac_palette<-renderUI({
    col<-getcolhabs(vals$newcolhabs,input$rda_colpalette,2)
    req(col[1]!=col[2])
    div(
      span("+ Factor:",
           inline(tipify(pickerInput(ns("rda_symbol_factor"),NULL,choices = rev(colnames(attr(vals$saved_data[[input$rda_X]],"factors"))), width='125px'), "symbol classification factor"))))
  })
  output$stats_crda<-renderUI({
    validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
    column(12,style="background: white",
           p(strong("Redundancy Analysis")),
           span(
             inline(
               span(style="width: 150px",

                    inline(uiOutput(ns("rda_Y")))
               )
             ),
             inline(uiOutput(ns("rda_X")))
           )

    )
  })
  output$rda_X<-renderUI({
    pickerInput(ns("rda_Y"),span("~ X Data", tiphelp("Predictors")), choices=names(vals$saved_data), selected=vals$cur_rda_Y)
  })
  output$rda_Y<-renderUI({
    req(input$rda_Y)
    pickerInput(ns("rda_X"),span("Y Data", tiphelp("Response data")), choices=names(vals$saved_data), selected=vals$cur_rda_X)
  })
  output$rda_options<-renderUI({
    req(input$rda_view=='Plot')
    div(
      div(
        span("+",
             inline(checkboxInput(ns("rda_show_symbols"),"Symbol" ,T, width='75px')))),
      uiOutput(ns("rda_show_symbols_out")),
      div(span("+",
               inline(checkboxInput(ns("rda_show_labels"),"Labels",F)
               ))),
      uiOutput(ns("rda_show_labels_out")),


      div(
        actionLink(
          ns('rda_downp'),span("+ Download",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  output$rda_show_symbols_out<-renderUI({
    req(isTRUE(input$rda_show_symbols))
    div(style="margin-left: 5px",
        div(
          span("+ Shape:",
               inline(pickerInput(inputId=ns("rda_symbol"),
                                  label = NULL,
                                  choices = df_symbol$val,
                                  options=list(container="body"),
                                  choicesOpt = list(content = df_symbol$img), width='75px')))),
        div(
          span("+ Size:",
               inline(numericInput(ns("rda_cexpoint"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
               ))
        ),
        div(class="palette",
            span("+ Color:",
                 inline(
                   tipify(
                     pickerInput(inputId=ns("rda_colpalette"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px'), "Symbol palette. Choose a gradient to color observations by a factor")))),
        uiOutput(ns("rda_fac_palette"))
    )

  })
  rda_symbol_factor<-reactive({
    req(input$rda_symbol_factor)
    col<-getcolhabs(vals$newcolhabs,input$rda_colpalette,2)
    if(col[1]!=col[2]){
      data = vals$saved_data[[input$rda_X]]
      attr(data,"factors")[rownames(data), input$rda_symbol_factor]
    }else{NULL}
  })
  output$rda_show_labels_out<-renderUI({
    req(isTRUE(input$rda_show_labels))

    column(12,
           div(span("+ Factor:",
                    inline(tipify(pickerInput(ns("rda_labfactor"),NULL,choices = colnames(attr(vals$saved_data[[input$rda_X]],"factors")), width="125px"),  "label classification factor")
                    ))),
           div(span("+ Lab Color:",
                    inline(tipify(
                      pickerInput(
                        inputId=ns("rda_labcolor"),
                        label = NULL,
                        selected=  vals$colors_img$val[12],choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),width="75px",
                        options=list(container="body")
                      ),  "label classification factor"
                    )
                    ))),
           div(span("+ Lab adj:",
                    inline(
                      tipify(pickerInput(ns("rda_labadj"),NULL,choices=c(1:4), width="75px", options=list(containder="body")),  "a position specifier for the text. If specified this overrides any adj value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified (x,y) coordinates.", placement = "right")
                    ))),
           div(span("+ Lab offset:",
                    inline(
                      tipify(numericInput(ns("rda_offset"),NULL,value = 0,step = .1, width="75px"),  "this value controls the distance ('offset') of the text label from the specified coordinate in fractions of a character width.")
                    ))),
           div(span("+ Size:",
                    inline(
                      tipify(numericInput(ns("rda_cextext"),NULL,value = 1,min = 0.1,max = 3,step = .1),  "label text size")
                    )))
    )

  })
  output$rda_view_plot<-renderUI({
    req(input$rda_view=='Plot')

    div(
      div(
        '+ Scaling:',
        inline(numericInput(ns("rda_scaling"),NULL, 2, step=1, min=0, max=3)),tipify(icon("fas fa-question-circle",style="color: gray"),"Scaling for species and site scores. Either species (2) or site (1) scores are scaled by eigenvalues, and the other set of scores is left unscaled, or with 3 both are scaled symmetrically by square root of eigenvalues.", placement = "bottom")
      ),
      div(span("+",checkboxInput(ns("biplot_rda"), span("Biplot", pophelp(NULL,"show biplot arrows")), T))),
      div(span("+",checkboxInput(ns("sp_rda"), span("Variables", pophelp(NULL,"Variables"))))),
      uiOutput(ns("sp_rda_out"))


    )

  })
  output$rda_sp_display<-renderUI({
    req(input$rda_sp_display=='Shape')
    inline(pickerInput(
      inputId=ns("rda_spshape"),
      label = NULL,
      choices = df_symbol$val,
      options=list(container="body"),
      selected=df_symbol$val[8],
      choicesOpt = list(content = df_symbol$img)))

  })
  observeEvent(input$disp_rda_summ,
               vals$disp_rda_summ<-input$disp_rda_summ)
  output$rda_view_summary<-renderUI({
    req(input$rda_view=='Summary')
    div(
      div(
        span(
          "+ Results:",
          inline(
            pickerInput(ns("disp_rda_summ"),NULL,choices=c('Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot'), selected=vals$disp_rda_summ,  options=list(container="body"), width="200px")
          )
        )
      ),
      div(
        span("+ Axes",
             inline(
               numericInput(ns("rda_axes"),NULL, value=2)
             )
        )
      ),
      div(
        tipify(
          actionLink(
            ns('downcenter_rda'),span("+ Download",icon("fas fa-download")), style="button_active"
          ),
          "Download selected results"
        )

      )
    )

  })
  output$sp_rda_out<-renderUI({
    req(isTRUE(input$sp_rda))

    div(style="margin-left: 5px",
        div(
          '+ Number:',
          inline(numericInput(ns("rda_spnum"),NULL, 10, step=1, width="100px")),tipify(icon("fas fa-question-circle",style="color: gray"),"Show N variables with the highest scores", placement = "bottom")
        ),
        div(
          "+ Display:",
          span(
            inline(
              pickerInput(ns("rda_sp_display"),NULL,choices=c("Label","Shape"), width="100px")
            ),
            inline(
              uiOutput(ns("rda_sp_display"))


            )
          )
        ),



        div(
          span("+ Variable Color:",
               tipify(
                 pickerInput(
                   inputId=ns("rda_spcolor"),
                   label = NULL,
                   selected=  vals$colors_img$val[getsolid_col()][4],
                   choices =   vals$colors_img$val[getsolid_col()],
                   choicesOpt = list(
                     content =   vals$colors_img$img[getsolid_col()]
                   ),
                   options=list(container="body")
                 ), "Variable Color."))
        )
    )

  })
  output$rda_plot<-renderPlot({

    prda(rda_model(),
         key = rda_symbol_factor(),
         points =input$rda_show_symbols,
         text = input$rda_show_labels,
         palette = input$rda_colpalette,
         cex.points = input$rda_cexpoint,
         cex.text = input$rda_cextext,
         pch=c(rda_symbol(),3),
         keytext=rda_text_factor(),
         biplot=input$biplot_rda,
         show.sp=input$sp_rda,
         n.sp=input$rda_spnum,
         sp.display=input$rda_sp_display,
         pch.sp=as.numeric(input$rda_spshape),
         col.sp=getcolhabs(vals$newcolhabs,input$rda_spcolor,1),
         textcolor=input$rda_labcolor,
         scaling=input$rda_scaling,
         newcolhabs=vals$newcolhabs,
         pos=input$rda_labadj,
         offset=input$rda_offset
    )
    vals$rda_plot<-recordPlot()
    rda

  })
  output$stats_rda<-renderUI({
    validate(need(!anyNA(vals$saved_data[[input$rda_X]]), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))

    mainPanel(
      tabsetPanel(id=ns("rda_view"),selected=vals$rda_view,
                  tabPanel("Plot", value="Plot",
                           plotOutput(ns("rda_plot"))),
                  tabPanel("Summary", value="Summary",
                           verbatimTextOutput(ns("rda_print"))))


    )
  })



  output$rda_print<-renderPrint({  rda_summary()})
  output$orda_options<-renderUI({
    div(
      div(
        span("+",
             checkboxInput(ns("rda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T)
        )
      ),
      uiOutput(ns("rda_view_summary")),
      uiOutput(ns("rda_view_plot")),



    )
  })

  observeEvent(input$rda_view,
               vals$rda_view<-input$rda_view)
  rda_text_factor<-reactive({
    if(isFALSE(input$rda_show_labels)){NULL} else{
      data = vals$saved_data[[input$rda_X]]
      attr(data,"factors")[rownames(data), input$rda_labfactor]}

  })
  rda_model<-reactive({
    data<-
      vals$saved_data[[input$rda_X]]
    x<-as.matrix(data)
    colnames(x)<-colnames(data)
    if(length(input$rda_Y)>0){
      y<-na.omit(vals$saved_data[[input$rda_Y]][rownames(x),,drop=F])
      colnames(y)<-colnames(vals$saved_data[[input$rda_Y]])
      x<-na.omit(x[rownames(y),])
      dim(data.frame(y))
      dim(x)
      model=vegan::rda(x~.,data=data.frame(y) ,scale=input$rda_scale)
    } else{model= vegan::rda(x,scale=input$rda_scale)}
    model})
  rda_summary<-reactive({
    res<-summary(rda_model())
    res<-switch(input$disp_rda_summ,
                "Variable scores"=res$species,
                "Observation scores"=res$sites,
                "Linear constraints"=res$constraints,
                "Biplot"=res$biplot,

                "Importance (unconstrained)"=res$cont$importance,
                "Importance (constrained)"=res$concont$importance

    )
    vals$rda_summary<-res[,1:input$rda_axes]
    vals$rda_summary

  })
  rda_symbol<-reactive({
    if(isFALSE(input$rda_show_symbols)){NA}else{as.numeric(input$rda_symbol)}
  })

  observeEvent(input$rda_downp,{
    vals$hand_plot<-"rda"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$rda_X,
               vals$cur_rda_X<-input$rda_X)
  observeEvent(input$rda_Y,
               vals$cur_rda_Y<-input$rda_Y)


  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      "Create Datalist: Niche results"={name_niche()},
      "Create factor using breakpoints from the dissimilarity profile"= {c(paste0("BP_"),nlevels(as.factor(vals$splitBP[,1])))},
      "Save pwRDA model in"={
        paste0(input$segrda_X,"~",input$segrda_Y,"[factor:",input$bp_column,"]")
      }
    )})




  output$data_over<-renderUI({
    data_overwritte$df<-F
    data<-vals$saved_data[[input$data_upload0]]
    choices<-c(names(vals$saved_data))
    if(vals$hand_save=="Create factor using breakpoints from the dissimilarity profile"){choices<-colnames(attr(data,"factors"))}
    req(input$hand_save=="over")
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
      "Create Datalist: Niche results"={saveniche()},
      "Create factor using breakpoints from the dissimilarity profile"= {save_bpfac()},
      "Save pwRDA model in"={save_pwrda()}

    )
    removeModal()

  })

  module_desctools <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
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


}
