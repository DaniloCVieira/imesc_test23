
#' @export
plot_dens_nb<-function(m,var="Chl",newcolhabs,palette="viridis",lwd=1,xlab="Variables"){
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2),nrow=1),widths = c("70","30"))
  colors<-getcolhabs(newcolhabs,palette,length(m$levels))
  tables<-m$finalModel$tables[[var]]
  if(lapply(m$finalModel$tables,class)[[1]]=="table"){
    par(mar=c(5,5,5,3),las=1)
    colors<-getcolhabs(newcolhabs,palette,length(levels(m$trainingData[[var]])))
    spineplot(tables,col=colors,las=1,ylab="",xlab=paste("Class:",attr(m,"supervisor")),off=1,axes =T,yaxlabels=F)
    par(mar=c(0,0,5,0),xpd=T)
    plot.new()
    legend(0,1,legend=levels(m$trainingData[[var]]),pch=15,col=colors,bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)

  } else{

    par(mar=c(5,5,5,0))
    res<-lapply(tables,function(x)
      c(
        minx=min(x$x),
        maxx=max(x$x),
        miny=min(x$y),
        maxy=max(x$y)
      ))
    res<-do.call(rbind,res)
    minx<-min(res[,1])
    maxx<-max(res[,2])
    miny<-min(res[,3])
    maxy<-max(res[,4])
    plot(m$finalModel$tables[[var]][[1]],xlim=c(minx,maxx),ylim=c(miny,maxy),type="n",main=var)
    i=1
    for(i in 1:length(tables)){
      lines(tables[[i]],col=colors[i],lwd=lwd)
    }

    par(mar=c(0,0,5,0),xpd=T)
    plot.new()
    legend(0,1,legend=m$levels,lty=1,col=colors,bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)
  }


}


#' @export
module_ui_nb <- function(id){

  ns <- NS(id)
  tagList(uiOutput(ns("nb_panels")))

}


#' @export
module_server_nb <- function (input,output,session,vals,df_colors,newcolhabs ){
  ns <- session$ns

  output$nb_panels<-renderUI({
   # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(

      column(12,style="background: white",
             uiOutput(ns("nb_inputs")),
             uiOutput(ns("war_nb")),
             column(12,
                    tabsetPanel(id = ns("nb_tab"),
                                tabPanel(strong("1. Training"),value='nb_tab1',
                                         uiOutput(ns("nb_params"))),
                                tabPanel(strong("2. Results"),value='nb_tab2',
                                         uiOutput(ns("results_nb"))),
                                tabPanel(strong("3. Predict"),value='nb_tab3',
                                         uiOutput(ns("predict_nb")))
                    )
             )

      )


    )
  })

  insert_nb_resutls<-reactiveValues(df=F)




  getnewdatanb<-reactive({

    datalist<-vals$saved_data
    Xattr<-attr(vals$nb_results,'inputs')$Xattr
    if(Xattr=="Data-Attribute"){
      datalist_comp<-lapply(
        vals$saved_data,function (x){
          colnames(x)<-gsub(" ",".",colnames(x))
          x
        }

      )

      datalist_comp<-vals$saved_data
    }
    if(Xattr=="Factor-Attribute"){
      datalist_comp<-lapply(
        vals$saved_data,function (x){
          x<-attr(x,"factors")
          colnames(x)<-  gsub("[^[:alnum:]]", "_",colnames(x))
          x
          }

      )
      datalist_comp<-datalist_comp

    }


    m<-vals$nb_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp,function (x){
        #x<-datalist_comp[[1]]
        data<-m$trainingData[-Y]
        colnames(data)<-  gsub("[^[:alnum:]]", "_",colnames(data))
        res<-colnames(x)%in%colnames(data)
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })



  get_supervisor_nb <- reactive({
    data <- vals$saved_data[[input$data_nbY]]
    labels <- attr(data,"factors")
    labels[input$nb_sup]
  })


  output$predict_nb<-renderUI({
    column(12,style="background: white", uiOutput(ns("nb_pred_type")),
           tabsetPanel(id="prednb_tab",
                       tabPanel(
                         strong("3.1. Results"), value="prednb_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("nb_tab3_1")))),
                       tabPanel(
                         strong("3.2. Performace"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("nb_tab3_2")))
                       ),
                       tabPanel(strong("3.3. Resampling predictions"), value="svm_tab3_4",
                                uiOutput(ns("resample_out"))
                       )

           )
    )
  })



  predall_nb<-reactive({
    validate(need(!anyNA(test_nb()),"NAs not allowed in the prediction Datalist"))
    m<-vals$nb_results
    #saveRDS(m,'m.rds')
    #beep()
    #factors<-attr(vals$saved_data[[input$prednb_new]],"factors")
    #m<-readRDS("m.rds")
    ctr<-data.frame(id=rownames(m$trainingData))
    rownames(ctr)<-1:nrow(m$trainingData)
    res0<-lapply(split(m$pred$pred,m$pred$rowIndex),function(x)data.frame(x))

    res<-t(do.call(cbind,res0))
    rownames(res)<-names(res0)
    rownames(res)<-ctr[rownames(res),]

    #pred_tab<-readRDS("pred_tab.rds")
    # m$control
    #predict(m,predict.all=T)

    pred <- res
    pred
  })
  nb_observed2<-reactive({
    req(input$nbpred_which)


    obs_data<-if(input$nbpred_which=="Datalist"){
      if(vals$nb_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$prednb_newY_tree]],'factors')
      } else {
        factors<-vals$saved_data[[input$prednb_newY_tree]]}

      res<-factors[,attr(vals$nb_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$nb_results,"supervisor")])
      res
    } else{
      attr(vals$nb_results,"sup_test")
    }
    obs_data
  })
  get_qclass<-reactive({
    m<-vals$nb_results
    req(m$modelType=="Regression")
    pred<-predall_nb()
    model_data<-test_nb()
    obs_data<-nb_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.nb.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.nb.int,ref1)
    lop<-mapply(list, lp,lo,lpred,SIMPLIFY=FALSE)

    x<-lop[[1]]
    res<-do.call(rbind,lapply(lop,function(x){
      interval<-x[[1]]
      obs<-unlist(x[[2]])
      cbind(x[[2]],do.call(rbind,lapply(obs, function(xx){

        res<-if(isTRUE(between(xx,interval[[1]],interval[[2]] ))){"as expected"} else{
          if(xx<=interval[[1]]){"lower"} else if(xx>=interval[[1]]){"higher"} else{"as expected"}
        }

        interval$q_class<-res
        wil_res<-interval
        wil_res
      })))
    }))

    colnames(res)[c(2,3)]<-c("q0.025",'q0.975')
    rownames(res)<-rownames(obs_data)
    res$q_class<-factor(res$q_class, levels=c("lower","as expected","higher"), labels=c("lower","as expected","higher"))
    vals$nb_treetest<-res
    vals$nb_treetest
  })
  output$nb_tree_show<-renderUI({
    val=if(vals$nb_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$nb_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_nb())
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))

    div(
      inline(pickerInput(ns('splitdata_trees'),"Observations:", choices=options_show, width="200px")),
      actionButton(ns('downp_summ_trees'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active")
    )
  })
  output$qclass_legend<-renderUI({
    req(vals$nb_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$nb_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("nb_tree_show"))),
        inline(uiOutput(ns("nb_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$nb_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$prednb_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$nb_results
    req(m$modelType=="Classification")
    pred<-predall_nb()
    pred.ind <- pred
    model_data<-test_nb()
    obs_data<-nb_observed2()
    obs_data<-data.frame(obs_data)
    data=t(pred.ind)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs=obs_data[ colnames(data),, drop=F]
    pred_fac<-do.call(data.frame,lapply(data.frame(data), function(x) factor(x, labels=m$levels, levels = m$levels)))
    res<-data.frame(do.call(rbind,lapply(pred_fac, function(x) table(x))))
    rownames(res)<-colnames(data)
    colnames(res)<-m$levels
    attr(res,'ntree')<-ncol(pred.ind)
    attr(res,'obs')<-obs
    vals$get_forest_class<-res_forest<-res
    vals$get_forest_class
  })
  output$pick_nbobs_pred<-renderUI({
    req(input$nbpred_which=="Datalist")
    sup_test<-attr(vals$nb_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobsnb()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  output$prednb_newY_ref<-renderUI({
    req(input$prednb_newY_tree)
    req(input$nbpred_which)
    req(vals$nb_results$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$prednb_newY_tree]],"factors")
    choices=if(input$nbpred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('nb_reftest'),NULL,choices, width="200px", selected=vals$nb_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$nb_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$nb_results$modelType=="Classification")
    div(class="palette",
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("forest_col"),
                           label = NULL,
                           choices =     vals$colors_img$val[getgrad_col()],
                           choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"), width="75px")
             )
        )
    )
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  output$pick_nbobs<-renderUI({
    sup_test<-attr(vals$nb_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("prednb_newY_tree"),
                       NULL,names(vals$saved_data[getobsnb()]), width="200px",selected=vals$prednb_newY_tree
           ))
    )
  })
  output$resample_out<-renderUI({
    m<-vals$nb_results
    validate(need(m$control$savePredictions=="all","This model was generated in an earlier version of iMESc. Please retrain the data to enable this functionality."))
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("qclass_side"))
      ),
      mainPanel(
        uiOutput(ns("qclass_out"))
      )
    )
  })
  output$qclass_side<-renderUI({

    div(
      class="map_control_style",style="color: #05668D",
      uiOutput(ns("pick_nbobs")),
      uiOutput(ns("prednb_newY_ref")),
      uiOutput(ns('forest_margin')),
      uiOutput(ns('forest_col')),

      div(
        span("+ Size:",
             inline(
               numericInput(ns("qclass_sizeplot"),NULL, value=1.5, width="75px")
             )
        )
      ),
      div(
        span("+ Width",
             inline(
               numericInput(ns("qclass_width"),NULL, value=800, width="75px")
             )

        )),
      div(
        span("+ Height",
             inline(
               numericInput(ns("qclass_height"),NULL, value=700, width="75px")
             )
        )
      ),

      uiOutput(ns("forest_saves"))


    )
  })
  output$forest_saves<-renderUI({
    req(vals$nb_results$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"+ Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"+ Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$nb_results$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_nb()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-nb_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$nb_treetest[colnames(data),]
    #data<-readRDS('data.rds')
    #pred_interval<-readRDS('pred_interval.rds')
    #q_class<-readRDS('q_class.rds')
    #obs<-readRDS('obs.rds')
    #beep(1)
    str_numerics3(numerics=data,
                  obs=obs_data[ colnames(data),],
                  pred_interval=pred_interval,
                  q_class=q_class,
                  cex=input$qclass_sizeplot)
    vals$vartrees<-recordPlot()
  })
  output$forestbyclass<- renderPlot({
    res_forest<-get_forest_class()

    plot_florest_class(res_forest,vals$newcolhabs, palette=input$forest_col, ylab="Resample")
  })
  observeEvent(input$prednb_newY_tree,{
    vals$prednb_newY_tree<-input$prednb_newY_tree
  })
  observeEvent(input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_nb())

  })
  observeEvent(input$resamp_pred_gdownp,{

    vals$hand_down<-"nb_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })





  output$nb_pred_type<-renderUI({
    choices=if(length(attr(vals$nb_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(inline(radioButtons(ns("nbpred_which"),"New data (X):",choices=choices,inline=T, width="200px")), inline(uiOutput(ns("datalist_pred_nb"))))
  })
observeEvent(input$prednb_new,{
  vals$prednb_new<-input$prednb_new
})
  output$datalist_pred_nb<-renderUI({
    req(input$nbpred_which =='Datalist')
    div(
      pickerInput(ns("prednb_new"),"Datalist",names(vals$saved_data[getnewdatanb()]), width = '300px', selected=vals$prednb_new)
    )
  })




  get_cm_pred<-reactive({
    obs<-get_nb_prederrors()$obs
    pred<-get_nb_prederrors()$pred
    conf<-table(obs,pred)
    conf
  })

  output$confusion_nb_pred<-renderUI({
    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100,input$nbpalette_pred, newcolhabs=vals$newcolhabs)
             vals$nb_cm_pred<-res
             res
           }))

  })
  output$confusion_nb2_pred<-renderPrint({
    res<-confusionMatrix(get_cm_pred())
    vals$nb_cm_test<-  as.data.frame.matrix(res$table)
    res
    })




  output$nb_tab3_2<-renderUI({
    sup_test<-attr(vals$nb_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    sidebarLayout(
      sidebarPanel(
        fluidRow(class="map_control_style",style="color: #05668D",
                 div(
                   span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
                        span(supname),
                        inline(pickerInput(ns("prednb_newY"),
                                           NULL,names(vals$saved_data[getobsnb()]),width="200px"))
                   )
                 ),
                 div(
                   span(tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"+ Palette",
                        inline(
                          pickerInput(inputId = ns("nbpalette_pred"),
                                      label = NULL,
                                      choices =     vals$colors_img$val,
                                      choicesOpt = list(content =     vals$colors_img$img),options=list(container="body"),width="75px")
                        )
                   )
                 ),
                 div(
                   actionLink(ns("downp_cmnb_pred"),span("+ Donwload plot"),style  = "button_active")
                 ),
                 div(
                   actionLink(ns("dowcenter_cmnb_pred"),span("+ Download table"),style  = "button_active"))
                 )),
      mainPanel(
        tags$style(
          paste(paste0("#",ns('nb_tab_errors0_test')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
        ),
        tags$style(
          paste0("#",ns('nb_tab_errors0_test')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
        ),
        div(
          p(strong("Prediction stats:")),
          inline(DT::dataTableOutput(ns('nb_tab_errors0_test')))
        ),
        div(
          p(strong("Confusion matrix:")),
          uiOutput(ns("confusion_nb_pred")),
          verbatimTextOutput(ns("confusion_nb2_pred"))
        )






      )
    )
  })


  observeEvent(input$dowcenter_cmnb_pred,{
    vals$hand_down<-"nb_cm_test"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

pred_test_nb<-reactive({
  obs<-get_nb_prederrors()$obs
  pred<-get_nb_prederrors()$pred
  table<-data.frame(t(postResample(pred,obs)))
  table
})

  output$nb_tab_errors0_test<-DT::renderDataTable({
    table<-pred_test_nb()
    table<-round(table,3)
    rownames(table)<-NULL

    DT::datatable(table,options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't',rownames = TRUE,class ='compact cell-border'))

  })
  get_nb_prederrors<-reactive({
    m<-vals$nb_results
    pred<-pred_nb()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$nbpred_which=="Partition"){
      attr(m,"sup_test")} else{
        if(m$modelType=="Classification"){
          attr(vals$saved_data[[input$prednb_newY]],"factors")[,attr(m,"supervisor")]} else{
            vals$saved_data[[input$prednb_newY]][,attr(m,"supervisor")]
          }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })

  getobsnb<-reactive({
    sup_test<-attr(vals$nb_results,"supervisor")
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x)attr(x,"factors"))
    m<-vals$nb_results
    res0<-unlist(
      lapply(datalist,function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  output$nb_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('nb_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")),style="button_active"
                       ),
                       "Create a datalist with the NB predictions",options=list(container="body")
                     )
                   ),
                   div(
                     actionLink(
                       ns('down_nb_tab3_1'),span("+ Download table"),style="button_active"
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('nbtab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('nbtab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('nbtab_pred')))


        )
      )
    )
  })


  observeEvent(input$down_nb_tab3_1,{
    vals$hand_down<-"NB - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

  output$nbtab_pred<-DT::renderDataTable({
    table<-vals$nbtab_pred<-pred_nb()
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'))

  },rownames = TRUE,class ='compact cell-border')

  test_nb<-reactive({
    req(input$nbpred_which)

    Xattr<-attr(vals$nb_results,'inputs')$Xattr
    req(length(Xattr)>0)
    if(input$nbpred_which=="Datalist"){
      req(input$prednb_new)
      if(Xattr=="Data-Attribute"){

        pred_tab<-vals$saved_data[[input$prednb_new]]

      }
      if(Xattr=="Factor-Attribute"){

        pred_tab<-attr(vals$saved_data[[input$prednb_new]],"factors")
      }

    } else {
      pred_tab<-attr(vals$nb_results,"test")
    }
    colnames(pred_tab)<-gsub(" ",".",colnames(pred_tab))
    pred_tab

  })

  pred_nb<-reactive({
    validate(need(!anyNA(test_nb()),"NAs not allowed in the prediction Datalist"))
    m<-vals$nb_results
    test_nb<-test_nb()
    m$finalModel$varnames<-  gsub("[^[:alnum:]]", "",m$finalModel$varnames)
    colnames(m$finalModel$x)<-  gsub("[^[:alnum:]]", "",colnames(m$finalModel$x))
    colnames(test_nb)<-  gsub("[^[:alnum:]]", "",colnames(test_nb))
    nb_pred <- predict(m$finalModel,newdata = test_nb)
    res<-data.frame(Predictions= nb_pred)
    rownames(res)<-rownames(test_nb())
    #colnames(res)<-attr(vals$nb_results,"supervisor")

    # attr(res,"obs")<-test_nb()
    res



  })

  observeEvent(input$data_nbY,{
    vals$cur_data_nbY<-input$data_nbY
  })
  observeEvent(input$nb_attribute,{
    vals$cur_nb_attribute<-input$nb_attribute
  })
  observeEvent(input$prednb_new,{
    vals$prednb_new<-input$prednb_new
  })

  observeEvent(input$nbpred_which,{
    vals$nbpred_which<-input$nbpred_which
  })
  output$nb_inputs <- renderUI({
    if(is.null(vals$cur_nb_attribute)){vals$cur_nb_attribute<-1}
    if(is.null(vals$cur_data_nbY)){vals$cur_data_nbY<-1}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
             if(input$nb_tab=="nb_tab1"){
               div(
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_nbX_out'))),"::",
                          inline(
                            div(
                              div("Attribute:"),
                              div(pickerInput(ns("nb_attribute"),NULL,choices =  c("Factor-Attribute","Data-Attribute"),width="150px",selected=vals$cur_nb_attribute))
                            )
                          )


                     )
                 ),

                 div(class="well3",
                      span(strong("Y:"),
                           inline(
                             div(
                               div("Datalist:"),
                               div(pickerInput(ns("data_nbY"),NULL,choices =  names(vals$saved_data),width="150px",selected=vals$cur_data_nbY))
                             )
                           ),
                           "::",
                           inline(uiOutput(ns('nb_supervisor'))),

                           inline(uiOutput(ns("nb_test_part"))),
                           "::",
                           inline(uiOutput(ns("nb_partition")))
                      )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_nbX_out'))),
                      inline(uiOutput(ns("nb_results_menu"))),
                      inline(uiOutput(ns("savenb_button"))),
                      inline(uiOutput(ns("rmnb_button"))),
               )
             }
    )
  })


  output$nb_params<- renderUI({
    column(12,class="well2",
           div(
             uiOutput(ns("teste"))
           ),

           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               div(

                 tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for Laplace correction (if a vector,comma delimited between 0 and 1)",options=list(container="body")),
                 "+ fL",
                 textInput(ns('fL_nb'),NULL,value ='1',width="200px")
               ),
               div(
                 tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for Bandwidth Adjustment (larger numbers mean more flexible density estimate; if a vector must be comma delimited between 0 and 1)",options=list(container="body")),
                 "+ Bandwidth Adjustment",
                 textInput(ns('bandw_nb'),NULL,value ='1',width="200px")

               ),
               div(
                 tipify(icon("fas fa-question-circle"),textseed(),options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seednb"),NULL,value = NULL,width="122px")
                 )
               ),

               div(popify(icon("fas fa-question-circle"),NULL,
                          HTML(paste0(
                            div(HTML(paste0(strong("repeatedcv:")," repeated k-fold cross-validation;"))),
                            div(HTML(paste0(strong("boot:")," bootstraping;"))),

                            div(HTML(paste0(strong("LOOCV:")," Leave one out;"))),
                            div(HTML(paste0(strong("LGOCV:")," Leave-group out")))

                          )),options=list(container="body")

               ),
               "+ Resampling method:",
               pickerInput(ns("nb_res_method"),NULL,choices=list("repeatedcv","boot","LOOCV","LGOCV"),width = "100px")
               ),
               uiOutput(ns("nb_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("nb_war"))
           ),

           column(12,align = "center",
                  popify(actionButton(ns("trainNB"),h4(img(src=nb_icon,height='20',width='20'),"train Naive Bayes",icon("fas fa-arrow-circle-right")),style = "background:  #05668D; color: white"),NULL,"Click to run")
           ),

    )
  })


  output$nb_resampling<-renderUI({
    div(
      inline(
        if(input$nb_res_method=='cv'|input$nb_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds",options=list(container="body")),"+ cv:",
            numericInput(ns("cvnb"),NULL,value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$nb_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute",options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatsnb"),NULL,value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$nb_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations",options=list(container="body")),
              "+ number:",
              numericInput(ns("cvnb"),NULL,value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$nb_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage",options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavenb"),NULL,value = 10,width="100px")
            )
          )
        }
      )
    )
  })




  output$nb_supervisor <- renderUI({
    req(input$data_nbY)

    if(is.null(vals$cur_nb_sup)){vals$cur_nb_sup<-1}
    data <- vals$saved_data[[input$data_nbY]]
    labels <- attr(data,"factors")
    choices<-rev(colnames(labels))
    div(well="class2",
      div("Variable"),
      div(tipify(pickerInput(
        ns("nb_sup"),
        NULL,
        choices =choices ,
        width="150px",selected=vals$cur_nb_sup
      ),"The response vector"))
    )

  })
  observeEvent(input$nb_sup,{
    vals$cur_nb_sup<-input$nb_sup
  })
  output$data_nbX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_nbX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_nbs")))

    )
  })


  output$saved_nbs<-renderUI({
    req(input$data_nbX)
    req(length(names(attr(vals$saved_data[[input$data_nbX]],"nb")))>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names(attr(vals$saved_data[[input$data_nbX]],"nb")))), "saved model(s)")
  })


  observeEvent(input$data_nbX,{
    vals$cur_data<-input$data_nbX
  })
  output$nb_test_part<-renderUI({
    req(input$data_nbX)
    if(is.null(vals$cur_nb_test_partition)){vals$cur_nb_test_partition<-1}
    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("nb_test_partition"),NULL,choices=c("None",colnames(attr(vals$saved_data[[input$data_nbY]],"factors"))),width="150px",selected=vals$cur_nb_test_partition))
    )
  })
  observeEvent(input$nb_test_partition,{
    vals$cur_nb_test_partition<-input$nb_test_partition
  })
  output$nb_partition<-renderUI({
    req(input$nb_test_partition!="None")
    req(input$data_nbY)
    if(is.null(vals$cur_testdata_nb)){vals$cur_testdata_nb<-1}
    fac<-attr(vals$saved_data[[input$data_nbY]],"factors")[,input$nb_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training,and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_nb"),NULL,choices=choices,width="200px",selected=vals$cur_testdata_nb)
    )
  })
  observeEvent(input$testdata_nb,{
    vals$cur_testdata_nb<-input$testdata_nb
  })
  output$nb_results_menu<-renderUI({
    if(is.null(vals$cur_nb_models)){vals$cur_nb_attribute<-1}
    div(pickerInput(ns("nb_models"),strong("NB results:",tiphelp("Naive Bayes Results. Click to select nb results saved in the Training Datalist (X).")),choices= names(attr(vals$saved_data[[input$data_nbX]],"nb")),width="200px",selected = vals$cur_nb_models)
    )
  })
  observeEvent(input$nb_models,{
    vals$cur_nb_models<-input$nb_models
  })
  output$rmnb_button<-renderUI({
    req(input$nb_models!="new nb (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmnb"),icon("far fa-trash-alt")),"Remove the nb model from the training Datalist (X)")
    )

  })



  get_parts_nb<-reactive({
    req(input$data_nbY)
    req(input$testdata_nb)
    partition<- attr(vals$saved_data[[input$data_nbY]],"factors")[rownames(vals$saved_data[[input$data_nbX]]),input$nb_test_partition]
    test<-which(partition==input$testdata_nb)
    train<-which(partition!=input$testdata_nb)
    list(train=train,test=test)
  })

  getdata_nbX<-reactive({
    req(input$data_nbX)

    if(input$nb_attribute=="Factor-Attribute"){
      data<-attr(vals$saved_data[[input$data_nbX]],"factors")
    } else{
      data=vals$saved_data[[input$data_nbX]]
    }
    data
  })


  getdata_nbY<-reactive({
  data <- vals$saved_data[[input$data_nbY]]
  labels <- attr(data,"factors")
   labels<-labels[rownames(vals$saved_data[[input$data_nbX]]),input$nb_sup]
   labels
})



  observeEvent(input$trainNB,{
    try({


      x<-x_o<-data.frame(getdata_nbX())
      y<-y_o<-getdata_nbY()
      output$nb_war<-renderUI({
        column(12,style="color: red",align="center",
               if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
               if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
        )
      })
      validate(need(anyNA(x)==F,"NAs not allowed in X"))
      validate(need(anyNA(y)==F,"NAs not allowed in Y"))

      if(input$nb_test_partition!="None"){
        parts<-get_parts_nb()
        train<-parts$train
        test<-parts$test
        x<-data.frame(getdata_nbX()[train,])
        y<-as.factor(getdata_nbY()[train])
      }

      if(input$nb_attribute=="Factor-Attribute"){
        if(input$data_nbY==input$data_nbX){
          x[input$nb_sup]<-NULL
        }
      }

      if(input$nb_attribute=="Data-Attribute"){
        usekernel=T
      } else{usekernel=c(T,F)}


      bandw_nb<-as.numeric(unlist(strsplit(input$bandw_nb,",")))
      fL_nb<-as.numeric(unlist(strsplit(input$fL_nb,",")))
      grid <- expand.grid(fL=c(fL_nb),usekernel = c(usekernel),adjust=c(bandw_nb))


      seed<-if (!is.na(input$seednb)){ input$seednb} else{
        NULL
      }
      nb_search<-input$nb_search

      colnames(x)<-gsub(" ",".",colnames(x))
      colnames(x_o)<-gsub(" ",".",colnames(x_o))

      if (!is.na(input$seednb)){set.seed(input$seednb)}
      withProgress(message = "Running Naive Bayes ...",
                   min = 1,
                   max = 1,
                   {
                     NB<-train(x,y,'nb',

                               trControl=trainControl(

                                 method = input$nb_res_method,
                                 number = input$cvnb,
                                 repeats = input$repeatsnb,
                                 p=input$pleavenb/100,
                                 savePredictions = "all"

                               ),
                               tuneGrid=grid
                     )
                     attr(NB,"test_partition")<-paste("Test data:",input$nb_test_partition,"::",input$testdata_nb)
                     attr(NB,"Y")<-paste(input$data_nbY,"::",input$nb_sup)
                     attr(NB,"Datalist")<-paste(input$data_nbX)

                     vals$nb_unsaved<-NB

                     if(input$nb_test_partition!="None"){
                       attr(vals$nb_unsaved,'test')<-x_o[test,]
                       attr(vals$nb_unsaved,"sup_test")<-y_o[test]
                     } else{ attr(vals$nb_unsaved,'test')<-c("None")}
                     vals$bag_nb<-T
                     attr(vals$nb_unsaved,"supervisor")<-input$nb_sup
                     attr(vals$nb_unsaved,"inputs")<-list(
                       Ydatalist=input$data_nbY,
                       Y=input$nb_sup,
                       Xdatalist=input$data_nbX,
                       Xattr=input$nb_attribute
                     )
                     updateTabsetPanel(session,"nb_tab","nb_tab2")
                   })
      beep(10)
      attr(vals$saved_data[[input$data_nbX]],"nb")[['new nb (unsaved)']]<-vals$nb_unsaved
      vals$cur_nb_models<-"new nb (unsaved)"
      vals$bag_nb<-T
      #saveRDS(vals$nb_unsaved,"nb.rds")



    })
  })


  output$results_nb<-renderUI({
    validate(need(length(vals$nb_results)>0,"No models have been trained yet"))
    req(length(vals$nb_results))
    m<-vals$nb_results
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_nb())
             )
           ),
           tabsetPanel(
             tabPanel("1. Summary",
                      uiOutput(ns("nb_tab_2_1"))
                      ),
             tabPanel("2. Penbormace",
                      uiOutput(ns("nb_tab_2_2"))
                      ),
             tabPanel(if(lapply(m$finalModel$tables,class)[[1]]=="table"){"3. Mosaic plot"} else {"3. Density plot"},
                      uiOutput(ns("nb_tab_2_3"))
             ),
             tabPanel("4. Variable importance",
                      sidebarLayout(
                        sidebarPanel(
                       uiOutput(ns("nb_tab_2_4side"))
                        ),
                        mainPanel(  uiOutput(ns("nb_tab_2_4")))
                      )

             ),
             tabPanel("5. Confusion matrix",
                      uiOutput(ns("nb_tab_2_5"))
             )
           )

    )
  })

  output$nb_tab_2_1<-renderUI({
    m<-vals$nb_results
    div(
      renderPrint(m),
      div(style="height: 100px",
          popify(downloadButton(ns("down_nb_results"),label=span("Download nb model",img(src=nb_icon,height='20',width='20')),style = "button_active"),NULL,
                 HTML(paste0(
                   div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
                 )))
      )

    )
  })
  output$down_nb_results <- {
    downloadHandler(
      filename = function() {
        paste0("nb","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$nb_results,file)
      })
  }

 output$nb_tab_2_4side<-renderUI({
   req(length(vals$nb_results))
   m<-vals$nb_results
   Y<-which(colnames(m$trainingData)==".outcome")
   nvars<-ncol(m$trainingData[,-Y])
   req(length(nvars)>0)
   val<-if(nvars>10){10} else{
     nvars
   }
    div(
      class="map_control_style",style="color: #05668D",
      div("+ Number of variables:",
          numericInput(ns("varImp_n_nb"),NULL,value=val,step=1,max=nvars,width="80px")
      ),
      div("+ Plot type:", inline(pickerInput(ns("varimp_type"),NULL,c("default","gg"),width="100px"))),

      div(uiOutput(ns("varimp_gg"))),
      br(),
      div(
        actionLink(
          ns('nb_varImp'),span("+ Download table"),style="button_active"
        )
      ),
      div(
        actionLink(
          ns('nb_varImp_plot'),span('+ Download plot'),style="button_active"
        )
      )


    )
  })

 output$varimp_gg<-renderUI({
   req(input$varimp_type=='gg')
   div(class="palette",
     div(
         span("+ Palette:",
              inline(
                pickerInput(inputId = ns("varimp_col"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px')))),
     div(
       "+ display", inline(pickerInput(ns("varimp_pos"),NULL,c("dodge","stack"),width="100px"))
     )
   )
 })

output$nb_tab_2_3<-renderUI({
  m<-vals$nb_results
  Y<-which(colnames(m$trainingData)==".outcome")
  choices=colnames(m$trainingData[-Y])
  sidebarLayout(
    sidebarPanel(
      fluidRow(class="map_control_style",style="color: #05668D",

               div("+ Variable",
                   pickerInput(ns("nb_dens_var"),NULL,choices=rev(choices),width="150px")
                   ),
               div(
                 span("+ Palette",
                      inline(
                        pickerInput(inputId = ns("nb_dens_palette"),
                                    label = NULL,
                                    choices =     vals$colors_img$val,
                                    choicesOpt = list(content =     vals$colors_img$img),options=list(container="body"),width="75px")
                      )
                 )
               ),
               div(
                 "+ Line width",
                 inline(numericInput(ns("nb_dens_lwd"),NULL,1,step=0.1,width="75px"))
               ),
               div(tipify(actionLink(ns("downp_nb_dens"),span("+ Donwload",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"Download plot"))

      )
    ),
    mainPanel(
      uiOutput(ns("nb_densplot"))
    )
  )
})


output$nb_densplot<-renderUI({
  renderPlot({
    plot_dens_nb(vals$nb_results,var=input$nb_dens_var,palette=input$nb_dens_palette,lwd=input$nb_dens_lwd,newcolhabs=vals$newcolhabs)
    vals$nb_densplot<-recordPlot()
  })
})
  output$nb_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('nb_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")),style="button_active"
                       ),
                       "Create a datalist with the NB training errors",options=list(container="body")
                     )
                   ),
                   div(
                     actionLink(
                       ns('nb_down_errors_train'),span("+ Download table"),style="button_active"
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('nb_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('nb_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('nb_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('nb_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('nb_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('nb_tab_errors_train')))
          )



        )
      )
    )
  })

  observeEvent(input$downp_cmnb,{
    vals$hand_plot<-"NB - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)})

  observeEvent(input$downp_cmnb_pred,{
    vals$hand_plot<-"NB - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)})

  observeEvent(input$nb_varImp_plot,{
    vals$hand_plot<-"NB - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })
  observeEvent(input$downp_nb_dens,{
    vals$hand_plot<-"NB - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })





  observeEvent(input$nb_down_errors_train,{
    vals$hand_down<-"NB - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

  observeEvent(input$nb_varImp,{
    vals$hand_down<-"NB - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })



  output$nb_tab_errors0_train<-DT::renderDataTable({
    m<-vals$nb_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table,options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't',rownames = TRUE,class ='compact cell-border'))

  })

  output$nb_tab_errors_train<-DT::renderDataTable({
    m<-vals$nb_results
    table<-accu_rf_class(m)
    vals$nb_down_errors_train<-table
    table
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'),rownames = TRUE,class ='compact cell-border')

  })
  savereac<-reactive({
    vals<-reactiveValuesToList(vals)
    vals2<-vals[-c(which(names(vals)=='saved_data'))]
    vals3<-vals2[-which(unlist(
      lapply(vals2,function(x) object.size(x)))>=50000)]
    vals3$saved_data<-vals$saved_data
    saveRDS(vals3,'vals.rds')
    saveRDS(reactiveValuesToList(input),'input.rds')
    beep(10)
  })


#  input<-readRDS('input.rds')
 # vals<-readRDS('vals.rds')
  output$nb_tab_2_4<-renderUI({
   req(vals$nb_results)
    req(input$varImp_n_nb)
    renderPlot({
      m<-vals$nb_results
      vals$nb_varImp<-caret::varImp(m)$importance
      vals$nb_varImp_plot<-plot_varimport(m,palette=input$varimp_col,vals$newcolhabs,input$varimp_pos, type=input$varimp_type, nvars=input$varImp_n_nb)
      vals$nb_varImp_plot
     })
  })


output$nb_tab_2_5<-renderUI({
 # validate(need(is.factor( vals$nb_results$finalModel$y),"Confusion matrices are only valid for classification (factor)models."))
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        class="map_control_style",style="color: #05668D",
        div(
          span(
            "+ Type: ",
            pickerInput(inputId = ns("nb_cm_type"),
                        label = NULL,
                        choices = c("Resampling","Optimal Model"),

                        width="100px"
            )
          )
        ),
        div(
          span(
            "+ Palette",
            pickerInput(inputId = ns("nbpalette"),
                        label = NULL,
                        choices = vals$colors_img$val,
                        choicesOpt = list(
                          content = vals$colors_img$img
                        ),
                        options=list(container="body"),
                        selected=vals$colors_img$val[1],
                        width="75px"
            )
          )
        ),
        div(
          popify(actionLink(ns("downp_cmnb"),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
        ),
        div(
          popify(actionLink(ns("dowcenter_cmnb"),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM table")
        )

      )

    ),
    mainPanel(
      plotOutput(ns("confusion_nb")),
      verbatimTextOutput(ns("confusion_nb2"))
    )
  )

})
observeEvent(input$dowcenter_cmnb,{
  vals$hand_down<-"nb_cm"
  module_ui_downcenter("downcenter")
  mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
})

output$confusion_nb <- renderPlot({
  m<-vals$nb_results
  if(input$nb_cm_type=="Resampling"){
    res<-plotCM(m,input$nbpalette, newcolhabs=vals$newcolhabs)
  } else{
    cm<-table(predict(vals$nb_results),vals$nb_results$trainingData[,'.outcome'])
    res<-plotCM(cm,input$nbpalette,vals$newcolhabs)
    }
  vals$cm_nb<-res
  res
})
output$confusion_nb2<-renderPrint({
  res<-if(input$nb_cm_type=="Resampling"){
    confusionMatrix(vals$nb_results$pred$pred,vals$nb_results$pred$obs)
  } else{
    confusionMatrix(predict(vals$nb_results),vals$nb_results$trainingData[,'.outcome'])
  }
  vals$nb_cm<-  as.data.frame.matrix(res$table)
  res

})


  observe({
    req(input$nb_models)
    if(input$nb_models=="new nb (unsaved)"){
      vals$nb_results<-vals$nb_unsaved } else{
        vals$nb_results<-attr(vals$saved_data[[input$data_nbX]],"nb")[[input$nb_models]][[1]]
      }
  })






  getmodel_nb<-reactive({

    attri<-attr(vals$nb_results,'inputs')
    res<-c(attri$Ydatalist,
           attri$Y,
           attri$Xdatalist,
           attri$Xattr)
    model<-  paste( paste0(res[1],"::",res[2]),"~",paste0(res[3],"::",res[4]))
    model

  })

  observeEvent(input$tools_rmnb,{
    attr(vals$saved_data[[input$data_nbX]],"nb")[input$nb_models]<-NULL
  })


  ###
  observeEvent(input$tools_savenb,{
    if(input$tools_savenb %% 2){
      vals$hand_save<-"Save NB model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_nbX,style="color:gray"),strong("::"),em("NB-Attribute",style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL
      showModal(module_nb())
      }
  })
  observeEvent(input$nb_create_errors_train,{
    vals$hand_save<-"Create Datalist: NB training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(module_nb())
  })
  observeEvent(input$nb_create_predictions,{
    vals$hand_save<- "Create Datalist: NB predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute,and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL

    showModal(module_nb())
  })




  output$savenb_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savenb"),icon("fas fa-save")),"Save the nb model in the training Datalist (X)")
    )
  })


  resample_create<-reactive({

    m<-vals$nb_results
    var<-attr(m,"supervisor")
    temp<-vals$nb_treetest
    datao<-vals$saved_data[[input$data_nbX]]
    factors<-attr(temp,"factors")

    factors<-temp[c('q_class')]
    colnames(factors)<-paste(var,colnames(factors),sep="_")
    temp[c('w_class','sig','q_class')]<-NULL



    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      attr(temp,"factors")<-factors
      vals$saved_data[[input$newdatalist]]<-temp

    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      attr(temp,"factors")<-factors
      vals$saved_data[[input$over_datalist]]<-temp

    }

  })
  name_resample_create<-reactive({

    bag<-1
    var<-attr(vals$nb_results,"supervisor")
    name0<-paste("nb",var,"qclass", sep="_")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste(name0,bag)

  })


  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##nb
      "Save NB model in"= { name_save_nb()},
      "Create Datalist: NB training errors -obs"={name_nb_train_errors()},
      "Create Datalist: NB predictions"={ name_nb_pred()},
      "resample_create"={name_resample_create()}
    )})
  savenb<-reactive({
    req(vals$cur_nb_models=='new nb (unsaved)')
    temp<-vals$nb_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_nbX]],"nb")[[input$newdatalist]]<-c(temp,attr(vals$saved_data[[input$data_nbX]],"nb")[[input$newdatalist]])
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_nbX]],"nb")[[input$over_datalist]]<-c(temp,attr(vals$saved_data[[input$data_nbX]],"nb")[[input$over_datalist]])
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[input$data_nbX]],"nb")['new nb (unsaved)']<-NULL
    vals$bag_nb<-F
    vals$cur_nb_models<-cur
    vals$cur_nb<-cur

  })
  nb_create_training_errors<-reactive({
    temp<-vals$nb_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_nbX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  nb_create_pred<-reactive({
    temp<-vals$nbtab_pred[-1]
    datao<-if(vals$nbpred_which=="Datalist"){
      vals$saved_data[[vals$prednb_new]]
    } else{ vals$saved_data[[input$data_nbX]]}
    temp<-data_migrate(datao,temp,"newdatalist")
    attr(temp,"factors")<-cbind(attr(temp,"factors")[rownames(temp),],vals$nbtab_pred[1])

    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_nb<-reactive({
    bag<-1
    name0<-paste0("NB")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_nbX]],"nb"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_nbX]],"nb"))) break
      }}
    paste(name0,bag)
  })
  name_nb_train_errors<-reactive({
    bag<-1
    name0<-paste0("NB training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_nb_pred<-reactive({
    bag<-1
    name0<-paste0("NB predictions")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    if(vals$hand_save=='Save NB model in'){choices<-names(attr(vals$saved_data[[input$data_nbX]],'nb'))}
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
      "Save NB model in"= {savenb()},
      "Create Datalist: NB training errors -obs"=nb_create_training_errors(),
      "Create Datalist: NB predictions"={nb_create_pred()},
      'resample_create'={resample_create()}

    )
    removeModal()

  })
  module_nb <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    uiOutput(ns('savenb_teste')),
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
