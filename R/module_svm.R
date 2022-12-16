

#' @export
svmGrid <- function(x, y, len = NULL, search = "grid") {

  ## This produces low, middle and high values for sigma
  ## (i.e. a vector with 3 elements).
  sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                       C = 2 ^((1:len) - 3))
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}
#' @export
plot_dens_svm<-function(m,var="Chl",newcolhabs, palette="viridis", lwd=1, xlab="Variables"){
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1), widths = c("70","30"))
  colors<-getcolhabs(newcolhabs,palette,length(m$levels))
  tables<-m$finalModel$tables[[var]]
  if(lapply(m$finalModel$tables, class)[[1]]=="table"){
    par(mar=c(5,5,5,3), las=1)
    colors<-getcolhabs(newcolhabs,palette,length(levels(m$trainingData[[var]])))
    spineplot(tables, col=colors, las=1,ylab="", xlab=paste("Class:",attr(m,"supervisor")),off=1, axes =T,yaxlabels=F)
    par(mar=c(0,0,5,0), xpd=T)
    plot.new()
    legend(0,1, legend=levels(m$trainingData[[var]]), pch=15, col=colors, bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)

  } else{

    par(mar=c(5,5,5,0))
    res<-lapply(tables, function(x)
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
    plot(m$finalModel$tables[[var]][[1]], xlim=c(minx,maxx), ylim=c(miny,maxy), type="n", main=var)
    i=1
    for(i in 1:length(tables)){
      lines(tables[[i]], col=colors[i], lwd=lwd)
    }

    par(mar=c(0,0,5,0), xpd=T)
    plot.new()
    legend(0,1, legend=m$levels, lty=1, col=colors, bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)
  }


}

#' @export
module_ui_svm <- function(id){

  ns <- NS(id)
  uiOutput(ns("svm_panels"))

}

#' @export
module_server_svm <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns


  output$svm_panels<-renderUI({
   # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(


      column(12,style="background: white",
             uiOutput(ns("svm_inputs")),
             uiOutput(ns("war_svm")),
             column(12,
                    tabsetPanel(id = ns("svm_tab"),
                                tabPanel(strong("1. Training"),value='svm_tab1',
                                         uiOutput(ns("svm_params"))),
                                tabPanel(strong("2. Results"),value='svm_tab2',
                                         uiOutput(ns("results_svm"))),
                                tabPanel(strong("3. Predict"),value='svm_tab3',
                                         uiOutput(ns("predict_svm")))
                    )
             )

      )


    )})
  insert_svm_resutls<-reactiveValues(df=F)




  output$predict_svm<-renderUI({
    column(12,style="background: white", uiOutput(ns("svm_pred_type")),
           tabsetPanel(id="predsvm_tab",
                       tabPanel(
                         strong("3.1. Results"), value="predsvm_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("svm_tab3_1")))),
                       tabPanel(
                         strong("3.2. Pesvmormace"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("svm_tab3_2")))
                       ),
                       tabPanel(strong("3.3. Resampling predictions"), value="svm_tab3_4",
                                uiOutput(ns("resample_out"))
                       )

           )
    )
  })
  output$svm_tab3_2<-renderUI({
    sup_test<-attr(vals$svm_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    sidebarLayout(
      sidebarPanel(
        fluidRow(class="map_control_style",style="color: #05668D",
                 div(
                   span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
                        span(supname),
                        inline(pickerInput(ns("predsvm_newY"),
                                           NULL,names(vals$saved_data[getobssvm()]),width="200px"))
                   )
                 ),
                 div(
                   span(tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"+ Palette",
                        inline(
                          pickerInput(inputId = ns("svmpalette_pred"),
                                      label = NULL,
                                      choices =     vals$colors_img$val,
                                      choicesOpt = list(content =     vals$colors_img$img),options=list(container="body"),width="75px")
                        )
                   )
                 ),
                 div(
                   actionLink(ns("downp_cmsvm_pred"),span("+ Donwload plot"),style  = "button_active")
                 ),
                 div(
                   actionLink(ns("dowcenter_cmsvm_pred"),span("+ Download table"),style  = "button_active"))
        )),
      mainPanel(
        tags$style(
          paste(paste0("#",ns('svm_tab_errors0_test')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
        ),
        tags$style(
          paste0("#",ns('svm_tab_errors0_test')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
        ),
        div(
          p(strong("Prediction stats:")),
          inline(DT::dataTableOutput(ns('svm_tab_errors0_test')))
        ),
        div(
          p(strong("Confusion matrix:")),
          uiOutput(ns("confusion_svm_pred")),
          verbatimTextOutput(ns("confusion_svm2_pred"))
        )






      )
    )
  })

  output$svm_tab_2_4<-renderUI({
    # validate(need(is.factor( vals$svm_results$finalModel$y),"Confusion matrices are only valid for classification (factor)models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("svm_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("svmpalette"),
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
            popify(actionLink(ns("downp_cmsvm"),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
          ),
          div(
            popify(actionLink(ns("dowcenter_cmsvm"),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM table")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("confusion_svm")),
        verbatimTextOutput(ns("confusion_svm2"))
      )
    )

  })

  predall_svm<-reactive({
    validate(need(!anyNA(test_svm()),"NAs not allowed in the prediction Datalist"))
    m<-vals$svm_results
    #factors<-attr(vals$saved_data[[input$predsvm_new]],"factors")
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
  svm_observed2<-reactive({
    req(input$svmpred_which)


    obs_data<-if(input$svmpred_which=="Datalist"){
      if(vals$svm_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$predsvm_newY_tree]],'factors')
      } else {
        factors<-vals$saved_data[[input$predsvm_newY_tree]]}

      res<-factors[,attr(vals$svm_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$svm_results,"supervisor")])
      res
    } else{
      attr(vals$svm_results,"sup_test")
    }
    obs_data
  })
  get_qclass<-reactive({
    m<-vals$svm_results
    req(m$modelType=="Regression")
    pred<-predall_svm()
    model_data<-test_svm()
    obs_data<-svm_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.svm.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.svm.int,ref1)
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
    vals$svm_treetest<-res
    vals$svm_treetest
  })
  output$svm_tree_show<-renderUI({
    val=if(vals$svm_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$svm_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_svm())
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
    req(vals$svm_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$svm_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("svm_tree_show"))),
        inline(uiOutput(ns("svm_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$svm_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$predsvm_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$svm_results
    req(m$modelType=="Classification")
    pred<-predall_svm()
    pred.ind <- pred
    model_data<-test_svm()
    obs_data<-svm_observed2()
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
  output$pick_svmobs_pred<-renderUI({
    req(input$svmpred_which=="Datalist")
    sup_test<-attr(vals$svm_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobssvm()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  output$predsvm_newY_ref<-renderUI({
    req(input$predsvm_newY_tree)
    req(input$svmpred_which)
    req(vals$svm_results$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$predsvm_newY_tree]],"factors")
    choices=if(input$svmpred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('svm_reftest'),NULL,choices, width="200px", selected=vals$svm_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$svm_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$svm_results$modelType=="Classification")
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
  output$pick_svmobs<-renderUI({
    sup_test<-attr(vals$svm_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predsvm_newY_tree"),
                       NULL,names(vals$saved_data[getobssvm()]), width="200px",selected=vals$predsvm_newY_tree
           ))
    )
  })
  output$resample_out<-renderUI({
    m<-vals$svm_results
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
      uiOutput(ns("pick_svmobs")),
      uiOutput(ns("predsvm_newY_ref")),
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
    req(vals$svm_results$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"+ Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$svm_results$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_svm()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-svm_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$svm_treetest[colnames(data),]
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
  observeEvent(input$predsvm_newY_tree,{
    vals$predsvm_newY_tree<-input$predsvm_newY_tree
  })
  observeEvent(input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_svm())

  })
  observeEvent(input$resamp_pred_gdownp,{

    vals$hand_down<-"svm_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })





  output$svm_pred_type<-renderUI({
    choices=if(length(attr(vals$svm_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(inline(radioButtons(ns("svmpred_which"),"New data (X):",choices=choices,inline=T, width="200px")), inline(uiOutput(ns("datalist_pred_svm"))))
  })

  observeEvent(input$predsvm_new,{
    vals$predsvm_new<-input$predsvm_new
  })
  output$datalist_pred_svm<-renderUI({
    req(input$svmpred_which =='Datalist')
    div(
      pickerInput(ns("predsvm_new"),"Datalist",names(vals$saved_data[getnewdatasvm()]), width = '300px', selected=vals$predsvm_new)
    )
  })



  output$confusion_svm_pred<-renderUI({
    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$svmpalette_pred,  newcolhabs=vals$newcolhabs)
             vals$svm_cm_pred<-res
             res
           }))

  })
  observeEvent(input$dowcenter_cmsvm_pred,{
    vals$hand_down<-"svm_cm_test"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

  output$confusion_svm2_pred<-renderPrint({
    res<-confusionMatrix(get_cm_pred())
    vals$svm_cm_test<-  as.data.frame.matrix(res$table)
    res
  })



  output$svm_tab_errors0_test<-DT::renderDataTable({
    table<-pred_test_svm()
    table<-round(table,3)
    rownames(table)<-NULL
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))
  })
  output$svm_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('svm_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the svm predictions", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('down_svm_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "+ Download table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('svmtab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svmtab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('svmtab_pred')))
        )
      )
    )
  })
  output$svmtab_pred<-DT::renderDataTable({
    table<-vals$svmtab_pred<-pred_svm()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))

  }, rownames = TRUE,class ='compact cell-border')


  output$svm_inputs <- renderUI({
    if(is.null(vals$cur_svm_method)){vals$cur_svm_method<-"svmLinear"}
    if(is.null(vals$cur_data_svmY)){vals$cur_data_svmY<-1}
    if(is.null(vals$cur_svm_type)){vals$cur_svm_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
             if(input$svm_tab=="svm_tab1"){
               div(
                 div(class='well3',
                     strong("Type:"),inline(radioButtons(ns('svm_type'),NULL,choices=c("Classification","Regresion"),inline =T, selected=vals$cur_svm_type))
                 ),
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_svmX_out'))),
                          inline(
                            div(
                              div("Method:"),
                              pickerInput(ns("svm_method"),NULL,choices=c("svmLinear","svmRadial"), width="150px", selected=vals$cur_svm_method)
                            )
                          )
                     )
                 ),
                 div(class="well3",
                     span(strong("Y:"),
                          inline(
                            uiOutput(ns("data_svmY_out"))
                          ),
                          "::",
                          inline(uiOutput(ns('svm_Y'))),

                          inline(uiOutput(ns("svm_partition"))),
                          "::",
                          inline(uiOutput(ns("svm_test_ref")))
                     )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_svmX_out'))),
                      inline(uiOutput(ns("svm_results_menu"))),
                      inline(uiOutput(ns("savesvm_button"))),
                      inline(uiOutput(ns("rmsvm_button"))),
               )
             }
    )
  })
  output$data_svmY_out<-renderUI({
    div(
      div("Datalist:"),
      div(pickerInput(ns("data_svmY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_data_svmY))
    )
  })
  output$svm_Y <- renderUI({
    req(input$data_svmY)
    req(input$svm_type)

    if(is.null(vals$cur_svm_sup)){vals$cur_svm_sup<-1}
    data <- vals$saved_data[[input$data_svmY]]
    labels <- attr(data,"factors")
    choices<-if(input$svm_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("svm_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_svm_sup
        ),"The response vector"))
    )

  })
  output$svm_partition<-renderUI({
    req(input$data_svmX)
    if(is.null(vals$cur_svm_test_partition)){vals$cur_svm_test_partition<-1}
    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("svm_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_svmY]],"factors"))), width="150px", selected=vals$cur_svm_test_partition))
    )
  })
  output$svm_test_ref<-renderUI({
    req(input$svm_test_partition!="None")
    req(input$data_svmY)
    if(is.null(vals$cur_testdata_svm)){vals$cur_testdata_svm<-1}
    fac<-attr(vals$saved_data[[input$data_svmY]],"factors")[,input$svm_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_svm"),NULL, choices=choices, width="200px", selected=vals$cur_testdata_svm)
    )
  })
  output$data_svmX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_svmX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_svms")))

    )
  })

  output$saved_svms<-renderUI({
    req(input$data_svmX)
    req(length(names(attr(vals$saved_data[[input$data_svmX]],"svm")))>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names(attr(vals$saved_data[[input$data_svmX]],"svm")))), "saved model(s)")
  })

  output$svm_params<- renderUI({
    column(12,class="well2",
           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               div(
                 div(
                   tipify(icon("fas fa-question-circle"),"Tuning search", options = list(container="body")),
                   "+ search:",
                   inline(
                     pickerInput(ns("svm_search"), NULL, choices = c("grid","random","user-defined"), width="110px")
                   )
                 ),
                 uiOutput(ns("svm_search_length")),
               ),
               uiOutput(ns("svm_grid"))
               ,
               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedsvm"), NULL, value = NULL, width="122px")
                 )
               ),

               div(popify(icon("fas fa-question-circle"),NULL,
                          HTML(paste0(
                            div(HTML(paste0(strong("repeatedcv:")," repeated k-fold cross-validation;"))),
                            div(HTML(paste0(strong("boot:")," bootstraping;"))),

                            div(HTML(paste0(strong("LOOCV:")," Leave one out;"))),
                            div(HTML(paste0(strong("LGOCV:")," Leave-group out")))

                          )), options=list(container="body")

               ),
               "+ Resampling method:",
               pickerInput(ns("svm_res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               uiOutput(ns("svm_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("svm_war"))
           ),

           column(12, align = "center",
                  popify(actionButton(ns("trainsvm"), h4(img(src=svm_icon,height='20',width='20'),"train Support Vector Machine",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
           ),

    )
  })
  output$svm_grid<-renderUI({
    req(input$svm_search=="user-defined")
    div(
      div(
        tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for the cost regularization parameter. This parameter controls the smoothness of the fitted function, essentially higher values for C lead to less smooth functions (if a vector, comma delimited)", options=list(container="body")),
        "+ Cost",
        textInput(ns('cost_svm'), NULL, value ='1', width="200px")),
      if(input$svm_method=="svmRadial") {
        div(
          tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for bandwidth of kernel function. If the sigma value is very small, then the decision boundary is highly non-linear. If a vector must be comma delimited between 0 and 1)", options=list(container="body")),
          "+ Sigma",
          textInput(ns('sigma_svm'), NULL, value ='1', width="200px")

        )
      }
    )
  })
  output$svm_search_length<-renderUI({
    req(input$svm_search!='user-defined')
    if(is.null(vals$cur_svm_tuneLength)){vals$cur_svm_tuneLength<-5}

    div(
      tipify(icon("fas fa-question-circle"),"The maximum number of mtry- combinations that will be generated", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("svm_tuneLength"), NULL, value = vals$cur_svm_tuneLength, width="82px")
      )
    )
  })
  output$svm_resampling<-renderUI({
    div(
      inline(
        if(input$svm_res_method=='cv'|input$svm_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
            numericInput(ns("cvsvm"), NULL, value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$svm_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatssvm"),NULL, value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$svm_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
              "+ number:",
              numericInput(ns("cvsvm"), NULL, value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$svm_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavesvm"), NULL, value = 10,width="100px")
            )
          )
        }
      )
    )
  })


  output$svm_results_menu<-renderUI({
    if(is.null(vals$cur_svm_models)){vals$cur_svm_models<-1}
    div(pickerInput(ns("svm_models"),strong("svm results:", tiphelp("Support Machine Vector Results. Click to select svm results saved in the Training Datalist (X).")), choices= names(attr(vals$saved_data[[input$data_svmX]],"svm")), width="200px", selected = vals$cur_svm_models)
    )
  })
  output$rmsvm_button<-renderUI({
    req(input$svm_models!="new svm (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmsvm"), icon("far fa-trash-alt")), "Remove the svm model from the training Datalist (X)")
    )

  })
  output$svm_tab_2_1<-renderUI({
    m<-vals$svm_results
    div(
      renderPrint(m),
      div(style="height: 100px",
          popify(downloadButton(ns("down_svm_results"),label=span("Download svm model",img(src=svm_icon,height='20',width='20')),style = "button_active"),NULL,
                 HTML(paste0(
                   div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
                 )))
      )

    )
  })
  output$down_svm_results <- {
    downloadHandler(
      filename = function() {
        paste0("SVM","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$nb_results,file)
      })
  }
  observeEvent(input$svm_tab2,{
    vals$svm_tab2<-input$svm_tab2
  })

  output$results_svm<-renderUI({
    validate(need(length(vals$svm_results)>0,"No models have been trained yet"))
    if(is.null(vals$svm_tab2)){vals$svm_tab2<-'svm_tab2_1'}
    #saveRDS(vals$svm_results,"svm.rds")
    req(length(vals$svm_results))
    m<-vals$svm_results
    #m<-readRDS("svm.rds")
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_svm())
             )
           ),
           tabsetPanel(id=ns("svm_tab2"),selected=vals$svm_tab2,
                       tabPanel("1. Summary",value="svm_tab2_1",
                                uiOutput(ns("svm_tab_2_1"))
                       ),
                       tabPanel("2. Pesvmormace",value="svm_tab2_2",
                                uiOutput(ns("svm_tab_2_2"))
                       ),
                       tabPanel("3. Variable importance",
                                sidebarLayout(
                                  sidebarPanel(
                                    uiOutput(ns("svm_tab_2_3side"))
                                  ),
                                  mainPanel(  uiOutput(ns("svm_tab_2_3")))
                                )

                       ),
                       tabPanel("4. Confusion matrix",value="svm_tab2_4",
                                uiOutput(ns("svm_tab_2_4"))
                       )
           )

    )
  })


  output$svm_tab_2_3side<-renderUI({
    req(length(vals$svm_results))
    m<-vals$svm_results
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    if(m$modelType=="Classification"){    choices<-c("default","gg")} else{
      choices<-c("default")
    }


    div(
      class="map_control_style",style="color: #05668D",
      div("+ Number of variables:",
          numericInput(ns("varImp_n_svm"),NULL,value=val,step=1,max=nvars,width="80px")
      ),
      div("+ Plot type:", inline(pickerInput(ns("varimp_type"),NULL,choices=choices,width="100px"))),

      div(uiOutput(ns("varimp_gg"))),
      br(),
      div(
        tipify(
          actionLink(
            ns('svm_varImp'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style="button_active"
          ),
          "Download Table (importances)",options=list(container="body")
        )
      ),
      div(
        tipify(
          actionLink(
            ns('svm_varImp_plot'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style="button_active"
          ),
          "Download plot",options=list(container="body")
        )
      )


    )
  })

  output$varimp_gg<-renderUI({
    #req(input$varimp_type=='gg')
    div(class="palette",
        div(
          span("+ Palette:",
               inline(
                 pickerInput(inputId = ns("varimp_col"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px')))),
        div(class="palette",
          div("+ display", inline(pickerInput(ns("varimp_pos"),NULL,c("dodge","stack"),width="100px"))
        ))
    )
  })

  output$svm_tab_2_3<-renderUI({
    req(vals$svm_results)
    req(input$varImp_n_svm)
    renderPlot({
      m<-vals$svm_results
      vals$svm_varImp<-caret::varImp(m)$importance
      vals$svm_varImp_plot<-plot_varimport(m,palette=input$varimp_col,vals$newcolhabs,input$varimp_pos, type=input$varimp_type, nvars=input$varImp_n_svm)
      vals$svm_varImp_plot
    })
  })


  output$svm_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('svm_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the svm training errors", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('svm_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "+ Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('svm_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svm_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('svm_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svm_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('svm_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('svm_tab_errors_train')))
          )



        )
      )
    )
  })
  output$svm_tab_errors0_train<-DT::renderDataTable({
    m<-vals$svm_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$svm_tab_errors_train<-DT::renderDataTable({
    m<-vals$svm_results
    table<-accu_rf_class(m)
    vals$svm_down_errors_train<-table
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })



  observeEvent(input$dowcenter_cmsvm,{
    vals$hand_down<-"svm_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$confusion_svm <- renderPlot({
    m<-vals$svm_results
    if(input$svm_cm_type=="Resampling"){
      res<-plotCM(m, input$svmpalette,  newcolhabs=vals$newcolhabs)
    } else{
      cm<-table(predict(vals$svm_results),vals$svm_results$trainingData[,'.outcome'])
      res<-plotCM(cm,input$svmpalette,vals$newcolhabs)
    }
    vals$cm_svm<-res
    res
  })
  output$confusion_svm2<-renderPrint({
    res<-if(input$svm_cm_type=="Resampling"){
      confusionMatrix(vals$svm_results$pred$pred,vals$svm_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$svm_results),vals$svm_results$trainingData[,'.outcome'])
    }
    vals$svm_cm<-as.data.frame.matrix(res$table)
    res
  })



  output$savesvm_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savesvm"), icon("fas fa-save")), "Save the svm model in the training Datalist (X)")
    )
  })

  getnewdatasvm<-reactive({
    datalist<-vals$saved_data

    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }

    )

    datalist_comp<-vals$saved_data




    m<-vals$svm_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  get_supervisor_svm <- reactive({
    m<-vals$svm_results
    att<-if(m$modelType=="Classification")
    {
      data <- vals$saved_data[[input$data_svmY]]
      labels <- attr(data,"factors")
      labels[input$svm_sup]
    } else{
      data <- vals$saved_data[[input$data_svmY]]
      data[input$svm_sup]
    }

  })
  get_cm_pred<-reactive({
    obs<-get_svm_prederrors()$obs
    pred<-get_svm_prederrors()$pred
    conf<-table(obs, pred)
    conf
  })
  pred_test_svm<-reactive({
    obs<-get_svm_prederrors()$obs
    pred<-get_svm_prederrors()$pred
    table<-data.frame(t(postResample(pred,obs)))
    table
  })


  get_svm_prederrors<-reactive({
    m<-vals$svm_results
    pred<-pred_svm()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$svmpred_which=="Partition"){
      attr(m,"sup_test")} else{
        if(m$modelType=="Classification"){
        attr(vals$saved_data[[input$predsvm_newY]],"factors")[,attr(m,"supervisor")]} else{
          vals$saved_data[[input$predsvm_newY]][,attr(m,"supervisor")]
        }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })


   getobssvm<-reactive({
    sup_test<-attr(vals$svm_results,"supervisor")

    datalist<-vals$saved_data
    if(vals$svm_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$svm_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })
  test_svm<-reactive({
    req(input$svmpred_which)
    pred_tab<-if(input$svmpred_which=="Partition"){attr(vals$svm_results,"test")} else if(input$svmpred_which=="Datalist"){
      req(input$predsvm_new)
      pred_tab<-vals$saved_data[[input$predsvm_new]]
    }
    model_data<-pred_tab
    pred_tab

  })
  pred_svm<-reactive({
    validate(need(!anyNA(test_svm()),"NAs not allowed in the prediction Datalist"))
    m<-vals$svm_results


    svm_pred <- predict(m$finalModel,newdata = test_svm())
    res<-data.frame(Predictions= svm_pred)
    rownames(res)<-rownames(test_svm())
    #colnames(res)<-attr(vals$svm_results,"supervisor")

    # attr(res,"obs")<-test_svm()
    res



  })
  get_parts_svm<-reactive({
    req(input$data_svmY)
    req(input$testdata_svm)
    partition<- attr(vals$saved_data[[input$data_svmY]],"factors")[rownames(vals$saved_data[[input$data_svmX]]), input$svm_test_partition]
    test<-which(partition==input$testdata_svm)
    train<-which(partition!=input$testdata_svm)
    list(train=train,test=test)
  })
  getdata_svmX<-reactive({
    req(input$data_svmX)
    data=vals$saved_data[[input$data_svmX]]
    data
  })
  getdata_svmY<-reactive({
    req(input$data_svmY)
    req(input$svm_sup)

    data <- vals$saved_data[[input$data_svmY]]
    if(input$svm_type=="Classification"){
      labels <- attr(data,"factors")
      labels<-labels[rownames(vals$saved_data[[input$data_svmX]]),input$svm_sup]
      labels
    } else{
      data<-data[rownames(vals$saved_data[[input$data_svmX]]),input$svm_sup]
      data
    }

  })
  getmodel_svm<-reactive({
    m<-vals$svm_results
    att<-if(m$modelType=="Classification"){
      "Factor-Attribute"
    } else{ "Data-Attribute"}
    attri<-attr(m,'inputs')
    res<-c(attri$Ydatalist,
           att,
           attri$Y,
           attri$Xdatalist)
    model<-  paste( paste0(res[1],"::",res[2],"::",res[3]),"~",paste0(res[4]))
    model

  })


  observeEvent(input$predsvm_new,{
    vals$predsvm_new<-input$predsvm_new
  })
  observeEvent(input$svmpred_which,{
    vals$svmpred_which<-input$svmpred_which
  })
  observeEvent(input$svm_down_errors_train,{
    vals$hand_down<-"SVM - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$svm_varImp,{
    vals$hand_down<-"SVM - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$down_svm_tab3_1,{
    vals$hand_down<-"SVM - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(input$data_svmY,{
    vals$cur_data_svmY<-input$data_svmY
  })
  observeEvent(input$svm_type,{
    vals$cur_svm_type<-input$svm_type
  })
  observeEvent(input$svm_tuneLength,{
    vals$cur_svm_tuneLength<-input$svm_tuneLength
  })
  observeEvent(input$svm_method,{
    vals$cur_svm_method<-input$svm_method
  })
  observeEvent(input$svm_sup,{
    vals$cur_svm_sup<-input$svm_sup
  })
  observeEvent(input$data_svmX,{
    vals$cur_data<-input$data_svmX
  })
  observeEvent(input$svm_test_partition,{
    vals$cur_svm_test_partition<-input$svm_test_partition
  })
  observeEvent(input$testdata_svm,{
    vals$cur_testdata_svm<-input$testdata_svm
  })
  observeEvent(input$svm_models,{
    vals$cur_svm_models<-input$svm_models
  })
  observeEvent(input$trainsvm,{
    x<-x_o<-data.frame(getdata_svmX())
    y<-y_o<-getdata_svmY()



    output$svm_war<-renderUI({
      column(12,style="color: red",align="center",
             if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
             if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
      )
    })
    validate(need(anyNA(x)==F,"NAs not allowed in X"))
    validate(need(anyNA(y)==F,"NAs not allowed in Y"))
    req(input$svm_test_partition)
    if(input$svm_test_partition!="None"){
      parts<-get_parts_svm()
      train<-parts$train
      test<-parts$test
      x<-data.frame(getdata_svmX()[train,])
      y<-getdata_svmY()[train]
    }
    #readrd(y,"y.rds")
    #saveRDS(x,"x.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #y<-readRDS("y.rds")
    #x<-readRDS("x.rds")
    #input<-readRDS("input.rds")
    if(input$svm_search=="user-defined"){
      nb_search<-"grid"
      cost_svm<-as.numeric(unlist(strsplit(input$cost_svm,",")))
      if(input$svm_method!="svmLinear"){

        sigma_svm<-as.numeric(unlist(strsplit(input$sigma_svm,",")))
        grid <- expand.grid(C=c(cost_svm),sigma=c(sigma_svm))
      } else{
        grid <- expand.grid(C=c(cost_svm))
      }


    } else{
      grid<-svmGrid(x,y, len=input$svm_tuneLength, input$svm_search)
      if(input$svm_method=="svmLinear"){
        grid<-grid["C"]
      }
    }

    seed<-if (!is.na(input$seedsvm)) { input$seedsvm} else{
      NULL
    }
    svm_search<-input$svm_search

    colnames(x)<-gsub(" ",".", colnames(x))
    if (!is.na(input$seedsvm)) {set.seed(input$seedsvm)}
    withProgress(message = "Running Support Vector Machine ...",
                 min = 1,
                 max = 1,
                 {
                   svm<-train(x,y,input$svm_method,

                              trControl=trainControl(

                                method = input$svm_res_method,
                                number = input$cvsvm,
                                repeats = input$repeatssvm,
                                p=input$pleavesvm/100,
                                savePredictions = "all"

                              ),
                              tuneGrid=grid
                   )
                   attr(svm,"test_partition")<-paste("Test data:",input$svm_test_partition,"::",input$testdata_svm)
                   attr(svm,"Y")<-paste(input$data_svmY,"::",input$svm_sup)
                   attr(svm,"Datalist")<-paste(input$data_svmX)

                   vals$svm_unsaved<-svm

                   if(input$svm_test_partition!="None"){
                     attr(vals$svm_unsaved,'test')<-x_o[test,]
                     attr(vals$svm_unsaved,"sup_test")<-y_o[test]
                   } else{ attr(vals$svm_unsaved,'test')<-c("None")}
                   vals$bag_svm<-T
                   attr(vals$svm_unsaved,"supervisor")<-input$svm_sup
                   attr(vals$svm_unsaved,"inputs")<-list(
                     Ydatalist=input$data_svmY,
                     Y=input$svm_sup,
                     Xdatalist=input$data_svmX
                   )
                   updateTabsetPanel(session,"svm_tab","svm_tab2")
                 })
    beep(10)
    attr(vals$saved_data[[input$data_svmX]],"svm")[['new svm (unsaved)']]<-vals$svm_unsaved
    vals$cur_svm_models<-"new svm (unsaved)"
    vals$bag_svm<-T
    #saveRDS(vals$svm_unsaved,"svm.rds")
  })
  observeEvent(input$downp_cmsvm,{
    vals$hand_plot<-"SVM - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$downp_cmsvm_pred,{
    vals$hand_plot<-"SVM - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(input$svm_varImp_plot,{
    vals$hand_plot<-"SVM - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_svm_dens,{
    vals$hand_plot<-"SVM - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$svm_models)
    if(input$svm_models=="new svm (unsaved)"){
      vals$svm_results<-vals$svm_unsaved } else{
        vals$svm_results<-attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]][[1]]
      }
  })
  observeEvent(input$svm_models,{
    vals$cur_svm<-input$svm_models
  })
  observeEvent(input$tools_rmsvm,{
    attr(vals$saved_data[[input$data_svmX]],"svm")[input$svm_models]<-NULL
  })
  observeEvent(input$tools_savesvm,{
    if(input$tools_savesvm %% 2){
      vals$hand_save<-"Save SVM model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_svmX, style="color:gray"),strong("::"), em("svm-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL

      showModal(module_svm())

    }
  })
  observeEvent(input$svm_create_errors_train,{
    vals$hand_save<-"Create Datalist: SVM training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_svm())
  })
  observeEvent(input$svm_create_predictions,{
    vals$hand_save<- "Create Datalist: SVM predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL

    showModal(module_svm())
  })



  resample_create<-reactive({

    m<-vals$svm_results
    var<-attr(m,"supervisor")
    temp<-vals$svm_treetest
    datao<-vals$saved_data[[input$data_svmX]]
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
    var<-attr(vals$svm_results,"supervisor")
    name0<-paste("svm",var,"qclass", sep="_")
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
      ##svm
      "Save SVM model in"= { name_save_svm()},
      "Create Datalist: SVM training errors -obs"={name_svm_train_errors()},
      "Create Datalist: SVM predictions"={ name_svm_pred()},
      "resample_create"={name_resample_create()}
    )})
  savesvm<-reactive({
    req(vals$cur_svm_models=='new svm (unsaved)')
    temp<-vals$svm_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_svmX]],"svm")[[input$newdatalist]]<-c(temp,attr(vals$saved_data[[input$data_svmX]],"svm")[[input$newdatalist]])
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$svm_over
      attr(vals$saved_data[[input$data_svmX]],"svm")[input$svm_over]<-temp
      cur<-input$svm_over
    }
    attr(vals$saved_data[[input$data_svmX]],"svm")['new svm (unsaved)']<-NULL
    vals$bag_svm<-F
    vals$cur_svm_models<-cur
    vals$cur_svm<-cur

  })
  svm_create_training_errors<-reactive({
    temp<-vals$svm_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_svmX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  svm_create_pred<-reactive({
    temp<-data.frame(vals$svmtab_pred)
    temp[,1]<-as.numeric( temp[,1])
    attr(temp,"factors")<-temp

    datao<-if(vals$svmpred_which=="Datalist"){
      vals$saved_data[[vals$predsvm_new]]
    } else{ vals$saved_data[[input$data_svmX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_svm<-reactive({
    bag<-1
    name0<-paste0("SVM")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_svmX]],"svm"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_svmX]],"svm"))) break
      }}
    paste(name0,bag)
  })
  name_svm_train_errors<-reactive({
    bag<-1
    name0<-paste0("SVM training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_svm_pred<-reactive({
    bag<-1
    name0<-paste0("SVM predictions")
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
    if(vals$hand_save=='Save SVM model in'){choices<-names(attr(vals$saved_data[[input$data_svmX]],'svm'))}
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
      "Save SVM model in"= {savesvm()},
      "Create Datalist: SVM training errors -obs"=svm_create_training_errors(),
      "Create Datalist: SVM predictions"={svm_create_pred()},
      'resample_create'={resample_create()}

    )
    removeModal()

  })
  module_svm <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    uiOutput(ns('savesvm_teste')),
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
