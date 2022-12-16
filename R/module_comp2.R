#' @export
module_ui_comp2 <- function(id){
  ns <- NS(id)

  column(12,uiOutput(ns('COMB_comp2')))

}

#' @export
module_server_comp2 <- function (input, output, session,vals,df_colors,newcolhabs){
  ns <- session$ns
  insert_rv <- reactiveValues(page = 1)
  output$COMB_comp2<-renderUI({
   # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(12,
           splitLayout(cellWidths = c("70px","80%","70px"),
                       div(uiOutput(ns("comp2_prev_bnt"))),
                       div(class="card",
                           uiOutput(ns("comp2_pages"))),
                       div(uiOutput(ns("comp2_next_bnt")))


           ))
  })
  observeEvent(input$comp2_results,{
    vals$comp2_results<-input$comp2_results
  })

  output$page1<-renderUI({

    div(
      uiOutput(ns("data_comp2_picker")),
      uiOutput(ns("data_comp2_out"))
    )
  })
  output$page2<-renderUI({
    pic<-input$limodels2
    req(length(vals$choices_models_equals2)>0)
    choices<-vals$choices_models_equals2
    act<-which(choices==input$limodels2)

    vals$comp2_active<-input[[unlist(lapply(seq_along(names(vals$listamodels2)),function(x)paste0("equals",names(vals$liequals)[x])))[act]]]
    modellist2<-vals$listamodels2[[act]][vals$comp2_active]
    vals$modellist2<-modellist2

    div(
      div(
        span(
          inline(
            div(
              div(strong("New data for predictions")),
              div( pickerInput(ns("predcomb_new"),NULL,names(vals$saved_data[getobs_models()]), width = '300px', selected=vals$predcomb_new))

            )
          ),
          inline(actionButton(ns("run_preds"),"RUN"))
        ),
        tabsetPanel(
          tabPanel("2.1. Predictions",  uiOutput(ns("combclass"))),
          tabPanel("2.2. Performace",
                   uiOutput(ns("perfaout"))
                  ),
          tabPanel(strong("2.3. Resampling predictions"),
                   uiOutput(ns("resample_out"))
          )
        )

      )
    )
  })
  output$createerror<-renderUI({
    req(!is.null(vals$comb_down_errors_train))
    tipify(
      actionLink(
        ns('comb_create_errors_train2'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
      ),
      "Create a datalist with the comb training errors", options=list(container="body")
    )
  })
  output$perfaout<-renderUI({

    sidebarLayout(
      sidebarPanel(


        class="map_control_style",style="color: #05668D",
        div(
          span(tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"), "+ Palette",
               inline(
                 pickerInput(inputId = ns("comb2palette_pred"),
                             label = NULL,
                             choices =     vals$colors_img$val,
                             choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
               )
          )
        ),
        div(
          "+ Round",
          inline(numericInput(ns('round_summ'),NULL,3, width="60px"))
        ),
        div(uiOutput(ns("createerror"))
        ),
        div(
          tipify(
            actionLink(
              ns('comb_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
            ),
            "+ Download Table", options=list(container="body")
          )
        ),

        div(
          span("+ Y Datalist:",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values",
                                      options=list(container="body")),
               inline(
                 pickerInput(ns("predreg_newY"),
                             NULL,names(vals$saved_data[getobscomp()])
                             ,width='150px')
               )
          )
        )


      ),
      mainPanel(
        uiOutput(ns("cmcomb")),
        uiOutput(ns("regcomb"))
      )
    )
  })

  observeEvent(input$comb_create_errors_train2,{
    vals$hand_save<-"Create Datalist: comb training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())
  })



  observeEvent(input$comb_create_errors_train,{
    vals$hand_save<-"Create Datalist: comb training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())
  })


  output$comb_tab_errors_train<-DT::renderDataTable({
   # input<- readRDS('input.rds')
    #vals<- readRDS('vals.rds')
   # beep()
    res<-do.call(data.frame,vals$res_compre)
    sup_test<- attr(vals$modellist2[[1]],'supervisor')
    obs<-vals$saved_data[[input$predreg_newY]][,sup_test]



    res$obs<-obs
    reslist<-split(res,as.factor(rownames(res)))
    res<-do.call(rbind,lapply(reslist, function(x)    postResample(x[length(x)],x[-length(x)])))
    vals$comb_down_errors_train<-res
    res
    DT::datatable(round(res,input$round_summ), options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  output$comb_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('comb_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the comb training errors", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('comb_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "+ Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel()
      )
    )
  })
  output$comb_tab_errors0_train<-DT::renderDataTable({


    DT::datatable({
      sup_test<- attr(vals$modellist2[[1]],'supervisor')
      pred<-vals$rescompres[,1]
      obs<-vals$saved_data[[input$predreg_newY]][,sup_test]
      table<-caret::postResample(pred,obs)
      #table<-readRDS("table.rds")
      data.frame(as.list(table))
    }, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })


  predall_comp<-reactive({
    res<-do.call(data.frame,vals$res_compre)
    colnames(res)<-input$predcomb_new
    res
  })
  comp_observed2<-reactive({

    if(vals$modellist2[[1]]$modelType=="Classification"){
      factors<-attr(vals$saved_data[[input$predreg_newY]],'factors')
    } else {
      factors<-vals$saved_data[[input$predreg_newY]]}

    res<-factors[,attr(vals$modellist2[[1]],"supervisor")]
    names(res)<-rownames(factors[attr(vals$modellist2[[1]],"supervisor")])
    res

  })
  get_qclass<-reactive({
    m<-vals$modellist2[[1]]
    req(m$modelType=="Regression")
    pred<-predall_comp()
    model_data<-test_comp()
    obs_data<-comp_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.comp.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.comp.int,ref1)
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
    vals$comp_treetest<-res
    vals$comp_treetest
  })
  output$comp_tree_show<-renderUI({
    val=if(vals$modellist2[[1]]$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$comp_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_comp())
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
    req(vals$modellist2[[1]]$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("comp_tree_show"))),
        inline(uiOutput(ns("comp_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$predreg_newY)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$modellist2[[1]]
    req(m$modelType=="Classification")
    pred<-predall_comp()
    pred.ind <- pred
    model_data<-test_comp()
    obs_data<-comp_observed2()
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
  output$pick_compobs_pred<-renderUI({
    req(input$comppred_which=="Datalist")
    sup_test<-attr(vals$modellist2[[1]],"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobscomp()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  output$predcomp_newY_ref<-renderUI({
    req(input$predreg_newY)
    req(input$comppred_which)
    req(vals$modellist2[[1]]$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$predreg_newY]],"factors")
    choices=if(input$comppred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('comp_reftest'),NULL,choices, width="200px", selected=vals$comp_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Classification")
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
  output$pick_compobs<-renderUI({
    sup_test<-attr(vals$modellist2[[1]],"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predreg_newY"),
                       NULL,names(vals$saved_data[getobscomp()]), width="200px",selected=vals$predreg_newY
           ))
    )
  })
  test_comp<-reactive({

    if(vals$modellist2[[1]]$modelType=="Classification"){
      factors<-attr(vals$saved_data[[input$predreg_newY]],'factors')
    } else {
      factors<-vals$saved_data[[input$predreg_newY]]}

    res<-factors[,attr(vals$modellist2[[1]],"supervisor")]
    names(res)<-rownames(factors[attr(vals$modellist2[[1]],"supervisor")])
    res

  })
  getobscomp<-reactive({
    sup_test<-attr(vals$modellist2[[1]],"supervisor")

    datalist<-vals$saved_data
    if(vals$modellist2[[1]]$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$modellist2[[1]]
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  output$resample_out<-renderUI({
    m<-vals$modellist2[[1]]

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
      uiOutput(ns("pick_compobs")),
      uiOutput(ns("predcomp_newY_ref")),
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
    req(vals$modellist2[[1]]$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"+ Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$modellist2[[1]]$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_comp()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-comp_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$comp_treetest[colnames(data),]
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
  observeEvent(input$predreg_newY,{
    vals$predreg_newY<-input$predreg_newY
  })
  observeEvent(input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())

  })
  observeEvent(input$resamp_pred_gdownp,{

    vals$hand_down<-"comp_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  output$comb_err_class<-renderUI({
    div(

      renderPrint({
        res<-do.call(data.frame,vals$res_compre)
        colnames(res)<-input$predcomb_new
        res
      })


    )
  })

  output$regcomb<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    div(
      uiOutput(ns("reg_performace")),

      div(
        p(strong("Global:")),
        inline(DT::dataTableOutput(ns('comb_tab_errors0_train')))
      ),

      div(
        p(strong("Observations:")),
        inline(DT::dataTableOutput(ns('comb_tab_errors_train')))
      )




    )

  })
  getobs_reg<-reactive({
    sup_test<- attr(vals$modellist2[[1]],'supervisor')
    datalist<-vals$saved_data
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })
  output$reg_performace<-renderUI({
    validate(need(!is.null(vals$rescompres),"No predictions have been generated yet. Please click 'Run' on panel '2.1. Predictions'"))
    renderPrint({
      sup_test<- attr(vals$modellist2[[1]],'supervisor')
      pred<-vals$rescompres[,1]
      obs<-vals$saved_data[[input$predreg_newY]][,sup_test]
      caret::postResample(pred,obs)
    })
  })
  output$cmcomb<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Classification")
    sup_test<- attr(vals$modellist2[[1]],'supervisor')

    pred<-vals$rescompres[,1]
    validate(need(is.factor(pred),"Confusion Matrices are only valid for Classification models"))
    obs<-attr(vals$saved_data[[input$predreg_newY]],"factors")[,sup_test]

    conf<-table(obs, pred )
    vals$comb_perfomace_class<-postResample(pred,obs)
    tabsetPanel(
      tabPanel("Global",
               div(
                 p(strong("Global:")),
                 inline(DT::dataTableOutput(ns('errors_class_global')))
               ),
               renderPlot({
                 res<-plotCM(conf/sum(conf)*100,input$comb2palette_pred, newcolhabs=vals$newcolhabs)
                 vals$combcm<-res
                 vals$combcm
               }),
               renderPrint(confusionMatrix(table(pred,obs)))),
      tabPanel("Obs. Errors",
               div(
                 p(strong("Observations:")),
                 inline(DT::dataTableOutput(ns('errors_class_obs')))
               )



      )
    )

  })

  output$errors_class_obs<-DT::renderDataTable({

    sup_test<- attr(vals$modellist2[[1]],'supervisor')
    res<-do.call(data.frame,vals$res_compre)
    obs<-attr(vals$saved_data[[input$predreg_newY]],"factors")[,sup_test]

    res$obs<-obs
    sup_test<- attr(vals$modellist2[[1]],'supervisor')

    reslist<-split(res,as.factor(rownames(res)))

    x<-reslist[[1]]
    res<-do.call(rbind,lapply(reslist, function(x){
      Acc<-sum(as.numeric(unlist(x[-length(x)])==unlist(x[length(x)])))/length(unlist(x[-length(x)]))*100
      Err<-100-Acc
      c(Acc,Err)
    }))
    colnames(res)<-c("Accuracy","Error")
    vals$comb_down_errors_train<-res
    res
    DT::datatable(round(res,input$round_summ), options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })
  output$errors_class_global<-DT::renderDataTable({


    DT::datatable({round( data.frame(as.list(vals$comb_perfomace_class)),input$round_summ)}, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })

  getobs_bcm<-reactive({

    sup_test<- attr(vals$modellist2[[1]],'supervisor')
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x) attr(x,"factors"))
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })
  observeEvent(input$run_preds,{


    withProgress(min=0,max=length(vals$modellist2),message="Running",{
      vals$res_compre<-lapply(vals$modellist2,function(x){
        try({

          incProgress(1)
          req(input$predcomb_new)
          res<-predict(x,newdata=vals$saved_data[[input$predcomb_new]])
          res

        })
      })
      pic<-which(unlist(lapply(vals$res_compre,function(x)class(x)=="try-error")))
      if(length(pic)>0){
        vals$res_compre<-vals$res_compre[-pic]
      }

    })

    attr(vals$res_compre,"rownames")<-rownames(vals$saved_data[[input$predcomb_new]])

  })

  output$combclass<-renderUI({
    req(!is.null(vals$res_compre))
    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style",style="color: #05668D",
          div(
            tipify(
              actionLink(
                ns('combb_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
              ),
              "Create a datalist with the combb predictions", options=list(container="body")
            )
          )
        )
      ),
      mainPanel(inline(DT::dataTableOutput(ns('pred_out'))))
    )

  })

  output$pred_out<-DT::renderDataTable({
    getpredictions()
  },options = list(pageLength = 20, info = FALSE,dom = 't'), rownames = TRUE,class ='cell-border compact stripe')
  getpredictions<-reactive({

    type=vals$listamodels2[[1]][[1]]$modelType
    if(is.factor(vals$res_compre[[1]])){
      res<-data.frame(as.factor(names(unlist(lapply(apply(do.call(data.frame,vals$res_compre),1,function(x) table(x)), function(x) which.max(x))))))}else{
        res<-data.frame(apply(do.call(data.frame,vals$res_compre),1,function(x) mean(x)))
      }


    rownames(res)<-  attr(vals$res_compre,"rownames")
    colnames(res)<-"comb_class"
    vals$rescompres<-res
    res

  })


  observeEvent(input$combb_create_predictions,{
    vals$hand_save<- "Create Datalist: combb predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL
    showModal(module_combb())
  })

  getobs_models<-reactive({
    datalist<-vals$saved_data
    data<-vals$saved_data[[input$data_comp2]]
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(data)
        sum(res)==ncol(data)
      })
    )
    names(res0[res0==T])
  })

  output$data_comp2_picker<-renderUI({
    div(
      h5(strong("1. Select the Datalist")),
      pickerInput(ns("data_comp2"),NULL,choices=names(vals$saved_data),  selected=vals$cur_data_comp, options=list(container="body","style-base" = "form-control", style = "")))
  })
  observeEvent(input$data_comp2,{
  vals$cur_data_comp<-input$data_comp2
})
  getdata_comp2<-reactive({
  req(input$data_comp2)
  vals$saved_data[[input$data_comp2]]
})


  observe({
    req(vals$cur_tab=="menu_comp")
    #data<-readRDS("savepoint.rds")$saved_data[["nema_araca"]]
    data<-getdata_comp2()
    choices<-list(
      if(check_model(data,"rf")){"rf"},
      if(check_model(data,"nb")){"nb"},
      if(check_model(data,"svm")){"svm"},
      if(check_model(data,"knn")){"knn"},
      if(check_model(data,"sgboost")){"sgboost"}
    )

    attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]
    vals$attributes2<-attributes
    validate(need(length(vals$attributes2)>0,"No models saved in selected datalist"))
    limodels2<-lapply(attributes,function(x) attr(data,x))
    names0<-unlist(lapply(limodels2,names))
    limodels2<-flattenlist(limodels2)

    linames<-unlist(lapply(attributes,function(x) names(attr(data,x))))
    resmethos<-unlist(lapply(limodels2,function(x) paste0("Type:",x$modelType,"; ")))
    #pic_not<-which(unlist(lapply(limodels2,function(x) is.null(x$resample))))
    #if(length(pic_not)>0){limodels2<-limodels2[-pic_not]}
    liequals<-split(limodels2,as.vector(unlist(lapply(limodels2,function(x)nrow(x$resample)))))
    liequals<-lapply(liequals,function(x) names(x))
    listamodels2<-split(limodels2,resmethos)
    listanames<-split(names0,resmethos)
    for(i in 1:length(listamodels2)){
      names(listamodels2[[i]])<-listanames[[i]]
    }



    vals$choices_models_equals2<-names(listamodels2)
    vals$listamodels2<-listamodels2
  })


  observe({

    output$data_comp2_out<-renderUI({
      choiceValues<-names(vals$listamodels2)
      choiceNames=lapply(seq_along(names(vals$listamodels2)),function(x){
        div(uiOutput(ns(paste0('liequals',x))))
      })
      req(length(choiceValues)>0)
      req(length(choiceNames)>0)
      #data<-readRDS("savepoint.rds")$saved_data[["nema_araca"]]
      validate(need(length(vals$attributes2)>0,"No models saved in selected datalist"))

      div(class='card',


          h5(strong('2. Select the models', tiphelp("comp2arisons are only allowed for models with comp2atible resampling methods."))),
          div(id='comp2_choices',
              radioButtons(ns("limodels2"), NULL, choiceValues=choiceValues,choiceNames =choiceNames, inline=T)
          )

      )
    })
  })



  observeEvent(input$limodels2,{
  vals$cur_limodels2<-input$limodels2
})


  observe({
  req(input$limodels2)
  listamodels2<-vals$listamodels2
  req(!is.null(listamodels2))
  req(vals$cur_tab=="menu_comp")

  liequals<-lapply(listamodels2, names)
  names(liequals)<-1:length(liequals)
  vals$liequals<-liequals
  ress<-list()
  lapply(seq_along(names(listamodels2)),function(x){
    output[[paste0('liequals',x)]]<-renderUI({
      div(class='card',h5(strong(names(listamodels2)[x])),width=12,
        if(input$limodels2==names(listamodels2)[x]){
          div(
            checkboxGroupInput(ns(paste0("equals",names(liequals)[x])), NULL, choices=liequals[[x]], selected=liequals[[x]]), style="max-width: 190px; white-space: normal;font-size: 12px; padding:0px"
          )
        })
    })
  })

})




  insert_npages <- 2
  navPage <- function(direction) {
    insert_rv$page <- insert_rv$page + direction
  }

  observe({
    toggleState(id = ns("insert_prev"), condition = insert_rv$page > 1)
    toggleState(id = ns("page1"), condition = insert_rv$page > 1)
    toggleState(id = ns("insert_next"), condition = insert_rv$page < insert_npages)
    toggleState(id = ns("page2"), condition = insert_rv$page < insert_npages)
    hide(selector = ".page")
    shinyjs::show(paste0("insert_step", insert_rv$page))
  })
  observeEvent(input$comp2_prev, {navPage(-1)})
  observeEvent(input$comp2_next, {
    navPage(1)
    vals$res_compre<-NULL
    vals$rescompres<-NULL
  })
  observeEvent(input$insert_prev, navPage(-1))
  observeEvent(input$insert_next, navPage(1))
  output$comp2_pages<-renderUI({
    insert_rv$page<-1
    div(
      div(class = "page",
          id = ns('insert_step1'),
          uiOutput(ns("page1"))
      ),
      hidden(
        div(class = "page",
            id = ns('insert_step2'),
            uiOutput(ns("page2"))))

    )
  })
  output$comp2_prev_bnt<-renderUI({
    req(insert_rv$page!=1)
    actionButton(ns("comp2_prev"),span(
      div(strong("2. Prev")),
      div("<<"),
    ), style="height: 70px", width="70px")

  })
  output$comp2_next_bnt<-renderUI({
    req(insert_rv$page<insert_npages)
    actionButton(ns("comp2_next"),span(
      div(strong("3. Next")),
      div(">>"),
    ), style="height: 70px", width="70px")
  })

  name_comb_train_errors<-reactive({
    bag<-1
    name0<-paste0("comb training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })

  comb_create_training_errors<-reactive({
    temp<-vals$comb_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$predreg_newY]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Create Datalist: combb predictions"={ name_combb_pred()},
      "resample_create"={ resample_create_name()},
      "Create Datalist: comb training errors -obs"={name_comb_train_errors()},
    )})

  resample_create_name<-reactive({

    bag<-1
    var<-attr(vals$modellist2[[1]],"supervisor")
    name0<-paste("comb",var,"qclass", sep="_")
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
  resample_create<-reactive({

    m<-vals$modellist2[[1]]
    var<-attr(m,"supervisor")
    temp<-vals$comp_treetest
    datao<-vals$saved_data[[input$predreg_newY]]
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
  combb_create_pred<-reactive({


    temp<-vals$rescompres

    datao<- vals$saved_data[[input$data_comp2]]
    newdata<- vals$saved_data[[ input$predcomb_new]]
    newdata[,colnames(temp)]<-as.numeric(temp[,1])
    attr(newdata,"factors")<-temp



    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-newdata
    } else{
      vals$saved_data[[input$over_datalist]]<-newdata
    }

  })

  name_combb_pred<-reactive({
    bag<-1
    name0<-paste0("Comb predictions")
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
    if(vals$hand_save=='Save combb model in'){choices<-names(attr(vals$saved_data[[input$data_combbX]],'combb'))}
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
      "Create Datalist: combb predictions"={combb_create_pred()},
      "resample_create"={resample_create()},
      "Create Datalist: comb training errors -obs"=comb_create_training_errors(),

    )
    removeModal()

  })
  module_combb <- function() {
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

  }
