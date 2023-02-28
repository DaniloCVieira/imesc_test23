#' @export
module_ui_comp2 <- function(id){
  ns <- NS(id)

  div(
    actionButton(ns("teste_comb"),"SAVE"),
    uiOutput(ns('COMB_comp2')))

}

#' @export
module_server_comp2 <- function (input, output, session,vals,df_colors,newcolhabs){
  ns <- session$ns
  insert_rv <- reactiveValues(page = 1)
  output$COMB_comp2<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(
      column(12,
             splitLayout(cellWidths = c("70px","90%","70px"),
                         div(uiOutput(ns("comp2_prev_bnt"))),
                         div(class="card",
                             uiOutput(ns("comp2_pages"))),
                         div(uiOutput(ns("comp2_next_bnt")))


             )),
      column(12,
             uiOutput(ns("page3")))
    )
  })


  output$page3<-renderUI({
    req(is.data.frame(vals$res_compre))
    req(insert_rv$page==2)
    div(style="margin-top: 5px",id="combpage3",
        sidebarLayout(
          uiOutput(ns('side_ensemble')),
          mainPanel(
            div(class="card",
                tabsetPanel(id=ns("tab_comb"),
                            tabPanel(strong("4. Model results"),
                                     value="tab1",
                                     tabsetPanel(id=ns("tab_model"),
                                                 tabPanel("4.1. Predictions",
                                                          value="tab1",
                                                          uiOutput(ns("models_pred"))),
                                                 tabPanel(strong("4.4. Feature importance"),
                                                          value='tab2',
                                                          uiOutput(ns("feat_models"))
                                                 )
                                     )),
                            tabPanel(strong("5. Ensemble results"),
                                     value="tab2",
                                     tabsetPanel(id=ns("tab_ense"),
                                                 tabPanel("5.1. Predictions",
                                                          value="tab1",
                                                          uiOutput(ns("ensemb_pred"))),
                                                 tabPanel("5.2. Performace",
                                                          value="tab2",
                                                          uiOutput(ns("perfaout"))),
                                                 tabPanel("5.3. Observation errors",
                                                          value="tab3",
                                                          uiOutput(ns("tab2_out"))),
                                                 tabPanel("5.4. Plot model predictions",
                                                          value="tab4",
                                                          uiOutput(ns("qclass_out"))),
                                                 tabPanel(strong("5.5. Feature importance"),
                                                          value='tab5',
                                                          uiOutput(ns("feat_ensemble"))
                                                 ),
                                                 tabPanel(strong("5.6. Interactions"),
                                                          value='tab6',
                                                          uiOutput(ns("interactions_show")),
                                                          uiOutput(ns("interactions_out"))
                                                 )
                                     ))


                )
            )
          )
        )
    )
  })

  output$interactions_out<-renderUI({
    req(input$inter_show)
    if(input$inter_show=="Plot"){
      uiOutput(ns('interactions_plot'))
    } else{ uiOutput(ns("interactions_result")) }
  })



  output$interactions_show<-renderUI({
    req(!is.null(vals$inter_res0))
    div(strong("+ Show:"),
        inline(radioButtons(ns('inter_show'),NULL,c('Plot', 'Results'), inline=T))
    )
  })


  observeEvent(input$help_weig2,{
    showModal(
      weig_help()
    )
  })
  weig_help<-reactive({
    modalDialog(
      title='Weights for ensembling models',
      easyClose =T,
      size="l",
      div(uiOutput(ns("votes_help")),
          uiOutput(ns("wmean_help")))
    )
  })
  observeEvent(input$help_weig,{
    showModal(
      weig_help()
    )
  })


  observeEvent(list(input$inter_sig,vals$inter_res0),{
    req(!is.null(vals$inter_res0))
    req(input$inter_sig)
    req(input$inter_rep)
    vals$inter_res<-get_inter_results( vals$inter_res0, reps=input$inter_rep,sig=input$inter_sig)
  })

  observeEvent(input$run_inter,{
    predtab<-vals$res_compre
    modelist<-vals$modellist2
    newdata<-vals$saved_data[[input$predcomb_new]]
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]
    obc<-get_obc()
    inter_res0<-getinteraction(newdata,predtab,obc,reps=input$inter_rep,modelist,sig=input$inter_sig,top.features=input$inter_top, scale=input$inter_scale, method=input$en_method,root=input$inter_root)
    vals$inter_res0<-inter_res0
  })

  output$interactions_result<-renderUI({
    div(
      div(
        "Table"
      ),
      inline(DT::dataTableOutput(ns('interactions_df')))

      # inline(DT::dataTableOutput(ns('interactions_df')))

    )
  })

  output$interactions_df<-DT::renderDataTable({
    data.frame(vals$inter_res$diff_to_root)
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')



  output$interactions_plot<-renderUI({
    req(!is.null(vals$inter_res))


    renderPlot({
      vals$intercomb_plot<-get_interplot2(vals$inter_res,
                                          palette=input$pal_inter,
                                          vals$newcolhabs,
                                          cex.axes=input$inter_cex.axes,
                                          cex.lab=input$inter_cex.lab,
                                          cex.main=input$inter_cex.main,
                                          cex.leg=input$inter_cex.leg,
                                          leg=input$inter_leg,
                                          xlab=input$inter_xlab,
                                          ylab=input$inter_ylab)

      vals$intercomb_plot
    })
  })

  output$inter_root<-renderUI({
    predtab<-vals$res_compre
    modelist<-vals$modellist2
    req(input$inter_scale)
    req(input$en_method)
    req(input$inter_top)

    tops<-gettop(modelist,scale=input$inter_scale,method=input$en_method,weis,top.features=input$inter_top)
    obc<-get_obc()

    if(modelist[[1]]$modelType=="Classification") {
      weis<-getClass_wei(predtab, obc)}else {
        weis<-getReg_wei(predtab, obc)}
    checkboxGroupInput(ns("inter_root"),"+ Root",tops, selected=tops[1])
  })
  output$inter_scale<-renderUI({
    div(
      checkboxInput(ns("inter_scale"), "+ Scale importances", T, width="150px")
    )
  })
  output$inter_top<-renderUI({
    div("+ Top variables",inline(numericInput(ns("inter_top"),NULL, value=10, width="75px", step=0.02))
    )
  })
  output$inter_sig<-renderUI({
    div("+ Significance level",inline(numericInput(ns("inter_sig"),NULL, value=0.05, width="75px", step=0.02))
    )
  })
  output$run_inter<-renderUI({
    req(length(input$inter_top)>0)
    div(class="save_changes",actionButton(ns("run_inter"),"+ Calculate Interactions"))
  })
  output$inter_rep<-renderUI({
    div(
      span("+ Repetions:",
           inline(
             numericInput(ns("inter_rep"),NULL, value=5, width="75px", step=2)
           )
      )
    )
  })
  output$inter_ggplot<-renderUI({
    div(
      div("+ Title size",inline(numericInput(ns("inter_cex.main"),NULL, value=15, width="75px", step=1))),
      div("+ Axes size",inline(numericInput(ns("inter_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("inter_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("inter_cex.leg"),NULL, value=13, width="75px", step=1))),
      hr(),
      div("+ Xlab",inline(textInput(ns("inter_ylab"), NULL, 'Uncertainty decrease (%)', width="200px")),
          div("+ Ylab",inline(textInput(ns("inter_xlab"), NULL, 'Variables', width="200px"))),
      ),
      div("+ Legend title",inline(textInput(ns("inter_leg"), NULL, "", width="200px")))
    )
  })
  output$inter_palette<-renderUI({
    span("+ Palette",
         inline(
           pickerInput(inputId = ns("pal_inter"),
                       label = NULL,
                       choices =     vals$colors_img$val,
                       choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
         )
    )
  })
  output$side_interactions<-renderUI({
    req(input$tab_comb=="tab2")
    req(input$tab_ense=="tab6")
    div(
      hr(),
      div(
        div(strong("Interactions:", pophelp(NULL,"Interaction I (X, Y) is defined as the decrease in uncertainty about X (root) after observing Y. Low, close to zero, I means that the variables are close to independent. The larger the I, the larger the reduction of uncertainty in X when Y is known."))),
        div(
          inline(uiOutput(ns('inter_top'))), inline(uiOutput(ns('inter_scale')))
        ),
        uiOutput(ns('inter_root')),
        uiOutput(ns('inter_rep')),
        uiOutput(ns('run_inter'))
      ),
      hr(),
      uiOutput(ns('inter_plot_params'))


    )
  })

  output$inter_plot_params<-renderUI({
    req(!is.null(vals$inter_res0))
    div(
      div(strong("Plot parameters:")),
      uiOutput(ns('inter_sig')),
      uiOutput(ns('inter_palette')),
      uiOutput(ns('inter_ggplot'))
    )

  })

  output$side6<-renderUI({
    if(input$tab_comb=='tab1'){
      req(input$tab_model=='tab2')}
    if(input$tab_comb=='tab2'){
      req(input$tab_ense=='tab5')}

    div(
      div(
        checkboxInput(ns("scale_imp"), "+ Scale importances", T)
      ),
      div(style="margin-top: 5px; margin-bottom: 5px",
          hr(),
          div(strong("Color by:")),
          radioButtons(ns("colby"), NULL, choiceValues=c("models",'aculoss'),choiceNames =c("Models",'Performace loss'), inline=T),
          div(style="margin-left: 5px;",

              uiOutput(ns('rep_aculoss')),
              uiOutput(ns('run_aculoss')),
              uiOutput(ns('nvars_feaimp')),
              uiOutput(ns('sig_lev'))

          ),
          hr()
      ),

      uiOutput(ns('feamp_textlab')),
    )
  })


  output$feamp_textlab<-renderUI({
    req(input$colby)
    div(
      div("+ Title size",inline(numericInput(ns("feaimp_cex.main"),NULL, value=15, width="75px", step=1))),
      div("+ Axes size",inline(numericInput(ns("feaimp_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.leg"),NULL, value=13, width="75px", step=1))),
      hr(),

      div("+ Xlab",inline(textInput(ns("feaimp_xlab"), NULL, 'Variables', width="200px"))),
      div("+ ylab",inline(textInput(ns("feaimp_ylab"), NULL, 'Importance', width="200px"))),
      div("+ Legend title",inline(textInput(ns("feaimp_leg"), NULL, if(input$colby=="models") {'Model'}else{"Performace loss (%)"}, width="200px")))
    )
  })

  observeEvent(input$rep_aculoss,{
    vals$aculoss<-NULL
  })
  output$sig_lev<-renderUI({
    req(input$colby=="aculoss")
    div(
      div(
        span(
          column(12,"+ Filter Perfomace loss higher then"),
          div(inline(numericInput(ns("aculoss_sig"),NULL, value=1, width="75px", step=0.02)),"(%)"),


        )
      )
    )
  })

  output$nvars_feaimp<-renderUI({
    req(input$colby=="models")
    modelist<-vals$modellist2
    newdata<-vals$saved_data[[input$predcomb_new]]
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]

    div(
      span("+ Number of variables:",
           inline(
             numericInput(ns("aculoss_npic"),NULL, value=ncol(newdata), width="75px", step=1)
           )
      )
    )
  })

  output$rep_aculoss<-renderUI({
    req(input$colby=="aculoss")
    div(
      span("+ Repetions:",
           inline(
             numericInput(ns("aculoss_rep"),NULL, value=5, width="75px", step=2)
           )
      )
    )
  })
  output$run_aculoss<-renderUI({
    req(input$colby=="aculoss")
    req(is.null(vals$aculoss))
    div(class="save_changes",actionButton(ns("run_aculoss"),"+ Calculate Performace Loss"))
  })
  observeEvent(input$aculoss_rep,{
    vals$aculoss<-NULL
  })
  observeEvent(input$tab_comb,{
    vals$aculoss<-NULL
  })

  observeEvent(input$run_aculoss,{
    modelist<-vals$modellist2
    newdata<-vals$saved_data[[input$predcomb_new]]
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]

    obc<-get_obc()
    m<-modelist[[1]]
    if(input$tab_comb=="tab1"){
      withProgress(min=0, max=length(modelist),message="Running...",{
        vals$aculoss<-unlist(lapply(modelist,function(m) {
          get_acculoss(m, newdata,obc,reps=input$aculoss_rep,sig=0.01)
        }))*100
        incProgress(1)
      })} else{
        vals$aculoss<-get_acculoss_comb(modelist, vals$res_compre,newdata,obc,reps=input$aculoss_rep,method=input$en_method)*100
      }



  })

  comb_feaimp.reac<-reactive({
    allimportance<- comb_feaimp(vals$modellist2,  scale=input$scale_imp,progress = T)
  })

  output$feat_models<-renderUI({
    req(input$pal_combine)
    req(input$feaimp_xlab)
    req(input$feaimp_ylab)
    req(input$feaimp_leg)
    palette<-input$pal_combine
    modelist<-vals$modellist2
    if(input$colby=="aculoss"){
      req(length(vals$aculoss)>0)
    }
    allimportance<-comb_feaimp.reac()
    moldels_importance<-allimportance
    aculoss<-vals$aculoss
    renderPlot({
      vals$feaimp_plot1<-plot_model_features(moldels_importance,
                                             palette=palette,
                                             vals$newcolhabs,
                                             colby=input$colby,
                                             aculoss=aculoss,
                                             nvars=input$aculoss_npic,
                                             sig=input$aculoss_sig,
                                             xlab=input$feaimp_xlab,
                                             ylab=input$feaimp_ylab,
                                             leg=input$feaimp_leg,
                                             cex.axes=input$feaimp_cex.axes,
                                             cex.lab=input$feaimp_cex.lab,
                                             cex.main=input$feaimp_cex.main,
                                             cex.leg=input$feaimp_cex.leg)
      vals$feaimp_plot1
    })

  })

  observeEvent(input$comp2_results,{
    vals$comp2_results<-input$comp2_results
  })

  observeEvent(vals$data_comp2,{
    vals$aculoss<-NULL
    vals$res_compre<-NULL
  })

  output$feat_ensemble<-renderUI({
    req(!is.null(vals$res_compre))
    req(input$pal_combine)
    req(input$feaimp_xlab)
    req(input$feaimp_ylab)
    req(input$feaimp_leg)
    req(input$en_method)
    palette<-input$pal_combine




    if(input$colby=="aculoss"){
      req(length(vals$aculoss)>0)
    }

    vals$allimportance<-comb_feaimp.reac()



    allimportance<- vals$allimportance
    obc<-get_obc()
    predtab<-vals$res_compre
    modelist<-vals$modellist2

    if(modelist[[1]]$modelType=="Classification"){
      weis<-getClass_wei(predtab, obc)}else{
        weis<-getReg_wei(predtab, obc)}
    mean_importance<-get_weig_faimp(allimportance, method=input$en_method, weis=weis)

    moldels_importance<-data.frame(mean_importance)
    aculoss<-vals$aculoss
    renderPlot({
      vals$feaimp_plot2<-plot_model_features(moldels_importance,
                                             palette=palette,
                                             newcolhabs=vals$newcolhabs,
                                             colby=input$colby,
                                             aculoss=aculoss,
                                             nvars=input$aculoss_npic,
                                             sig=input$aculoss_sig,
                                             xlab=input$feaimp_xlab,
                                             ylab=input$feaimp_ylab,
                                             leg=input$feaimp_leg,
                                             cex.axes=input$feaimp_cex.axes,
                                             cex.lab=input$feaimp_cex.lab,
                                             cex.main=input$feaimp_cex.main,
                                             cex.leg=input$feaimp_cex.leg)
      vals$feaimp_plot2
    })




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

    div(style="height: 70px",column(12,
                                    column(6,
                                           inline(
                                             div(
                                               div(strong("3. New data for predictions:")),
                                               div( pickerInput(ns("predcomb_new"),NULL,names(vals$saved_data[getobs_models()]), width = '300px', selected=vals$predcomb_new))

                                             )
                                           ),
                                           inline(actionButton(ns("run_preds"),"RUN"))
                                    )



    ))
  })



  output$tab2_out<-renderUI({

    div(
      div(
        inline(DT::dataTableOutput(ns('comb_tab_errors_train'))),


      ),
      div(
        inline(DT::dataTableOutput(ns('errors_class_obs')))
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
    div(
      uiOutput(ns("cmcomb")),
      uiOutput(ns("regcomb"))
    )

  })


  getobscomp_new<-reactive({

    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')
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
    resul<-names(res0[res0==T])
    datalist<-datalist[resul]
    res1<-unlist(lapply(datalist, function (x){
      nrow(x)==nrow(vals$saved_data[[input$predcomb_new]])

    }))
    names(res1[res1==T])
  })

  observeEvent(input$comb_create_errors_train2,{
    vals$hand_save<-"Create Datalist: comb training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())
  })






  output$comb_tab_errors_train<-DT::renderDataTable({
    req(vals$modellist2[[1]]$modelType=="Regression")

    predtab<-vals$res_compre
    pred<-get_pred()
    pred<-unlist(pred)
    obc<-get_obc()
    predtab$pred<-pred
    res<-data.frame(t(
      apply(predtab,1,function(x){
        p<-x[length(x)]
        o<-x[-length(x)]
        postResample(p,o)
      })
    ))
    res$obs<-obc
    res$pred<-pred
    res$Rsquared<-NULL
    vals$comb_down_errors_train<-res
    res
    DT::datatable(round(res,input$round_summ), options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  output$comb_tab_errors0_train<-DT::renderDataTable({
    DT::datatable({
      obs<-get_obc()
      pred<-get_pred()
      table<-caret::postResample(pred,obs)
      data.frame(as.list(table))
    }, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })


  predall_comp<-reactive({
    res<-do.call(data.frame,vals$res_compre)
    colnames(res)<-input$predcomb_new
    res
  })
  comp_observed2<-reactive({

    if(vals$modellist2[[1]]$modelType=="Classification"){
      factors<-attr(vals$saved_data[[input$obc]],'factors')
    } else {
      factors<-vals$saved_data[[input$obc]]}

    res<-factors[,attr(vals$modellist2[[1]],"supervisor")]
    names(res)<-rownames(factors[attr(vals$modellist2[[1]],"supervisor")])
    res

  })
  get_qclass<-reactive({
    m<-vals$modellist2[[1]]
    req(m$modelType=="Regression")
    pred<-vals$res_compre
    model_data<-get_obc()
    obs_data<-get_obc()
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
    data=t(vals$res_compre)
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
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),paste(getensemble_method(),'predictions'))

        ))
  })

  getensemble_method<-reactive({
    if(input$en_method=="weighted"){
      'Weighted mean'
    } else{
      'Mean'
    }
  })
  output$plot_forest<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({

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
    req(input$obc)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$modellist2[[1]]
    req(m$modelType=="Classification")
    pred<-vals$res_compre
    pred.ind <- pred
    model_data<-get_obc()
    obs_data<-get_obc()
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
                       NULL,names(vals$saved_data[getobscomp_new()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })

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

  test_comp<-reactive({

    if(vals$modellist2[[1]]$modelType=="Classification"){
      factors<-attr(vals$saved_data[[input$obc]],'factors')
    } else {
      factors<-vals$saved_data[[input$obc]]}

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


  output$side3<-renderUI({
    req(input$tab_comb=='tab2')
    req(input$tab_ense=='tab4')

    div(
      class="map_control_style",style="color: #05668D",


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
    pred<-vals$res_compre
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-get_obc()
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
                  cex=input$qclass_sizeplot,
                  pred_ensemble=unlist(get_pred()))
    vals$vartrees<-recordPlot()
  })
  output$forestbyclass<- renderPlot({
    res_forest<-get_forest_class()

    plot_florest_class(res_forest,vals$newcolhabs, palette=input$forest_col, ylab="Resample")
  })
  observeEvent(input$obc,{
    vals$obc<-input$obc
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
      div(
        p(strong("Global:")),
        inline(DT::dataTableOutput(ns('comb_tab_errors0_train')))
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

  observeEvent(input$teste_comb,{
    saveRDS(reactiveValuesToList(vals),"vals.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')
  })
  output$cmcomb<-renderUI({

    req(vals$modellist2[[1]]$modelType=="Classification")
    div(
      div(
        inline(DT::dataTableOutput(ns('errors_class_global')))
      ),
      uiOutput(ns("conf_matrix")),
      uiOutput(ns("conf_matrix_print"))

    )
  })

  get_conf_matrix<-reactive({
    pred<-get_pred()
    obs<-get_obc()
    table(unlist(pred),obs)

  })

  output$conf_matrix_print<-renderUI({
    conf<-get_conf_matrix()
    renderPrint(confusionMatrix(conf))
  })

  output$conf_matrix<-renderUI({
    conf<-get_conf_matrix()

    renderPlot({
      res<-plotCM(conf/sum(conf)*100,input$pal_combine, newcolhabs=vals$newcolhabs)
      vals$combcm<-res
      vals$combcm
    })
  })

  output$errors_class_global<-DT::renderDataTable({
    pred<-get_pred()
    obs<-get_obc()
    vals$comb_perfomace_class<-postResample(pred,obs)
    DT::datatable({round( data.frame(as.list(vals$comb_perfomace_class)),input$round_summ)}, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })


  output$errors_class_obs<-DT::renderDataTable({
    req(vals$modellist2[[1]]$modelType=="Classification")
    obc<-get_obc()
    pred<-get_pred()
    res<-getobs_errors_class(vals$res_compre,obc)
    res<-data.frame(res)
    res<-round(res,input$round_summ)
    res$obs<-unlist(obc)
    res$pred<-unlist(pred)
    vals$comb_down_errors_train<-res
    res
    DT::datatable(res, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

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
    newdata<-vals$saved_data[[input$predcomb_new]]
    modelist<-vals$modellist2
    vals$res_compre<-predcomb_table(modelist,newdata)
  })

  output$en_method_class<-renderUI({
    choiceNames<-if(vals$modellist2[[1]]$modelType=="Classification"){c("Majority weighted votes","Majority votes")
    } else{
      c("Weighted mean","Mean")
    }
    hidden(
      radioButtons(ns("en_method"), span("Ensembling Method:",tipify(actionLink(ns("help_weig"),icon("fas fa-question-circle")),"Click for details")), choiceNames=choiceNames,choiceValues=c("weighted",'non-weighted'), inline=F)
    )
  })

  observe({
    req(input$tab_comb)
    if(input$tab_comb=='tab1'){        shinyjs::hide('en_method')} else{
      shinyjs::show('en_method')
    }
  })
  observe({
    req(input$tab_comb)
    if(input$tab_comb=='tab1'){
      if(input$tab_model=='tab1'){
        shinyjs::hide('validation_picker')
      } else{
        shinyjs::show('validation_picker')
      }
    } else{
      shinyjs::show('validation_picker')
    }
  })






  output$wmean_help<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    div(

      p(h4("Mean"),
        p("Takes mean of predicted values.")),
      p(h4("Weighted mean"),
        p("Takes the model performaces (Rsquared) as weights for calculating the weighted mean"))
    )
  })
  output$votes_help<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Classification")
    div(
      h4("Majority weighted votes"),
      p(
        "Weights are determined according to the classification performances of classifiers.In this approach, there are two phases:"
      ),
      hr(),
      p(strong('(1) Determine the weights of the classifiers using validation set'),"In this phase, each classifier generates a decision pointing to predicted class label of a single instance and then these decisions are evaluated to update weights. In this approach, the weights are updated for each instance in the validation set. Initially, all weights are equal to 1. All instances in the validation dataset are traversed and processed through ",em("n")," classifiers once. The weights of the classifiers that correctly predict class label of an instance are incremented by the ratio of the number of incorrectly predicting classifiers to the whole number of classifiers (",em("n"),"). The output can be outlined with the following equation:"),
      p(
        withMathJax(
          helpText('$$w_{ij}=\\begin{cases}
               w_{i-1,j}+\\alpha_i,  & \\text{if $j^{th}$ classifier makes a correct prediction for $i^{th}$ instance} \\\\
               w_{i-1,j}, & \\text{if $j^{th}$ classifier makes an incorrect prediction for $i^{th}$ instance}
               \\end{cases}\\!$$')),
        div("where",inline(helpText("$$w_{ij}$$")),"is the weight of",inline(helpText("$$j^{th}$$"))," classifier as a result of the operation realized on",inline(helpText("$$i^{th}$$")),"instance, where", inline(helpText("$$i={1,2,...,m}$$")),HTML('&nbsp;'), "and", inline(helpText("$$j={1,2,...,n}$$"))),inline(";"),inline(helpText("$$\\alpha_i$$")),"is the change in weight and calculated as ",inline(helpText("$$\\alpha_i=\\frac{Y_i}{n}$$"))," where ",inline(helpText("$$Y_i$$"))," is the number of incorrect predictions for ",inline(helpText("$$i^{th}$$"))," instance and ",inline(helpText("$$n$$"))," is the number of classifiers"
      ),
      hr(),
      p(strong('(2) Combine the outputs of individual classifiers by considering their weights.'),"In the final decision, all weighted votes are summed for each class and the class receiving the most weighted votes becomes predicted class of an instance to be classified, as
given in:",
p(
  withMathJax(
    helpText("$$\\displaystyle{\\max_{1\\geqslant{j}\\geqslant{k}}}{\\sum_{t = 1}^{n} {w_t} {d_{t,j}}} $$"))
))
    )
  })

  observeEvent(input$help_weig,{
    showModal(
      modalDialog(
        title='Weights for ensembling models',
        easyClose =T,
        size="l",
        div(uiOutput(ns("votes_help")),
            uiOutput(ns("wmean_help")))
      )
    )
  })


  output$validation_Y<-renderUI({
    sup_test<-attr(vals$modellist2[[1]],"supervisor")
    supname<-paste0("+ Validation data (Y) [",sup_test,"]")

    hidden(div(style="color: #05668D",id=ns("validation_picker"),
               strong(
                 span(supname,tipify(icon("fas fa-question-circle"),"Validation dataset. Data containing observed values to be compared with predicted values"),
                 )
               ),

               pickerInput(ns("obc"),
                           NULL,names(vals$saved_data[getobscomp_new()]), width="200px",selected=vals$predreg_newY
               )

    ))
  })

  output$side_ensemble<-renderUI({
    req(!is.null(vals$res_compre))
    sidebarPanel(
      uiOutput(ns('validation_Y')),
      div(
        uiOutput(ns('en_method_class'))
      ),
      div(class="map_control_style",style="color: #05668D",


          uiOutput(ns("side1")),
          uiOutput(ns("side2")),
          uiOutput(ns("side_round")),
          uiOutput(ns("side3")),
          uiOutput(ns("side4")),
          uiOutput(ns("side6")),
          uiOutput(ns('side_interactions'))
      ),




    )
  })
  output$side1<-renderUI({
    req(input$tab_comb=='tab2')
    req(input$tab_ense=="tab1")
    div(
      tipify(
        actionLink(
          ns('create_ensemble_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
        ),
        "Create a datalist with the ensemble predictions", options=list(container="body")
      )
    )
  })

  output$side4<-renderUI({
    req(input$tab_comb=='tab1')
    req(input$tab_ense=="tab2")
    div(

      div(uiOutput(ns("createerror"))
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
  })

  output$side_round<-renderUI({
    if(input$tab_comb=='tab2'){req(input$tab_ense=='tab2'|input$tab_ense=='tab3')}

    div(
      "+ Round",
      inline(numericInput(ns('round_summ'),NULL,3, width="60px"))
    )
  })
  output$side2<-renderUI({
    req(input$tab_comb)
    if(input$tab_comb=="tab2"){
      req(input$tab_ense=="tab2"|input$tab_ense=="tab5")
    }

    div(

      div(
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("pal_combine"),
                           label = NULL,
                           choices =     vals$colors_img$val,
                           choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
             )
        )
      )

    )
  })

  output$ensemb_pred<-renderUI({
    req(!is.null(vals$res_compre))
    inline(DT::dataTableOutput(ns('ensemble_predictions')))
  })

  output$models_pred<-renderUI({
    req(!is.null(vals$res_compre))
    inline(DT::dataTableOutput(ns('model_predictions')))
  })





  get_obc<-reactive({
    req(input$obc)

    modelist<-vals$modellist2
    data<-vals$saved_data[[input$obc]]
    if(vals$modellist2[[1]]$modelType=="Regression"){
      obc<-data[,attr(modelist[[1]],"supervisor")]
      names(obc)<-rownames(data)
    } else {

      factors<-attr(data,"factors")

      obc<-factors[,attr(modelist[[1]],"supervisor")]
      names(obc)<-rownames(factors)

    }
    obc
  })

  get_pred<-reactive({
    req(input$obc)
    req(input$en_method)
    req(is.data.frame(vals$res_compre))
    obc<-NULL
    predtab<-vals$res_compre
    modelist<-vals$modellist2
    data<-vals$saved_data[[input$obc]]
    if(input$en_method=="weighted"){
      obc<-get_obc()
    }
    pred<-get_ensemble_pred(predtab,modelist, method=input$en_method, obc=obc)
    pred
  })


  output$ensemble_predictions<-DT::renderDataTable({
    get_pred()
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')



  output$model_predictions<-DT::renderDataTable({
    vals$res_compre
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')



  observeEvent(input$create_ensemble_predictions,{
    vals$hand_save<- "Create Datalist: ensemble predictions"
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
    hide(selector = ".page2")
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
      div(class = "page2",
          id = ns('insert_step1'),
          uiOutput(ns("page1"))
      ),
      hidden(
        div(class = "page2",
            id = ns('insert_step2'),
            uiOutput(ns("page2"))
        ))

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
    temp<-data_migrate(vals$saved_data[[input$obc]],temp,"newdatalist")
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
      "Create Datalist: ensemble predictions"={ name_combb_pred()},
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
    datao<-vals$saved_data[[input$obc]]
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
      "Create Datalist: ensemble predictions"={combb_create_pred()},
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
