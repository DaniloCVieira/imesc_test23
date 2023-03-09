#' @export



module_ui_comp2 <- function(id){
  ns <- NS(id)

  div(class='choosechannel',


    div(
      inline( actionButton(ns("teste_comb"),"SAVE")),
      inline(uiOutput(ns("teste_comb")))
    ),
    uiOutput(ns('COMB_comp2')))

}

#' @export
module_server_comp2 <- function (input, output, session,vals,df_colors,newcolhabs){
  ns <- session$ns
  output$rs_teste<-renderUI({
    renderPrint({})
  })

  insert_rv <- reactiveValues(page = 1)
  output$COMB_comp2<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(
        div(

          div(class="well3",div(style="margin:0; padding:0; position: absolute; left: 15px;padding-top: 10px;",
              uiOutput(ns("comp2_prev_bnt"))),
          div(style="margin:0; padding:0; position: absolute; right: 15px;padding-top: 10px; ",
              uiOutput(ns("comp2_next_bnt"))),
          div(style="padding-left:70px;",
              uiOutput(ns("comp2_pages")))),
          uiOutput(ns("page3"))
        )
    )
  })


  observeEvent(input$ensemble_tab,{
    vals$ensemble_tab<-input$ensemble_tab
  })

  observeEvent(input$ensemble_tab1,{
    vals$ensemble_tab1<-input$ensemble_tab1
  })

  observeEvent(input$ensemble_tab2,{
    vals$ensemble_tab2<-input$ensemble_tab2
  })

  output$page3<-renderUI({
    #cat("\n",vals$ensemble_tab1)
   # cat("\n",vals$ensemble_tab)
    req(is.data.frame(get_predtab()))
    req(insert_rv$page==2)
    div(
      style="margin-top: 5px",id="combpage3",
      sidebarLayout(
        uiOutput(ns('side_ensemble')),
        mainPanel(
          div(class="card",
              tabsetPanel(
                id=ns("ensemble_tab"),
                selected=vals$ensemble_tab,
                tabPanel(strong("4. Model results"),
                         value="tab1",
                         tabsetPanel(
                           id=ns("ensemble_tab1"),
                           selected=vals$ensemble_tab1,
                           tabPanel("4.1. Predictions",
                                    value="tab1",
                                    uiOutput(ns("Model_tab1"))
                                    ),
                           tabPanel(strong("4.2. Feature importance"),
                                    value='tab2',
                                    uiOutput(ns("Model_tab2"))
                           ),
                           tabPanel(strong("4.3. Permutation Importance"),
                                    value='tab3',
                                    uiOutput(ns("Model_tab3"))
                           ))),
                tabPanel(
                  strong("5. Ensemble results"),
                  value="tab2",
                  div(class="card"),
                  tabsetPanel(
                    id=ns("ensemble_tab2"),
                    selected=vals$ensemble_tab2,
                    tabPanel("5.1. Predictions",
                             value="tab1",
                             uiOutput(ns("Ensemble_tab1"))),
                    tabPanel("5.2. Performace",
                             value="tab2",
                             uiOutput(ns("Ensemble_tab2"))),
                    tabPanel("5.3. Observation errors",
                             value="tab3",
                             uiOutput(ns("Ensemble_tab3"))),
                    tabPanel("5.4. Plot model predictions",
                             value="tab4",
                             uiOutput(ns("Ensemble_tab4"))),
                    tabPanel(strong("5.5. Feature importance"),
                             value="tab5",
                             uiOutput(ns("Ensemble_tab5"))
                    ),
                    tabPanel(strong("5.6. Permutation Importance"),
                             value='tab6',
                             uiOutput(ns("Ensemble_tab6"))
                    ),
                    tabPanel(strong("5.7. Interactions"),
                             value='tab7',
                             div(
                               inline(uiOutput(ns("inter_method"))),
                               inline(uiOutput(ns('run_inter')))
                             ),
                             uiOutput(ns("Ensemble_tab7")),
                             uiOutput(ns("Ensemble_tab7_plot"))
                    )


                  )

                )
              )
          )
        )
      )
      )
  })

  permimp_model_args<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    req(input$pal_combine)
    palette<-input$pal_combine
    req(input$feaimp_xlab)
    req(input$feaimp_ylab)
    req(input$feaimp_leg)
    palette<-input$pal_combine
    sigvars<-attr(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$scoreloss_models,'sigvars')
    acc<-getacc_models()
    acc<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[input$ensemble_models]]$models_resampling
    req(length(acc)>0)
    args<-list(

      moldels_importance=NULL,
      palette=palette,
      newcolhabs=vals$newcolhabs,
      colby="acc",
      aculoss=1,
      nvars=input$aculoss_npic,
      sig=input$aculoss_sig,
      xlab=input$feaimp_xlab,
      ylab=input$feaimp_ylab,
      leg=input$feaimp_leg,
      cex.axes=input$feaimp_cex.axes,
      cex.lab=input$feaimp_cex.lab,
      cex.main=input$feaimp_cex.main,
      cex.leg=input$feaimp_cex.leg,
      sigvars=sigvars,
      acc=acc,
      facet_grid=T,
      modelist=vals$modellist2

    )
    args


  })

  output$Model_tab3<-renderUI({

    args<-permimp_model_args()
    #args<-readRDS("args.rds")

    renderPlot({
      p<-do.call(plot_model_features,args)
      vals$feaimp_plot2<-p
      vals$feaimp_plot2
    })

  })
  inter_args<-reactive({
    predtab<-get_predtab()
    modelist<-vals$modellist2
    newdata<-get_newdata()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]
    obc<-get_obc()
    pred<-get_pred()
    weis=get_weis()
    args<-list(
      newdata=newdata,
      predtab=predtab,
      weis=weis,
      obc=obc,
      pred=pred,
      reps=input$inter_rep,
      modelist=modelist,
      sig=input$inter_sig,
      top.features=input$inter_top,
      scale=input$inter_scale,
      en_method=input$en_method,
      root=input$inter_root,
      weitype=input$wei_datatype,
      inter_method= input$inter_method
    )
    args
  })


  observeEvent(input$run_inter,{

    args<-inter_args()
    inter_res0<-do.call(interact_ml,args)

    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$inter_res0<-inter_res0
    updateTabsetPanel(session,"ensemble_tab","tab2")
    updateTabsetPanel(session,"ensemble_tab2",vals$ensemble_tab2)

  })
  output$interactions_plot<-renderUI({

    i_scores<-get_inter_res0()
    i_result<-i_importance(i_scores)
    i_sig<-i_t.test(i_scores,i_result, sig=input$inter_sig)
    importances_alone<-i_scores$score_alone
    importances_both<-i_result
    isig<-rownames(i_sig)[which(i_sig$Sig_both)]
    args<-list(
      moldels_importance=importances_both,
      palette=input$pal_inter,
      newcolhabs=vals$newcolhabs,
      colby='acc',
      facet_grid=F,
      nvars=nrow(importances_both),
      sig=NULL,
      sigvars=isig,
      size.sig=1.5,
      acc=importances_both,
      title=NULL,
      modelist=NULL,
      scale=T,
      cex.axes=input$inter_cex.axes,
      cex.lab=input$inter_cex.lab,
      cex.main=input$inter_cex.main,
      cex.leg=input$inter_cex.leg,
      leg=input$inter_leg,
      xlab=input$inter_xlab,
      ylab=input$inter_ylab
    )


    renderPlot({
     p<-do.call(plot_model_features,args)

      vals$intercomb_plot<-p
      p
    })
  })


  output$sig_forward<-renderUI({
    renderPrint({
      req(input$data_ensemble_X)
      getsigs2_loss(get_inter_res0(),sig=input$inter_sig)

    })
  })

  output$Ensemble_tab7_plot<-renderUI({
    req(input$inter_show)
    if(input$inter_show=="Plot"){
      uiOutput(ns('interactions_plot'))
    } else{ uiOutput(ns("interactions_result")) }
  })


  output$weis_out<-renderUI({

    req(input$ensemble_tab=="tab2")
    req(is.data.frame(get_predtab()))
    req(insert_rv$page==2)

    req(input$en_method)
    req(input$en_method!='non-weighted')
    req(input$ensemble_tab)
    req(input$ensemble_tab=="tab2")
    req(!is.null(get_predtab()))
    div(
      div(strong("Weights")),
      #inline(strong("Weights results:")),
      div(style="font-size: 11px;width: 110px;height: 130px; overflow-y: scroll",id="weistab",

          inline(DT::dataTableOutput(ns("weis_table")))
      )

    )
  })
  output$weis_table<-DT::renderDataTable({
    req(length(get_weis())>0)
    data.frame(weights=round(get_weis(),2))
  },
    class="cell-border compact stripe",
    options=list(ordering=FALSE,scrollX = FALSE,deferRender = TRUE,dom = 't',
                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
                 fixedColumns = list(leftColumns = 1))
    ,rownames=T, colnames=''




  )
  output$weis_df<-DT::renderDataTable({},options = list(info = FALSE, autoWidth=T,dom = 't'), rownames = F,class ='cell-border compact stripe')


  output$Ensemble_tab7<-renderUI({
    req(input$data_ensemble_X)
    req(!is.null(get_inter_res0()))
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





  output$interactions_result<-renderUI({
    div(style="overflow-x: scroll",
        div(
          "Table"
        ),
        inline(DT::dataTableOutput(ns('interactions_df')))

        # inline(DT::dataTableOutput(ns('interactions_df')))

    )
  })

  output$interactions_df<-DT::renderDataTable({},options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')









  output$inter_root<-renderUI({
    predtab<-get_predtab()
    modelist<-vals$modellist2
    req(input$inter_scale)
    req(input$en_method)
    req(input$inter_top)
    weis<-get_weis()
    obc<-get_obc()
    tops<-gettop(modelist,
                 scale=input$inter_scale,
                 en_method=input$en_method,
                 weis,
                 top.features=input$inter_top)

    checkboxGroupInput(ns("inter_root"),"+ Root",tops, selected=tops[1])
  })
  output$inter_scale<-renderUI({
    if(is.null(vals$inter_scale)){
      vals$inter_scale<-T
    }
    div(
      checkboxInput(ns("inter_scale"), "+ Scale importances", vals$inter_scale, width="150px")
    )
  })
  observeEvent(input$inter_scale,
               vals$inter_scale<-input$inter_scale)


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

  observeEvent(input$inter_rep,{
    vals$inter_rep<-input$inter_rep
  })

  output$inter_rep<-renderUI({
    if(is.null(vals$inter_rep)){vals$inter_rep<-5}
    div(
      span("+ Repetions:",
           inline(
             numericInput(ns("inter_rep"),NULL, value=vals$inter_rep, width="75px", step=2)
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
      div("+ Xlab",inline(textInput(ns("inter_ylab"), NULL, 'Importance', width="200px")),
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
    req(input$ensemble_tab=="tab2")
    req(input$ensemble_tab2=="tab7")
    div(
      hr(),
      div(
        div(strong("Interactions:", pophelp(NULL,"Interaction I (X, Y) is defined as the decrease in uncertainty about X (root) after observing Y. Low, close to zero, I means that the variables are close to independent. The larger the I, the larger the reduction of uncertainty in X when Y is known."))),
        div(
          inline(uiOutput(ns('inter_top'))), inline(uiOutput(ns('inter_scale')))
        ),

        uiOutput(ns('inter_root')),
        uiOutput(ns('inter_rep'))
      ),
      hr(),
      uiOutput(ns('inter_plot_params'))


    )
  })
  observeEvent(input$inter_method,{
    vals$inter_method<-input$inter_method
  })
  output$inter_method<-renderUI({
    div(div(strong("3. Method:")),
        div( pickerInput(ns("inter_method"),NULL,c("Paired","Forward")
                         ,selected=vals$inter_method

                         )))

  })
  output$inter_plot_params<-renderUI({
    req(input$data_ensemble_X)
    req(!is.null(get_inter_res0()))
    div(
      div(strong("Plot parameters:")),
      uiOutput(ns('inter_sig')),
      uiOutput(ns('inter_palette')),
      uiOutput(ns('inter_ggplot'))
    )

  })


  output$side_performace_loss<-renderUI({
    if(is.null(vals$score_loss)){
      vals$score_loss<-F
    }
    if(is.null(vals$en_scale)){
      vals$en_scale<-T
    }
    if(input$ensemble_tab=='tab1'){
      req(input$ensemble_tab1!="tab1")}
    if(input$ensemble_tab=='tab2'){
      req(input$ensemble_tab2=="tab5"|input$ensemble_tab2=="tab6")}

    div(style="margin-top: 5px; margin-bottom: 5px",
        div(
          checkboxInput(ns("en_scale"), "+ Scale importances", vals$en_scale)
        ),
        hr(),
        uiOutput(ns('perm_imp'))

    )
  })

  output$perm_imp<-renderUI({
    if(input$ensemble_tab=='tab1'){
      req(input$ensemble_tab1=="tab3")}

    if(input$ensemble_tab=='tab2'){
      req(input$ensemble_tab2=="tab6")}
    div(
      div(
        strong("Permutation Importance",inline(actionLink(ns("help_loss"),icon("fas fa-question-circle"))))),
      div(
        div(style="margin-left: 5px;",

            uiOutput(ns('en_rep')),
            uiOutput(ns('side_sig_aculoss')),
            uiOutput(ns('run_aculoss'))


        ),
        hr()
      )

    )
  })

  observeEvent(input$en_scale,
               vals$en_scale<-input$en_scale)

  observeEvent(input$score_loss,
               vals$score_loss<-input$score_loss)


  observeEvent(input$help_loss,{
    showModal(
      modalDialog(
        div(p("The permutation feature importance algorithm is based on Fisher, Rudin, and Dominici (2018). "),
            p("For each predictor ",em('j')," in the dataset:"),
            div(style="margin-left: 10px",
              p(
                p("1. Permute feature ",em('j')," in the data X."),
                p('2. Estimate the  performace metric based on the predictions of the permuted data.'),
                p("3. Calculate permutation feature importance ",  HTML(paste0("P",tags$sub("j")))," as the difference between prediction errors before and after the feature is permuted.")
              )),

            p("In iMESC, this process is repeated ",em("n")," times, and the significance of the feature is calculated using a one-sided t-test,  which tests if the mean feature metric ",  HTML(paste0("P",em(tags$sub("j")))),"is higher than the average of the remaining feature metrics",  HTML(paste0("P",em(tags$sub("-j")))),"."
            )
            ),
        title="Permutation-based variable importance",
        easyClose =T
      )
    )
  })


  output$side_feature_axes_plot<-renderUI({
    if(input$ensemble_tab=='tab1'){
      req(input$ensemble_tab1!="tab1")}
    if(input$ensemble_tab=='tab2'){
      req(input$ensemble_tab2=="tab5"|input$ensemble_tab2=="tab6")}
    div(
      div("+ Title size",inline(numericInput(ns("feaimp_cex.main"),NULL, value=13, width="75px", step=1))),
      div("+ Axes size",inline(numericInput(ns("feaimp_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.leg"),NULL, value=13, width="75px", step=1))),
      div("+ Xlab",inline(textInput(ns("feaimp_xlab"), NULL, 'Variables', width="200px"))),
      div("+ ylab",inline(textInput(ns("feaimp_ylab"), NULL, 'Importance', width="200px"))),
      div("+ Legend title",inline(textInput(ns("feaimp_leg"), NULL, if(isFALSE(input$score_loss)) {'Model'}else{"Perm Importance"}, width="200px")))
    )
  })

  colby<-reactive({

    if(isFALSE(input$score_loss)){
      'models'
    } else{ "acculoss"}
  })



  output$side_sig_aculoss<-renderUI({
    div(
      div(
        span(
          column(12,"+ Sig level:"),
          div(inline(numericInput(ns("aculoss_sig"),NULL, value=0.05, width="75px", step=0.02)), inline(tiphelp("perfomace loss higher then")))


        )
      )
    )
  })


  observeEvent(get_newdata(),{
    modelist<-vals$modellist2
    newdata<-get_newdata()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]
    vals$aculoss_npic<-ncol(newdata)
  })

  output$side_filter_feature_plot_nvars<-renderUI({
    req(!is.null(vals$aculoss_npic))
    if(input$ensemble_tab=='tab1'){
      req(input$ensemble_tab1=='tab2')}
    if(input$ensemble_tab=='tab2'){
      req(input$ensemble_tab2=="tab5"|input$ensemble_tab2=="tab6")}
    #req(isFALSE(input$score_loss))

    div(
      span("+ Number of variables:",tiphelp("Filter variables with the highest mean importance"),
           inline(
             numericInput(ns("aculoss_npic"),NULL, value=vals$aculoss_npic, width="75px", step=1)
           )
      )
    )
  })

  observeEvent(input$aculoss_npic,{
    vals$aculoss_npic<-input$aculoss_npic
  })


  observeEvent(input$aculoss_rep,{
    vals$aculoss_rep<-input$aculoss_rep
  })

  output$en_rep<-renderUI({

    if(is.null(vals$aculoss_rep)){vals$aculoss_rep<-5}
    div(
      span("+ Repetions:",
           inline(
             numericInput(ns("aculoss_rep"),NULL, value=vals$aculoss_rep, width="75px", step=2)
           )
      )
    )
  })
  output$run_aculoss<-renderUI({

    #req(is.null(vals$aculoss))
    div(class="save_changes",actionButton(ns("run_aculoss"),"+ Calculate Permutation Importance"))
  })









  comb_feaimp.reac<-reactive({
    allimportance<- comb_feaimp(vals$modellist2,  scale=input$en_scale,progress = T)
  })


 plotargs_models_feaimp<-reactive({

   req(input$pal_combine)
   req(input$feaimp_xlab)
   req(input$feaimp_ylab)
   req(input$feaimp_leg)
   palette<-input$pal_combine
   modelist<-vals$modellist2
   allimportance<-comb_feaimp.reac()


   moldels_importance<-allimportance
   args<-list(
     moldels_importance=moldels_importance,
     palette=palette,
     newcolhabs=vals$newcolhabs,
     colby='models',
     aculoss=NULL,
     nvars=input$aculoss_npic,
     sig=input$aculoss_sig,
     xlab=input$feaimp_xlab,
     ylab=input$feaimp_ylab,
     leg=input$feaimp_leg,
     cex.axes=input$feaimp_cex.axes,
     cex.lab=input$feaimp_cex.lab,
     cex.main=input$feaimp_cex.main,
     cex.leg=input$feaimp_cex.leg
   )
   args

 })

  output$Model_tab2<-renderUI({
    args<-plotargs_models_feaimp()
    renderPlot({
      vals$feaimp_plot1<-do.call(plot_model_features,args)
      vals$feaimp_plot1
    })

  })

  observeEvent(input$comp2_results,{
    vals$comp2_results<-input$comp2_results
  })

  getacc<-reactive({
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[input$ensemble_models]]$ensemble_resampling
  })
  getacc_models<-reactive({
    acc<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[input$ensemble_models]]$models_resampling
    acc
  })


  getargs_plot_scoreloss<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)


    req(input$pal_combine)
    palette<-input$pal_combine
    req(input$feaimp_xlab)
    req(input$feaimp_ylab)
    req(input$feaimp_leg)
    req(input$en_method)
    palette<-input$pal_combine
    sigvars<-attr(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$scoreloss_ensemble,'sigvars')
    acc<-getacc()
    req(length(acc>0))

    args<-list(

      moldels_importance=NULL,
      palette=palette,
      newcolhabs=vals$newcolhabs,
      colby="acc",
      aculoss=1,
      nvars=input$aculoss_npic,
      sig=input$aculoss_sig,
      xlab=input$feaimp_xlab,
      ylab=input$feaimp_ylab,
      leg=input$feaimp_leg,
      cex.axes=input$feaimp_cex.axes,
      cex.lab=input$feaimp_cex.lab,
      cex.main=input$feaimp_cex.main,
      cex.leg=input$feaimp_cex.leg,
      sigvars=sigvars,
      acc=acc,
      facet_grid=F

    )
    args
  })

  getargs_ensemble_feaimp<-reactive({
    req(!is.null(get_predtab()))
    req(input$pal_combine)
    req(input$feaimp_xlab)
    req(input$feaimp_ylab)
    req(input$feaimp_leg)
    req(input$en_method)
    palette<-input$pal_combine
    req(input$data_ensemble_X)

    vals$allimportance<-comb_feaimp.reac()
    allimportance<- vals$allimportance
    obc<-get_obc()
    predtab<-get_predtab()
    modelist<-vals$modellist2
    weis<-get_weis()
    mean_importance<-get_weig_faimp(allimportance, en_method=input$en_method, weis=weis)
    moldels_importance<-data.frame(mean_importance)
    aculoss<-NULL
    args<-list(
      moldels_importance=moldels_importance,
      palette=palette,
      newcolhabs=vals$newcolhabs,
      colby="acculoss",
      aculoss=aculoss,
      nvars=input$aculoss_npic,
      sig=input$aculoss_sig,
      xlab=input$feaimp_xlab,
      ylab=input$feaimp_ylab,
      leg=input$feaimp_leg,
      cex.axes=input$feaimp_cex.axes,
      cex.lab=input$feaimp_cex.lab,
      cex.main=input$feaimp_cex.main,
      cex.leg=input$feaimp_cex.leg,
      sigvars=NULL,
      facet_grid=F
    )

    args
  })

  output$Ensemble_tab5<-renderUI({
    args<-getargs_ensemble_feaimp()

   # saveRDS(args,'args.rds')
    renderPlot({
      vals$feaimp_plot2<-do.call(plot_model_features,args)
      vals$feaimp_plot2
    })




  })

  output$Ensemble_tab6<-renderUI({
    req(!is.null(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")))
    acc<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$ensemble_resampling
    validate(need(length(acc)>0,"Click in the flashing blue button 'Calculate Permutation Importance'"))

    args<-getargs_plot_scoreloss()

    p<-do.call(plot_model_features,args)
    vals$ensemble_permimp<-p
    renderPlot({vals$ensemble_permimp})
  })






  output$page1<-renderUI({

    div(
      uiOutput(ns("data_comp2_picker")),
      uiOutput(ns("data_comp2_out"))
    )
  })
  observeEvent(get_predtab(),{
    if(is.null(get_predtab())){
      addClass('run_ensemble','save_changes')
    } else{
      removeClass('run_ensemble','save_changes')
    }
  })

  getmodels<-reactive({
    pic<-input$limodels2
    req(length(vals$choices_models_equals2)>0)
    choices<-vals$choices_models_equals2
    vals$ens_modeltype<-which(choices==input$limodels2)
    model_names<-unlist(lapply(seq_along(names(vals$listamodels2)),function(x)paste0("equals",names(vals$liequals)[x])))[vals$ens_modeltype]
    req(length(model_names)==1)
    vals$ensemble_active<-input[[model_names]]
    modellist2<-vals$listamodels2[[vals$ens_modeltype]][vals$ensemble_active]
    vals$modellist2<-modellist2
  })

  observeEvent(input$ensemble_models,{
   try( getmodelist())
  })


  getmodelist<-reactive({
    req(input$data_ensemble_X)
    getmodel_ui()
    ens_modeltype<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[input$ensemble_models]]$ens_modeltype
    ensemble_active<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[input$ensemble_models]]$ensemble_active
    if(!is.null(ens_modeltype)&!is.null(ensemble_active)){
      vals$modellist2<-vals$listamodels2[[ens_modeltype]][ensemble_active]
    }

  })

  output$pagina2<-renderUI({
    try(getmodels())
    div(

      div(style="height: 90px; display: table; vertical-align: top;",
          inline(
            div(div(strong("3. New data for predictions:")),
                div( pickerInput(ns("data_ensemble_X"),NULL,names(vals$saved_data[getobs_models()]), width="250px",selected=vals$data_ensemble_X
                )))
          ),
          inline(div(class="ensemble_in",uiOutput(ns('validation_Y')))),
          inline(div(class="ensemble_in",div(class="ensemble_in",id=ns('run_ensemble'),actionButton(ns("run_preds"),"RUN")))),
          inline(div(class="ensemble_in",uiOutput(ns('ensemble_results_menu')))),
          inline(div(class="ensemble_in",uiOutput(ns('save_ensemble_button')))),
          inline(div(class="ensemble_in",uiOutput(ns('del_ensemble_button'))))


      )
    )
  })




  observeEvent(input$data_ensemble_X,{
    vals$data_ensemble_X<-input$data_ensemble_X
  })

  output$Ensemble_tab3<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    div(
      div(
        inline(DT::dataTableOutput(ns('observation_errors_reg'))),


      ),
      div(
        inline(DT::dataTableOutput(ns('observation_errors_class')))
      )
    )

  })

  output$side_create_errors<-renderUI({
    req(input$ensemble_tab=='tab2')
    req(input$ensemble_tab2=="tab3")

    tipify(
      actionLink(
        ns('comb_create_errors_train2'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
      ),
      "Create a datalist with the comb training errors", options=list(container="body")
    )
  })
  output$Ensemble_tab2<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    div(
      uiOutput(ns("cmcomb")),
      uiOutput(ns("regcomb"))
    )

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

  output$regcomb<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    div(
      div(
        p(strong("Global:")),
        inline(DT::dataTableOutput(ns('comb_tab_errors0_train')))
      )

    )

  })
  observeEvent(input$comb_create_errors_train2,{
    vals$hand_save<-"Create Datalist: comb training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())
  })




  ensemble_errors_regression<-reactive({
    predtab<-get_predtab()
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
    vals$comb_down_errors_train
  })


  output$observation_errors_reg<-DT::renderDataTable({
    req(vals$modellist2[[1]]$modelType=="Regression")
    ensemble_errors_regression()
    res<-vals$comb_down_errors_train
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
    res<-do.call(data.frame,get_predtab())
    colnames(res)<-input$data_ensemble_X
    res
  })
  comp_observed2<-reactive({
    req(length(input$obc)>0)
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
    pred<-get_predtab()
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
    data_factors=attr(get_newdata(),"factors")
    res_forest<-if(vals$modellist2[[1]]$modelType=="Classification"){resforest()}else{ensemble_errors_regression()}
    sortby=input$sort_df

    res<-get_options_show(res_forest,sortby ,get_obc(),  input$nhist_tree,data_factors=data_factors)
    options_num<-res$options_num
    options_show<-res$options_show

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
    if(input$en_method=="non-weighted"){'Mean'} else{
      'Weighted mean'
    }
  })
  output$plot_forest<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$Ensemble_tab4<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    choices<-if(vals$modellist2[[1]]$modelType=="Classification"){
      c('Accuracy','Error')
    } else{
      c('RMSE','MAE')
    }
    myList <- as.list(c("Accuracy","Error"))
    names(myList)<-choices



    div(style="background: white",
        inline(uiOutput(ns("comp_tree_show"))),
        inline(pickerInput(ns("sort_df"),
                           "Sort data by",
                           c(myList,
                             colnames(attr(get_newdata(),"factors"))
                           ),

                           width="200px"
        )),
        inline(uiOutput(ns("comp_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))
  })









  output$forest_byclass<-renderUI({
    req(input$qclass_height)
    req(input$qclass_width)
    req(vals$modellist2[[1]]$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


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
    req(length(input$obc)>0)
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


  output$side_qclass<-renderUI({
    req(input$ensemble_tab=='tab2')
    req(input$ensemble_tab2=='tab4')

    div(
      class="map_control_style",style="color: #05668D",
      div(strong("Plot parameters:"),
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
          ) ),

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

    ensemble_errors_regression()
    df<-vals$comb_down_errors_train
    data_factors=attr(get_newdata(),"factors")
    ord<-get_qclass_ord(df,get_obc(), input$sort_df,data_factors=data_factors)
    pred<-get_predtab()[ord,]
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
    str_numerics3(numerics=data,
                  obs=obs_data[ colnames(data),],
                  pred_interval=pred_interval,
                  q_class=q_class,
                  cex=input$qclass_sizeplot,
                  pred_ensemble=unlist(get_pred()))
    vals$vartrees<-recordPlot()
  })

  getsolid_col<-reactive({

    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic

  })

  output$side_qclass_axes_plot<-renderUI({
    req(input$ensemble_tab)
    req(input$ensemble_tab=='tab2')
    req(input$ensemble_tab2=='tab4')

    div(
      div("+ Text Color",
          inline(
            pickerInput(inputId = ns("qclass_textcolor"),
                        label = NULL,
                        choices =     vals$colors_img$val[getsolid_col()],
                        selected="black",
                        choicesOpt = list(content =     vals$colors_img$img[getsolid_col()]), options=list(container="body"), width="75px")
          )),
      div("+ Plot size",inline(numericInput(ns("qclass_size_points"),NULL, value=3, width="75px", step=1))),
      div("+ Plot title",inline(textInput(ns("qclass_main"), NULL, "", width="200px"))),
      div("+ Title size",inline(numericInput(ns("qclass_cex.main"),NULL, value=13, width="75px", step=1))),
      div("+ Axes size",inline(numericInput(ns("qclass_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("qclass_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("qclass_cex.leg"),NULL, value=13, width="75px", step=1))),
      div("+ Xlab",inline(textInput(ns("qclass_xlab"), NULL, 'Accuracy', width="200px"))),
      div("+ ylab",inline(textInput(ns("qclass_ylab"), NULL, 'Observations', width="200px"))),
      div("+ Legend title",inline(textInput(ns("qclass_leg"), NULL, "Model predictions", width="200px"))),
      div("+ Legend title",inline(textInput(ns("qclass_leg2"), NULL, "Final prediction", width="200px")))
    )
  })


  getord<-reactive({
    req(input$sortby)
    predtab=get_predtab()
    modelist=vals$modellist2
    obc=get_obc()
    pred=get_pred()
    round=input$round_summ
    en_method=input$en_method
    res_forest<-resforest()
    result<-get_obs_erros_ensemble_class(res_forest,predtab=predtab,modelist=modelist,obc=obc,pred=pred, round=5, en_method=en_method)
    vals$comb_down_errors_train<-res
    if(input$sortby=="Accuracy"){
      ord<-order(result$Accuracy, decreasing = T)
    } else{
      ord<- order(result$Error, decreasing = T)
    }
    ord<-rownames(result[ord,])
    ord
  })

  observeEvent(input$obc,
               vals$cur_obc<-input$cur_obc)

  output$forestbyclass<- renderPlot({
    pred=get_pred()
    predtab=get_predtab()
    modelist=vals$modellist2
    sortby=input$sort_df
    obc=get_obc()
    res_forest<-  get_wei_percetages(
      predtab=get_predtab(),
      modelist=vals$modellist2,
      obc=get_obc(), en_method = input$en_method,weitype=input$wei_datatype,
      newdata=get_newdata()
    )
    data_factors<-attr(get_newdata(),"factors")
    ord<-get_qclass_ord(res_forest,obc, sortby,data_factors=data_factors)

    req(input$splitdata_trees)
    vals$data_qclass_show<- getshow_qclass(
      res_forest=resforest(),
      sortby=input$sort_df,
      predtab=get_predtab(),
      obc=get_obc(),
      splitdata_trees=input$splitdata_trees,
      nhist_tree=input$nhist_tree,
      data_factors=data_factors
    )
    #ord<-ord[1:99]
    ord<-ord[vals$data_qclass_show[1]:vals$data_qclass_show[2]]
    result_forest<-res_forest[ord,]
    #input$splitdata_trees<-options_show[1]
    pred_res<-pred[rownames(result_forest),,drop=F]
    obc_res<-obc[rownames(result_forest)]


    vals$vartrees<-plotforest_qclass(
      result_forest, pred=pred_res,palette=input$forest_col, newcolhabs=vals$newcolhabs,
      ylab=input$qclass_ylab,
      xlab=input$qclass_xlab,
      leg=input$qclass_leg,
      leg2=input$qclass_leg2,
      cex.axes=input$qclass_cex.axes,
      cex.lab=input$qclass_cex.lab,
      cex.main=input$qclass_cex.main,
      cex.leg=input$qclass_cex.leg,
      textcolor=input$qclass_textcolor,
      main=input$qclass_main,
      obc=obc_res,
      size_points=input$qclass_size_points)
    vals$vartrees
  })

  observeEvent(input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())

  })
  observeEvent(input$resamp_pred_gdownp,{

    vals$hand_down<-"ensemble_qclass"
    vals$down_ensemble<-data.frame(resample_data())

    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  output$comb_err_class<-renderUI({
    div(

      renderPrint({
        res<-do.call(data.frame,get_predtab())
        colnames(res)<-input$data_ensemble_X
        res
      })


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




  savereac<-reactive({



    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    tosave$modellist2<-vals$modellist2
    #tosave$ensemble_args<-getargs_root()
    #tosave$ensemble_args_plot<-getargs_plot_scoreloss()




    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })
  observeEvent(input$teste_comb,{
    savereac()
  })


  get_conf_matrix<-reactive({
    obs<-get_obc()
    pred<-get_pred()
    #req(length(as.vector(pred))==length(as.vector(obs)))
    conf<-table(unlist(pred),obs)

    conf
  })

  output$conf_matrix_print<-renderUI({
    conf<-get_conf_matrix()
    vals$ensemble_confusion<-data.frame(conf)
    renderPrint(confusionMatrix(conf))
  })


  output$cm_title<-renderUser({
    req(input$ensemble_tab=="tab2")
    req(input$ensemble_tab2=="tab2")
    div("+ Title",inline(textInput(ns("cm_title"), NULL, 'Confusion Matrix', width="200px")))
  })
  output$conf_matrix<-renderUI({
    conf<-get_conf_matrix()
    req(input$cm_title)
    req(input$pal_combine)
    renderPlot({
      res<-plotCM(conf/sum(conf)*100,input$pal_combine, newcolhabs=vals$newcolhabs, title=input$cm_title)
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




  resforest<-reactive({
    req(input$wei_datatype)
    weitype<-input$wei_datatype
    resforest<-get_wei_percetages(

      predtab=get_predtab(),
      modelist=vals$modellist2,
      obc=get_obc(), en_method = input$en_method,weitype=weitype,
      newdata=get_newdata()
    )
    resforest
  })

  output$observation_errors_class<-DT::renderDataTable({
    req(vals$modellist2[[1]]$modelType=="Classification")
    predtab=get_predtab()
    modelist=vals$modellist2
    obc=get_obc()
    pred=get_pred()
    round=input$round_summ
    en_method=input$en_method
    resforest<-resforest()


    res<-get_obs_erros_ensemble_class(resforest,predtab=predtab,modelist=modelist,obc=obc,pred=pred, round=5, en_method=en_method)
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


  weitype<-reactive({
    req(input$en_method)
    data_weis<-if(input$en_method=="weighted"){
      req(input$wei_datatype)
      input$wei_datatype
    } else if(input$en_method=="accu_weighted"){
      req(input$wei_datatype)
      input$wei_datatype}
  })





  output$en_method_class<-renderUI({
    req(input$ensemble_tab=="tab2")
    req(is.data.frame(get_predtab()))
    req(insert_rv$page==2)
    if(vals$modellist2[[1]]$modelType=="Classification"){
      if(is.null(get_obc())){

        choiceNames<-c(
          "Weighted votes (overall performace)",
          "Majority votes"
        )
        choiceValues=c("accu_weighted","non-weighted")

      } else{
        choiceNames<-c(
          "Weighted votes (adaptative)",
          "Weighted votes (overall performace)",
          "Majority votes"
        )
        choiceValues=c("weighted","accu_weighted","non-weighted")
      }

    } else{
      choiceNames<-c(
        "Weighted Mean (overall performace)",
        "Mean"
      )
      choiceValues=c("accu_weighted","non-weighted")
    }
    mylist<-as.list(choiceValues)
    names(mylist)<-choiceNames
    div(
      id=ns('en_method_id'),
      inline(
        div(
          div(strong("Ensembling Method:"),tipify(actionLink(ns("help_weig"),icon("fas fa-question-circle")),"Click for details")),
          pickerInput(ns("en_method"), NULL,mylist, width="200px",selected=vals$en_method)
        )
      )


    )
  })

  observeEvent(input$en_method,
               vals$en_method<-input$en_method)


  output$weidata_out<-renderUI({
    req(input$ensemble_tab=="tab2")
    req(is.data.frame(get_predtab()))
    req(insert_rv$page==2)
    req(!is.null(get_predtab()))

    choices<-if(is.null(get_obc())){
      c(list("Training data"="training"))
    } else{
      c(list('New data/validation set'="test"),
         list("Training data"="training"))
    }

    req(input$en_method=="weighted"|input$en_method=="accu_weighted")
    div(
      div(strong("Estimate weigths using:")),
      pickerInput(
        ns("wei_datatype"),NULL,
        choices,
        width="200px",selected=vals$wei_datatype
      )
    )
  })
  observeEvent(input$wei_datatype,
               vals$wei_datatype<-input$wei_datatype)






  output$wmean_help<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Regression")
    column(12,
           p(h4(strong("1. Mean")),
             p("The ensemble prediction is calculated as the average of the observation predictions. Each ensemble member contributes an equal amount to the final prediction."),
             hr(),
             h4(strong("2. Weighted Mean")),

             p("A weighted ensemble is an extension of a model averaging ensemble where the contribution of each member to the final prediction is weighted by the performance of the model (Rsquared) which can be estimated either using the  training data (i. e., the original X and Y data on which the models were trained) or the new data X and Y.")),
           p("Finding the weights using the same training set used to fit the ensemble members will likely result in an overfit mode. A more robust approach is to use a data set unseen by the ensemble members during training."),
           p("The sum of all weights equals one, allowing the weights to indicate the percentage of trust or expected performance from each model.")
    )
  })
  output$votes_help<-renderUI({
    req(vals$modellist2[[1]]$modelType=="Classification")
    column(12,

           p(h4(strong("1. Marjority Votes")),
             p("In this method, each  every individual model votes for a class, and the majority wins."),
             hr(),
             h4(strong("2. Marjority weighted votes (overall performace)")),
             p("The models have varying degrees of effect on the final prediction. Each classifier is associated with a weight proportional to its classification performance on a validation set (Accuracy).The sum of all weights equals one, allowing the weights to indicate the percentage of trust or expected performance from each model. The final decision is made by summing up all weighted votes and by selecting the class with the highest aggregate. "),
             p("Wheighs can be estimated either using the  training data (i. e., the original X and Y data on which the models were trained) or the new data X and Y."),
             p("Finding the weights using the same training set used to fit the ensemble members will likely result in an overfit mode. A more robust approach is to use a data set unseen by the ensemble members during training."),
             hr(),
             h4(strong("3. Majority weighted votes (adaptative)")),
             p("Similar to the method described above, with the difference that, the models that correctly classify observations which are not correctly classified by most of the classifiers gain more weights in the ensemble. In this approach, there are three phases:"
             ),


             p(strong('(1) Determine the weights of the classifiers using validation set'),"In this phase, each classifier generates a decision pointing to predicted class label of a single instance and then these decisions are evaluated to update weights. In this approach, the weights are updated for each instance in the validation set. Initially, all weights are equal to 1. All instances in the validation dataset are traversed and processed through ",em("n")," classifiers once. The weights of the classifiers that correctly predict class label of an instance are incremented by the ratio of the number of incorrectly predicting classifiers to the whole number of classifiers (",em("n"),"). The output can be outlined with the following equation:"),
             p(
               withMathJax(
                 helpText('$$w_{ij}=\\begin{cases}
               w_{i-1,j}+\\alpha_i,  & \\text{if $j^{th}$ classifier makes a correct prediction for $i^{th}$ instance} \\\\
               w_{i-1,j}, & \\text{if $j^{th}$ classifier makes an incorrect prediction for $i^{th}$ instance}
               \\end{cases}\\!$$')),
               div("where",inline(helpText("$$w_{ij}$$")),"is the weight of",inline(helpText("$$j^{th}$$"))," classifier as a result of the operation realized on",inline(helpText("$$i^{th}$$")),"instance, where", inline(helpText("$$i={1,2,...,m}$$")),HTML('&nbsp;'), "and", inline(helpText("$$j={1,2,...,n}$$"))),inline(";"),inline(helpText("$$\\alpha_i$$")),"is the change in weight and calculated as ",inline(helpText("$$\\alpha_i=\\frac{Y_i}{n}$$"))," where ",inline(helpText("$$Y_i$$"))," is the number of incorrect predictions for ",inline(helpText("$$i^{th}$$"))," instance and ",inline(helpText("$$n$$"))," is the number of classifiers"
             ),

             p(strong('(2) Combine the outputs of individual classifiers by considering their weights.'),"In the final decision, all weighted votes are summed for each class and the class receiving the most weighted votes becomes predicted class of an instance to be classified, as
given in:",
p(
  withMathJax(
    helpText("$$\\displaystyle{\\max_{1\\geqslant{j}\\geqslant{k}}}{\\sum_{t = 1}^{n} {w_t} {d_{t,j}}} $$"))
))
           ))
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
    modelist<-vals$modellist2
    m<-modelist[[1]]
    sup_test<-attr(m,"supervisor")
    supname<-paste0("+ Validation data (Y) [",sup_test,"]")
    #if(is.null(vals$cur_obc)){}

    #res<-attr(m,"Y")
    #vals$cur_obc<-gsub("\\::.*","",gsub(" ","",res))
    div(style="color: #05668D",id=ns("validation_picker"),
        strong(
          span(supname,tipify(icon("fas fa-question-circle"),"Validation dataset. Data containing observed values to be compared with predicted values"),
          )),
        pickerInput(ns("obc"),NULL,names(vals$saved_data[validation_names()]), width="200px",selected=vals$cur_obc))})


  validation_names<-reactive({
    newdata<-vals$saved_data[[input$data_ensemble_X]]
    sup_test<-attr(vals$modellist2[[1]],"supervisor")

    datalist<-vals$saved_data
    if(vals$modellist2[[1]]$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}


    res1<-unlist(
      lapply(datalist, function (x){
        nrow(x)==nrow(newdata)
      })
    )
    req(length(res1)>0)
    datalist<-datalist[which(res1)]
    # rownames(datalist[[1]])<-1:nrow(datalist[[1]])

    res2<-unlist(
      lapply(datalist, function (x){
        sum(rownames(x)==rownames(newdata))==nrow(newdata)
      })
    )

    datalist<-datalist[which(res2)]
    #colnames(datalist[[1]])<-1:ncol(datalist[[1]])
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)
      })
    )
    req(length(res0)>1)

    resul<-names(which(res0))
    resul
  })



  output$side_ensemble<-renderUI({
    req(!is.null(get_predtab()))

    sidebarPanel(
      div(
          inline(
            div(
              div(class="ensemble_in",uiOutput(ns('en_method_class'))),
              div(class="ensemble_in",uiOutput(ns('weidata_out')))
            )
          ),
          inline(div(class="ensemble_in",uiOutput(ns('weis_out'))))

      ),

      div(
        hr(),
        div(class="map_control_style",style="color: #05668D",
            uiOutput(ns("side_round")),
            uiOutput(ns("cm_title"))),
      ),
      div(class="map_control_style",style="color: #05668D",
          uiOutput(ns("side_qclass")),
          uiOutput(ns("side_qclass_axes_plot")),
          uiOutput(ns("side_performace_loss")),
          uiOutput(ns('side_interactions')),
          uiOutput(ns("side_create_preds")),
          uiOutput(ns("side_create_errors")),
          uiOutput(ns("side_download_table")),
          uiOutput(ns('side_feature_plot')),
          uiOutput(ns("side_download_plot"))

      )

    )
  })

  output$side_feature_plot<-renderUI({
    req(input$ensemble_tab)
    if(input$ensemble_tab=="tab1"){req(input$ensemble_tab1!='tab1')}else{
      req(input$ensemble_tab2%in%c("tab2","tab5","tab6"))
    }

    div(
      div(strong("Plot parameters:")),
      uiOutput(ns("side_palette")),
      uiOutput(ns('side_filter_feature_plot_nvars')),
      uiOutput(ns("side_feature_axes_plot"))
    )
  })

  output$side_download_table<-renderUI({
    req(input$data_ensemble_X)
    req(input$ensemble_tab)
    if(input$ensemble_tab=="tab1"){req(input$ensemble_tab1=='tab1')}
    if(input$ensemble_tab=="tab2"){
      req(input$ensemble_tab2%in%c("tab1","tab2","tab3","tab7"))
      if(input$ensemble_tab2=="tab7"){
        req(!is.null(get_inter_res0()))

        req(input$inter_show=='Results')
      }
    }
    div(
      tipify(
        actionLink(
          ns('download_table'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })
  output$side_download_plot<-renderUI({
    req(input$ensemble_tab)
    if(input$ensemble_tab=="tab1"){req(input$ensemble_tab1=="tab2")} else{
      req(input$ensemble_tab2 %in% c('tab2','tab4',"tab5",'tab7',"tab6"))
      if(input$ensemble_tab2=="tab7"){
        req(!is.null(get_inter_res0()))

        req(input$inter_show=='Plot')

      }
    }

    div(
      tipify(
        actionLink(
          ns('download_plot'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
        ),
        "+ Download plot", options=list(container="body")
      )
    )
  })


  observeEvent(input$download_plot,{

    vals$hand_plot<- if(input$ensemble_tab=="tab1"){
      if(input$ensemble_tab1=='tab2'){
        vals$plot_ensemble<-vals$feaimp_plot1
        "feature_importance_models"
      }
    } else{
      switch(input$ensemble_tab2,
             "tab2"={
               vals$plot_ensemble<-vals$combcm
               "ensemble_confusion_plot"
             },
             "tab4"={
               vals$plot_ensemble<-vals$vartrees
               if(vals$modellist2[[1]]$modelType=="Classification"){
                 "ensemble_predictions_class_plot"
               } else{
                 "ensemble_predictions_reg_plot"
               }

             },
             "tab5"={
               vals$plot_ensemble<-vals$feaimp_plot2
               "ensemble_feature_importance_plot"
             },
             "tab6"={
               vals$plot_ensemble<-vals$ensemble_permimp
               "ensemble_feature_importance_plot"
             },
             "tab7"={
               vals$plot_ensemble<-vals$intercomb_plot
               "ensemble_interactions_plot"
             }

      )

    }


    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$download_table,{
    vals$hand_down<-if(input$ensemble_tab=="tab1"){
      if(input$ensemble_tab1=="tab1"){
        vals$down_ensemble<-data.frame(get_predtab())
        "ensemble_model_predictions"}
    } else
    {
      switch(input$ensemble_tab2,
             "tab1"={
               vals$down_ensemble<-data.frame(get_pred())
               "ensemble_predictions"
             },
             "tab2"={
               vals$down_ensemble<-data.frame(vals$ensemble_confusion)
               "ensemble_confusion"
             },
             "tab3"={
               vals$down_ensemble<-data.frame(vals$comb_down_errors_train)
               "ensemble_obs_errors"
             },
             "tab7"={
               vals$down_ensemble<-data.frame(vals$inter_res$diff_to_root)
               "ensemble_interactions"
             }

      )




    }
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)



  })


  output$side_create_preds<-renderUI({
    if(input$ensemble_tab=='tab1'){
      req(input$ensemble_tab1=="tab1")}

    if(input$ensemble_tab=='tab2'){
      req(input$ensemble_tab2=="tab1")}
    div(
      tipify(
        actionLink(
          ns('create_ensemble_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
        ),
        "Create a datalist with results", options=list(container="body")
      )
    )
  })


  output$side_round<-renderUI({
    if(input$ensemble_tab=='tab1'){
      req(input$ensemble_tab1=='tab1')

    }
    if(input$ensemble_tab=='tab2'){req(input$ensemble_tab2=='tab2'|input$ensemble_tab2=='tab3')}

    div(
      "+ Round",
      inline(numericInput(ns('round_summ'),NULL,3, width="60px"))
    )
  })
  output$side_palette<-renderUI({
    req(input$ensemble_tab)
    if(input$ensemble_tab=="tab1"){ req(input$ensemble_tab1!="tab1")}
    if(input$ensemble_tab=="tab2"){
      req(input$ensemble_tab2=="tab2"|input$ensemble_tab2=="tab5"|input$ensemble_tab2=="tab6")
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

  output$Ensemble_tab1<-renderUI({
    req(!is.null(get_predtab()))
    inline(DT::dataTableOutput(ns('ensemble_predictions')))
  })

  output$Model_tab1<-renderUI({
    req(!is.null(get_predtab()))
    div(style="max-width: 600px;overflow-x: scroll",
        inline(DT::dataTableOutput(ns('Model_tab1_table')))
    )
  })

  output$Model_tab1_table<-DT::renderDataTable({
    req(input$round_summ)
    res<-get_model_performaces()

    rbind(round(res,input$round_summ),get_predtab())
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')







  output$ensemble_predictions<-DT::renderDataTable({
    get_pred()
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')



  get_model_performaces<-reactive({

    modelist<-vals$modellist2
    newdata<-get_newdata()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]
    obc<-get_obc()

    if(is.null(obc)){
     li<-lapply(modelist,function(m){
    accu<-postResample(m$pred$pred,m$pred$obs)
    if(m$modelType=="Classification"){accu["Accuracy"]
    } else{accu['Rsquared']}
  })
     res<-do.call(cbind,li)
     return(res)


    }

    validate(need(length(obc)==nrow(newdata),"Error: The observed and predicted values have different lengths."))


    li<-lapply(modelist,function(m){
      accu<-postResample(predict(m,newdata=newdata),obc)
      if(m$modelType=="Classification"){accu["Accuracy"]
      } else{accu['Rsquared']}

    })

    res<-do.call(cbind,li)
    res

  })




  observeEvent(input$create_ensemble_predictions,{
    if(input$ensemble_tab=='tab1'){
      if(input$ensemble_tab2=="tab1"){
        vals$hand_save<- "Create Datalist: model predictions"
      }}

    if(input$ensemble_tab=='tab2'){
      if(input$ensemble_tab2=="tab1"){
        vals$hand_save<- "Create Datalist: ensemble predictions"
      }}

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


  getmodel_ui<-reactive({
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
    req(vals$cur_tab=="menu_comp")
       getmodel_ui()

    })


  observe({

    output$data_comp2_out<-renderUI({
      choiceValues<-names(vals$listamodels2)
      choiceNames=lapply(seq_along(names(vals$listamodels2)),function(x){
        div(uiOutput(ns(paste0('liequals',x))))
      })
      req(length(choiceValues)>0)
      req(length(choiceNames)>0)
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
              uiOutput(ns("pagina2"))
          ))

    )
  })
  output$comp2_prev_bnt<-renderUI({
    req(insert_rv$page!=1)
    actionButton(ns("comp2_prev"),span(
      div(strong("2. Prev")),
      div("<<"),
    ), width="70px", style="height: 70px")

  })
  output$comp2_next_bnt<-renderUI({
    req(insert_rv$page<insert_npages)
    actionButton(ns("comp2_next"),span(
      div(strong("3. Next")),
      div(">>"),
    ), style="height: 70px", width="70px")
  })



  comb_create_training_errors<-reactive({
    req(length(input$obc)>0)
    modelist<-vals$modellist2
    temp<-vals$comb_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$obc]],temp,"newdatalist")
    if(modelist[[1]]$modelType=="Classification"){
      facs<-temp[,c('obs',"pred")]
      facs$pred<-factor(facs$pred, levels=levels(get_obc()))
      factors<-attr(temp,"factors")
      factors<-data.frame(factors,facs)
      temp<-temp[,1:2]
      attr(temp,"factors")<-factors
    }
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
      "Save results in"= name_save_ensemble(),
      "Create Datalist: ensemble predictions"={ name_combb_pred()},
      "Create Datalist: model predictions"={ name_combb_pred()},
      "resample_create"={ resample_create_name()},
      "Create Datalist: comb training errors -obs"={name_comb_train_errors()},
    )})


  resample_data<-reactive({
    req(length(input$obc)>0)
    m<-vals$modellist2[[1]]
    var<-attr(m,"supervisor")
    temp<-vals$comp_treetest
    datao<-vals$saved_data[[input$obc]]
    factors<-attr(temp,"factors")

    factors<-temp[c('q_class')]
    colnames(factors)<-paste(var,colnames(factors),sep="_")
    temp[c('w_class','sig','q_class')]<-NULL
    temp
  })

  resample_create<-reactive({

    temp<-resample_data()
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
    if(input$ensemble_tab=='tab1'){
      if(input$ensemble_tab2=="tab1"){
        temp<-get_predtab()
      }}

    if(input$ensemble_tab=='tab2'){
      if(input$ensemble_tab2=="tab1"){
        temp<-get_pred()
      }}


    datao<-newdata<- get_newdata()
    newdata[rownames(temp),colnames(temp)]<-as.numeric(temp[,1])
    newdata[which(!colnames(newdata)%in%colnames(temp))]<-NULL

    modelist<-vals$modellist2
    if(modelist[[1]]$modelType=="Regression"){
      attr(newdata,"factors")<-attr(datao,"factors")
    }else{
      facs<- attr(newdata,"factors")[rownames(datao),, drop=F]
      facs<-cbind(facs,temp)
      facs[,colnames(temp)]<-factor(facs[,colnames(temp)], levels=levels(get_obc()))
      attr(newdata,"factors")<-facs
    }




    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-newdata
    } else{
      vals$saved_data[[input$over_datalist]]<-newdata
    }

  })



  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    if(vals$hand_save=='Save results in'){choices<-names(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble"))}
    res<-pickerInput(ns("over_datalist"), NULL,choices, width="350px")
    data_overwritte$df<-T
    inline(res)
  })

  output$ensemble_results_menu<-renderUI({
    req(input$data_ensemble_X)
    saved_models<-names(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble"))
    req(length(saved_models)>0)
    req(insert_rv$page==2)
    div(pickerInput(ns("ensemble_models"),strong("Results:", tiphelp("Ensemble Results. Click to select the results saved in the datalist used for predictions (X)")), choices= saved_models, width="150px",selected=vals$ensemble_models))  })




  observeEvent(input$ensemble_models,
               vals$ensemble_models<-input$ensemble_models)

  output$save_ensemble_button<-renderUI({
    req(input$ensemble_models=="new ensemble (unsaved)")
    div(style="margin-top: 20px",class='save_changes',
        tipify(actionButton(ns("save_ensemble"), icon("fas fa-save")), "Save the ensemble model in the training Datalist (X)")

    )
  })

  output$del_ensemble_button<-renderUI({
    req(length(input$ensemble_models)>0)
    div(style="margin-top: 20px",
        tipify(actionButton(ns("del_ensemble"), icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt")), "Remove model")

    )
  })

  observeEvent(input$del_ensemble,{
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")<-NULL
  })


  observeEvent(input$comp2_next,{
    req(input$data_ensemble_X)
    req(length(vals$saved_data[[input$data_ensemble_X]])>0)
    if(is.null(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble"))){
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")<-list()}
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]<-list()
    vals$ensemble_models<-'new ensemble (unsaved)'
  })




  observeEvent(input$run_preds,{
    newdata<-vals$saved_data[[input$data_ensemble_X]]
    #attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$predtab<-NULL
    # attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$newdata<-NULL
    modelist<-vals$modellist2
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]<-NULL
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$tabpred<-predcomb_table(modelist,newdata)
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$newdata<-newdata
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$obc<-get_OBC()
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$ens_modeltype<-vals$ens_modeltype
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$ensemble_active<-vals$ensemble_active
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]$listamodels2<-names(vals$listamodels2)
    #attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")<-list_results
    insert_rv$page<-2
    if(length(input$ensemble_models)>0){
      if(input$ensemble_models!="new ensemble (unsaved)")
        updatePickerInput(session,"ensemble_models",selected='new ensemble (unsaved)')
    }
    })
  get_predtab<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    predtab<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[input$ensemble_models]]$tabpred
    predtab
  })
  get_newdata<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    newdata<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$newdata
    #req(is.data.frame(predtab))
    newdata
  })
  get_inter_res0<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    inter_res0<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$inter_res0
    inter_res0
  })
  scoreloss_models<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$scoreloss_models
  })
  scoreloss_ensemble<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    aculoss=attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$scoreloss_ensemble
    aculoss
  })
  get_weis<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    weis<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$weis
    weis
  })
  get_obc<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    obc<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$obc
    obc
  })
  get_pred<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    pred<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$predictions
    pred
  })


  observeEvent(input$run_aculoss,{
    if(input$ensemble_tab=="tab1"){
      get_scoreloss_models()
    } else{
      get_scoreloss_ensemble()
    }
  })
  getargs_root<-reactive({
    req(input$ensemble_models)
    predtab<-get_predtab()
    obc<-get_obc()
    pred<-get_pred()
    weis=get_weis()
    newdata<-get_newdata()

    modelist<-vals$modellist2
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]




    accu<-postResample(pred,obc)
    m<-modelist[[1]]
    accu<-if(m$modelType=="Classification"){accu["Accuracy"]
    } else{accu['Rsquared']}


    args<-list(
      newdata=newdata,
      obc=obc,
      reps=input$aculoss_rep,
      modelist=modelist,
      weis=weis,
      accu=accu,
      scale=input$en_scale,
      en_method=input$en_method,
      top.features=ncol(newdata),
      root=NULL,
      inter_method="Paired",
      progress=T,
      ms=NULL,
      type="modelist",
      feaimp=F
    )
    args
  })
  get_scoreloss_ensemble<-reactive({
    args<-getargs_root()
    vals$ensemble_args<-args
    acc<-do.call(getroot,args)

    rep200<-getsig_aculoss(acc,accu=NULL,sig=input$aculoss_sig)
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$ensemble_resampling<-acc
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$scoreloss_ensemble<-rep200
    updateTabsetPanel(session,"ensemble_tab","tab2")
    updateTabsetPanel(session,"ensemble_tab2","tab6")

  })
  get_scoreloss_models<-reactive({
    predtab<-get_predtab()
    modelist<-vals$modellist2
    newdata<-get_newdata()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]
    obc<-get_obc()
    #pred<-get_pred()
    weis=rep(1,length(modelist))
    accu<-get_model_performaces()


    withProgress(min=0,max=length(modelist), message="Calculating Permutation Importance ...",{
      resultnovo<-lapply(1:length(modelist),function(i){
        m<-modelist[[i]]
        ac<-accu[[i]]
        acc<-getroot(newdata=newdata,
                     obc=obc,
                     reps=input$aculoss_rep,
                     modelist=modelist,
                     weis=weis,
                     accu=ac,
                     scale=input$en_scale,
                     en_method=input$en_method,
                     top.features=ncol(newdata),
                     root=NULL,
                     inter_method="Paired",
                     progress=T,
                     ms=m,
                     type="model",
                     feaimp=F)
        res<-getsig_aculoss(acc, NULL,sig=input$aculoss_sig)
        incProgress(1, message=paste0('model:',names(modelist)[i]))
        list(res, acc)
      })

    })
    result<-lapply(resultnovo, function(x) x[[1]])
    acc<-lapply(resultnovo, function(x) x[[2]])
    sigvars<-lapply(resultnovo, function(x) attr(x[[1]],"sigvars"))
    result<-unlist(result)
    attr(result,"sigvars")<-sigvars


    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$models_resampling<-acc
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$scoreloss_models<-result
    updateTabsetPanel(session,"ensemble_tab","tab1")
    updateTabsetPanel(session,"ensemble_tab1","tab3")
  })
  get_OBC<-reactive({
    req(length(input$obc)>0)
    modelist<-vals$modellist2
    data<-vals$saved_data[[input$obc]]
    var<-attr(modelist[[1]],"supervisor")
    if(vals$modellist2[[1]]$modelType=="Regression"){
      req(var%in%colnames(data))
      obc<-data[,var]
      names(obc)<-rownames(data)
    } else {
      factors<-attr(data,"factors")
      req(var%in%colnames(factors))
      obc<-factors[,var]
      names(obc)<-rownames(factors)
    }
    obc

  })
  get_PRED<-reactive({

    #req(length(input$obc)>0)
    req(length(vals$modellist2)>0)
    req(input$data_ensemble_X)
    req(input$en_method)
    req(is.data.frame(get_predtab()))
    req(input$wei_datatype)
    obc<-NULL
    predtab<-get_predtab()
    modelist<-vals$modellist2
    obc<-get_obc()
    pred<-get_ensemble_pred(
      predtab,modelist, en_method=input$en_method, obc=obc, weitype=input$wei_datatype,
      newdata=get_newdata(),weis=get_weis()
    )
    pred[,1]<-if(modelist[[1]]$modelType=="Classification"){

      factor(pred[,1],levels =modelist[[1]]$levels)}else{
        pred
      }
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$predictions<-pred
  })

  observeEvent(list(input$en_method, input$wei_datatype),{
    req(input$en_method)
    req(input$wei_datatype)
    get_WEIS()
    get_OBC()

  })

  get_WEIS<-reactive({
    newdata<-get_newdata()
    class_whei<-if(vals$modellist2[[1]]$modelType=="Classification"){
      getClass_wei(
        get_predtab(),
        get_obc(),
        vals$modellist2,
        en_method=input$en_method,
        weitype=input$wei_datatype,
        newdata=newdata
      )
    } else{
      req(input$en_method)
      req(input$wei_datatype)
      #req(length(get_obc())==length(nrow(newdata)))
      # rep(1,length(vals$modellist2))
      args<-list(modelist=vals$modellist2,newdata=newdata,obc=get_obc(),en_method=input$en_method,weitype=input$wei_datatype)
      do.call(getReg_wei,args)

    }
    if(!is.null(class_whei)){
      class_whei<-as.vector(class_whei)
      names(class_whei)<-names(vals$modellist2)
      attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$weis<-class_whei
      attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[ input$ensemble_models]]$weis<-class_whei
      class_whei
    }
  })



  observe({
    req(is.null(get_weis()))
    get_WEIS()
  })

  observe({
    req(is.null(get_pred()))
    get_PRED()
  })


  output$teste_comb<-renderUI({})


  observeEvent(input$ensemble_models,{
    get_OBC()
    get_pred()

  })

  ensemble_save<-reactive({

    name<-if(input$hand_save=="create"){
      input$newdatalist}else{input$over_datalist}
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[[name]]<-attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")[["new ensemble (unsaved)"]]<-NULL
    if(input$hand_save=="create"){
      vals$ensemble_models<-input$newdatalist}else{vals$ensemble_models<-input$over_datalist}

    })


  observeEvent(input$save_ensemble,{
    vals$hand_save<- "Save results in"
    showModal(module_combb())
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
      "Save results in"={ensemble_save()},
      "Create Datalist: ensemble predictions"={combb_create_pred()},
      "Create Datalist: model predictions"={combb_create_pred()},
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



  name_save_ensemble<-reactive({
    name0<-paste0("Ensemble")
    names<-names(attr(vals$saved_data[[input$data_ensemble_X]],"ensemble"))
    if(length(names)>0){
      name1<-make.unique(c(names,name0), sep="_")
      name1[length(names)+1]
    } else{
      name1<-name0
    }


  })
  resample_create_name<-reactive({    bag<-1
    var<-attr(vals$modellist2[[1]],"supervisor")
    name0<-paste("comb",var,"qclass", sep="_")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]


  })
  name_combb_pred<-reactive({
    if(input$ensemble_tab=='tab1'){
      if(input$ensemble_tab2=="tab1"){
        name0<- "Model predictions"
      }}
    if(input$ensemble_tab=='tab2'){
      if(input$ensemble_tab2=="tab1"){
        name0<- "Ensemble predictions"
      }}
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]

  })
  name_comb_train_errors<-reactive({
    name0<-paste0("Ensemble errors")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]

  })
}
