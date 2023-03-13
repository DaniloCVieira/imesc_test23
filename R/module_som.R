
#' @export
module_ui_som<-function(id){

  ns<-NS(id)
  tagList(

    div(
      class='choosechannel',
      inline(uiOutput(ns("som_type"))),
      uiOutput(ns("som_inputs")),
      uiOutput(ns("som_panels"))
    )
  )

}

#' @export

module_server_som<-function (input,output,session,vals,df_colors,newcolhabs,df_symbol ){



  palette=c(
    "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
  )
  symbols<-c("pch1","pch2","pch3","pch4")

  dfcolors <- data.frame(
    val = palette
  )

  ns<-session$ns
  ns_som <- NS('som')
  tunesom<-reactiveValues(
    finetopo=F,
    finesom=F,
    dim=c(5,5),
    seed=NA,
    rlen=500,
    distmethod="BrayCurtis",
    toroidal="FALSE",
    neighbourhood.fct='bubble',
    a1=0.05,
    a2=0.01,
    r1=0,
    r2=0,
    mode="online",
    maxna=0.001,
    topo="hexagonal",
    sugtopo=T
  )
  saved_sompred<-reactiveValues()
  re<-reactiveValues(df=NULL)
  cur_predsom_res1<-reactiveValues(df="Data predictions (X)")
  cur_predsom_res2<-reactiveValues(df="Data predictions (X)")
  cur_predsom_res3<-reactiveValues(df="Data predictions (X)")
  predsom_war<-reactiveValues(df=F)
  bmu_p_bgpalette<-reactiveValues(df=dfcolors$val[16])
  bmu_bgpalette<-reactiveValues(df=dfcolors$val[16])
  bmu_p_training<-reactiveValues(df=dfcolors$val[1])
  bmu_facpalette<-reactiveValues(df=dfcolors$val[11])
  bmu_p_test<-reactiveValues(df=dfcolors$val[14])
  bmu_p_border_grid<-reactiveValues(df=dfcolors$val[4])
  bmu_border_grid<-reactiveValues(df=dfcolors$val[4])
  bmu_p_dotlabel<-reactiveValues(df='symbols')

  bmu_p_symbol<-reactiveValues(df= df_symbol$val[1])
  bmu_symbol<-reactiveValues(df= df_symbol$val[1])
  output$showgrid <- renderPlot({
    if(isTRUE(input$splitdata_som)){data=training_data$df}else{
      data = getdata_som()}
    validate(need(input$xdim!="", ""))
    validate(need(input$ydim!="", ""))
    validate(need( (input$xdim*input$ydim)<=nrow(data), "The number of map units must be less than or equal to the number of observations. Please decrease the 'xdim' and/or 'ydim' dimensions"))
    try({
      par(mar = c(0, 0, 0, 0))

      grid<-kohonen::somgrid(input$xdim, input$ydim, topo = input$topo, neighbourhood.fct=tunesom$neighbourhood.fct, toroidal=toroidal())
      plot.som_grid(grid)
    })
  })
  output$var_pproperty <- renderUI({
    data = data.frame(vals$saved_data[[input$data_som]],"factors")
    column(
      12,
      selectInput(ns("variable_pproperty"),
                  label = "select a variable",
                  choices = colnames(data),
                  selected=vals$variable_pproperty
      )
    )
  })

  observeEvent(input$variable_pproperty,{
    vals$variable_pproperty<-input$variable_pproperty
  })
  observeEvent(input$npic,{
    vals$npic<-input$npic
  })
  observeEvent(input$vfm_type,{
    vals$vfm_type<-input$vfm_type
  })
  output$vfm_type_out<-renderUI({
    my_choices<-c("Highest correlations", "Clockwise-correlations")
    div(span("+",inline(
      pickerInput(ns("vfm_type"),NULL,
                  choices = c("var", "cor"),
                  choicesOpt = list(
                    content = stringr::str_trunc(my_choices, width = 75)
                  ),
                  selected=vals$vfm_type,
                  width="150px"
      ))))
  })
  output$npic_out<-renderUI({
    if(is.null(vals$npic)){vals$npic<-10}
    div(
      span("+ Number",
           inline(
             tipify(
               numericInput(ns("npic"), NULL, value = vals$npic, min = 2, width="75px"),"Number of variables to display"
             )))
    )
  })
  output$varfac_out<-renderUI({
    req(isTRUE(input$varfacmap_action))
    column(12,
           uiOutput(ns('vfm_type_out')),
           uiOutput(ns('npic_out')))})


  output$bmu_shape<-renderUI({
    req(input$bmu_dotlabel=='symbols')
    if(is.null(vals$bmu_symbol)){vals$bmu_symbol<-df_symbol$val[1]}

    div(span("+ Shape",inline(
      tipify(
        pickerInput(ns("bmu_symbol"),
                    label = NULL,
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),

                    selected=vals$bmu_symbol, width="75px")
        ,"symbol shape"
      )
    )))
  })
  observeEvent(input$bmu_symbol,{
    vals$bmu_symbol<-input$bmu_symbol
  })
  observeEvent(input$varfacmap_action,
               vals$varfacmap_action<-input$varfacmap_action)
  observeEvent(input$bmu_dotlabel,{
    vals$bmu_dotlabel<-input$bmu_dotlabel
  })
  observeEvent(input$bmu_facpalette,
               vals$bmu_facpalette<-input$bmu_facpalette)
  observeEvent(input$bmu_symbol_size,{
    vals$bmu_symbol_size<-input$bmu_symbol_size
  })
  output$vfm_check<-renderUI({
    if(is.null(vals$varfacmap_action)){vals$varfacmap_action<-T}
    div(
      span("+ ",
           inline(checkboxInput(ns("varfacmap_action"), span("Variable factor map",actionLink(ns("varfacmap"), tipify(icon("fas fa-question-circle"), "Click for more details"))),value =vals$varfacmap_action, width="100px")))
    )
  })
  output$bmu_display_out<-renderUI({
    if(is.null(vals$bmu_dotlabel)){vals$bmu_dotlabel<-'symbols'}
    div(span("+ Display:",inline(
      radioButtons(
        ns("bmu_dotlabel"),NULL , choices = c("labels", "symbols"),selected=vals$bmu_dotlabel,inline=T, width="100px")
    )))
  })
  output$bmu_facpalette_out<-renderUI({
    if(is.null(vals$bmu_facpalette)){vals$bmu_facpalette<-dfcolors$val[11]}
    div(span("+ Obs color:",inline(
      tipify(
        pickerInput(ns("bmu_facpalette"),
                    label =NULL,
                    choices = vals$colors_img$val,
                    choicesOpt = list(content = vals$colors_img$img),
                    selected=vals$bmu_facpalette, width="75px"),
        "Symbol colors. Choose a gradient to color observations by a factor"
      )
    )))
  })

  output$bmu_symbol_size_out<-renderUI({
    div(span("+ Size:",
             inline(
               tipify(numericInput(ns("bmu_symbol_size"),strong(),value = vals$bmu_symbol_size,min = 0.1,max = 3,step = .1, width="75px"),"symbol size")
             )))
  })
  output$bmu_bgpalette_out<-renderUI({
    if(is.null(vals$bmu_bgpalette)){vals$bmu_bgpalette<-dfcolors$val[16]}
    div(span("+ Background",
             inline(
               tipify(
                 pickerInput(ns("bmu_bgpalette"),
                             label = NULL,
                             choices = vals$colors_img$val,
                             choicesOpt = list(content = vals$colors_img$img),

                             selected=vals$bmu_bgpalette, width="75px"),
                 "Color of grid units"
               )
             )
    ))
  })
  observeEvent(input$bmu_bgpalette,
               vals$bmu_bgpalette<-input$bmu_bgpalette)


  output$bmu_bg_transp_out<-renderUI({
    div(span("+ Transparency:",
             inline( tipify(
               numericInput(ns("bmu_bg_transp"),NULL,
                            value=vals$bmu_bg_transp, min=0, max=1,step=0.1, width="75px"),
               "Background transparency"))
    ))
  })
  output$bmu_border_grid_out<-renderUI({
    if(is.null(vals$bmu_border_grid)) {vals$bmu_border_grid<-vals$colors_img$val[getsolid_col()] [7]}
    div(span("+ Border", inline(
      tipify(
        pickerInput(ns("bmu_border_grid"),
                    label =NULL,
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(
                      content =  vals$colors_img$img[getsolid_col()] ),
                    selected= vals$bmu_border_grid, width="75px"),
        "Grid border color"
      )
    )))
  })
  output$bmu_var_color_out<-renderUI({
    if(is.null(vals$bmu_var_color)){vals$bmu_var_color<-"black"}
    div(span("+ Var color:",inline(
      tipify(
        pickerInput(ns("bmu_var_color"),
                    label =NULL,
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(
                      content =  vals$colors_img$img[getsolid_col()]), width="75px",
                    options=list(container="body"),
                    selected=vals$bmu_var_color
        ),
        "Variable color"
      )
    )))
  })

  output$bmu_cexvar_out<-renderUI({
    div(span("+ Var size",
             inline(
               tipify(numericInput(ns("bmu_cexvar"),NULL,value = vals$bmu_cexvar,min = 0.1,max = 3,step = .1, width="75px"),"variable text size (only for the variable factor map)")
             )))
  })

  output$bmu_down_links<-renderUI({
    div(
      div(
        tipify(
          actionLink(
            ns('downp_bmu'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-image")))
          ),
          "download BMU plot",     options=list(container="body")
        )
      ),

      div(
        tipify(
          actionLink(
            ns('down_pcorr_results'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table")))
          ),
          "download variable factor results",     options=list(container="body")
        )
      )
    )

  })
  output$pop_pcorr<-renderUI({


    fluidRow(class="map_control_style",style="color: #05668D",
             uiOutput(ns("vfm_check")),
             uiOutput(ns("varfac_out")),
             br(),
             uiOutput(ns("bmu_display_out")),
             uiOutput(ns("bmu_facpalette_out")),
             uiOutput(ns("bmu_fac_control")),
             uiOutput(ns("bmu_shape")),
             uiOutput(ns("bmu_symbol_size_out")),
             uiOutput(ns("bmu_bgpalette_out")),
             uiOutput(ns("bmu_bg_transp_out")),
             uiOutput(ns("bmu_border_grid_out")),
             uiOutput(ns("bmu_var_color_out")),
             uiOutput(ns("bmu_cexvar_out")),
             uiOutput(ns("bmu_legend_out")),
             uiOutput(ns("bmu_down_links"))
    )

  })
  observeEvent(input$bmu_var_color,{
    vals$bmu_var_color<-input$bmu_var_color
  })
  output$bmu_legend_out<-renderUI({
    req(input$bmu_dotlabel=="symbols")
    col<-getcolhabs(vals$newcolhabs,input$bmu_facpalette,2)
    req(col[1]!=col[2])

    div(
      actionLink(ns("bmu_legend") ,h5("+ Legend adjustment:",style="color: blue")),
      uiOutput(ns("pcorr_legcontrol"))
    )
  })
  output$pcorr_legcontrol<-renderUI({
    #req(isTRUE(vals$bmuleg))
    column(12,
           div(span("+ leg x",inline(
             tipify(numericInput(ns("bmu_insertx"),NULL,value=vals$bmu_insertx,step=0.05, width="75px"),"legend position relative to the x location")
           ))),
           div(span("+ leg y",
                    inline(
                      tipify(numericInput(ns("bmu_inserty"),NULL,value=vals$bmu_inserty,step=0.05, width="75px"),"legend position relative to the y location")
                    ))),
           div(span("+ ncol leg",
                    inline(
                      tipify(numericInput(ns("bmu_ncol"),NULL,value=vals$bmu_ncol,step=1, width="75px"),"the number of columns in which to set the legend items")
                    ))),
           div(span("+ bg leg",
                    inline(
                      tipify(numericInput(ns("bmu_leg_transp"),NULL,value=vals$bmu_leg_transp,step=0.05, max=1, width="75px"),"Legend background transparency")
                    )))
    )


  })
  observeEvent(input$bmu_leg_transp,{
    vals$bmu_leg_transp<-input$bmu_leg_transp
  })
  observeEvent(input$bmu_bg_transp,{
    vals$bmu_bg_transp<-input$bmu_bg_transp
  })


  observeEvent(input$bmu_ncol,{
    vals$bmu_ncol<-input$bmu_ncol
  })
  observeEvent(input$bmu_insertx,{
    vals$bmu_insertx<-input$bmu_insertx
  })
  observeEvent(input$bmu_insertx,{
    vals$bmu_inserty<-input$bmu_inserty
  })

  output$pcorr_control <- renderUI({
    column(12,style="background: white",
           p(strong(h4("Best matching units"))),
           sidebarLayout(
             sidebarPanel(uiOutput(ns("pop_pcorr"), width=300)),
             mainPanel(
               div( plotOutput(ns("pCorrCodes")),
                    #renderPrint({
                    #m=vals$som_results
                    #grid<-m$grid$pts
                    #dtopo<-unit.distances(m$grid)
                    #dcodes<-object.distances(m,"codes")
                    #res<-unlist(lapply(codes, function (x)  sum(dist(x)/dist(dtopo))))
                    #sort(res, dec=T)})

               )
             )
           ))
  })
  observeEvent(input$bmu_factors,{
    vals$bmu_factors<-input$bmu_factors
  })
  output$bmu_fac_control<-renderUI({
    req(input$bmu_facpalette)
    req(input$bmu_dotlabel)
    col<-getcolhabs(vals$newcolhabs,input$bmu_facpalette,2)
    if(input$bmu_dotlabel=='labels'|col[1]!=col[2])
    {span("+ Labels",
          inline(
            tipify(pickerInput(ns("bmu_factors"),NULL,
                               choices = c(colnames(attr(vals$saved_data[[input$data_som]],"factors","factors"))), selected=vals$bmu_factors, width="150px"),"color observations by factor")
          )
    )
    }})
  output$pCorrCodes <- renderPlot({
    #req(input$varfacmap_action)
    #req(input$npic)
    p<-BMUs()
    vals$pcorr_results<-data.frame(attr(p,"result"))
    p
  })
  output$pchanges <- renderPlot({
    pchanges(vals$som_results)
  })
  output$pcounts <- renderPlot({
    pcounts(vals$som_results)
  })
  output$pUmatrix <- renderPlot({
    pUmatrix(vals$som_results)
  })
  output$pproperty <- renderPlot({
    req(input$variable_pproperty)
    pproperty(vals$som_results,
              input$variable_pproperty,
              input$variable_pproperty)
    vals$pprop_plot<-recordPlot()
  })
  observeEvent(input$somgridh,{
    output$somgridhelp <- renderText({
      paste0(br(),
             h4("somgrid {kohonen}"),
             getHelp('unit.distances'))
    })
  })



  observeEvent(input$somgridhelp, {
    showModal(

      modalDialog(
        uiOutput(ns("textsomgrid")),
        title = h4(strong("somgrid")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )

    )
  })

  output$textsomgrid<-renderUI({
    div(
      tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('somgrid')," function from the",
                      code("kohonen"),"package."),
                    p(icon("fas fa-exclamation-circle"),"All arguments are passed to the function;"),
                    p(icon("fas fa-exclamation-circle"), "Arguments are described 'in the ",actionLink(ns("somgridh"),"help page")," of the function;")
    ),
    column(12,
           htmlOutput(ns("somgridhelp"))
    )))
  })
  output$textvarfacmap<-renderUI({

    div(

      tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(
      column(12,
             h4("Variable factor map"),
             p("The chart is very similar to the variable factor map obtained from the principal component analysis (PCA). It calculates the weighted correlation for each variable using the coordinates (x, y) of the neurons and their weights (number of instances). The codebooks vectors of the cells correspond to an estimation of the conditional averages, calculating their variance for each variable is equivalent to estimating the between-node variance of the variable, and hence their relevance."),
             p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("clock-wise correlations")," returns",code("npic")," variables with the highest correlation considering along the different directions of the codebook")
      )

    )
    )

  })
  observeEvent(input$varfacmap, {
    showModal(modalDialog(
      uiOutput(ns("textvarfacmap")),
      title = h4(strong("Variable factor map")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    ))
  })
  output$textsupersom<-renderUI({
    div(
      tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('som')," function from the",
                      actionLink(ns("kohonen"),"kohonen"),"package."),
                    p(icon("fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink(ns("supersomh"),"help page")," of the function;")
    ),
    column(12,
           htmlOutput(ns("supersomhelp"))
    )))

  })
  observeEvent(input$supersomh,{
    output$supersomhelp<-renderText({
      paste0(
        br(),
        h4("supersom {kohonen}"),
        p(
          icon("fas fa-exclamation-circle"),
          "Argument",
          code("X"),
          "fixed to your uploaded and pre-treated data;"
        ),
        p(
          icon("fas fa-exclamation-circle"),
          code("a1"),
          "and" ,
          code("a2") ,
          "refers to the",
          code("alpha"),
          "argument"
        ),
        p(
          icon("fas fa-exclamation-circle"),
          code("r1"),
          "and" ,
          code("r2") ,
          "refers to the",
          code("radius"),
          "argument"
        ),
        p(
          icon("fas fa-exclamation-circle"),
          "Arguments fixed to their default values:",
          code("whatmap"),
          ", " ,
          code("user.weights"),
          ", " ,
          code("dist.fcts"),
          ", " ,
          code("cores"),
          ", " ,
          code("init"),
          ", " ,
          code("normalizeDataLayers")
        ),
        getHelp('supersom')
      )
    })
  })

  observeEvent(input$kohonen,{
    output$supersomhelp<-renderText({
      getHelp('kohonen')
    })
  })


  observeEvent(input$supersomhelp, {
    showModal( modalDialog(
      div(
        uiOutput(ns("textsupersom")))
      ,
      title = "Self-Organizing Maps",
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    ))
  })

  observeEvent(input$sugtopohelp, {
    showModal(

      modalDialog(
        uiOutput(ns('textsugtopohelp')),
        title = "Suggested topology",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )

    )
  })

  output$textsugtopohelp<-renderUI({

    div(
      tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    'The number of map nodes and the side length ratio is performed with the following steps (Vesanto, 2000 ):',
                    column(12, style="margin-left: 10px; margin-top: 5px;",
                           p(strong("1."),"Determine the number of map nodes using the heuristic recommendation:",withMathJax(helpText(
                             "$$ M = 5{\\sqrt{N}}$$"
                           )),"where N is the number of observations in the input data set ( Vesanto, 2000 ),"),
                           p(strong("2."),"Determine the eigenvectors and eigenvalues in the data from the autocorrelation matrix,"),
                           p(strong("3."),"Set the ratio between the two sides of the grid equivalent to the ratio between the two largest eigenvalues, and "),
                           p(strong("4."),"Scale the side lengths so that their product (xdim * ydim) is as close as possible to the number of map units determined above."))
    )))

  })

  output$side_predsom_cm<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(class="palette",
                 span("+ Palette:",
                      inline(
                        pickerInput(ns("cm_supsom_palette"),
                                    label = NULL,
                                    choices =     vals$colors_img$val[getgrad_col()],
                                    choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"),width='100px')
                      )
                 )
             ),
             div(
               span("+ Y Datalist:",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values",
                                           options=list(container="body")),
                    inline(
                      pickerInput(ns("predsom_newY"),
                                  NULL,names(vals$saved_data[getobs_som()])
                                  ,width='100px')
                    )
               )
             ),
             div(
               span("+ Y variable:",
                    inline(
                      pickerInput(ns("predsom_y"),NULL, choices=rev(names(pred_som()$predictions[-1])),width='100px')
                    )
               )
             ),
             div(
               actionLink(
                 ns('downp_confsom'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
               )

             )
    )
  })
  output$predsom_cm<-renderUI({
    column(12, style = "background: Snow;",
           sidebarLayout(
             sidebarPanel(uiOutput(ns("side_predsom_cm"))),
             mainPanel( uiOutput(ns("conf_som")))
           )
    )
  })
  output$predsom_tabs<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))
    predsom.reactive()})
  output$conf_som<-renderUI({
    fluidRow(
      plotOutput(ns("predsom_confusion")),
      renderPrint(confusionMatrix(get_predsom_cm()))

    )
  })
  output$predsom_confusion<-renderPlot({
    conf<-get_predsom_cm()
    res<-plotCM(conf/sum(conf)*100, input$cm_supsom_palette,  newcolhabs=vals$newcolhabs)
    vals$conf_som<-res
    res
  })


  output$bmu_p_tools<-renderUI({
    column(12,
           fluidRow(
             popify(bsButton(ns("downp_bmu_pred"), span(icon("fas fa-download"),icon("fas fa-image")),style = "button_active"),NULL,"download BMU plot",
                    options=list(container="body")

             )
           )
    )
  })
  output$pred_bmu<-renderUI({
    column(12,style="background: white",
           p(strong(h4("Predictions - Best matching units"))),
           uiOutput(ns("bmu_p_tools")),
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("pop_bmu_p"))
             ),  mainPanel(
               plotOutput(ns("bmu_pCodes"))
             )
           ))


  })
  observeEvent(input$bmu_p_factors,{
    vals$bmu_p_factors<-input$bmu_p_factors
  })
  output$bmup_labels<-renderUI({
    req(input$bmu_p_dotlabel)
    req(input$bmu_p_dotlabel=='labels')
    div(span("+ Labels:",inline(
      pickerInput(ns("bmu_p_factors"),NULL,choices = c(colnames(attr(vals$saved_data[[input$data_som]],"factors"))),width = '75px', selected=vals$bmu_p_factors)
    )))

  })
  observeEvent(input$bmu_p_symbol,
               vals$bmu_p_symbol<-input$bmu_p_symbol)
  output$bmup_symbol<-renderUI({
    req(input$bmu_p_dotlabel)
    req(input$bmu_p_dotlabel=='symbols')
    if(is.null(vals$bmu_p_symbol)){vals$bmu_p_symbol<-df_symbol$val[1]}
    div(span("+ Shape:", inline(
      pickerInput(ns("bmu_p_symbol"),
                  label = NULL,
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),

                  selected=vals$bmu_p_symbol, width = '75px')
    )))

  })
  observeEvent(input$bmu_p_dotlabel,
               vals$bmu_p_dotlabel<-input$bmu_p_dotlabel)
  output$bmu_p_dotlabel_out<-renderUI({
    if(is.null(vals$bmu_p_dotlabel)){vals$bmu_p_dotlabel<-'symbols'}
    div(span("+ Display:",
             inline(radioButtons(ns("bmu_p_dotlabel"), NULL, choices = c("labels", "symbols"),selected=vals$bmu_p_dotlabel, inline=T, width="100px"
             ))

    ))
  })
  observeEvent(input$bmu_p_training,{
    vals$bmu_p_training<-input$bmu_p_training
  })
  output$bmu_p_training_out<-renderUI({
    if(is.null(vals$bmu_p_training)){vals$bmu_p_training<-dfcolors$val[1]}
    div(span("+ Training color:",inline(
      pickerInput(ns("bmu_p_training"),
                  label =NULL,
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                  selected=vals$bmu_p_training, width = '75px'
      )
    )))
  })
  observeEvent(input$bmu_p_test,{
    vals$bmu_p_test<-input$bmu_p_test
  })
  output$bmu_p_test_out<-renderUI({
    if(is.null(vals$bmu_p_test)){vals$bmu_p_test<-dfcolors$val[14]}
    div(span("+ Test color:", inline(
      pickerInput(ns("bmu_p_test"),
                  label ="",
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                  selected=vals$bmu_p_test, width = '75px')
    )))
  })
  observeEvent(input$bmu_p_symbol_size,{
    vals$bmu_p_symbol_size<-input$bmu_p_symbol_size
  })
  output$bmu_p_symbol_size_out<-renderUI({
    div(span("+ Size:", inline(tipify(
      numericInput(ns("bmu_p_symbol_size"),NULL,value = vals$bmu_p_symbol_size,min = 0.1,max = 3,step = .1, width = '75px'),"symbol size"
    ))))
  })
  observeEvent(input$bmu_p_bgpalette,{
    vals$bmu_p_bgpalette<-input$bmu_p_bgpalette
  })
  output$bmu_p_bgpalette_out<-renderUI({
    if(is.null(vals$bmu_p_bgpalette)){vals$bmu_p_bgpalette<-dfcolors$val[16]}
    div(span("+ Background:",
             inline(
               pickerInput(ns("bmu_p_bgpalette"),
                           label = NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img),

                           selected=vals$bmu_p_bgpalette, width = '75px')
             )))
  })
  observeEvent(input$bmu_p_bg_transp,{
    vals$bmu_p_bg_transp<-input$bmu_p_bg_transp
  })
  output$bmu_p_bg_transp_out<-renderUI({
    div(span("+ Transparency:",inline(
      tipify(
        numericInput(ns("bmu_p_bg_transp"),NULL,
                     value=vals$bmu_p_bg_transp, min=0, max=1,step=0.1, width = '75px'),
        "Background transparency")
    )))
  })
  observeEvent(input$bmu_p_border_grid,{
    vals$bmu_p_border_grid<-input$bmu_p_border_grid
  })
  output$bmu_p_border_grid_out<-renderUI({
    if(is.null(vals$bmu_p_border_grid)) {vals$bmu_p_border_grid<-vals$colors_img$val[getsolid_col()][7]}
    div(span(span('+ Border:'),inline(
      pickerInput(ns("bmu_p_border_grid"),
                  label =NULL,
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(
                    content =  vals$colors_img$img[getsolid_col()] ),
                  selected= vals$bmu_p_border_grid, width = '75px')
    )))
  })




  output$pop_bmu_p<-renderUI({
    fluidRow(
      class="map_control_style",style="color: #05668D",
      uiOutput(ns('bmu_p_dotlabel_out')),
      uiOutput(ns("bmup_labels")),
      uiOutput(ns("bmu_p_training_out")),
      uiOutput(ns("bmu_p_test_out")),
      uiOutput(ns("bmup_symbol")),
      uiOutput(ns('bmu_p_symbol_size_out')),
      uiOutput(ns("bmu_p_bgpalette_out")),
      uiOutput(ns("bmu_p_bg_transp_out")),
      uiOutput(ns("bmu_p_border_grid_out"))
    )

  })
  output$bmu_p_fac_control<-renderUI({
    fluidRow(
      splitLayout(
        column(12,
               p(strong("Test color", style="color: #0D47A1;")),
               tipify(
                 pickerInput(ns("bmu_p_test"),
                             label =NULL,
                             choices =  vals$colors_img$val[getsolid_col()] ,
                             choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                             selected= vals$colors_img$val[getsolid_col()] [4],
                             options=list(container="body")),
                 "Color of the testing observations",options=list(container="body"))),
        fluidRow(
          p(strong("Training color", style="color: #0D47A1;")),
          tipify(
            pickerInput(ns("bmu_p_training"),
                        label =NULL,
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                        selected= vals$colors_img$val[getsolid_col()] [2],
                        options=list(container="body")),
            "Color of the training observations",
            options=list(container="body")))
      )
    )
  })
  output$bmu_pCodes<-renderPlot({vals$bmus_pred_plot})
  output$bmu_pCodes<-renderPlot({
    p<-BMUs_pred()
    vals$bmu_p_results<-data.frame(attr(p,"result"))
    p
  })
  output$som_type<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    req(input$som_tab=='som_tab1')
    div(id=ns("som_mode"),class="som_mode",
        span(
          strong("Type:", style="color: #0D47A1;"),
          inline(radioButtons(ns("som_type"), NULL, choices=c("Unsupervised", "Supervised"), inline=T, selected='Unsupervised'))
        )
    )
  })

  observe({

    shinyjs::hide('som_mode')
  })


  observeEvent(input$som_models,{
    vals$cur_train<-input$som_models
  })

  output$som_models<-renderUI({
    if(is.null(vals$cur_train)){vals$cur_train<-"new som (unsaved)"}
    req(input$data_som)
    req(input$som_tab=="som_tab2"|input$som_tab=="som_tab3")
    choices<-names(attr(vals$saved_data[[input$data_som]],"som"))

    div(
      div(
        div(strong("Som results:", tiphelp("SOM results. Click to see SOM results saved in the selected Datalist"))),
        div(
          inline(pickerInput(ns("som_models"),
                             NULL,
                             choices=choices,
                             selected=vals$cur_train, width='150px')),
          inline( bsButton(ns("som_model_delete"),icon("fas fa-trash-alt")))
        )
      ),

    )
  })
  output$save_som<-renderUI({
    req(input$som_models=="new som (unsaved)")
    req(input$som_tab=="som_tab2"|input$som_tab=="som_tab3")
    popify(
      div(class="save_changes",
          bsButton(ns("tools_savesom"), div(icon("fas fa-save")),style  = "animation: glowing 1000ms infinite;", type="action",value=FALSE)
      )
      , NULL, "Save the som model in the Datalist",
    )
  })
  output$som_cur<-renderUI({
    req(input$som_tab=="som_tab2"|input$som_tab=="som_tab3")
    div(
      p(strong("X:"), em(input$data_som)),
      p(style="margin-top: -10px",strong("Y:"),em(input$som_type2,";"),em(attr(vals$som_results,"Method"))),
      p(style="margin-top: -10px",strong("Test Partition:"),em(attr(vals$som_results,"test_partition"))),
      p(style="margin-top: -10px",strong("Som-Attribute:"), em(input$som_models)),


    )
  })
  output$som_inputs<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(style='choosechannel',
        div(class="well3",
            splitLayout(cellWidths = c("75%","25%"),
                        div(
                          span(strong("X:"), inline(uiOutput(ns("som_x"))),
                               #inline(uiOutput(ns("som_partition")))
                          ),
                          inline(uiOutput(ns("som_models"))),
                          inline(uiOutput(ns("save_som"))),

                        ),
                        div(style="width: 100px",
                            inline(uiOutput(ns("som_cur")))
                        )
            )
        ),
        uiOutput(ns("som_datalist_y")))
  })
  output$som_datalist_y<-renderUI({
    if(is.null(vals$cur_datasomY)){vals$cur_datasomY<-names(vals$saved_data)[1]}

    req(input$som_type=='Supervised')
    req(input$som_tab=="som_tab1")
    div(strong("Y:"), class="well3",
        span(
          inline(
            div(
              div("Y Datalist:"),
              pickerInput(ns("data_somY"),NULL, choices =names(vals$saved_data), selected=vals$cur_datasomY, width="250px", options=list(container="body"))
            )
          ),
          inline(uiOutput(ns("som_y"))),
          inline(uiOutput(ns("som_y_sel")))

        ))
  })
  output$som_x<-renderUI({
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_som"),NULL,choices =names(vals$saved_data),width="250px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_soms")))

    )
  })

  savereac<-reactive({



    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img

    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })
 #input<- readRDS('input.rds')
  #vals<-readRDS('savepoint.rds')
  output$saved_soms<-renderUI({

    req(input$data_som)
    names_som<-names(attr(vals$saved_data[[input$data_som]],"som"))
    req(length(names_som)>0)
    pic<-which(names_som%in%"new som (unsaved)")
    if(length(pic)>0){
      names_som<-names_som[-pic]
    }
    req(length(names_som)>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_som)), "saved model(s)")
  })

  output$som_y<-renderUI({
    req(input$som_tab)
    if(is.null(vals$cur_som_type2)){vals$cur_som_type2<-"Numeric"}
    if(input$som_tab=="som_tab1"){

      req(input$som_type=='Supervised')
      div(
        div("Y Attribute:"),
        div( pickerInput(ns("som_type2"),NULL, choices=c("Numeric","Factors"), selected=vals$cur_som_type2, width="150px"))
      )
    }
  })
  observeEvent(input$selecfac,vals$selecfac<-input$selecfac)
  output$som_y_sel<-renderUI({
    if(is.null(input$selecfac)){vals$selecfac<-colnames(attr(vals$saved_data[[input$data_somY]],"factors"))}
    req(length(input$som_tab)>0)
    if(input$som_tab=="som_tab1"){
      req(input$som_type=='Supervised')
      req(input$som_type2=='Factors')
      div(class = "choosechannel",
          pickerInput(ns("selecfac"),NULL,
                      choices=vals$selecfac,
                      multiple = T,
                      selected=colnames(attr(vals$saved_data[[input$data_somY]],"factors")),
                      width="100px", inline=T))
    }
  })
  output$selecfac<-renderUI({
    column(12,
           checkboxGroupInput(ns("selecfac"), "Select the factors", choices=colnames(attr(vals$saved_data[[input$data_somY]],"factors"))))
  })
  output$som_partition<-renderUI({
    req(input$data_som)
    req(input$som_tab)
    if(is.null(vals$cur_partsom)){vals$cur_partsom<-1}
    if(input$som_tab=="som_tab1"){
      div(
        inline(div(
          div(span('Partition:', tiphelp("choose a factor from Datalist X as reference for data partioning"))),
          div(pickerInput(ns("som_test_pick"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_som]],"factors"))),selected=vals$cur_partsom,width="150px",))
        )),inline(uiOutput(ns("som_test_ref")))
      )
    }
  })
  output$som_test_ref<-renderUI({
    req(input$som_test_pick)
    req(input$som_test_pick!="None")
    req(input$data_som)
    if(is.null(vals$cur_testsom)){vals$cur_testsom<-1}
    req(input$som_test_pick%in%colnames(attr(vals$saved_data[[input$data_som]],"factors")))
    fac<-attr(vals$saved_data[[input$data_som]],"factors")[,input$som_test_pick]
    choices<-levels(fac)
    if(input$som_tab=="som_tab1"){
      div(
        div(span("Test reference", tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
        div(
          pickerInput(ns("som_test_ref"),NULL, choices=choices, selected=vals$cur_testsom,width="150px",)
        )
      )} else{NULL}
  })
  output$som_panels<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))

    div(

      tabsetPanel(id = ns("som_tab"),selected=vals$cursomtab,
                  tabPanel(
                    strong("1. Training"), value = "som_tab1",
                    uiOutput(ns("training_panel"))
                  ),
                  tabPanel(value = "som_tab2",
                           strong("2. Results"),
                           uiOutput(ns("results_panel"))),
                  tabPanel(value = "som_tab3",style="background: white",
                           strong("3. Predict"),
                           uiOutput(ns("predsom_panel")),
                           uiOutput(ns("predsom_tabs"))
                  )
      )
    )})
  output$som_topo<-renderUI({
    column(12,
           br(),
           strong("1.1. Set the grid",
                  actionLink(ns("somgridhelp"), tipify(icon("fas fa-question-circle"), "Click for more details"))),
           column(12,
                  fluidRow(
                    column(12,
                           splitLayout(cellWidths = c('10%','90%'),
                                       checkboxInput(ns("sugtopo"), NULL, value =tunesom$sugtopo
                                       ),
                                       p(style="margin-top: 9px",
                                         "suggested topology",
                                         tipify(actionLink(
                                           ns("sugtopohelp"), icon("fas fa-question-circle")
                                         ), "Click for more details")))),
                    column(12,
                           withSpinner(type=8,color="SeaGreen",uiOutput(ns("somgridtopo")))

                           ,
                           column(12,

                                  #span(class="finesom_btn",bsButton(ns("finetopo"),tipify(span("Fine tuning*"), "show all parameters available"), type="toggle", value=tunesom$finetopo)),
                                  uiOutput(ns("finetopo_out")))))))
  })
  output$training_panel<-renderUI({
    div(id = "train_tab0",    style = "background: white;",
        div(    style = "background: white;",
                div(
                  splitLayout(
                    uiOutput(ns("som_topo")),
                    div(br(),
                        strong("2.2. Set the training parameters",
                               actionLink(ns("supersomhelp"), tipify(
                                 icon("fas fa-question-circle"), "Click for more details"
                               ))),
                        div(style = "background: white;",
                            uiOutput(ns("somcontrol"))



                        )
                    )
                  )
                )
        )
    )
  })

  output$results_panel<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))


    validate(need(!is.null(vals$saved_data),"no data found"))
    div(style = "background: WhiteSmoke;",
        tabsetPanel(id=ns("som_res"),selected=vals$som_res,
                    tabPanel(
                      value= 'train_tab1',"3.1. Parameters",
                      column(12, style = "background: white; margin-top: 10px",
                             column(12,
                                    splitLayout(
                                      uiOutput(ns("som_errors")),
                                      div(
                                        p(popify(bsButton(ns("down_pcodes_results"), span(icon("fas fa-download"),icon("fas fa-table")),style = "button_active"),NULL,"download codebook results",options=list(container="body"))),
                                        p(popify(downloadButton(ns("down_kohonen_results"), icon("fas fa-braille"),style = "button_active"),NULL,"download kohonen object as rds file containing the object of class kohonen with components")))
                                    )
                             ),
                             column(12, style="margin-top: 10px",splitLayout(cellWidths = c("50%","10%"),div(p(strong("Training Parameters")),tableOutput(ns("train.summary")))))
                      )
                    ),
                    tabPanel("3.2. Changes",value = "train_tab2",
                             column(12,
                                    style = "background: white;",
                                    column(12,actionButton(ns("downp_pchanges"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12,plotOutput(ns("pchanges"))))
                    ),
                    tabPanel("3.3. Couting", value = "train_tab3",
                             column(12,style = "background: white;",
                                    column(12,actionButton(ns('downp_pcounts'),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12, plotOutput(ns("pcounts"))))),
                    tabPanel("3.4. BMUs", value = "train_tab5",
                             uiOutput(ns("plot_bmu"))
                    )

        )
    )
  })
  output$plot_bmu<-renderUI({

    req(length(getsom())>0)
    sidebarLayout(
      sidebarPanel(uiOutput(ns("bmu_plot_side"))),
      mainPanel(uiOutput(ns("bmu_plot")))
    )
  })

  output$bmu_plot_side<-renderUI({
    req(length(getsom())>0)
    # req(input$model_or_data=="som codebook")
    div(class="map_control_style",style="color: #05668D",
        tagList(module_ui_somplot(ns("som"))),
        uiOutput(ns("bmu_down_links")))
  })

  output$bmu_plot<-renderUI({
    req(length(getsom())>0)
    hc=NULL
    callModule(module_server_somplot,
               "som",
               vals=vals,
               data_target=input$data_som,
               som_model=input$som_models,
               background_type="uMatrix",
               property=NULL,
               hc=hc,
               df_symbol=df_symbol
    )

    #vals<-readRDS('vals.rds')

    # saveRDS(vals[[ns_som("somplot_args")]],"somplot_args.rds")


    #vals$som_som
    #valsk$k_means_results[[1]]
    # valsk$som_result
    renderPlot({
      #vals<-readRDS("savepoint.rds")
      # vals<-readRDS("savepoint_som.rds")
      # input<-readRDS("input_som.rds")
      args<-vals[[ns(ns_som("somplot_args"))]]
      req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp

      vals$bmus_plot<-do.call(bmu_plot,args)
      vals$bmus_plot
    })

  })

  getsom<-reactive({
    req(input$som_models)
    req(length(attr(getdata_som(),"som"))>0)
    m<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]
    req( class(m) == "kohonen")
    m
  })

  output$som_umatrix<-renderUI({
    column(12,style = "background: white;",
           column(12,actionButton(ns("downp_pmatrix"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
           column(12, plotOutput(ns("pUmatrix")))
    )
  })
  observeEvent(input$sompred_type,{
    vals$sompred_type<-input$sompred_type
  })
  output$predsom_panel<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))

    test<-attr(vals$som_results,"test_partition")

    column(12,style="background: white",
           splitLayout(cellWidths = c("30%",'40%','10%'),
                       if(test!="None"){
                         radioButtons(ns("sompred_type"),"New data (X):",choices=c("Partition","Datalist"), inline=T, selected=vals$sompred_type)
                       } else{ radioButtons(ns("sompred_type"),"New data (X):",choices=c("Datalist"),inline=T, selected="Datalist") },
                       uiOutput(ns('sompred_type_datalist'))


           )
    )
  })
  observeEvent(input$predsom_new,{
    vals$predsom_new<-input$predsom_new
  })
  output$sompred_type_datalist<-renderUI({
    req(input$sompred_type =='Datalist')
    div(selectInput(ns("predsom_new"),"Datalist",choices=names(vals$saved_data[getobs_somX()]), selected=vals$predsom_new))
  })
  output$predsom_var<-renderUI({
    selectInput(ns("predsom_var"),"variable",colnames(test_som()))
  })
  output$pred_som_results<-renderUI({

    #validate(need(res[[input$predsom_new]],"Incompatible variables"))

    column(12,style="background: white",
           uiOutput(ns("pred_som_results_control"))

    )
  })
  output$pred_som_results_control<-renderUI({
    som_pred<-pred_som()
    div(
      #renderPrint(som_pred),
      uiOutput(ns("som_pred_results"))
    )
  })
  output$trainsom<-renderUI({
    if(input$distmethod=="BrayCurtis"){
      validate(need(!anyNA(getdata_som()),"NA values not allowed with BrayCurtis distance"))
    }
    column(12,
           column(9,align = "center",
                  br(),
                  popify(actionButton(ns("trainSOM"), h4(icon("fas fa-braille"),"train SOM",
                                                         icon("fas fa-arrow-circle-right")), style = "background: #05668D; color: white"),"TrainSOM","Train the Numeric-Attribute from the selected Datalist")),
           column(3, align = "right", actionButton(ns("resetsom"), "reset")),

           column(12,
                  span(class="finesom_btn",
                       tipify(bsButton(ns("finesom"),"Fine tuning*", type="toggle", value=tunesom$finesom),"show all parameters available")
                  )),
           column(12,uiOutput(ns("finetuning_som")))
    )

  })
  output$finetuning_som<-renderUI({
    req(isTRUE(input$finesom))
    div(id="finesom_out",
        splitLayout(
          numericInput(ns("a1"),label = "a1",value = tunesom$a1,step = 0.01),
          numericInput(ns("a2"),label = "a2",value = tunesom$a2,step = 0.01),
          numericInput(ns("r1"),label = "r1",value = tunesom$r1),
          numericInput(ns("r2"),label = "r2",value = tunesom$r2,step = 0.01 )),
        splitLayout(selectInput(ns("mode"), "mode", choices = c("online","batch", "pbatch"), selected=tunesom$mode),
                    numericInput(ns("maxna"),"maxNA.fraction",value = tunesom$maxna,step = 0.01)))

  })
  output$somcontrol<-renderUI({

    div(
      div(
        div(br()),
        div(br()),
        splitLayout(
          style="margin-top: 10px",
          selectInput(ns("distmethod"),strong("dist.fcts",tipify(icon("fas fa-question-circle"),"Distance measure between each neuron and input data")),
                      choices = c("BrayCurtis","euclidean","sumofsquares","manhattan",
                                  "tanimoto"),selected=tunesom$distmethod),
          numericInput(ns("rlen"),strong("rlen",tipify(icon("fas fa-question-circle"),"The number of times the complete dataset will be presented to the network")),value = tunesom$rlen,min = 1,step = 1),
          numericInput(ns("seed"), strong("seed",tipify(icon("fas fa-question-circle"),"A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")), value =tunesom$seed, min=0, step=1)),
        uiOutput(ns("trainsom"))))

  })
  output$finetopo_out<-renderUI({
    #req(isTRUE(input$finetopo))
    splitLayout(id="finetopo_out",
                selectInput(ns("neighbourhood.fct"),label = "neighbourhood.fct",choices = c("bubble","gaussian"),selected=tunesom$neighbourhood.fct),
                selectInput(ns("toroidal"),label = "toroidal",choices = c(F, T), selected=tunesom$toroidal))

  })
  output$somgridtopo<-renderUI({topocontrol()})
  output$train.summary<-renderTable({
    train.summary()
  }, rownames = T, colnames = F,striped=T, spacing ="xs")
  output$nopredictions<-renderPrint({
    cat("no predictions in \n'",input$data_som,"' \n to overwritte")
  })
  output$nosommodel<-renderPrint({
    cat("no som model in \n'",input$data_som,"' \n to overwritte")
  })
  observeEvent(input$predsom_results,{
    vals$cur_predsom_results<-input$predsom_results
  })
  output$som_pred_results<-renderUI({
    if(attr(vals$som_results,"Method")=="Unsupervised"){
      choices<-c("Data predictions (X)", "Obs errors (X)","Var errors (X)","BMUs","Neuron predictions (X)","Quality measures")
      selected= vals$cur_predsom_results
    } else{
      if(attr(vals$som_results,"som_type2")=="Numeric"){
        choices<-c("Data predictions (X)","Data predictions (Y)","Obs errors (X)", "Obs errors (Y)","Var errors (X)", "Var errors (Y)","BMUs","Neuron predictions (X)","Neuron predictions (Y)","Quality measures")
        selected= vals$cur_predsom_results
      } else{
        choices<-c("Data predictions (X)","Data predictions (Y)","BMUs","Neuron predictions (X)" ,"Neuron predictions (Y)","Quality measures")
        selected= vals$cur_predsom_results
      }
    }
    column(12,
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 class="map_control_style",
                 style="color: #05668D",
                 div(
                   h5("+ Results:"),
                   column(12,radioButtons(ns("predsom_results"),NULL,choices, selected=selected))
                 ),
                 div(
                   tipify(
                     actionLink(
                       ns('down_som_pred_results'),span("+ Download",icon("fas fa-download")), style="button_active"
                     ),
                     "Download selected results"
                   )
                 ),
                 uiOutput(ns("savesom_pred"))
               )),
             mainPanel(
               uiOutput(ns("predsom_newY2")),
               uiOutput(ns("pred_som_results_out"))
             )

           )


    )
  })


  output$predsom_newY2<-renderUI({
    req(attr(vals$som_results,"Method")!="Unsupervised")
    req(input$predsom_results%in%c("Obs errors (Y)"))
    span("+ Y Datalist:",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values",
                                options=list(container="body")),
         inline(
           pickerInput(ns("predsom_newY2"),
                       NULL,names(vals$saved_data[getobs_som2()]),
                       inline=T)
         )
    )
  })
  output$savesom_pred<-renderUI({
    req(input$predsom_results=='Data predictions (X)'|input$predsom_results=='Data predictions (Y)'|input$predsom_results=='Obs errors (Y)'|input$predsom_results=='Obs errors (X)')
    div(
      tipify(
        actionLink(
          ns('tools_savesom_pred'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
        ),
        "Create a datalist  with the selected prediction"
      )
    )
  })
  output$pred_som_results_out<-renderUI({
    validate(need(!anyNA(newdata_som()),""))
    column(12,
           #renderPrint({correctsom()}),
           div(uiOutput(ns("corrected_predsom_warning"))),
           div(uiOutput(ns("dt_predsom_res_va")))
    )})
  output$dt_predsom_res_va<-renderUI({
    req(length(get_sompred_results())>0)
    validate(need(ncol(get_sompred_results())<1000,"Too big. download the table to view it"))
    div(
      div(uiOutput(ns("global_somX"))),
      div(uiOutput(ns("global_somY"))),
      div(uiOutput(ns("obsErr_som")))
    )


  })
  output$obsErr_som<-renderUI({
    div(
      tags$style('#DT_predsom_res td {padding: 0}'),
      inline(
        DT::dataTableOutput(ns("DT_predsom_res"))
      )

    )
  })
  output$global_somX<-renderUI({
    req(input$predsom_results=='Data predictions (X)')
    div(
      tags$style('#som_globalERR_X td {padding: 0}'),
      p(strong("Global (Test):")),
      inline(
        DT::dataTableOutput(ns("som_globalERR_X"))
      )
    )
  })
  output$global_somY<-renderUI({
    req(input$predsom_results=='Data predictions (Y)')
    div(
      tags$style('#som_globalERR_Y td {padding: 0}'),
      p(strong("Global (Test):")),
      inline(
        DT::dataTableOutput(ns("som_globalERR_Y"))
      )
    )
  })
  output$som_globalERR_X<-DT::renderDataTable({

    m<-vals$som_results
    predX<- correctsom()$predX
    observed<-newdata_som()
    res0<-data.frame(as.list(postResample(predX,observed)))
    vals$predsom_reults<-res0
    res0


  },options = list(pageLength = 20, info = FALSE,dom='t', autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  output$som_globalERR_Y<-DT::renderDataTable({

    m<-vals$som_results
    predX<- correctsom()$predY
    observed<-newdata_som()
    res0<-data.frame(postResample(predX,observed))
    vals$predsom_reults<-res0
    res0
  },options = list(pageLength = 20, info = FALSE,dom='t', autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  output$DT_predsom_res<-DT::renderDataTable({
    req(length(get_sompred_results())>0)
    req(ncol(get_sompred_results())<1000)
    vals$predsom_reults<-data.frame(get_sompred_results())
    vals$predsom_reults
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  output$corrected_predsom_warning<-renderUI({
    req(input$sompred_type)
    #req(length(attr(vals$som_results,"test"))>0)
    newx<-nrow(attr(correctsom(),"newx"))
    req(!is.null(newx))
    req(newx>0)
    req(input$predsom_results=='Data predictions (X)'|input$predsom_results=='Data predictions (Y)')
    div(nrow(attr(correctsom(),"newx")),actionLink(ns("showNA_predsom"),"samples"),"were allocated to units which no training data were mapped, leading to NA values in the predictions.To avoid the NA values, predictions for these observations were estimated using the neighboring units."
    )
  })

  output$pred_property<-renderUI({



    column(12,style="background: white",

           uiOutput(ns("predsom_var")),
           plotOutput(ns("predsom_property")))
  })
  output$predsom_property<-renderPlot({
    req( input$predsom_var)
    som_pred<-pred_som()
    plot(vals$som_results, type = "property", main = input$predsom_var, property = som_pred$unit.predictions[[1]][,input$predsom_var], palette.name=coolBlueHotRed,cex=0.5,shape="straight",keepMargins = TRUE)})

  getobs_somX<-reactive({
    datalist<-vals$saved_data
    m<-vals$som_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$data[[1]])
        sum(res)==ncol(m$data[[1]])
      })
    )
    names(res0[res0==T])
  })
  test_som<-reactive({
    pred_tab<-if(input$sompred_type=="Partition"){attr(vals$som_results,"test")}
    if(input$sompred_type=="Datalist"){
      pred_tab<-vals$saved_data[[input$predsom_new]]}
    pred_tab
  })
  checkpredsom<-reactive({
    req(input$predsom_new)
    check<-vals$saved_data[[input$data_som]]
    res<-res0<-lapply(vals$saved_data,function(x) match_col(x,check) )
    res<-names(which(unlist(res)==T))
    choices=names(unlist(c(res0[res0==T],res0[ res0==F])))
    #validate(need(isTRUE(res0[[input$predsom_new]]),paste("Variables from Datalist",input$predsom_new, "incompatible with the Training data", input$data_som)))
    choices
  })
  errors_somX_sp<-reactive({
    m<-vals$som_results
    predX<- correctsom()$predX

    observed<-newdata_som()

    rmse_res<- mse_res<-mape_res<-mae_res<-list()
    for(i in 1:ncol(observed))
    {
      mape_res[[i]]<-mape(observed[,i],predX[,i])
      mse_res[[i]]<-mse(observed[,i],predX[,i])
      rmse_res[[i]]<-rmse(observed[,i],predX[,i])
      mae_res[[i]]<-mae(observed[,i], predX[,i])
    }
    rmse<-data.frame(RMSE=do.call(c,rmse_res))
    mae<-data.frame(MAE=do.call(c,mae_res))
    mape<-data.frame(MAPE=do.call(c,mape_res))
    mse<-data.frame(MSE=do.call(c,mse_res))
    rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
    data.frame(mae=mae,mse=mse,rmse=rmse)

  })
  errors_somY_sp<-reactive({
    m<-vals$som_results
    predY<- correctsom()$predY

    observed<-newdata_somY()
    rmse_res<- mse_res<-mape_res<-mae_res<-list()
    for(i in 1:ncol(observed))
    {
      mape_res[[i]]<-mape(observed[,i],predY[,i])
      mse_res[[i]]<-mse(observed[,i],predY[,i])
      rmse_res[[i]]<-rmse(observed[,i],predY[,i])
      mae_res[[i]]<-mae(observed[,i], predY[,i])
    }
    rmse<-data.frame(RMSE=do.call(c,rmse_res))
    mae<-data.frame(MAE=do.call(c,mae_res))
    mape<-data.frame(MAPE=do.call(c,mape_res))
    mse<-data.frame(MSE=do.call(c,mse_res))
    rownames(mape)<- rownames(mse)<-rownames(rmse)<-  rownames(mae)<-colnames(observed)
    data.frame(mae=mae,mse=mse,rmse=rmse)

  })
  errors_somX<-reactive({
    m<-vals$som_results
    predX<- correctsom()$predX

    observed<-newdata_som()

    res0<-data.frame(get_unit_errors(data.frame(predX),observed))
    res0

  })
  errors_somY<-reactive({
    m<-vals$som_results
    predX<- correctsom()$predY

    observed<-newdata_somY()
    res0<-data.frame(get_unit_errors(data.frame(predX),observed))
    res0

  })
  newdata_somY<-reactive({
    newdata<-NULL
    if(length(input$sompred_type)>0){
      if(length(input$predsom_newY2)>0){
        m<-vals$som_results
        newdata= if(input$sompred_type=="Partition"){
          attr(m,"test")} else{
            vals$saved_data[[input$predsom_newY2]]
          }

      }
    }
    newdata

  })
  getobs_som2<-reactive({
    datalist<-vals$saved_data
    m<-vals$som_results


    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$data[[2]])
        sum(res)==ncol(m$data[[2]])
      })
    )
    names(res0[res0==T])
  })
  correctsom<-reactive({
    som_pred<-pred_som()
    m<-vals$som_results
    newdata=as.matrix(newdata_som())
    pred_som<-predict(m,newdata=newdata, whatmap = 1)

    res<-get_correct_predsom(m,pred_som,newdata)
    res
  })
  get_sompred_results<-reactive({
    req(input$predsom_results)
    som_pred<-pred_som()
    m<-vals$som_results
    obs=attr(m,"sup_test")
    m<-vals$som_results
    newdata<-newdata_som()
    newdataY<-newdata_somY()
    som_pred<-pred_som()
    predsom_results<-input$predsom_results
    #saveRDS(predsom_results,"predsom_results.rds")
    #saveRDS(m,"m.rds")
    #saveRDS(newdata,"newdata.rds")
    #saveRDS(newdataY,"newdataY.rds")
    #saveRDS(som_pred,"som_pred.rds")

    #som_pred<-readRDS("som_pred.rds")

    #m<-readRDS("m.rds")

    vals$predsom_reults<-data.frame(get_somERR(predsom_results,m,newdata,newdataY,som_pred))
    vals$predsom_reults

  })
  newdata_som<-reactive({
    req(input$sompred_type)
    m<-vals$som_results
    newdata= if(input$sompred_type=="Partition"){
      attr(m,"test")} else{
        vals$saved_data[[input$predsom_new]]
      }
    newdata
  })
  pred_som<-reactive({
    validate(need(!anyNA(newdata_som()),"NAs not allowed in the prediction Datalist"))
    m<-vals$som_results
    som_pred<-predict(m,newdata = as.matrix(newdata_som()), whatmap = 1)
    #names(som_pred$predictions)<-c("X","Y")
    #names(som_pred$unit.classif)<-rownames(som_pred$predictions[[1]])
    attr(som_pred,"coords")<-attr(m,"coords")
    som_pred
  })
  choices_som_sup<-reactive({
    data<-vals$saved_data[[input$data_somY]]
    res<-switch (input$som_type2,
                 'Factors' = colnames(attr(data,"factors")),
                 'Numeric'=colnames(data))
    res
  })
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


  get_sup_som_test<-reactive({
    data = getdata_som()
    data_Y<-vals$saved_data[[input$data_somY]]
    factors<-if(input$som_type2=="Factors"){
      attr(data_Y,"factors")[,input$selecfac, drop=F]} else{data_Y}
    res<-factors[rownames(attr(data,"test_data")),]

    res


  })
  output$som_errors<-renderUI({
    res<-errors_som(vals$som_results)
    res_names<-rownames(res)
    fluidRow(
      strong("Quality measures:"),
      p(pophelp("Quantization error","Average squared distance between the data points and the map prototypes to which they are mapped. Lower is better.",placement="right"),em("Quantization error:"),strong(res[1,1])),
      p(pophelp("Percentage of explained variance","Similar to other clustering methods, the share of total variance that is explained by the clustering (equal to 1 minus the ratio of quantization error to total variance). Higher is better.",placement="right"),em("Percentage of explained variance:"),strong(res[2,1])),
      p(pophelp("Topographic error","Measures how well the topographic structure of the data is preserved on the map. It is computed as the share of observations for which the best-matching node is not a neighbor of the second-best matching node on the map. Lower is better: 0 indicates excellent topographic representation (all best and second-best matching nodes are neighbors), 1 is the maximum error (best and second-best nodes are never neighbors).",placement="right"),em("Topographic error:"),strong(res[3,1])),
      p(pophelp("Kaski-Lagus error","Combines aspects of the quantization and topographic error. It is the sum of the mean distance between points and their best-matching prototypes, and of the mean geodesic distance (pairwise prototype distances following the SOM grid) between the points and their second-best matching prototype.",placement="right"),em("Kaski-Lagus error:"),strong(res[4,1])),
      p(pophelp("Neuron Utilization","The percentage of neurons that are not BMU of any observation",placement="right"),em("Neuron Utilization:"),strong(res[5,1] ))
    )
  })
  output$down_kohonen_results <- {
    downloadHandler(
      filename = function() {
        paste0("Kohonen","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$som_results,file)
      })
  }
  get_supsom_list<-reactive({
    factors<-attr(vals$saved_data[[input$data_somY]],'factors')[rownames(getdata_som()),input$selecfac, drop=F]
    re<-lapply(factors,function(x) classvec2classmat(x))
    res<-do.call(cbind,re)
    names(re)<-colnames(factors)
    attr(res,"faclist")<-re
    res
  })

  bmu_points<-reactive({
    req(input$bmu_dotlabel)
    if(input$bmu_dotlabel == 'symbols'){ T} else {F}
  })

  BMUs<-reactive({
    req(input$npic)
    req(input$vfm_type)
    req(length(input$varfacmap_action)>0)
    req(input$bmu_bgpalette)
    req(input$bmu_facpalette)
    req(input$bmu_border_grid)
    req(input$bmu_var_color)
    if (isTRUE(input$varfacmap_action)) {
      npic=as.numeric(input$npic)} else{
        npic = 0
      }
    p <-pcorr(
      m=vals$som_results,
      npic = npic,
      indicate = input$vfm_type,
      pch = as.numeric(input$bmu_symbol),
      labels.ind = bmu_factors_reac(),
      cex =as.numeric(input$bmu_symbol_size),
      bg_palette = as.character(input$bmu_bgpalette),
      factor.pal=as.character(input$bmu_facpalette),
      points=bmu_points(),
      cex.var=input$bmu_cexvar,
      ncol=input$bmu_ncol,
      insetx=input$bmu_insertx,
      insety=input$bmu_inserty,
      alpha.legend=input$bmu_leg_transp,
      alpha_bg=input$bmu_bg_transp,
      border=getcolhabs(vals$newcolhabs,input$bmu_border_grid,1),

      col.text= getcolhabs(vals$newcolhabs,input$bmu_var_color,1),
      newcolhabs=vals$newcolhabs,

    )
    vals$bmus_plot<-recordPlot()
    p



  })
  bmu_symbol.reac <- reactive({
    if (input$topo == "hexagonal") {
      16
    } else if (input$topo == 'rectangular') {
      15
    }
  })

  topo.reactive <- reactive({

    df=data.frame(na.omit(getdata_som()))
    N<-nrow(getdata_som())
    SIZE<-sqrt(5*sqrt(N))
    L<- floor(SIZE)
    if(!nrow(df)>0){
      c(L*L,L,L)
    } else{
      topology(getdata_som(), dist = input$distmethod)
    }
  })
  getdata_som<-reactive({


    req(input$data_som)
    data_o<-data<-vals$saved_data[[input$data_som]]
    factors<-attr(data,"factors")
    #req(input$som_test_pick)
    if(length(input$som_test_pick)>0){
      if(input$som_test_pick!="None"){
        req(input$som_test_ref)
        req(input$som_test_pick%in%colnames(factors))
        fac<-factors[,input$som_test_pick]
        pic<-which(fac==input$som_test_ref)
        factors<-factors[pic,, drop=F]
        trains<-which(!rownames(data)%in%rownames(factors))
        data<-data[trains,]
        attr(data,"test_data")<-data_o[-trains,]
      }
    }

    validate(need(length(data)>0,"no data found"))
    attr(data,"test_data")<-data

    data
  })
  predsom.reactive<-reactive({
    #checkpredsom()
    if(is.null(attr(vals$som_results,"supervisor"))){
      tabsetPanel(id=ns("predsom_tab"),selected=vals$predsom_tab,
                  tabPanel(
                    strong("3.1. Results"),style="background: white",
                    value = "predsom_tab01",
                    uiOutput(ns("pred_som_results"))
                  ),

                  tabPanel(
                    strong("3.2. BMUs"),style="background: white",
                    value = "predsom_tab03",
                    uiOutput(ns("pred_bmu"))
                  ),
                  tabPanel(
                    strong("3.3. Property"),style="background: white",
                    value = "predsom_tab04",
                    uiOutput(ns("pred_property")))


      )
    } else{
      tabsetPanel(id=ns("predsom_tab"),
                  tabPanel(
                    strong("3.1. Results"),style="background: white",
                    value = "predsom_tab01",
                    uiOutput(ns("pred_som_results"))
                  ),

                  tabPanel(
                    strong("3.2. BMUs"),style="background: white",
                    value = "predsom_tab03",
                    uiOutput(ns("pred_bmu"))
                  ),
                  tabPanel(
                    strong("3.3. Property"),style="background: white",
                    value = "predsom_tab04",
                    uiOutput(ns("pred_property")))
                  ,
                  tabPanel(
                    strong("3.4. Confusion Matrix"),
                    value = "predsom_tab05",
                    style = "background: Snow;",
                    uiOutput(ns("predsom_cm"))
                  )
      )
    }
  })
  getobs_som<-reactive({
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x) attr(x,"factors"))

    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)%in%names(pred_som()$predictions[-1]))

      })
    )
    names(res0[res0==T])
  })
  get_predsom_cm<-reactive({
    factors<-attr(vals$saved_data[[input$predsom_newY]],"factors")
    obs<-if(input$sompred_type=="Datalist"){
      factors[,input$predsom_y]
    } else{attr(vals$som_results,"sup_test")[,input$predsom_y]}
    pred<- pred_som()$prediction[[input$predsom_y]]
    pred<-factor(pred, levels=levels(obs))
    conf<-table(obs,pred)
    conf
  })
  bmu_pred_points<-reactive({
    req(input$bmu_p_dotlabel)
    if(input$bmu_p_dotlabel == 'symbols'){ T} else {F}
  })
  bmu_p_factors_reac<-reactive({
    factors<-attr(vals$saved_data[[input$data_som]],"factors","factors")
    m<-vals$som_results
    if (length(input$bmu_p_factors)>0) {
      c(factors[rownames(vals$saved_data[[input$data_som]],"factors"), input$bmu_p_factors],
        if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), input$bmu_p_factors]})
    } else{c(factors[rownames(vals$saved_data[[input$data_som]],"factors"),1],
             if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), ]})}
  })
  BMUs_pred<-reactive({
    req(input$bmu_p_bg_transp)
    req(input$bmu_p_symbol)
    req(input$bmu_p_bgpalette)
    req(input$bmu_p_training)
    req(input$bmu_p_border_grid)
    req(input$bmu_p_test)
    p<-pcorr(
      m=vals$som_results,
      npic = 0,
      pch = as.numeric(input$bmu_p_symbol),
      labels.ind = bmu_p_factors_reac(),
      cex =as.numeric(input$bmu_p_symbol_size),
      bg_palette = as.character(input$bmu_p_bgpalette),
      factor.pal=as.character(input$bmu_p_training),
      points=bmu_pred_points(),
      legend=F,
      predict=pred_som(),
      alpha_bg=input$bmu_p_bg_transp,
      border=getcolhabs(vals$newcolhabs,input$bmu_p_border_grid,1),
      pred_col=as.character(input$bmu_p_test),
      newcolhabs=vals$newcolhabs
    )
    vals$bmus_pred_plot<-recordPlot()
    p
  })

  output$xdim_topo<-renderUI({
    numericInput(ns("xdim"),"xdim",value = tunesom$dim[[1]],min = 0,step = 1)
  })
  output$ydim_topo<-renderUI({
    numericInput(ns("ydim"),"ydim",value = tunesom$dim[[2]],min = 0,step = 1)
  })
  topocontrol<-reactive({
    fluidRow(id = "topocontrol",
             splitLayout(cellWidths = c('30%','30%','40%'),
                         uiOutput(ns('xdim_topo')),
                         uiOutput(ns('ydim_topo')),
                         selectInput(ns("topo"),"topo",choices = c("hexagonal", "rectangular"),selected=tunesom$topo)),
             column(10, plotOutput(ns("showgrid"), height = "150px")),
             column(2, align = "right", actionButton(ns("resettopo"), "reset"))

    )
  })
  toroidal<-reactive({
    switch (tunesom$toroidal,
            'TRUE' = TRUE,
            "FALSE" = FALSE)
  })

  bmu_factors_reac<-reactive({
    factors<-attr(vals$saved_data[[input$data_som]],"factors","factors")
    if (length(input$bmu_factors)>0) {
      factors[rownames(vals$saved_data[[input$data_som]],"factors"), input$bmu_factors]
    } else{factors[,1]}
  })

  train.summary<-reactive({
    m<-vals$som_results
    traindata<-data.frame(m$data[[1]])
    mean = round(mean(unlist(traindata)), 2)
    n.obs = nrow(traindata)
    n.variables = ncol(traindata)
    summ<-m$grid[-1]
    summ$neighbourhood.fct<-as.character(summ$neighbourhood.fct)
    summ<-do.call(rbind, summ)
    mode<-attr(m,"mode")
    alpha = paste0(m$alpha, collapse = "; ")
    radius = paste0(round(m$radius, 3), collapse = "; ")
    # user.weights = m$user.weights
    maxNA.fraction = m$maxNA.fraction
    dist.fcts = m$dist.fcts
    if(length(dist.fcts)>1){
      dist.fcts<-paste0(dist.fcts[1],dist.fcts[2], sep="/")
    }


    Parameters<-
      rbind(
        n.obs,
        n.variables,
        summ,
        alpha,
        radius,
        #user.weights,
        maxNA.fraction,
        dist.fcts,
        mode
      )
    data.frame(Parameters)
  })

  observeEvent(input$som_results,{
    if(attr(vals$som_results,"Method")=="Unsupervised"){
      vals$cur_predsom_results<-vals$som_results
    } else{
      if(attr(vals$som_results,"som_type2")=="Numeric"){
        vals$cur_predsom_results<-vals$som_results
      } else{
        cur_predsom_res3$df<-vals$som_results
      }

    }
  })
  observeEvent(input$showNA_predsom,{

  })
  observeEvent(input$sugtopo,{
    tunesom$sugtopo<-input$sugtopo
  })
  observeEvent(input$topo,{
    tunesom$topo<-input$topo
  })
  observeEvent(input$maxna,{
    tunesom$maxna<-input$maxna
  })
  observeEvent(input$mode,{
    tunesom$mode<-input$mode
  })
  observeEvent(input$r2,{
    tunesom$r2<-input$r2
  })
  observeEvent(input$r1,{
    tunesom$r1<-input$r1
  })
  observeEvent(input$a2,{
    tunesom$a2<-input$a2
  })
  observeEvent(input$a1,{
    tunesom$a1<-input$a1
  })
  observeEvent(input$neighbourhood.fct,{
    tunesom$neighbourhood.fct<-input$neighbourhood.fct
  })
  observeEvent(input$toroidal,{
    tunesom$toroidal<-input$toroidal
  })
  observeEvent(input$distmethod,{
    tunesom$distmethod<-input$distmethod
  })
  observeEvent(input$xdim,{
    tunesom$dim[[1]]<-input$xdim
  })
  observeEvent(input$ydim,{
    tunesom$dim[[2]]<-input$ydim
  })
  observeEvent(input$seed,{
    tunesom$seed<-input$seed
  })
  observeEvent(input$rlen,{
    tunesom$rlen<-input$rlen
  })
  observeEvent(input$data_som,{
    if(anyNA(getdata_som())){
      updateSelectInput(session,"distmethod")
    }
  })
  observeEvent(input$finetopo,{
    tunesom$finetopo<-input$finetopo
  })
  observe({
    req(input$xdim)
    req(input$ydim)
    tunesom$r1<-as.vector(quantile(unit.distances(
      kohonen::somgrid(input$xdim,input$ydim,topo = input$topo,toroidal = toroidal(), neighbourhood.fct=input$neighbourhood.fct)), 2 / 3))
  })

  observeEvent(input$som_type2,{
    vals$cur_som_type2<-input$som_type2
  })
  observeEvent(input$som_test_pick,{
    vals$cur_partsom<-input$som_test_pick
  })
  observeEvent(input$som_test_ref,{
    vals$cur_testsom<-input$som_test_ref
  })
  observeEvent(input$data_somY,{
    vals$cur_datasomY<-input$data_somY
  })
  observeEvent(input$data_som,{
    vals$cur_data<-input$data_som
  })
  observeEvent(input$data_som,{
    vals$cur_data<-input$data_som
  })
  observeEvent(input$som_part,{
    updateTabsetPanel(session,"som_tab","som_tab1")
  })

  observeEvent(input$finesom,{
    tunesom$finesom<-input$finesom
  })
  observeEvent(input$som_res,{
    vals$som_res<-input$som_res
  })



  observeEvent(input$bmu_legend,{
    vals$bmuleg<-if(isFALSE(vals$bmuleg)){T} else if(isTRUE(vals$bmuleg)){F}

  })

  observeEvent(  list(getdata_som(),input$sugtopo),{

    req(input$sugtopo)
    if(isTRUE(input$sugtopo)){
      dim = topo.reactive()
      tunesom$dim<-c(dim[[2]],dim[[3]])
    }
  })
  observeEvent(list(input$xdim, input$ydim), {
    if (length( names(vals$saved_data)) > 0) {
      dim = topo.reactive()
      ydim <- dim[[3]]
      xdim <- dim[[2]]
      if (input$xdim != xdim|input$ydim != ydim) {
        updateCheckboxInput(session, "sugtopo", NULL, FALSE)
      } else{
        updateCheckboxInput(session, "sugtopo", NULL, TRUE)
      }

    }
  })




  observeEvent(input$predsom_tab,
               vals$predsom_tab<-input$predsom_tab)
  observeEvent(input$som_type,{
    vals$cur_somtype<-input$som_type
  })
  observeEvent(input$som_model_delete,{
    attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]<-NULL
  })

  observeEvent(input$som_tab,{
    vals$cursomtab<-input$som_tab
  })



  observeEvent(input$resetsom, {
    tunesom$rlen=500
    tunesom$distmethod="BrayCurtis"
    tunesom$seed=1
    tunesom$a1=0.05
    tunesom$a2=0.01
    tunesom$r1=0
    tunesom$r2=0
    tunesom$mode="online"
    tunesom$maxna=0.001
  })
  observeEvent(input$resettopo, {
    tunesom$sugtopo=T

    tunesom$toroidal="FALSE"
    tunesom$topo="hexagonal"
    tunesom$neighbourhood.fct='bubble'
  })

  observeEvent(input$trainSOM,{
    if(is.null(attr(vals$saved_data[[input$data_som]],"som"))){
      attr(vals$saved_data[[input$data_som]],"som")<-list()
    }
    data = data.frame(getdata_som())
    data_o<-data.frame(vals$saved_data[[input$data_som]])
    test<-which(!rownames(data_o)%in%rownames(data))
    withProgress(
      message = "Running som... the time taken will depend on the size of the data and the training.",
      min = 1,
      max = 1,
      {
        if(input$som_type=='Unsupervised')
        {
          if(is.na(input$seed)==F){set.seed(input$seed)}
          m<-try(
            supersom(
              list(X=as.matrix(data)),
              grid = kohonen::somgrid(
                input$xdim,
                input$ydim,
                topo = input$topo,
                toroidal = toroidal(),
                neighbourhood.fct=tunesom$neighbourhood.fct
              ),
              rlen = input$rlen,
              dist.fcts = input$distmethod,
              alpha = c(tunesom$a1, tunesom$a2),
              radius = c(tunesom$r1, tunesom$r2),
              mode = tunesom$mode,
              maxNA.fraction = tunesom$maxna
            )
          )

        } else {
          sup_test<-get_sup_som_test()
          if(is.na(input$seed)==F){set.seed(input$seed)}
          m<-try(xyf(
            X=as.matrix(data),
            Y=if(input$som_type2=="Numeric") {as.matrix(vals$saved_data[[input$data_somY]][rownames(data),])} else {
              get_supsom_list()
            },
            #whatmap = 1,
            grid = kohonen::somgrid(
              input$xdim,
              input$ydim,
              topo = input$topo,
              toroidal = toroidal(),
              neighbourhood.fct=tunesom$neighbourhood.fct
            ),
            rlen = input$rlen,
            dist.fcts = c(input$distmethod,
                          if(input$som_type2=="Numeric") {input$distmethod} else {'tanimoto'}),
            alpha = c(tunesom$a1, tunesom$a2),
            radius = c(tunesom$r1, tunesom$r2),
            mode = tunesom$mode,
            maxNA.fraction = tunesom$maxna
          ))
          attr(m,"sup_test")<-sup_test
          attr(m,"supervisor")<-input$data_somY

        }
        if (class(m) != "kohonen")
        {
          validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
        }
        attr(m,'mode')<-input$mode

        names(m$unit.classif)<-rownames(m$data[[1]])

        attr(m,"test_partition")<-"None"

        attr(m,"Method")<-if(input$som_type=="Unsupervised"){"Unsupervised"} else{
          input$data_somY
        }


        attr(m,"Datalist")<-input$data_som

        attr(m,"som_type2")<-input$som_type2
        if(input$som_type=="Supervised"){
          if(input$som_type2=="Factors"){attr(m,"faclist")<-attr(get_supsom_list(),"faclist")}}
        attr(m,"coords")<-attr(data,"coords")
        vals$som_unsaved<-m

        attr(vals$saved_data[[input$data_som]],"som")[["new som (unsaved)"]]<-m

          m


      }
    )

  })



  observeEvent(input$trainSOM, {
    vals$cur_train<-"new som (unsaved)"
    updateTabsetPanel(session, "som_tab", "som_tab2")
    updateTabsetPanel(session, "som_tab", "train_tab2")
    updateTabsetPanel(session, "som_res", "train_tab1")
    # updateSelectInput(session,"som_models",                      choices=c("new som (unsaved)", names(attr(vals$saved_data[[input$data_som]],"som"))))
   #updateSelectInput(session,"som_models",selected="new som (unsaved)")
  })
  observeEvent(input$resettopo, {
    output$somgridtopo<-renderUI({topocontrol()})})
  observe({

    req(input$som_models)
    if(input$som_models=="new som (unsaved)"){
      vals$som_results<-vals$som_unsaved
    } else{
      vals$som_results<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]}
  })
  observeEvent(input$data_som,{
    updateTabsetPanel(session,'som_tab', 'som_tab1')})
  observeEvent(input$som_test_ref,{
    updateTabsetPanel(session,'som_tab', 'som_tab1')})

  observeEvent(input$tools_savesom,{
    if(input$tools_savesom %% 2){
      vals$hand_save<-"Save new som in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_som, style="color:gray"),strong("::"), em("Som-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-em("Both 'create' and 'overwrite' options add the resulting BMU to the 'Factor-Attribute' of the corresponding Datalist."
      )
      showModal(
        module_som())}
  })
  observeEvent(input$tools_savesom_pred,{

    if(input$predsom_results=="Data predictions (X)"|input$predsom_results=="Data predictions (Y)")  {

      vals$hand_save<-"Save som predictions"
      vals$hand_save2<-NULL
      mess<-"Creates a Datalist with the SOM predictions"
      if(attr(vals$som_results,"Method")!="Unsupervised"){
        if(attr(vals$som_results,"som_type2")=="Numeric")
        {
          mess<-paste(mess,"The defined name will be automatically followed by the '_X' tag. Y (numerical) predictions will also be automatically saved, with the '_Y' tag instead" )
        } else{
          mess<-paste(mess,"The defined name will be automatically followed by the '_X' tag. The Y predictions (factors) will be assigned to the Factor-Attribute of the created datalist" )
        }
      } else{
        mess<-paste(mess,"The defined name will be automatically followed by the '_X' tag")
      }
      vals$hand_save3<-em(mess)}

    if(input$predsom_results=="Obs errors (X)"){
      vals$hand_save<-"Save errors from som predictions (X)"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
    }
    if(input$predsom_results=="Obs errors (Y)"){
      vals$hand_save<-"Save errors from som predictions (Y)"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
    }
    showModal(module_som())
  })


  observeEvent(input$downp_pchanges,{
    vals$hand_plot<-"Training plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_pcounts,{
    vals$hand_plot<-"Couting plot"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_pmatrix,{
    vals$hand_plot<-"uMatrix"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_bmu,{
    vals$hand_plot<-"BMUs"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_bmu_pred,{
    vals$hand_plot<-"BMUs predictions"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_pproperty,{
    vals$hand_plot<-"property plot"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(input$downp_confsom,{
    vals$hand_plot<-"Confusion Matrix SOM"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(input$down_pcorr_results,{
    vals$hand_down<-"pcorr"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  observeEvent(input$down_som_pred_results,{
    vals$hand_down<-"som predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)


  })
  observeEvent(input$down_pcodes_results,{
    vals$hand_down<-"pcodes"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })


  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Save new som in"=bag_somname(),
      "Save som predictions"=predsom_name(),
      "Save errors from som predictions (X)"=bag_sompred_eX(),
      "Save errors from som predictions (Y)"=bag_sompred_eY()
    )})
  paste("som",(NULL+1))
  bag_somname<-reactive({


    paste0("SOM (",length((attr(vals$saved_data[[input$data_som]],"som"))),")")
  })

  bag_somname_obd<-reactive({



    soms<-names(attr(vals$saved_data[[input$data_som]],"som"))
    if(is.null(soms)){
      bag=1
    } else{ bag=length(soms)+1}


    name0<-"SOM"
    name1<-paste0(name0," (",bag,")")
    if(name1%in%soms)
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%soms) break
      }
    }
    paste0(name0," (",bag,")")


  })
  predsom_name<-reactive({
    zero<-"Som_Pred"
    if(input$som_models!="new som (unsaved)"){
      zero=paste0(input$som_models,"_Pred")
    }
    name<-if(!length(saved_sompred$df)>0){
      zero
    } else{
      paste(zero,length(saved_sompred$df))
    }
    name
  })
  bag_sompred_eX<-reactive({
    bag<-1
    name0<-paste("Som_pred_errorsX")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Som_pred_errorsX",bag)

  })
  bag_sompred_eY<-reactive({
    bag<-1
    name0<-paste("Som_pred_errorsY")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Som_pred_errorsY",bag)

  })
  module_som <- function() {
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
  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    if(vals$hand_save=='Save new som in'){choices<-names(attr(vals$saved_data[[input$data_som]],'som'))}
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
  observeEvent( input$data_confirm,{
    req(!is.null(vals$hand_save))
    switch(
      vals$hand_save,
      "Save new som in"=savesom(),
      "Save som predictions"=savesompred(),
      "Save errors from som predictions (X)"=datalist_som_errorsX(),
      "Save errors from som predictions (Y)"=datalist_som_errorsY()
    )
    removeModal()

  })
  savesompred<-reactive({
    data=vals$saved_data[[input$data_som]]
    som_pred<-pred_som()
    predX<- correctsom()$predX
    predX<-data_migrate(data,predX,input$newdatalist)
    factors<-attr(data,"factors")[rownames(predX),]
    coords<-attr(som_pred,"coords")[rownames(predX),]
    attr(predX,"factors")<-factors
    attr(predX,"coords")<-coords
    newsaveX<-paste0(input$newdatalist,"_X")
    saved_sompred$df[[newsaveX]]<-1
    vals$saved_data[[newsaveX]]<-predX

    if(input$hand_save=="create"){
      if(attr(vals$som_results,"Method")!="Unsupervised"){
        predY<- correctsom()$predY
        if(attr(vals$som_results,"som_type2")=="Numeric"){
          newsave<-paste(input$newdatalist,"_Y")
          predY<-data_migrate(data,predY,newsave)
          vals$saved_data[[newsave]]<-predY
        } else{

          attr(vals$saved_data[[newsaveX]],"factors")<-predY

        }
      }
    } else {
      pred_data<-data.frame(pred_data)
      pred_data<-data_migrate(data,pred_data,input$over_datalist)
      factors<-attr(pred_data,"factors")[rownames(pred_data),]
      factors[,input$over_datalist]<-pred_factors
      coords<-attr(test_data,"coords")[rownames(pred_data),]
      attr(pred_data,"factors")<-factors
      attr(pred_data,"coords")<-coords
      attr(pred_data,"transf")<-attr(data,"transf")
      vals$saved_data[[input$over_datalist]]<-pred_data}
    updateTabsetPanel(session,'som_tab','som_tab3')
  })
  savesom<-reactive({
    curtab<-vals$cursomtab
    data<-getdata_som()
    temp<-vals$som_results
    bmu<-temp$unit.classif
    names(bmu)<-rownames(temp$data[[1]])
    bmu<-as.factor(bmu)
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_som]],"som")[input$newdatalist]<-c(temp,attr(vals$saved_data[[input$data_som]],"som")[[input$newdatalist]])
      attr(vals$saved_data[[input$data_som]],"factors")[names(bmu),paste0('bmu_',input$newdatalist)]<-bmu
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_som]],"som")[input$over_datalist]<-temp
      attr(vals$saved_data[[input$data_som]],"factors")[names(bmu),input$over_datalist]<-bmu
      cur<-input$over_datalist

    }
    vals$cur_train<-cur

    updatePickerInput(session,
                      "som_models",
                      choices=c(names(attr(vals$saved_data[[vals$cur_train]],"som"))),
                      selected=cur)
    delay(500,{updateTabsetPanel(session, "som_tab", curtab)
      updateTabsetPanel(session, "som_res", vals$som_res)})

    attr(vals$saved_data[[input$data_som]],"som")[['new som (unsaved)']]<-NULL

  })

  datalist_som_errorsX<-reactive({
    temp<-data.frame(get_sompred_results())
    if(input$hand_save=="create") {
      temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })
  datalist_som_errorsY<-reactive({
    temp<-data.frame(get_sompred_results())
    if(input$hand_save=="create") {
      temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })



}
