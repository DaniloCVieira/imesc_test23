#' @export
#'
module_ui_somplot <- function(id){
  ns <- NS(id)
  div(
    class='choosechannel',

    uiOutput(ns("side_somplot"))
  )
}

#' @export
#'
module_server_somplot <- function(input, output, session,vals,data_target,som_model,background_type="hc",property=NULL,hc=NULL,
                                  map_newdata=F,df_symbol,df_colors) {
  ns <- session$ns


  somplot_args<-reactiveValues(df=NULL)
  somval<-reactiveValues(hc=hc)
  data<-vals$saved_data[[data_target]]
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })

  get_model <- reactive({

    if(isFALSE(map_newdata)){
      return(attr(data,"som")[[as.character(som_model)]])
    } else{
      #input<-list()
      #input$newdata<-choices[1]
      #m0<-m<-attr(vals$saved_data$`fisico_quimico(i)_scaled`,"som")[[1]]
      req(input$newdata)
      m<-attr(data,"som")[[as.character(som_model)]]
      newdata=as.matrix(vals$saved_data[[input$newdata]])
      som_map<-kohonen:::map(m,newdata)
      names(som_map$unit.classif)<-rownames(newdata)
      pred<-predict(m,newdata)
      m$unit.classif
      mnew<-m
      mnew$distances<-som_map$distances
      mnew$unit.classif<-som_map$unit.classif
      mnew$data<-list(newdata)
      unit.predictions<-do.call(cbind,pred$unit.predictions)



      mnew$codes<-list(unit.predictions)
      somplot_args$som_model_pred<-mnew
      m<-mnew
      if(isTRUE(map_newdata)) {
        somval$hc<-factor(cutsom(m=m,
                                 groups=vals$saved_kcustom,
                                 method.hc=vals$method.hc0,
                                 palette=NULL,
                                 newcolhabs=vals$newcolhabs,
                                 dataX=NULL,
                                 weighted = T)$som.hc)
      }
      return(m)
    }


  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  get_choices_pal<-reactive({

    if(background_type!="hc") {
      req(length(input$plus_umatrix)>0)
      req(length(input$plus_property)>0)
      title="+ Background palette"
      if(sum(input$plus_umatrix,input$plus_property)==0){
        somplot_args$somplot_bg<-"gray"
          choices=getsolid_col()
      } else {

        somplot_args$somplot_bg<-"viridis"
        choices=getgrad_col()

      }

    } else {
      title="+ Cluster palette"
      somplot_args$somplot_bg<-"turbo"
      choices=getgrad_col()

    }
    attr(choices,"title")<-title
    choices
  })



  output$hc_palette<-renderUI({
    choices<-get_choices_pal()
    title<-attr(choices,"title")
    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
        div(class="palette",
            title,
            pickerInput(inputId = ns("bg_palette"),
                        label =NULL,
                        choices =  vals$colors_img$val[choices],
                        selected=somplot_args$somplot_bg,
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] ),
                        options=list(container="body"), width="100px")),
        uiOutput(ns("pcodes_bgalpha"))

    )
  })

  output$side_somplot<-renderUI({

    if(is.null(somplot_args$pclus_addpoints)){
      somplot_args$pclus_addpoints<-T
    }
    if(is.null(somplot_args$pclus_text_inputs)){
      somplot_args$pclus_text_inputs<-F
    }
    div(class="map_control_style",style="color: #05668D",
        div(
          #inline( actionButton(ns("teste_comb"),"SAVE")),
          uiOutput(ns('newdata')),
          #uiOutput("somcut_display"),
          uiOutput(ns('hc_palette')),

          uiOutput(ns('umapro')),
          div(style='border-bottom: 1px solid gray',
              checkboxInput(ns("pclus_addpoints"),"+ Points",
                            value=somplot_args$pclus_addpoints),
              div(style="margin-left: 10px",
                  uiOutput(ns("pclus_points_inputs")))),

          div(style='border-bottom: 1px solid gray',
              checkboxInput(ns("pclus_addtext"),"+ Labels",
                            value=somplot_args$pclus_addtext),
              div(style="margin-left: 10px",
                  uiOutput(ns("pclus_text_inputs")))),
          div( uiOutput(ns("vfm_check"))),
          uiOutput(ns("showerrors_som")),
          uiOutput(ns("theme")),
          uiOutput(ns("title"))


        ))
  })



  output$title<-renderUI({
    div("+ Title: ",
        inline(textInput(ns("title"), NULL, "", width="200px"))
    )
  })


  output$theme<-renderUI({
    if(is.null(somplot_args$theme)){
      somplot_args$theme<-T
    }
    div(style='border-bottom: 1px solid gray',
        inline(checkboxInput(ns("theme"),
                             label = "+ show neuron coordinates",
                             value=somplot_args$theme,
                             width="200px"
        ))
    )
  })

  observeEvent(input$theme,{
    somplot_args$theme<-input$theme
  })

  observeEvent(input$pclus_addtext,{
    somplot_args$pclus_addtext<-input$pclus_addtext
  })

  observeEvent(input$pclus_addpoints,{
    somplot_args$pclus_addpoints<-input$pclus_addpoints
  })
  output$umapro<-renderUI({
    req(background_type!="hc")
    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
        inline(uiOutput(ns('plus_umatrix'))),
        inline(uiOutput(ns('plus_property')))

    )

  })
  output$plus_umatrix <- renderUI({
    if(is.null(somplot_args$plus_umatrix)){somplot_args$plus_umatrix<-F}
    checkboxInput(ns("plus_umatrix"),strong("+ U-Matrix:"), value=somplot_args$plus_umatrix)
  })
  output$plus_property <- renderUI({
    if(is.null(somplot_args$plus_property)){somplot_args$plus_property<-F}
    div(
      style="font-size: 14px",
      checkboxInput(ns("plus_property"),strong("+ Property:"), value=somplot_args$plus_property),
      uiOutput(ns("var_pproperty"))

    )
  })
  output$var_pproperty<-renderUI({
    req(isTRUE(input$plus_property))
    choices = colnames(data)
    div(
      inline(uiOutput(ns('prev_property'))),
      inline(pickerInput(ns("variable_pproperty"),
                         label = NULL,
                         choices = choices,
                         selected=somplot_args$variable_pproperty,
                         width="250px"
      )), inline(uiOutput(ns('next_property')))


    )
  })
  output$prev_property<-renderUI({
    data = data.frame(data,"factors")
    req(which(colnames(data)==input$variable_pproperty)!=1)
    actionButton(ns("prev_property"),"<<")
  })
  output$next_property<-renderUI({
    data = data.frame(data,"factors")
    req(which(colnames(data)!=input$variable_pproperty)==ncol(data))

    data = data.frame(data,"factors")
    actionButton(ns("next_property"),">>")
  })
  observeEvent(input$plus_umatrix,{
    somplot_args$plus_umatrix<-input$plus_umatrix
    if(isTRUE(input$plus_umatrix)){
      somplot_args$plus_property<-F
      somplot_args$backtype<-"uMatrix"
    }
  })
  observeEvent(input$plus_property,{
    somplot_args$plus_property<-input$plus_property
    if(isTRUE(input$plus_property)){
      somplot_args$plus_umatrix<-F
      somplot_args$backtype<-"property"
    }
  })
  observeEvent(input$pp_labels,{
    somplot_args$pp_labels<-input$pp_labels
  })
  observeEvent(input$next_property,{
    data = data.frame(data,"factors")
    pnext<-colnames(data)[which(colnames(data)==input$variable_pproperty)+1]
    updateSelectInput(session,'variable_pproperty',selected=pnext)

  })
  observeEvent(input$prev_property,{
    data = data.frame(data,"factors")
    pprev<-colnames(data)[which(colnames(data)==input$variable_pproperty)-1]
    updateSelectInput(session,'variable_pproperty',selected=pprev)
  })
  observeEvent(input$variable_pproperty,{
    somplot_args$variable_pproperty<-input$variable_pproperty
  })
  output$showerrors_som<-renderUI({
    req(background_type=="hc")
    div(
      div(strong("+ Show error"),
          pickerInput(ns("show_mapcode_errors"), NULL,choices=c('None',"Topographic Error","Quantization Error","Explained Variance","Kaski-Lagus Errors"), width="200px", selected=somplot_args$show_mapcode_errors)
      ),
      div(
        "+ Round errors",inline(
          tipify(numericInput(ns("round_error"),NULL,value = 3,min = 0,max = 1,step = 1, width="75px"),"symbol size")
        )
      )
    )

  })

  observeEvent(input$round_error,{
    somplot_args$round_error<-input$round_error
  })


  observeEvent(input$show_mapcode_errors,{
    somplot_args$show_mapcode_errors<-input$show_mapcode_errors
  })
  output$newdata<-renderUI({
    req(isTRUE(map_newdata))
    choices<-getobs_mapcode()
    if(length(choices)>0){
      return(pickerInput(ns("newdata"),"+ Map New Data",choices=choices, selected=somplot_args$newdata_som, width="200px"))
    }else{
      div(
        p(
          "Check that the columns of the datalist selected in 'map new data' match exactly with the columns used during training."
        ),
        p(
          'Para recuperar os dados de treinamento, baixe o objeto kohonen no modulo SOM-tab1 e leia em R:'
        ),
        p(code("m<-readRDS('kohonen.rds')")),
        p(code('m$data')))
    }


  })
  savereac<-reactive({

    vals$args<-argsplot()

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
  observeEvent(input$teste_comb,{
    savereac()
  })

  observeEvent(input$newdata,{
    somplot_args$newdata_som<-input$newdata
  })



  getobs_mapcode<-reactive({
    datalist<-vals$saved_data

    m<-attr(data,"som")[[as.character(som_model)]]
    res0<-unlist(
      lapply(datalist, function (x){
        identical(colnames(x),colnames(do.call(cbind,m$data)))
      })
    )
    choices<-names(res0[res0==T])
    res0<-unlist(
      lapply(datalist[choices], function (x){
        !anyNA(x)
      })
    )


    choices<-names(res0[res0==T])
    choices
  })


  observeEvent(input$bg_palette,{
    somplot_args$somplot_bg<-input$bg_palette
  })
  output$pclus_points_inputs<-renderUI({
    req(isTRUE(input$pclus_addpoints))
    div(
      div("+ Palette",inline(uiOutput(ns("pclus_points_palette")))),
      div("+ Factor",inline(uiOutput(ns("pclus_points_factor_out")))),
      div("+ Shape",inline(uiOutput(ns("pclus_points_shape")))),
      div("+ Size",inline(uiOutput(ns("pclus_points_size"))))
    )
  })
  output$pclus_points_palette<-renderUI({

    if(is.null(somplot_args$pclus_points_palette)){somplot_args$pclus_points_palette<-"black"}
    inline(
      tipify(pickerInput(inputId = ns("pclus_points_palette"),
                         label =NULL,
                         choices = vals$colors_img$val,
                         choicesOpt = list(content = vals$colors_img$img),
                         selected=somplot_args$pclus_points_palette,
                         options=list(container="body"), width="75px"),
             "Symbol colors"
      )
    )
  })
  output$pclus_points_factor_out<-renderUI({
    req(input$pclus_points_palette)
    choices<-c(colnames(attr(data,"factors")))


    inline(
      pickerInput(ns("pclus_points_factor"),NULL,
                  choices = c(choices),selected=somplot_args$pclus_points_factor,width="150px")
    )


  })
  output$pclus_points_shape<-renderUI({
    tipify(pickerInput(inputId = ns("pclus_symbol"),
                       label = NULL,
                       choices = df_symbol$val,
                       choicesOpt = list(content = df_symbol$img),
                       options=list(container="body"), width="100px",
                       selected=somplot_args$pclus_symbol)
           ,"symbol shape")
  })
  output$pclus_points_size<-renderUI({
    if(is.null(somplot_args$pclus_points_size)){somplot_args$pclus_points_size<-1}
    inline(
      tipify(numericInput(ns("pclus_points_size"),NULL,value = somplot_args$pclus_points_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
    )
  })
  output$pclus_text_inputs<-renderUI({
    req(isTRUE(input$pclus_addtext))
    div(
      div("+ Palette",inline(uiOutput(ns("pclus_text_palette")))),
      div("+ Factor",inline(uiOutput(ns("pclus_text_factor_out")))),
      div("+ Size",inline(uiOutput(ns("pclus_text_size"))))
    )
  })
  output$pclus_text_palette<-renderUI({

    if(is.null(somplot_args$pclus_text_palette)){somplot_args$pclus_text_palette<-"black"}
    inline(
      tipify(pickerInput(inputId = ns("pclus_text_palette"),
                         label =NULL,
                         choices =  vals$colors_img$val[getsolid_col()],
                         selected=somplot_args$pclus_text_palette,
                         choicesOpt = list(
                           content =  vals$colors_img$img[getsolid_col()] ),
                         options=list(container="body"), width="75px"),
             "Symbol colors"
      )
    )
  })
  output$pclus_text_factor_out<-renderUI({
    req(input$pclus_text_palette)
    choices<-c(colnames(attr(data,"factors")))
    inline(
      pickerInput(ns("pclus_text_factor"),NULL,
                  choices = c(choices),selected=somplot_args$pclus_text_factor,width="150px")
    )


  })
  output$pclus_text_size<-renderUI({
    if(is.null(somplot_args$pclus_text_size)){somplot_args$pclus_text_size<-1}
    inline(
      tipify(numericInput(ns("pclus_text_size"),NULL,value = somplot_args$pclus_text_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
    )
  })


  output$vfm_check<-renderUI({
    if(is.null(somplot_args$pclus_varfacmap_action)){somplot_args$pclus_varfacmap_action<-T}
    div(style='border-bottom: 1px solid gray',
        span("+ ",
             inline(checkboxInput(ns("varfacmap_action"), span("Variable factor map",actionLink(ns("varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =somplot_args$pclus_varfacmap_action, width="100px"))),
        div(style="margin-left: 10px",uiOutput(ns("varfac_out")))
    )
  })
  observeEvent(input$pclus_border,{
    somplot_args$pclus_border<-input$pclus_border
  })
  output$varfac_out<-renderUI({
    req(isTRUE(input$varfacmap_action))
    if(is.null(somplot_args$pclus_border)){
      somplot_args$pclus_border<-"white"
    }
    div(
      uiOutput(ns('vfm_type_out')),
      uiOutput(ns('npic_out')))})
  output$pcodes_bgalpha<-renderUI({
    if(is.null(somplot_args$pclus_border)){somplot_args$pclus_border<-"white"}
    if(is.null(somplot_args$pcodes_bgalpha)){
      if(background_type!="hc"){
        somplot_args$pcodes_bgalpha<-0
      } else{
        somplot_args$pcodes_bgalpha<-0.5
      }

    }
    if(is.null(somplot_args$base_size)){
      somplot_args$base_size<-12
    }
    div(span(
      "+ Background lightness",inline(
        tipify(numericInput(ns("pcodes_bgalpha"),NULL,value = somplot_args$pcodes_bgalpha,min = 0,max = 1,step = .1, width="75px"),"symbol size")
      )
    ),
    div(span(span('+ Border:'),inline(
      div(class="palette", pickerInput(ns("pclus_border"),
                                       label =NULL,
                                       choices =  vals$colors_img$val[getsolid_col()] ,
                                       choicesOpt = list(
                                         content =  vals$colors_img$img[getsolid_col()] ),
                                       selected= somplot_args$pclus_border, width = '75px'))
    ))),

    div(
      "+ Base size",inline(
        tipify(numericInput(ns("base_size"),NULL,value = somplot_args$base_size, width="75px"),"symbol size")
      )
    )
    )
  })
  output$hc_tab4_out<-renderUI({
    div( div(class="map_control_style",style="color: #05668D",
             uiOutput(ns('custom_levels'))
    ),
    column(12,plotOutput(ns("pclus_code")))
    )
  })
  bmu_clus_points<-reactive({
    req(input$dot_label_clus)
    if(input$dot_label_clus == 'symbols'){ T} else {F}
  })
  output$vfm_type_out<-renderUI({
    my_choices<-c("Highest", "Clockwise","Cluster")
    div(span("+ Show correlation:",inline(
      pickerInput(ns("vfm_type"),NULL,
                  choices = c("var", "cor","cor_hc"),
                  choicesOpt = list(
                    content = stringr::str_trunc(my_choices, width = 75)
                  ),
                  selected=somplot_args$vfm_type,
                  width="150px"
      ))))
  })
  output$npic_out<-renderUI({
    if(is.null(somplot_args$npic)){somplot_args$npic<-10}
    if(is.null(somplot_args$pclus.cex.var)){somplot_args$pclus.cex.var=1}
    if(is.null(somplot_args$p.clus.col.text)){somplot_args$p.clus.col.text<-"black"}
    if(is.null(somplot_args$var_bg)){somplot_args$var_bg<-"white"}
    if(is.null(somplot_args$var_bg_transp)){somplot_args$var_bg_transp=0}
    div(
      div(
        span("+ Number",
             inline(
               tipify(
                 numericInput(ns("npic"), NULL, value = somplot_args$npic, min = 2, width="75px"),"Number of variables to display"
               )))
      ),
      div(
        span("+ Var size",
             inline(
               numericInput(ns("pclus.cex.var"), NULL, value = somplot_args$pclus.cex.var, min = 2, width="75px")))
      ),
      div(
        span("+ Var text color",
             inline(
               div(class="palette",
                   pickerInput(inputId = ns("p.clus.col.text"),
                               label = NULL,
                               choices = vals$colors_img$val[getsolid_col()],
                               choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                               selected=somplot_args$p.clus.col.text,
                               width="100px"))))
      ),
      div(
        span("+ Var background",
             inline(
               div(class="palette",
                   pickerInput(inputId = ns("var_bg"),
                               label = NULL,
                               choices = vals$colors_img$val[getsolid_col()],
                               choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                               selected=somplot_args$var_bg,
                               width="100px"))))
      ),
      div(
        span("+ Var transparency",
             inline(
               tipify(
                 numericInput(ns("var_bg_transp"), NULL, value = somplot_args$var_bg_transp, min = 2, width="75px"),"Number of variables to display"
               )))
      )
    )
  })


  indicate_hc<-reactive({
    npic<-NULL
    indicate<-NULL
    if(isTRUE(input$varfacmap_action)){

      npic<- input$npic
      indicate<- input$vfm_type
    }
    iind=list(indicate=indicate,npic=npic)
    iind
  })
  bp_som<-reactive({
    iind=indicate_hc()
    m<-get_model()
    bp<-getbp_som(m=m,indicate=iind$indicate,npic=iind$npic,hc=somval$hc)
    somplot_args$bp_som<-bp
    bp
  })
  #somplot_args<-list()
  #somplot_args$backtype="property"
  get_network<-reactive({
    # saveRDS(reactiveValuesToList(input),'input.rds')
    #saveRDS(reactiveValuesToList(vals),'savepoint.rds')
    # saveRDS(reactiveValuesToList(somplot_args),'somplot_args.rds')
    # input=readRDS('input.rds')
    # vals= readRDS('savepoint.rds')
    # somplot_args=readRDS('somplot_args.rds')
    #background_type="uMatrix"
    #beep(1)
    if(background_type=="hc"){
      backtype="hc"
      property=NULL

    } else{
      req(length(input$plus_umatrix>0))
      req(length(input$plus_property>0))
      if(isFALSE(input$plus_umatrix)&isFALSE(input$plus_property)){
        backtype=NULL
        property=NULL
      } else{
        req(length(somplot_args$backtype)>0)
        backtype=somplot_args$backtype
        property=NULL

        if(backtype=="property"){
          req(length(input$variable_pproperty)>0)
          property=input$variable_pproperty}
      }
    }



    m<-get_model()
    hexs<-get_neurons(m,background_type=backtype,property=property, hc=somval$hc)
    somplot_args$hc_network<-hexs
    hexs
  })
  # args<-readRDS('args.rds')
  # vals<-readRDS('savepoint.rds')
  # input<-readRDS('input.rds')
  get_copoints<-reactive({
    m<-get_model()
    copoints<-getcopoints(m)
    somplot_args$copoints_hc<-copoints
    copoints
  })
  copoints_scaled<-reactive({
    get_network()
    get_copoints()
    points_tomap=rescale_copoints(hexs=somplot_args$hc_network,copoints=somplot_args$copoints_hc)
    if(length(input$pclus_text_factor)>0){
      text_factor= attr(data,"factors")[rownames(data),input$pclus_text_factor, drop=F]
      points_tomap$label<-text_factor[rownames(points_tomap),]
    }
    if(length(input$pclus_points_factor)>0){
      points_factor= attr(data,"factors")[rownames(data),input$pclus_points_factor, drop=F]
      points_tomap$point<-points_factor[rownames(points_tomap),]
      attr(points_tomap,"namepoints")<-input$pclus_points_factor
    }
    somplot_args$hc_mapsom<-points_tomap
    points_tomap
  })





  observeEvent(input$vfm_type,{
    somplot_args$vfm_type<-input$vfm_type
  })
  observeEvent(input$npic,{
    somplot_args$npic<-input$npic
  })
  observeEvent(input$pclus.cex.var,{
    somplot_args$pclus.cex.var<-input$pclus.cex.var
  })
  observeEvent(input$p.clus.col.text,{
    somplot_args$p.clus.col.text<-input$p.clus.col.text
  })
  observeEvent(input$var_bg,{
    somplot_args$var_bg<-input$var_bg
  })
  observeEvent(input$var_bg_transp,{
    somplot_args$var_bg_transp.alpha<-input$var_bg_transp
  })
  observeEvent(input$pclus_points_palette,{
    somplot_args$pclus_points_palette<-input$pclus_points_palette
  })
  observeEvent(input$insertx_pclus,{
    vals$insertx_pclus<-input$insertx_pclus
  })
  observeEvent(input$inserty_pclus,{
    vals$inserty_pclus<-input$inserty_pclus
  })
  observeEvent(input$ncol_pclus,{
    vals$ncol_pclus<-input$ncol_pclus
  })
  observeEvent(input$bgleg_pclus,{
    vals$bgleg_pclus<-input$bgleg_pclus
  })
  observeEvent(input$pclus_symbol,{
    somplot_args$pclus_symbol<-input$pclus_symbol
  })
  observeEvent(input$dot_label_clus,{
    vals$dot_label_clus<-input$dot_label_clus
  })
  observeEvent(input$varfacmap_action,{
    somplot_args$pclus_varfacmap_action<-input$varfacmap_action
  })
  observeEvent(input$pclus_points_size,{
    somplot_args$pclus_points_size<-input$pclus_points_size
  })
  observeEvent(input$pcodes_bgalpha,{
    somplot_args$pcodes_bgalpha<-input$pcodes_bgalpha
  })
  observeEvent(input$pclus_points_factor,{
    somplot_args$pclus_points_factor<-input$pclus_points_factor
  })
  #args<-readRDS('arg.rds')
  get_error<-reactive({
    #somplot_args<-args
    if(input$show_mapcode_errors=="None"){
      res<-NULL
    } else {
      m<-get_model()
      hc_neu<-hc_hex(somplot_args$hc_network)
      hc<-rescale_copoints(hexs=somplot_args$hc_network,copoints=somplot_args$hc_mapsom)[,'hc']
      names(hc)<-rownames(somplot_args$hc_mapsom)
      x<-tapply(m$grid[[1]][,1],hc_neu, mean)
      y<-tapply(m$grid[[1]][,2],hc_neu, mean)
      errors<-somQuality4(m,hc)

      res<-data.frame(cbind(x,y,errors))
      colnames(res)[3:6]<-c("Quantization Error","Explained Variance","Topographic Error","Kaski-Lagus Errors")
      print(res)


    }
    res

  })

  geterror_plot<-reactive({
    errors<-get_error()
    errors
  })


  argsplot<-reactive({

    req(input$pcodes_bgalpha)
    if(isTRUE(input$pclus_addpoints)){
      req(input$pclus_points_factor)
      points_factor= attr(data,"factors")[rownames(data),input$pclus_points_factor, drop=F]
    }
    if(isTRUE(input$pclus_addtext)){
      req(input$pclus_text_factor)
      text_factor= attr(data,"factors")[rownames(data),input$pclus_text_factor, drop=F]
    }

    indicate=indicate_hc()

    m<-get_model()

    copoints_scaled()

    bp_som()
    errors<-NULL
    if(!is.null(input$show_mapcode_errors)){
      errors<-geterror_plot()[c('x','y',input$show_mapcode_errors)]
      if(!is.null(errors)){
        errors<-round(errors,input$round_error)
      }
    }

    copoints2<-somplot_args$copoints2
    copoints3<-copoints2
    #opoints2$point<-args$points_factor
    #attach(vals$args)


    args<-list(m=m,
               hexs=somplot_args$hc_network,
               points_tomap=somplot_args$hc_mapsom,
               bp=somplot_args$bp_som,
               points=input$pclus_addpoints,
               points_size=input$pclus_points_size,
               points_palette=input$pclus_points_palette,
               pch=as.numeric(input$pclus_symbol),
               text=input$pclus_addtext,
               text_size=input$pclus_text_size,
               text_palette=input$pclus_text_palette,
               bg_palette=input$bg_palette,
               newcolhabs=vals$newcolhabs,
               bgalpha=input$pcodes_bgalpha,
               border=input$pclus_border,
               indicate=indicate$indicate,
               cex.var=as.numeric(input$pclus.cex.var),
               col.text=input$p.clus.col.text,
               col.bg.var=input$var_bg,
               col.bg.var.alpha=1-input$var_bg_transp,
               show_error=errors,
               base_size=input$base_size,
               show_neucoords=input$theme,
               newdata=input$newdata,
               title=input$title
    )
    #saveRDS(args,"args.rds")
    #beep(3)
    args

  })

  observeEvent(argsplot(),{
    vals[[ns("somplot_args")]]<-argsplot()
    #args<-readRDS('args.rds')
    #saveRDS(argsplot(),"args.rds")
    #saveRDS(ns("somplot_args"),"name.rds")
    #readRDS('name.rds')
    #beep(2)

  })



}



