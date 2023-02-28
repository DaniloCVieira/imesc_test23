#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    shinydashboardPlus::dashboardPage(
      skin = "blue",
      shinydashboardPlus::dashboardHeader(controlbarIcon=NULL,disable=T),
      dashboardSidebar(
        extendShinyjs(text = "shinyjs.collapse = function(boxid) {$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();}",functions = 'collapse'),
        useShinyjs(),
        #use_cicerone(),
        #introjsUI(),
        width = 155,
        sidebarMenu(
          id = "tabs",
          value="menu_som",
          div(id = "sidebar-main",
            uiOutput("style_act"),
            div(style = "background-color:  #05668D; font-size: 13px",class = "sidebar-menu",
              menuItem(tabName = "menu_intro",div(style="min-height: 42px;max-height: 42px;",div(img(src = b64,height = '32',width = '32',style ="padding: 0px; margin: 0px; margin-left: 7px"), img(src=imesc_icon,height='20',width='70'),style = "color: white; font-size: 22px; font-family: 'Alata';padding: 0px; margin: 0px; margin-top: 4px;"), style = "margin-left: -5px; color: white; font-size: 14px; white-space: normal; margin-top: -6px;margin-bottom: -6px"),selected =T),
              menuItem(tabName = "menu_upload",div(icon("fas fa-database",style="height:20px;width: 20px"), HTML('&nbsp;'), "Data Bank", style = "", id="menu_data")),
              menuItem(div(icon("fas fa-binoculars",style="height:20px;width: 20px"), HTML('&nbsp;'), "Descriptive tools", style = ""),tabName = "menu_explore"),
              menuItem(div(icon("fas fa-map",style="height:20px;width: 20px"), HTML('&nbsp;'), "Spatial tools", style = ""),tabName = "menu_maps"),
              menuItem(div(img(src=div_icon,height='20',width='20'),HTML('&nbsp;'),"Biodiversity tools",style = ""),tabName = "menu_div"),
             menuItem(expandedName="unsup_expand",tags$div(class="needed",id="unsup_menu",div(class="unsup_menu",img(src=unsup_icon,height='20',width='20'),HTML('&nbsp;'),"Unsupervised",div("Algorithms", style = "margin-left: 30px"),style = "padding: 0px")),menuSubItem(icon=NULL,div(icon("fas fa-braille"),HTML('&nbsp;'),"Self-Organizing",div("Maps", style = "margin-left: 30px"),style = ""),tabName = "menu_som"),menuSubItem(icon=NULL,div(icon("fas fa-network-wired"),HTML('&nbsp;'),"Hierarchical",div("Clustering", style = "margin-left: 30px"),style = ""),tabName = "menu_hc"),menuSubItem(icon=NULL,div(img(src=kmeans_icon,height='20',width='20'),HTML('&nbsp;'),"K-means",style = ""),tabName = "menu_kmeans")),
              menuItem(expandedName="sup_expand",icon=NULL,div(class="needed",id="sup_menu",div(class="sup_menu",img(src=sup_icon,height='20',width='20'),HTML('&nbsp;'),"Supervised",div("Algorithms", style = "margin-left: 30px"),style = "")),menuSubItem(icon =NULL,div(id="menus",img(src=nb_icon,height='20',width='20'),HTML('&nbsp;'),"Naive Bayes",style = ""),tabName = "menu_nb"),menuSubItem(icon =NULL,div(class="menus",img(src=svm_icon,height='20',width='20'),HTML('&nbsp;'),"Support Vector",div("Machine", style = "margin-left: 30px"),style = ""),tabName = "menu_svm"),menuSubItem(icon =NULL,div(class="menus",img(src=knn_icon,height='20',width='20'),HTML('&nbsp;'),"K-nearest",div("neighbor", style = "margin-left: 30px"),style = ""),tabName = "menu_knn"),menuSubItem(icon =NULL,div(class="menus",icon("fas fa-tree"),icon("fas fa-tree", style = "margin-left: -8px;"),icon("fas fa-tree", style = "margin-left: -8px;"),HTML('&nbsp;'),"Random Forest",style = ""),tabName = "menu_rf"),menuSubItem(icon =NULL,div(class="menus",img(src=sgboost_icon,height='20',width='20'),HTML('&nbsp;'),"Stochastic",div("Gradient Boosting", style = "margin-left: 30px"),style = ""),tabName = "menu_sgboost"),menuSubItem(icon=NULL,div(icon("fas fa-braille"),HTML('&nbsp;'),"Self-Organizing",div("Maps", style = "margin-left: 30px"),style = ""),tabName = "menu_som2")),
              menuItem(div(img(src=comp_icon,height='20',width='20'),HTML('&nbsp;'),"Compare &",div("Combine Models", style = "margin-left: 30px"),style = ""),tabName ="menu_comp"),
             # menuItem(div("teste",style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"),tabName = "menu_teste"),
              # uiOutput("testet"),

              div(inline(div(uiOutput("footer_qsave"))))

            )
          )
        )
      ),
      dashboardBody(
        tags$head( tags$script(src = "https://kit.fontawesome.com/7af7c9478d.js")),
        tags$head(tags$script(HTML("$(document).on('.dataTables_filter').addClass('pull-left');"))),
        tags$head(tags$script(HTML(js_getid))),
        tags$head(tags$script(HTML('
        $(document).on("keydown", function (e) {
        Shiny.onInputChange("lastkeypresscode", e.keyCode);
        });
        '))),

        includeCSS("inst/app/www/styles.css"),
        tags$style(".pretty.p-default input:checked~.state label:after {background-color: SeaGreen !important;}"),
        tags$script("$(document).on('click', '.name-opt a', function (evt) {
  evt.preventDefault(); Shiny.setInputValue('name',this.dataset.name);
});"
        ),

tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
Shiny.onInputChange('shiny_width',myWidth)

});"),

tags$script("$(document).on('shiny:connected', function(event) {
var myHeight = $(window).height();
Shiny.onInputChange('shiny_height',myHeight)

});"),
useShinyjs(),
div(

  column(12,  style = "position: fixed; overflow: visible;",class="needed",id="header_app",' An Interactive Machine Learning App for Environmental Science'),
  column(12,  style = "position: fixed; overflow: visible;bottom: 0px;z-index: 99",
         fluidRow(class="needed",id="footer_app",
                  span(em("Version:"),paste0(packageVersion("imesc"),';'),em("Last update:"),last_update)
         )
  ),


  add_busy_spinner(spin = "hollow-dots",height="20px", color="yellow",width="20px", margins = c(15, 600),position="top-left"),

  uiOutput('preprocess'),
  uiOutput('tools_upload'),
  div(


    uiOutput("change_head"),
    uiOutput("change_background"),
  ),
  div(id="main_panel",class="needed",
      div(
        uiOutput('imesc_teste'),
        uiOutput("imesc_panel")
      )

  )
)

      )
    )

  )
}


