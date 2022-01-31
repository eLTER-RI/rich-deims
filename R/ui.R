
library('shiny')
library('bslib')
library('shinyjs')
library('shinycssloaders')
library('shinyTree')
library('shinyWidgets')
library('waiter')

## ui <- semanticPage(
ui <- fluidPage(

    theme = bslib::bs_theme(bootswatch = "yeti"),


    tags$head(tags$script(src="./js/ellipsis.js")),
    includeCSS('./www/app.css'),
    useShinyjs(),
    useAttendant(),
    title = "networkR",
    h1("networkR"),

    ## theme = bs_theme(bootswatch = "simplex"),
    fluidRow(

        column(2,
               div(p('select network')),
               div(
                   actionButton('clear_net_selection', 'clear all'),
                   actionButton('select_all_nets', 'select all')
               ),
               div(
                   selectizeInput(inputId = 'select_net', label = "",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                      placeholder = 'click to select networks',
                                      openOnFocus = TRUE
                                  )
                                  )
               ), 
               div(p('or enter sites')),
               textAreaInput(
                   inputId = 'custom_site_input_textarea',
                   label = NULL, 
                   ## height = '10em', width='100%',
                   placeholder = "paste site IDs here"
               ),
               actionButton('apply_custom_site_input', 'apply')
               ),
        column(10,
               tabsetPanel(type='tabs',
                           tabPanel('Data table',
                                    div(verbatimTextOutput('debugInfo')),
                                    div(class='btn-group p-3',
                                        dropdown(
                                            h4('pick attributes to display'),                                             
                                            shinyTree('tree_attributes',
                                                      checkbox = TRUE, search = TRUE,
                                                      themeDots = TRUE,
                                                      theme='proton', stripes = TRUE, multiple = TRUE,
                                                      animation = FALSE
                                                      ),
                                            attendantBar('tree_loader',
                                                         text = 'loading attributes',
                                                         striped = TRUE,
                                                         animated = TRUE
                                                         ),  
                                            label = 'pick columns',
                                            circle = FALSE
                                        ),                                                                                    
                                        span(class='p-1'),
                                        dropdown(
                                            label = 'flag rows',
                                            circle = FALSE,
                                            h4('show presence of these values in additional true/false columns:'),
                                            pickerInput(
                                                inputId = "one_hot_picker",
                                                label = "pick indicators",
                                                choices = list(),
                                                options = list(
                                                    `actions-box` = TRUE,
                                                    size = 10,
                                                    `selected-text-format` = "count > 3"
                                                ),
                                                multiple = TRUE
                                            )
                                        )
                                        ),
                                    uiOutput(outputId = 'wait_for_table'),                                      
                                    div(
                                        DT::DTOutput(outputId = 'active_site_table') %>%
                                        withSpinner(id='table_spinner', image='data/logo.svg')
                                    ),
                                    div(class='btn-group',
                                        downloadButton("download_csv", "CSV") ##,
                                        ## downloadButton("download_xls", "XLS")
                                        )
                                    ),


                           ## ------------------------------------------------------------
                           tabPanel('add own data',
                                    ## menu='Data store', id='tab_datastore',
                                    ## content= list(
                                    div(style='visibility:hidden',verbatimTextOutput('file_uploaded'), NULL), ## empty, triggers conditional panel
                                    div(verbatimTextOutput('debugInfo2')),
                                    div(class='card-columns p-2',
                                        div(class='card',
                                            div(class='card-header',
                                                h2(class='card-title',icon('upload'))
                                                ),
                                            div(class='card-body bg-white',
                                                fileInput('upload_data',
                                                          label = NULL,
                                                          multiple = FALSE,
                                                          accept = c(
                                                              'text/csv','text/tsv',
                                                              'text/comma-separated-values,text/plain',
                                                              '.csv','tsv'),
                                                          buttonLabel = "upload file",
                                                          placeholder = "No file selected"
                                                          ),
                                                uiOutput('upload_file_list')
                                                ),
                                            div(class='card-footer','max 5 MB')
                                            ),
                                        div(class='card',
                                            div(class='card-header',
                                                fluidRow(
                                                    column(2,
                                                           h2(icon('cog'))
                                                           ),
                                                    column(10,
                                                           uiOutput('upload_brief')
                                                           )
                                                )
                                                ),
                                            div(class='card-body',
                                                radioGroupButtons(
                                                    inputId = 'uploaded_dataset_selector',
                                                    label = 'select ID and value columns for:',
                                                    choices = c('no data uploaded' = 'no data')
                                                ),
                                                hr(),
                                                'select delimiter:',
                                                div(id = 'select_delimiter',
                                                    radioGroupButtons(
                                                    inputId = 'select_delimiter',
                                                    label = '',
                                                    selected = ',',
                                                    choices = c(',',';','|',' ')
                                                )),
                                                hr(),

                                                h4('ID column:'),
                                                prettyRadioButtons(inputId = 'select_id_column',
                                                                   label = '',
                                                                   choices = c('no data'='no ID detected')
                                                                   ),

                                                h4('value columns:'),
                                                awesomeCheckboxGroup(
                                                    inputId = 'select_value_columns',
                                                    label = '',
                                                    choices = c('no data' = 'no columns detected')
                                                )
                                                )
                                            )                       
                                        )

                                    )
                           )
               )
    )
)


