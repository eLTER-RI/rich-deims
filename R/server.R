## require necessary packages:
sapply(c('dplyr',
         'tidyr',
         'magrittr',
         'purrr',
         'DT',
         'shinyWidgets',
         'shinyjs',
         'shinycssloaders'
         ),
       function(p) {require(p, character.only = TRUE)})

## require helper functions
source('./helpers.R')
## load('./www/data/site-details-as-json.RData')
## load('./www/data/site-details-as-json-20220125.RData')
load('./www/data/hugo.RData')

load('./www/data/attribute_list.RData')

## TODO ============================== 
## - replace multiple occurence of current_upload with reactive value

ls()



server <- function(input, output) {
    ## store session for later use (if necessary, check redundancy!
    session = getDefaultReactiveDomain()
    shinyjs::useShinyjs()


    

    ## live retrieval like so (but takes 560s)
    ## jsons <- get_all_sites_as_big_json()

    ## show loading bar while attribute tree is calculated:
    ## this takes about five seconds, so show some loading bar
    att_tree_loader <- Attendant$new('tree_loader')
    att_tree_loader$set(5) ## set bar to 5% progress


    ## REACTIVATE FOR LIVE PROCESSING !!!
    site_details <- 
        (function(){
            site_details <- get_site_details_from_json(jsons)
            att_tree_loader$set(10) ## set loading bar to 10% 
            return(site_details)
        })()
    ## save(site_details, file='./www/data/site-details.RData')
    ## load('./www/data/site-details.RData')


    url_frag <- 'https://deims.org/exp/tagged_sites_'
    assign('tagged_sites_count', value = read.csv(paste0(url_frag,'count')))
    assign('tagged_sites_sub', value = read.csv(paste0(url_frag,'sub')))
    assign('tagged_sites_descriptive', value = read.csv(paste0(url_frag,'descriptive')))

    countable_attributes <- c(
        'attributes.environmentalCharacteristics.eunisHabitat.label'
    )

    ## output$debugInfo <- renderPrint(site_details %>% rows())

    site_details <- 
        site_details %>%
        mutate(
            `eLTER facility.count` = id %in% (tagged_sites_count %>% pull(1)),
            `eLTER facility.sub` = id %in% (tagged_sites_sub %>% pull(1)),
            `eLTER facility.descriptive` = id %in% (tagged_sites_descriptive %>% pull(1))
        )

    countable_attributes %>%
        walk(~ {            
            site_details[[paste0(.x,'_cnt')]] <<-
                site_details[.x] %>% map(~ strsplit('|',.x) %>% unlist %>% length)               
        })


    ## ==================== sidebar panel, network / site selection ====================

    network_choices <- get_network_names(site_details)
    ## network_memberships <- get_network_memberships(site_details)

    


    

    updateSelectizeInput(session = session,
                         inputId = 'select_net',
                         label = NULL,
                         choices = network_choices
                         )


    active_net_ids <- reactiveVal(c('4742ffca-65ac-4aae-815f-83738500a1fc'))
    active_site_ids <- reactiveVal(NULL)
    active_attributes <- reactiveVal(c('id','title'))
    active_site_data <- reactiveVal({
        site_details %>% select('id','title') %>% head(0)
    })
    

    observeEvent(input$tree_attributes, {       
        selection <- input$tree_attributes %>% get_selected(.,format='names')
        branches <- selection %>% map(~ paste0(attr(.x,'ancestry'),collapse='.'))
        leaves <- selection %>% unlist
        attribute_vector <- 
            paste(branches,leaves,sep='.') %>%
            gsub('(attributes|parent|children|name)[0-9]','\\1',.) %>%
            gsub('^\\.','',.) %>%
            .[. %in% names(site_details)]
        attribute_vector <- c('id',attribute_vector) %>% unique
        active_attributes(attribute_vector)
        active_site_data(site_details %>% 
                         filter(id %in% active_site_ids()) %>%
                         select(active_attributes()) %>%
                         left_join(foreign_data(), by=c('id'='id'))
                         )
     })


    observeEvent(input$select_net,{
        active_net_ids({input$select_net %>% as.vector %>% as.character})
        updateTextAreaInput(session, 'custom_site_input_textarea',
                            value = active_site_ids() %>% paste(collapse='\n')
                            )
        active_site_ids(
            get_active_sites_from_active_networks(site_details, active_net_ids())
        )

        active_site_data(site_details %>% 
                         filter(id %in% active_site_ids()) %>%
                         select(active_attributes()) %>%
                         left_join(foreign_data(), by=c('id'='id'))
                         )
    })
    

    observeEvent(input$clear_net_selection,{
        updateSelectizeInput(session,'select_net',selected = NA)
        updateTextAreaInput(session,'custom_site_input_textarea','')
        active_site_ids(NULL)
    })

    observeEvent(input$select_all_nets,{
        shiny::updateSelectizeInput(session,'select_net', selected = network_choices)
        active_site_ids(
            get_active_sites_from_active_networks(site_details, network_choices)
        )
    })

    shinyjs::onevent(event = 'click', id = 'custom_site_input_textarea',
                     shinyjs::runjs('$("#custom_site_input_textarea").select()'))


    observeEvent(input$apply_custom_site_input,{
        shiny::updateSelectizeInput(session,'select_net',selected = NA)
        active_site_ids(input$custom_site_input_textarea)

        active_site_data(site_details %>% 
                         filter(id %in% active_site_ids()) %>%
                         select(active_attributes()))

    })


    ## ==================== data tab ====================

    
    get_tree_data <- function(){
        att_tree_loader$set(10)    ## expand progress bar to 10%
        treeData <-   data.frame(s = paste0('root.',
                                            c(
                                                get_attribute_strings(jsons),
                                                'eLTER facility.count',
                                                'eLTER facility.sub',
                                                'eLTER facility.descriptive'                                  
                                            )
                                            ))
        att_tree_loader$set(30)
        treeData <- treeData %>%        
            data.tree::FromDataFrameTable(pathName = 's', pathDelimiter = '.')
        att_tree_loader$set(50)
        treeData <- treeData %>%
            data.tree::ToListSimple(.,
                                    nameName = '',
                                    filterFun = function(n) !isLeaf(n))
        att_tree_loader$set(100)
        treeData
    }
    output$tree_attributes <- renderTree(get_tree_data())


    one_hot_value_combos <- list(
        'affiliated networks' = list(
            'ACTRIS'='attributes.affiliation.networks.network.name|ACTRIS',
            'DANUBIUS'='attributes.affiliation.networks.network.name|DANUBIUS',
            'Lifewatch'='attributes.affiliation.networks.network.name|Lifewatch',
            'ICP Forests' = 'attributes.affiliation.networks.network.name|ICP Forests',
            'ICOS' = 'attributes.affiliation.networks.network.name|ICOS',
            'LTER Europe' = 'attributes.affiliation.networks.network.name|LTER Europe'
        )
    )

    
    shinyWidgets::updatePickerInput(
                      inputId = 'one_hot_picker', 
                      session = session,
                      choices = one_hot_value_combos
                  )

    


    observe({
        input$one_hot_picker %>%
            ## one_hot_choices() %>% 
            walk(~{
                if(length(.x) > 0 ){
                    choice_string <- strsplit(.x,'\\|') %>% unlist
                    hot_attribute <- choice_string[1]; hot_value  <- choice_string[2]
                    hot_attribute_short <- hot_attribute %>% gsub('.*\\.','',.)
                    shinyjs::logjs(hot_attribute_short)
                    if(!hot_attribute %in% active_attributes()) {
                        active_attributes(c(active_attributes(),hot_attribute ))} 
                    col_pos <- grep(hot_attribute, active_attributes())

                    
                    ## active_site_data() %>%
                    new_yn_col_title <- paste0(hot_attribute_short,':',hot_value)
                    new_data <- 
                        site_details %>% 
                        filter(id %in% active_site_ids()) %>%                
                        mutate('{new_yn_col_title}' := grepl(hot_value, .[[hot_attribute]])) %>%
                        ## ^^^ expand column name variable using '{expr}' and := 
                        select(id, all_of(new_yn_col_title))
                    left_join(active_site_data(), new_data) %>%
                        active_site_data(.)
                }
            })
    })




    ## find available plugins with DT:::available_plugins()
    output$active_site_table <- 
        DT::renderDT(datatable(
                data = active_site_data(),
                escape = FALSE,
                rownames = FALSE,
                colnames = prettify_colnames(names(active_site_data())),
                options = list(
                    buttons = c('copy','csv'),
                    scrollY =  '30vh',     
                    scrollX = TRUE,
                    dom = 'Blftp',
                    paging = TRUE,
                    pageLength = 10,
                    extensions = c('Buttons','Scroller'),
                    plugins = c('ellipsis')
                )
            )
            )




    output$download_csv <- downloadHandler(
        filename = function() {paste0('site-data-',Sys.Date(),'.csv')},
        content = function(file) {write.csv(active_site_data(), file, row.names = FALSE)}
    )



    ## --------------------  datastore tab  --------------------
    foreign_data <- reactive({})

    

    ## return first column containing a datum which resembles a site ID
    foreign_site_id_candidates <- reactive({
        if(!is.null(foreign_data())){
            z <- foreign_data() %>% head(5) %>%
                lapply(function(col) grepl('^.{8}-.{4}-.{4}-.{4}-.{12}$',col) %>% sum) %>%
                unlist
            z <- which(z > 0)
            ifelse(length(z) > 0, 
                   return(names(foreign_data())[z]),
                   return('undetected')
                   )
        }
    })

    
    foreign_key <- reactive(input$select_foreign_id)
    observeEvent(input$select_delimiter,{
        shinyWidgets::updateAwesomeRadio(
                          session = session,
                          inline = TRUE,
                          checkbox = TRUE,
                          inputId = 'select_foreign_id',
                          choices = foreign_site_id_candidates()
                      )
    })



    uploaded_data <- reactiveValues()

    observeEvent(input$upload_data, {
        infile_info <- input$upload_data
        if(!is.null(infile_info)){
            dataset_index <- paste0('upload ',sprintf('%02.0f',uploaded_data %>% names %>% length()+1))
            uploaded_data[[dataset_index]] <- 
                list(
                    'source' = infile_info$name,
                    'datapath' = infile_info$datapath,
                    'data' = readr::read_delim(infile_info$datapath,
                                               show_col_types = FALSE,
                                               delim = input$select_delimiter
                                               ),
                    'id_column' = 1
                )
            
            uploaded_data %>% names %>%
                walk(~ {
                    current_dataset <- uploaded_data[[.x]]$data
                    z <- current_dataset %>% head(5) %>%
                        lapply(function(col) grepl('^.{8}-.{4}-.{4}-.{4}-.{12}$',col) %>% sum) %>%
                        unlist
                    z <- which(z > 0)
                    ifelse(length(z) > 0, 
                           uploaded_data[[.x]]$id_candidates <- names(current_dataset)[z],
                           uploaded_data[[.x]]$id_candidates <- c('undetected')
                           )                    
                })
            
            dataset_choices <- set_names(
                uploaded_data %>% names,
                dataset_names <- uploaded_data %>% names %>%
                    map( ~ uploaded_data[[.x]]$source) %>% unlist
            )
            updateRadioGroupButtons(
                session = session,
                inputId = 'uploaded_dataset_selector',
                choices = dataset_choices
            )

            
            
        }
    })
    ## ==========  update which upload is displayed for preparation ==========
    observeEvent(input$uploaded_dataset_selector,
                 ## ignoreNULL = TRUE, ignoreInit = TRUE,
    {
        current_dataset <- uploaded_data[[input$uploaded_dataset_selector]]
        output$dataset_properties_1  <- renderText(current_dataset$source)
        id_choices = set_names(
            paste0(input$uploaded_dataset_selector,'_',current_dataset$id_candidates),
            current_dataset$id_candidates
        )

        updatePrettyRadioButtons(inputId = 'select_id_column', session = session,
                                 choices = id_choices
                                 )

    })


    ## ==========  update column separation and choice of ID columns ==========

    observeEvent(input$select_delimiter,                 
                 ignoreNULL = TRUE, ignoreInit = TRUE,
                 {
                     current_upload <- uploaded_data[[input$uploaded_dataset_selector]]

                     ## update dataframe with re-read upload with new delimiter:
                     current_upload$data = readr::read_delim(
                                                      current_upload$datapath,
                                                      show_col_types = FALSE,
                                                      delim = input$select_delimiter
                                                  )
                     
                     ## update ID candidates: ----------
                     z <- current_upload$data %>% head(5) %>%
                         lapply(function(col) grepl('^.{8}-.{4}-.{4}-.{4}-.{12}$',col) %>% sum) %>%
                         unlist
                     z <- which(z > 0)
                     ifelse(length(z) > 0, 
                            current_upload$id_candidates <- names(current_upload$data)[z],
                            current_upload$id_candidates <- c('undetected')
                            )                  
                     ## update ID selector: ----------
                     id_choices = set_names(
                         paste0(input$uploaded_dataset_selector,'_',current_upload$id_candidates),
                         current_upload$id_candidates
                     )
                     updatePrettyRadioButtons(inputId = 'select_id_column', session = session,
                                              choices = id_choices
                                              )

                     d <- current_upload$data
                     output$upload_brief <- renderUI(
                         div(tags$small(
                                      br(),sprintf('data: %s',current_upload$source),
                                      br(),sprintf('%i rows',nrow(d)),
                                      br(),sprintf('%i columns',ncol(d))
                                  )
                             )
                     )
                 }
                 )

    ## ========== update offer of value columns depending on ID:
    observeEvent(input$select_id_column,
                 ignoreNULL = TRUE, ignoreInit = TRUE,
                 {
                     current_upload <- uploaded_data[[input$uploaded_dataset_selector]]
                     variable_choices  <-  names(current_upload[['data']])
                     variable_choices <- set_names(
                         paste0(input$uploaded_dataset_selector,'_',variable_choices),
                         variable_choices
                     ) %>% .[-grep(input$select_id_column,.)] ## don't offer ID column
                     updateAwesomeCheckboxGroup(inputId = 'select_value_columns', session = session,
                                                choices = variable_choices
                                                )
                 }
                 )
    ##  ========= add or remove selected foreign variables from main table display
    ## foreign_data is a live table consisting of the full set of DEIMS site IDs 
    ## to which the uploaded dataset(s) are joined by site ID

    foreign_data <- reactiveVal(data.frame(id = site_details$id))

    ## output$debugInfo2 <- renderPrint(foreign_data())


    observeEvent(input$select_value_columns,                 
                 ignoreNULL = TRUE, ignoreInit = TRUE,{
                     current_upload <- uploaded_data[[input$uploaded_dataset_selector]]
                     foreign_id <- input$select_id_column
                     selected_columns  <- c(foreign_id,
                                            input$select_value_columns) %>%
                         gsub('^.*?_','',.)

                     tmp <- current_upload$data %>%
                         select(selected_columns) %>%
                         rename_all(~ paste0(current_upload$source,'.',.x))

                     ## remove all columns (of current upload) from live upload table,
                     ## so they won't get multiple times with each re-select
                     col_indices_to_remove <- 
                         grep(current_upload$source, names(foreign_data()))

                     tmp <- 
                         foreign_data() %>%
                         select(-col_indices_to_remove) %>%
                         ## active_site_data() %>%
                         left_join(., tmp, by=c('id' = names(tmp)[1]))
                     
                     ## update foreign_data:
                     foreign_data(tmp)
                     ## update whole set:
                     active_site_data(site_details %>% 
                                      filter(id %in% active_site_ids()) %>%
                                      select(active_attributes()) %>%
                                      left_join(foreign_data(), by=c('id'='id'))
                                      )



                 }
                 )

}


