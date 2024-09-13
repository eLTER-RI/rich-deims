open## require necessary packages:
sapply(c('dplyr',
         'tidyr',
         'magrittr',
         'purrr',
         'DT',
         'leaflet', 'sf',
         'shinyWidgets',
         'shinyjs',
         'shinycssloaders',
         'openxlsx',
         'jsonlite',
         'shinyBS',
         'cicerone',
         'shinyBS' #,
         ## devtools::install_github("trestletech/shinyStore")
         ## 'shinyStore'
),
function(p) {require(p, character.only = TRUE)})

## require helper functions
source('./helpers.R')

data_file_name <- 'site-details-2024-09-13.RData'



## setup first-time user guide:
guide <- Cicerone$
    new(opacity = 0)$ 
    step('select_network_panel', 'select network(s)',
         'select one or more DEIMS networks or a list of DEIMS sites')$
    ## step('select_net', 'network selector', 'select one or more DEIMS networks')$
    step('select_all_nets', 'select all', 'Add all DEIMS networks to selection.')$
    step('custom_site_input_textarea', 'enter custom site',
         'Enter a custom DEIMS site by its DEIMS-ID')$
    step('clear_net_selection', 'deselect all networks', 'Select one or more DEIMS networks')$
    step('panel_search_settings', 'search settings', 'Download or copy your search settings here, or paste (re-apply) earlier settings.')

# jsons  <- head(jsons, 20)

## TODO ============================== 
## - replace multiple occurence of current_upload with reactive value


server <- function(input, output, session) {
    ## store session for later use (if necessary, check redundancy!
    session = getDefaultReactiveDomain()
    shinyjs::useShinyjs()
    

    output$file_info <- renderUI(tags$small(p(paste('file:', data_file_name)),
                                            p(paste('last updated:', file.info(file.path('www/data', data_file_name))$ctime))
    ))
    
    
    output$timestamp <- renderUI(p(class = 'float-right', tags$span(class = 'badge', 'DEIMS data as of:'),
                                   tags$span(class = 'badge badge-info', 
                                             as.Date(file.info(file.path('./www/data/', data_file_name))$mtime))
    ))
    
    observeEvent(input$guide,  guide$init()$start(1))
    
    
    
    ## the .RDATA file has to contain the json info as an object named "jsons" !
    load(file.path('./www/data/', data_file_name))
    # load('./www/data/site-details-as-json.RData')
    # load('./www/data/attribute_list.RData')

    
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
    
    ## initialise output data table, otherwise DT extension "ellipsis" throws rendering error
    output$active_site_table <- 
        DT::renderDT(datatable(
            data = data.frame(msg = 'preparing table')
        ))
    
    
    
    url_frag <- 'https://deims.org/exp/tagged_sites_'
    assign('tagged_sites_count', value = read.csv(paste0(url_frag,'count')))
    assign('tagged_sites_sub', value = read.csv(paste0(url_frag,'sub')))
    assign('tagged_sites_descriptive', value = read.csv(paste0(url_frag,'descriptive')))
    
    countable_attributes <- c(
        'attributes.environmentalCharacteristics.eunisHabitat.label'
    )
    
    
    
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
    
    # =============== hide loading message when data are processed and remind to select network:

    observe({
        if(length(active_site_ids()) < 1) {
            shinytoastr::toastr_info("Please select DEIMS network.", newestOnTop = TRUE, timeOut = 3000,
                                     position = 'top-full-width', preventDuplicates = TRUE,
                                     showMethod = 'slideDown')
        }
    }) %>% bindEvent(active_site_data(), ignoreInit = TRUE)
    
    observe({
        if(!is.null(site_details)) shiny::removeUI('#loading_message')
    }) %>% bindEvent(active_site_data())
    # ==========================
    
    
    
    observe({
        shinytoastr::toastr_success("DEIMS data loaded", position = 'top-full-width',
                                    showMethod = 'slideDown', timeOut = 3000)
    }) %>% bindEvent(active_site_data(), once = TRUE)
    
    observe({
        if(length(active_site_ids()) < 1) {
            shinytoastr::toastr_info("Please select DEIMS network.", newestOnTop = TRUE, timeOut = 0,
                                     position = 'top-full-width',
                                     showMethod = 'slideDown')
        }
    }) %>% bindEvent(active_site_data())
    
    
    
    ## ==================== sidebar panel, network / site selection ====================
    network_choices <- get_network_names(site_details)
    ## network_memberships <- get_network_memberships(site_details)
    
    
    
    
    updateSelectizeInput(session = session,
                         inputId = 'select_net',
                         label = NULL,
                         choices = network_choices,
                         selected = network_choices
    )
    
    
    active_net_ids <- reactiveVal(c('4742ffca-65ac-4aae-815f-83738500a1fc'))
    active_site_ids <- reactiveVal(NULL)
    active_attributes <- reactiveVal(c('id','title'))
    active_site_data <- reactiveVal({
        site_details %>% select('id','title') %>% head(0)
    })
    
    
    
    
    
    
    observeEvent(input$tree_attributes, {       
        selection <- input$tree_attributes %>% get_selected(., format='names')
        branches <- selection %>% map(~ paste0(attr(.x,'ancestry'),collapse='.'))
        leaves <- selection %>% unlist
        attribute_vector <- 
            paste(branches,leaves,sep='.') %>%
            gsub('(attributes|parent|children|name)[0-9]','\\1',.) %>%
            gsub('^\\.','',.) %>%
            ## replace trailing '2' (which is automatically added to the attribute name
            ## when building the the tree selector to resolve
            ## conflicts with reserved names, e.g.: 'count') - so that attributes are found.
            gsub('2$', '', .) %>% 
            .[. %in% names(site_details)]
        attribute_vector <- c('id', attribute_vector) %>% unique
        active_attributes(attribute_vector)
        active_site_data(site_details %>% 
                             filter(id %in% active_site_ids()) %>%
                             select(active_attributes()) %>%
                             left_join(foreign_data(), by=c('id'='id'))
        )
    })
    
    search_settings <- reactiveVal()
    observe({
        one_hot_attributes <- input$one_hot_picker
        search_settings(parse_settings(a = active_attributes(),
                                       n = active_net_ids(),
                                       f = input$one_hot_picker
        )
        )
        updateTextInput(inputId = 'search_settings', session = session,
                        value = search_settings()
        )
    }) %>%
        bindEvent(input$select_net, input$tree_attributes, input$one_hot_picker,
                  active_net_ids()
                  )
    
    
    ## save settings to browser storage:
    ## observe({
    ##     if (input$save_to_browser <= 0){
    ##         ## On initialization, set the value of the text editor to the current val.
    ##         updateTextInput(session, "text", value=isolate(input$store)$text)
    ##         return()
    ##     }
    ##     updateStore(session, "text", isolate(input$search_settings))
    ## })
    
 
    
    observe({
        active_net_ids({input$select_net %>% as.vector %>% as.character})
        active_site_ids(
            get_active_sites_from_active_networks(site_details, active_net_ids())
        )
        
        active_site_data(site_details %>% 
                             filter(id %in% active_site_ids()) %>%
                             select(active_attributes()) %>%
                             left_join(foreign_data(), by=c('id'='id'))
        )
    }) %>%
        bindEvent(input$select_net, input$clear_net_selection, input$select_all_nets)
    
    
    
    
    
    observeEvent(input$clear_net_selection,{
        updateSelectizeInput(session,'select_net',selected = NA)
        updateTextAreaInput(session,'custom_site_input_textarea','')
        active_site_ids(NULL)
    })
    
    observeEvent(input$select_all_nets,{
        active_site_ids(
            get_active_sites_from_active_networks(site_details, network_choices)
        )
        updateSelectizeInput(session,'select_net', selected = network_choices)
    })
    
    shinyjs::onevent(event = 'click', id = 'custom_site_input_textarea',
                     shinyjs::runjs('$("#custom_site_input_textarea").select()'))
    
    
    observeEvent(input$apply_custom_site_input,{
        shiny::updateSelectizeInput(session,'select_net',selected = NA)
        active_site_ids(stringr::str_extract_all(input$custom_site_input_textarea,
                                                 '\\w{8}(-\\w{4}){3}-\\w{12}') %>%
                            unlist()
        )
        active_site_data(site_details %>% 
                             filter(id %in% active_site_ids()) %>%
                             select(active_attributes())
        )
    })
    
    observeEvent(input$btn_copy_search_settings, toastr_success("copied to clipboard", position = 'bottom-left'))
    observeEvent(input$btn_save_search_settings, toastr_success("saved locally", position = 'bottom-left'))
    output$btn_save_search_settings <- downloadHandler(
        filename = function() {paste0('search-settings-',Sys.Date(),'.txt')},
        contentType = 'application/json',
        content = function(file) {
            writeLines(paste(input$search_settings, collapse = ", "), file)
        }
    )
    

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
    
    
    updatePickerInput(
        inputId = 'one_hot_picker', 
        session = session,
        choices = one_hot_value_combos
    )
    
    
    observe({
        json <- jsonlite::parse_json(input$search_settings)
        active_attributes(unlist(json$attributes))
        all_attributes <- c(json$flag_attributes, input$one_hot_picker) %>% unique
        
        shiny::updateSelectizeInput('select_net', session = session,
                                    selected = unlist(json$network_ids)
        )
        
        all_attributes %>%
            walk(~{
                if(length(.x) > 0 ){
                    choice_string <- strsplit(.x,'\\|') %>% unlist
                    hot_attribute <- choice_string[1]; hot_value  <- choice_string[2]
                    hot_attribute_short <- hot_attribute %>% gsub('.*\\.','',.)
                    if(!hot_attribute %in% active_attributes()) {
                        active_attributes(c(active_attributes(), hot_attribute ))} 
                    col_pos <- grep(hot_attribute, active_attributes())
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
    }) %>%
        bindEvent(input$one_hot_picker,
                  input$btn_apply_search_settings,
                  input$tree_attributes,  ## selects attributes (columns)
                  ignoreInit = TRUE)
    
    
    ## find available plugins with DT:::available_plugins()

    output$active_site_table <- 
        DT::renderDT(datatable(
            class = "display nowrap",
            data = active_site_data(),
            escape = FALSE,
            rownames = FALSE,
            colnames = prettify_colnames(names(active_site_data())),
            options = list(
                # buttons = c('copy','csv'),
                scrollY =  '30vh',     
                scrollX = TRUE,
                dom = 'Blftp',
                paging = TRUE,
                pageLength = 10,
                extensions = c(#'Buttons',
                    'Scroller'),
                plugins = c('ellipsis'),
                language = list(zeroRecords = "no networks or sites selected"),
                
                
                columnDefs = list(list(
                    targets = (2:ncol(isolate(active_site_data()))) - 1,
                    render = JS(
                        "function(data, type, row, meta) {",
                        "var maxLen = 15",
                        "return type === 'display' && data.length > maxLen ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, maxLen) + '...</span>' : data;",
                        "}")
                )
                )

            )
        ))

    download_products <- reactiveValues(filepath_xlsx = NULL) 
    ## disable download buttons on init:
    observe({
        output$dummy <- renderPrint(class(active_site_data()))
        shinyjs::disable('download_zip')
        tmp_file <- paste0(tempfile(), '.xlsx')
        write.xlsx(x = active_site_data(), file = tmp_file)
        download_products$filepath_xlsx  <- tmp_file
        output_dir_name <- file.path(tempdir(),'output')
        slug <- file.path(output_dir_name, paste0("DEIMS-sitedata-", Sys.Date()))
        unlink(output_dir_name, recursive = TRUE, force = TRUE)
        dir.create(output_dir_name)
        date_stamp = as.character(Sys.Date())
        write.csv(active_site_data(), file = paste0(slug, '.csv'))
        write.xlsx(x = active_site_data(),
                   file = paste0(slug,'.xlsx'),
                   sheetName = 'DEIMS site data', rowNames = FALSE
        )
        writeLines(paste(input$search_settings, collapse = ", "), 
                   paste0(file.path(output_dir_name,'search-settings.json')))
        
        writeLines(sprintf('
---
title: "provenance"
author: %s
date: %s
output: pdf_document
---

- DEIMS data source: [https://deims.org/search/sites](https://deims.org/search/sites)
- DEIMS data retrieved on: %s
- licensed under a Creative Commons Attribution-Non Commercial 4.0 International License 
([CC BY-NC 4.0 International](https://creativecommons.org/licenses/by-nc/4.0/))
- processed for dowload via [https://elter-enrich.datalabs.ceh.ac.uk/](https://elter-enrich.datalabs.ceh.ac.uk/)

', 'automatically generated', Sys.time(),
as.Date(file.info('./www/data/site-details-as-json.RData')$mtime) ## retrieved on
        ),
paste0(file.path(output_dir_name,'provenance.txt'))
        )
        
        #### uncomment to render pdf:
        ## rmarkdown::render(file.path(output_dir_name, 'provenance.txt'))
        
        download_products$filepath_zip <- paste0(slug, '.zip')
        zip::zip(zipfile = download_products$filepath_zip,
                 files = dir(output_dir_name, full.names = TRUE),
                 mode = 'cherry-pick'
        )
        
        shinyjs::enable('download_zip')
    })
    
    output$download_zip <- 
        downloadHandler(contentType = 'application/zip',
                        filename = function() {paste0("DEIMS-sitedata-", Sys.Date(), ".zip")},
                        content = function(file) {file.copy(download_products$filepath_zip, file)}
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
    
    
    ## ============================== Map tab ====================
    
    observe({
        map_data <- site_details %>%
            filter(id %in% active_site_ids()) %>%
            select(site_id = id, title,
                   coords = attributes.geographic.coordinates
            ) %>%
            filter(!is.na(coords)) %>% 
            st_as_sf(wkt = "coords") %>%
            ## at least one European site is multipoint, which breaks leaflet,
            ## so convert it from MULTIPOINT to POINT:
            st_cast('POINT')
        

        elter_icon <- makeIcon(#iconUrl = "elter_icon.svg"
        )

        
        
         output$map <- renderLeaflet({
             leaflet()  %>%
                 addTiles(options = tileOptions(noWrap = TRUE))  %>%
                 addMarkers(data = st_geometry(map_data),
                            popup = paste0('<a href="https://deims.org/',
                                           map_data$site_id,'" target="_blank">',map_data$title,'</a>'),
                            icon = elter_icon,
                            clusterOptions = markerClusterOptions()
                 )
         })
    })

    
    
}


