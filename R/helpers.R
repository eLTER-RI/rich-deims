## dependencies: purrr, htmltools (or shiny), jsonlite

## glue components of site attributes to HTML-string
## for later display in table. e. g. combining URL and title of
## 'related ressources' into a link

## takes 560 seconds for 1240 sites
get_all_sites_as_big_json <- function(){
    ids <- jsonlite::fromJSON('https://deims.org/api/sites/') %>% 
         ## head %>%
        .$id %>% .$suffix
    get_single_json <- function(id){
        jsonlite::read_json(paste0('https://deims.org/api/sites/',id),
                            simplifyVector = TRUE ## important!
                            )
    }
    big_tree_list <- list()
    ids %>%
        purrr::walk(function(id){
            big_tree_list[[id]] <<- get_single_json(id)
        })
    big_tree_list
}

wormify <- function(l, url = '', title=''){
  sep = '|'
  urls <- pluck(l, url)
  titles <- pluck(l, title)
  intro  <- span(length(titles), class='badge')
  ifelse(url != '',
         return(paste0(intro,'<a href=',urls,'>', titles,'</a>',
                       collapse = sep)),
         paste(intro,titles, collapse = sep))
}

vector_as_named <- function(v){
    set_names(v, v)
}


## require(tidyverse)


## get a basic site description (title, ID, centroid, changed)
## from DEIMS API
## no need to store locally as this takes roughly a second
## sites_basic <- jsonlite::fromJSON('https://deims.org/api/sites/')
## sites_basic %>% nrow


## takes a nested JSON (like the JSON response from DEIMS api
## and returns a vector of attribute paths (like: foo.attributes.affiliation.networks.id)
get_attribute_strings <- function(jsons){
    get_strings <- function(l){l %>% rapply(.,function(li) li) %>% names}
    jsons %>% 
        map(~ get_strings(.x)) %>%
        unlist %>%
        gsub('[0-9]*$','',.) %>% ## remove trailing digits (for multiple entries like url1, url2 etc.
        unique %>%
        .[. != '']
}



## use this for the full JSON response,
## here, this response is contained in the object 'jsons'
get_all_attribute_strings <- function(json_response){
    get_attributes_of_nested_list <- function(l){
        names(rapply(l, function(li) li)) %>% unlist %>%
            gsub('[[:digit:]]$','',.) %>%## remove trailing enumerator for JSON array entries
            unique
    }
    json_response %>%
        map(~ get_attributes_of_nested_list(.x)) %>%
        reduce(c) %>%
        unique
}



get_site_details_from_json <- function(jsons){
    attribute_strings <- get_attribute_strings(jsons)
    setNames(
        attribute_strings %>%
        map(., function(attribute_path){
            plucklist = strsplit(attribute_path,'\\.')  %>% unlist    
            jsons %>%
                map(~ pluck(.x, !!!plucklist),.default = NA) %>%
                map(~ paste(.x, collapse='|')) %>%
                unlist
        }),
        attribute_strings
    ) %>%
        as_tibble %>%
        rename('id' = 'id.suffix')
}


get_one_hot_candidate_attributes <- function(data){
## returns names of columns which have more than one '|'-separated values
## in any cell
    data %>%
    rowwise %>%
    mutate(across(everything(), ~ length(strsplit(as.character(.x),'\\|') %>% unlist))) %>%
    ungroup %>%
    summarise_all(max) %>% unlist %>% 
    .[. > 1] %>% 
    names %>%
    ## exclude the following too verbose attributes:
    setdiff(.,
            c("attributes.general.abstract")
            )
}


get_network_names <- function(site_details){
    data.frame(
        id = site_details %>%  select(attributes.affiliation.networks.network.id.suffix) %>%
            separate_rows(.,1,sep='\\|') %>% pull,
        name = site_details %>%  select(attributes.affiliation.networks.network.name) %>%
            separate_rows(.,1,sep='\\|') %>% pull
    ) %>% 
        filter(id != '') %>% distinct %>%
        pull(id, name)
}


## return rows for sites which belong to one or more of active (=selected) networks:
get_active_sites_from_active_networks <- function(data, active_network_ids){
    data %>%
        select(net_id = "attributes.affiliation.networks.network.id.suffix",id) %>%
        filter(net_id != '') %>%
        separate_rows(net_id, sep='\\|') %>%
        filter(net_id %in% active_network_ids) %>%
        pull(id) %>% unique
}



extract_ids_from_textinput <- function(raw_text){
raw_text %>% unlist %>% as.character %>%
    gsub('[^a-z0-9-]+','|',.) %>%
    strsplit(., split='\\|') %>% unlist
}

prettify_colnames <- function(col_titles){
    col_titles %>%
        gsub('attributes\\.','',.) %>%
        gsub('(.*\\.)','<small>\\1</small>',.) %>%
        gsub('\\.','\n',.)
}


