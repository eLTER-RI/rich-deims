require(tidyverse)
require(shinyTree)
require(data.tree)
source('./helpers.R')



load("./www/data/site-details-as-json.RData")

ls()

get_site_details_from_json(jsons) %>%
    .[3:10, 20:25]


jsons_orig <- jsons



load('C:/Users/offenthaler/Desktop/fremd/eLTER/networkR/app/R/www/data/site-details-as-json-20220124.RData')
jsons_neu <- jsons




id <- '45722713-80e3-4387-a47b-82c97a6ef62b'
json_object <- jsonlite::read_json(paste0('https://deims.org/api/sites/',id))
json_string <-   jsonlite::serializeJSON(json_object) #%>% is.character


json_object %>%
    pluck(!!!list('attributes','affiliation','networks')) %>%
    length


gsub('[[:digit:]]','','asdf23')



data.frame(json = json_string %>% as.character) %>%
  mutate(json = map(json, ~ jsonlite::fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json)

## id with multiple networks:
## '45722713-80e3-4387-a47b-82c97a6ef62b'


## get all attribute strings by recursing
## through all jsons



example_list <- list('entryA' = list('abc'=3,'bbc'=4), 'entryB'=list('abc'=2345,'cbc'=3))


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


all_attribute_strings <- 
    get_all_attribute_strings(jsons_neu) %>%
    purrr::set_names(.,.)


jsons_neu %>%
    head(10) %>%


list(hugo = json_object) %>%
     map(~{
         pluckstring <- 'attributes.affiliation.networks.network.id.prefix' %>% strsplit(.,'\\.') %>% unlist
         print(pluckstring)
         pluck(.x, !!!pluckstring, .default = 'nix') %>%
             paste0(collapse = '|') ## collapse JSON arrays into string
    })


all_attribute_strings %>%
    head(5) %>%
    map_df(~{
        pluckstring <- strsplit(.x,'\\.') %>% unlist
        print(pluckstring)
        ## jsons_neu[[1]] %>%
        ##     map(~ pluck(!!!pluckstring))
    })


all_attribute_strings %>% .[grep('network',.)]
