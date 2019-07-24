#################################
###### elections      ###########
#################################


#####################################################
##### 1. Arrange queries before calling mongoDB #####
#####################################################
# [] To be developed

# elec = "cong", "sen", "local", ["ref"], "euro", "cabildo", ["aut"]
# year = year in which the election took place
# format = "long", "wide"
# level =  "national", "regional", "province", "island"[only if cabildo], "local", "district", "section", "ballot"
# data = "summary", "particip", "candidat", "all" 
# format = "long", "wide"




arrange_queries <- function(elec, year, 
                            caut = NULL, cprov = NULL, cisland = NULL, cmun = NULL, cdist = NULL, csec = NULL, cballot = NULL,
                            cmultiple = NULL,
                            level = "national",
                            data = "summary",
                            format = "long"){

## 1. load libraries ##
library(tidyverse)

## 2. Initial transformations and checks ##
## Transform NULL inputs ##
if(is.null(caut)){caut <- NA}
if(is.null(cprov)){cprov <- NA}
if(is.null(cisland)){cisland <- NA}
if(is.null(cmun)){cmun <- NA}
if(is.null(cdist)){cdist <- NA}
if(is.null(csec)){csec <- NA}
if(is.null(cballot)){cballot <- NA}
  
## check argument data ##
if(!all(data %in% c("all", "particip", "votes", "summary"))) stop("argument data is wrong")

if(data == "summary" | data ==  "all"){
  data_orig <- data
  data <- c("particip", "votes")
} else {
    data_orig <- data
  }
    
## check argument year ##
if(elec == "cong" & !all(year %in% c(1977, 1979, 1982, 1986, 1989, 1993, 1996, 2000, 2004, 2008, 2011, 2015, 2016, 2019))) stop("year provided is not valid")
if(elec == "sen" & !all(year %in% c(1986, 1989, 1993, 1996, 2000, 2004, 2008, 2011, 2015, 2016, 2019))) stop("year provided is not valid")
if(elec == "euro" & !all(year %in% c(1987, 1989, 1994, 1999, 2004, 2009, 2014))) stop("year provided is not valid")
if(elec == "local" & !all(year %in% c(1979, 1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015))) stop("year provided is not valid")
if(elec == "ref" & !all(year %in% c(1986, 2005))) stop("year provided is not valid")
if(elec == "cabildo" & !all(year %in% c(1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015))) stop("year provided is not valid")
  
## check argument elec and change to code ##
if(length(elec) > 1) stop("elec must be length 1")  
if(elec != "cong" & elec != "sen" &
   elec != "local" & elec != "ref" &
   elec != "euro" & elec != "cabildo") stop("elec not defined correctly")

switch (elec,
  "cong" = elec <- 2,
  "sen" = elec <- 3,
  "local" = elec <- 4,
  "ref" = elec <- 1,
  "euro" = elec <- 7,
  "cabildo" = elec <- 6,
  "aut" = elec <- 5
)  

## check level argument ##
if(!(level %in% c("national", "regional", "province", "island", "local", "district", "section", "ballot"))) stop("level does not exist")
if(length(level) > 1) stop("level must be length 1")
  
## check format argument ##
if(format != "long" & format != "wide") stop("format must be 'long' or 'wide'")
  

## 3. set data frame for extractions and fill in ##    
## set data to extract ##    
 data_to_extract <- data.frame(elec = integer(0), 
                                year = integer(0),
                                caut = integer(0),
                                cprov = integer(0),
                                cisland = integer(0),
                                cmun = integer(0), 
                                cdist = integer(0), 
                                csec = character(0), 
                                cballot = character(0),
                                level = character(0),
                                data = character(0),
                                stringsAsFactors = FALSE) 
if(is.null(cmultiple)){  
  ## confirm level(s) and for ech generate query##
if("national" %in% level) {
  caut <- 99
  data_to_extract_national <- expand.grid(elec, year, level, data, caut, stringsAsFactors = FALSE)
  colnames(data_to_extract_national) <- c("elec", "year", "level", "data", "caut")
  data_to_extract <- bind_rows(data_to_extract, data_to_extract_national)
} 
if("regional" %in% level) {
  data_to_extract_regional <- expand.grid(elec, year, caut, level, data, stringsAsFactors = FALSE)
  colnames(data_to_extract_regional) <- c("elec", "year", "caut", "level", "data")
  data_to_extract <- bind_rows(data_to_extract, data_to_extract_regional)
} 
 if("province" %in% level) {
   data_to_extract_province <- expand.grid(elec, year, caut, cprov, level, data, stringsAsFactors = FALSE)
   colnames(data_to_extract_province) <- c("elec", "year", "caut", "cprov", "level", "data")
   data_to_extract <- bind_rows(data_to_extract, data_to_extract_province)
 } 
if("island" %in% level) {
    data_to_extract_island<- expand.grid(elec, year, caut, cprov, cisland, level, data, stringsAsFactors = FALSE)
    colnames(data_to_extract_island) <- c("elec", "year", "caut", "cprov", "cisland", "level", "data")
    data_to_extract <- bind_rows(data_to_extract, data_to_extract_island)
  } 
 if("local" %in% level) {
   data_to_extract_local <- expand.grid(elec, year, caut, cprov, cmun, level, data, stringsAsFactors = FALSE)
   colnames(data_to_extract_local) <- c("elec", "year", "caut", "cprov", "cmun", "level", "data")
   data_to_extract <- bind_rows(data_to_extract, data_to_extract_local)
 } 
 if("district" %in% level) {
   data_to_extract_district <- expand.grid(elec, year, caut, cprov, cmun, cdist, level, data, stringsAsFactors = FALSE)
   colnames(data_to_extract_district) <- c("elec", "year", "caut", "cprov", "cmun", "cdist", "level", "data")
   data_to_extract <- bind_rows(data_to_extract, data_to_extract_district)
 } 
if("section" %in% level) {
  if(!is.character(csec) & !is.na(csec)) stop("csec must be a character")
  data_to_extract_section <- expand.grid(elec, year, caut, cprov, cmun, cdist, csec, level, data, stringsAsFactors = FALSE)
  colnames(data_to_extract_section) <- c("elec", "year", "caut", "cprov", "cmun", "cdist", "csec", "level", "data")
  data_to_extract <- bind_rows(data_to_extract, data_to_extract_section)
  } 
if("ballot" %in% level) {
  if(!is.character(csec) & !is.na(csec)) stop("csec must be a character")
  if(!is.character(cballot) & !is.na(cballot)) stop("cballot must be a character")
  data_to_extract_ballot <- expand.grid(elec, year, caut, cprov, cmun, cdist, csec, cballot, level, data, stringsAsFactors = FALSE)
  colnames(data_to_extract_ballot) <- c("elec", "year", "caut", "cprov", "cmun", "cdist", "csec", "cballot", "level", "data")
  data_to_extract <- bind_rows(data_to_extract, data_to_extract_ballot)
  }
} else {
## check multiple table and generate queries if it exists ##
if(!is.data.frame(cmultiple)) stop("cmultiple must be a data frame")
if(colnames(cmultiple) != colnames(data_to_extract)) stop("colnames of cmultiple are worng")  
data_to_extract <- return_df  
}

## finalise queries with  level and collection ##
data_to_extract <- mutate(data_to_extract, 
                          data_orig = data_orig,
                          collect = case_when(
  level == "national" & data == "particip" ~ "upper_muni_data",
  level == "national" & data == "votes" ~ "upper_muni_candidatures",
  level == "regional" & data == "particip" ~ "upper_muni_data",
  level == "regional" & data == "votes" ~ "upper_muni_candidatures",
  level == "province" & data == "particip" ~ "upper_muni_data",
  level == "province" & data == "votes" ~ "upper_muni_candidatures",
  level == "island" & data == "particip" ~ "upper_muni_data",
  level == "island" & data == "votes" ~ "upper_muni_candidatures",
  level == "local" & data == "particip" ~ "muni_data",
  level == "local" & data == "votes" ~ "muni_candidatures",
  level == "district" & data == "particip" ~ "precint_data",
  level == "district" & data == "votes" ~ "precint_candidatures",
  level == "section" & data == "particip" ~ "precint_data",
  level == "section" & data == "votes" ~ "precint_candidatures",
  level == "ballot" & data == "particip" ~ "precint_data",
  level == "ballot" & data == "votes" ~ "precint_candidatures"
))  

## 4. prepare output ##  
## return_df for extraction## 
return_df <- data_to_extract %>% 
  arrange(elec, year, caut, cprov, cisland, cmun, cdist, csec, cballot, level, data)
return(return_df)

}

#####################################################
##### 2. Extract data                           #####
#####################################################
# [] To be developed

# queries = data frame with queries


extract_data <- function(queries){

## 1. load libraries ##
  
  library(mongolite)
  library(tidyverse)
  
## 2. set mongo coneection and output files ##
  ## mongoDB credential
  my_credential <- "mongodb://pablocal:pablocal1990@sociocav.usal.es:27017"
  
  ## output
  list_names <- character()
  return_list <- list()
 
## 3. generate queries ##  
  ## create find and extract query
for(i in 1:nrow(queries)){  

    
  if(queries$caut[i] == 99 & queries$level[i] == "national"){
    caut <- paste0(', "aut_id": 99')
  } else if (!is.na(queries$caut[i]) & queries$level[i] == "regional") {
    caut <- paste0(', "aut_id": ', queries[i, "caut"], ', "prov_id": 99')
  } else if (is.na(queries$caut[i]) & queries$level[i] == "regional") {
    caut <- paste0(', "aut_id": {"$ne" : 99}', ', "prov_id": 99')
  } else if (!is.na(queries$caut[i])){
    caut <- paste0(', "aut_id": ', queries[i, "caut"])
  } else {
    caut <- ""
  }
  
if(is.na(queries$cprov[i]) & queries$level[i] == "province"){
    cprov <- paste0(', "prov_id": {"$ne" : 99}', ', "constituency_id": 9')
  } else if (!is.na(queries$cprov[i])) {
    cprov <- paste0(', "prov_id": ', queries[i, "cprov"])
  } else {
    cprov <- ""
  }
  
  if(is.na(queries$cisland[i]) & queries$level[i] == "island"){
    cisland <- paste0(', "constituency_id": {"$ne" : 9}')
  } else if (!is.na(queries$cisland[i])) {
    cisland <- paste0(', "constituency_id": ', queries[i, "cisland"])
  } else {
    cisland <- ""
  }
  
  if(is.na(queries$cmun[i])){
    cmun <- ""
  } else if(!is.na(queries$cmun[i]) & queries$level[i] == "local") {
    cmun <- paste0(', "mun_id": ', queries[i, "cmun"], ', "dist_mun_id": 99')
  } else {
    cmun <- paste0(', "mun_id": ', queries[i, "cmun"])
  }
  
  if(is.na(queries$cdist[i])){
    cdist <- ""
  } else {
    cdist <- paste0(', "dist_id": ', queries[i, "cdist"])
  }
  
  if(is.na(queries$csec[i])){
    csec <- ""
  } else {
    csec <- paste0(', "sec_id": ', '"', queries[i, "csec"], '"')
  }

  if(is.na(queries$cballot[i])){
    cballot <- ""
  } else {
    cballot <- paste0(', "precint_id": ', '"', queries[i, "cballot"], '"')
  }
  
  query <- paste0("{", '"elec_id":', queries[i, "elec"], 
                  caut,
                  cprov,
                  cisland,
                  cmun,
                  cdist,
                  csec,
                  cballot,
              ', "year":', queries[i, "year"],
              "}")
  
## 4. extract data ##  
  ## find data and extract
  mongo_db <- mongo(collection = queries$collect[i], 
                    db = "elec_data", 
                    url = my_credential)
 
   
  extract <- mongo_db$find(
    query = query
  )
  
if(nrow(extract) == 0) stop(paste0(query, "is empty"))  

## 5. extract candidatures names ##     
  if(str_sub(queries$collect[i], str_length(queries$collect[i]) - 11, str_length(queries$collect[i]) ) == "candidatures"){
    
    # connect to mongo and extract
    mongo_db <- mongo(collection = "candidatures", 
                      db = "elec_data", 
                      url = my_credential)
    
    query <- paste0("{", '"elec_id":', queries[i, "elec"], 
                    ', "year":', queries[i, "year"],
                    "}")
    
    candidatures <- mongo_db$find(
      query = query,
      fields = '{"_id": false, "candidature_acron": true, "candidature_id": true, "candidature_id_country": true}'
      )   

  } 
  

## 7. extract candidates names ##
  
  if(str_sub(queries$collect[i], str_length(queries$collect[i]) - 11, str_length(queries$collect[i]) ) == "candidatures" & queries$elec[i] == 3){
    
    # connect to mongo and extract
    mongo_db <- mongo(collection = "candidates", 
                      db = "elec_data", 
                      url = my_credential)
    
    query <- paste0("{", '"elec_id":', queries[i, "elec"], 
                    ', "year":', queries[i, "year"],
                    "}")
    
    candidates <- mongo_db$find(
      query = query,
      fields = '{"_id": false, "constituency_id": true, "prov_id": true, "mun_id": true, "candidature_id": true, "candidate_name": true}'
    )   
    
  candidates <- candidates %>% 
    rename(candidature_id_party = candidature_id) %>% 
    mutate(candidature_id = prov_id * 10000 + constituency_id * 1000 + mun_id) %>% 
    select(candidature_id, candidate_name, candidature_id_party)
    
  }
  

## 8. Join candidate and candidatures files with extract
  
  
  if(str_sub(queries$collect[i], str_length(queries$collect[i]) - 11, str_length(queries$collect[i]) ) == "candidatures" & queries$elec[i] == 3){
    
    extract <- left_join(extract, candidates, by = "candidature_id")
    
    extract <- extract %>% 
      rename(candidate_id = candidature_id,
             candidature_id = candidature_id_party)
      
    extract <- left_join(extract, candidatures, by = "candidature_id")
    
    
  } else if(str_sub(queries$collect[i], str_length(queries$collect[i]) - 11, str_length(queries$collect[i]) ) == "candidatures"){
    
    # combine with extract
    extract <- left_join(extract, candidatures, by = "candidature_id")
    
  }
  

# 9. Clean the cnadidates and candidatures names
  
if(str_sub(queries$collect[i], str_length(queries$collect[i]) - 11, str_length(queries$collect[i])) == "candidatures" & queries$elec[i] == 3){

  extract <- extract %>% 
    rowwise() %>% 
    mutate(candidate_name = str_squish(candidate_name))
  
}  
  
if(str_sub(queries$collect[i], str_length(queries$collect[i]) - 11, str_length(queries$collect[i]) ) == "candidatures"){
  
  # generate list of national names
  extract_names <- extract %>% 
    group_by(candidature_id_country, candidature_id, candidature_acron) %>% 
    summarise(count = n(), votes = sum(votes_candidature)) %>% 
    arrange(candidature_id_country, -votes) %>% 
    group_by(candidature_id_country) %>% 
    summarise(candidature_acron = first(candidature_acron))
  
  # join and clean final variable  
  extract <- extract %>% 
    select(-candidature_id, -candidature_acron) %>% 
    left_join(extract_names, by = "candidature_id_country") %>% 
    select(-candidature_id_country) %>% 
    mutate(candidature_acron = str_trim(candidature_acron, side = "both"),
           candidature_acron = str_remove_all(candidature_acron, "\\,"),
           candidature_acron = str_remove_all(candidature_acron, "\\."),
           candidature_acron = str_remove_all(candidature_acron, "\\'"),
           candidature_acron = str_remove_all(candidature_acron, "\\´"),
           candidature_acron = str_remove_all(candidature_acron, "\\`"),
           candidature_acron = str_remove_all(candidature_acron, "\\-$"),
           candidature_acron = str_remove_all(candidature_acron, "[:blank:]"),
           candidature_acron = str_replace_all(candidature_acron, "[:punct:]", "_"))
  
} 
  
  
## 6. insert in list ## 
  return_list[[length(return_list)+1]] <- extract
  list_names <- c(list_names, paste0("query", i))
  
} 
  
## 7. Finalise return list ##   
  return_list[[length(return_list)+1]] <- queries
  names(return_list) <- c(list_names, "meta")
  return(return_list)
}

#####################################################
##### 3. Prepare output                          #####
#####################################################
# [] To be developed

# query_list = list with queries and metadata
# format = "long", "wide"
  
format_output <- function(query_list, format){

## 1. load packages ##
library(tidyverse)  
  
## 2. set intermediate objects ##  
nqueries <- length(query_list) - 1
summary_list <- list()

## 3. format the data ##
for(i in 1:nqueries){
  
    meta <- query_list$meta[i,]
    level <- meta$level[1]
    data <- query_list[[i]]
  
    if (str_sub(meta$collect[1], str_length(meta$collect[1]) - 11, str_length(meta$collect[1])) == "candidatures" & meta$elec[1] == 3) {
    
      if (level == "national"){
        data <- data %>% 
          select(candidature_acron, votes_candidature) %>% 
          rename(Party = candidature_acron, Votes = votes_candidature) %>% 
          group_by(Party) %>% 
          summarise_all(sum) %>% 
          arrange(-Votes)
      } else if (level == "regional"){ 
        data <- data %>% 
          select(aut_id, candidature_acron, votes_candidature) %>% 
          rename(CAUT = aut_id, Party = candidature_acron, Votes = votes_candidature) %>% 
          group_by(CAUT, Party) %>% 
          summarise_all(sum) %>% 
          arrange(CAUT, -Votes)
      } else if (level == "province"){ 
        data <- data %>% 
          select(aut_id, prov_id, candidature_acron, candidate_name, votes_candidature) %>% 
          rename(CAUT = aut_id, CPROV = prov_id, Party = candidature_acron, Votes = votes_candidature, Candidate = candidate_name) %>% 
          group_by(CAUT, CPROV, Party, Candidate) %>%
          summarise_all(sum) %>% 
          arrange(CAUT, CPROV, -Votes)
      } else if (level == "island"){ 
        data <- data %>% 
          select(aut_id, prov_id, constituency_id, candidature_acron, candidate_name, votes_candidature) %>% 
          rename(CAUT = aut_id, CPROV = prov_id, CISLAND = constituency_id, Party = candidature_acron, Votes = votes_candidature, Candidate = candidate_name) %>% 
          group_by(CAUT, CPROV, CISLAND, Party, Candidate) %>%
          summarise_all(sum) %>% 
          arrange(CAUT, CPROV, CISLAND, -Votes)
      } else if (level == "local"){
        data <- data %>% 
          select(prov_id, mun_id, candidature_acron, candidate_name, votes_candidature) %>% 
          rename(CPROV = prov_id, CMUN = mun_id, Party = candidature_acron, Votes = votes_candidature, Candidate = candidate_name) %>% 
          group_by(CPROV, CMUN, Party, Candidate) %>% 
          summarise_all(sum) %>% 
          arrange(CPROV, CMUN, -Votes)
      } else if (level == "district") {
        data <- data %>% 
          select(aut_id, prov_id, mun_id, dist_id, candidature_acron, candidate_name, votes_candidature) %>% 
          rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, Party = candidature_acron, Votes = votes_candidature, Candidate = candidate_name) %>% 
          group_by(CAUT, CPROV, CMUN, CDIST, Party, Candidate) %>% 
          summarise(Votes = sum(Votes)) %>% 
          arrange(CAUT, CPROV, CMUN, CDIST, -Votes)
      } else if (level == "section") {
        data <- data %>% 
          select(aut_id, prov_id, mun_id, dist_id, sec_id, candidature_acron, candidate_name, votes_candidature) %>% 
          rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CSEC = sec_id, Party = candidature_acron, Votes = votes_candidature, Candidate = candidate_name) %>% 
          group_by(CAUT, CPROV, CMUN, CDIST, CSEC, Party, Candidate) %>% 
          summarise(Votes = sum(Votes)) %>% 
          arrange(CAUT, CPROV, CMUN, CDIST, CSEC, -Votes)
      } else if (level == "ballot"){  
        data <- data %>% 
          select(aut_id, prov_id, mun_id, dist_id, sec_id, precint_id, candidature_acron, votes_candidature) %>% 
          rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CSEC = sec_id, CBALLOT = precint_id, Party = candidature_acron, Votes = votes_candidature, Candidate = candidate_name) %>% 
          group_by(CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT, Party, Candidate) %>% 
          summarise_all(sum) %>% 
          arrange(CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT, -Votes)
      }
    
    } else if (str_sub(meta$collect[1], str_length(meta$collect[1]) - 11, str_length(meta$collect[1])) == "candidatures"){
    
      if (level == "national"){
        data <- data %>% 
          select(candidature_acron, votes_candidature) %>% 
          rename(Party = candidature_acron, Votes = votes_candidature) %>% 
          group_by(Party) %>% 
          summarise_all(sum) %>% 
          arrange(-Votes)
        } else if (level == "regional"){ 
        data <- data %>% 
          select(aut_id, candidature_acron, votes_candidature) %>% 
          rename(CAUT = aut_id, Party = candidature_acron, Votes = votes_candidature) %>% 
          group_by(CAUT, Party) %>% 
          summarise_all(sum) %>% 
          arrange(CAUT, -Votes)
        } else if (level == "province"){ 
          data <- data %>% 
            select(aut_id, prov_id, candidature_acron, votes_candidature) %>% 
            rename(CAUT = aut_id, CPROV = prov_id, Party = candidature_acron, Votes = votes_candidature) %>% 
            group_by(CAUT, CPROV, Party) %>%
            summarise_all(sum) %>% 
            arrange(CAUT, CPROV, -Votes)
        } else if (level == "island"){ 
          data <- data %>% 
            select(aut_id, prov_id, constituency_id, candidature_acron, votes_candidature) %>% 
            rename(CAUT = aut_id, CPROV = prov_id, CISLAND = constituency_id, Party = candidature_acron, Votes = votes_candidature) %>% 
            group_by(CAUT, CPROV, CISLAND, Party) %>%
            summarise_all(sum) %>% 
            arrange(CAUT, CPROV, CISLAND, -Votes)
        } else if (level == "local"){
          data <- data %>% 
            select(prov_id, mun_id, candidature_acron, votes_candidature) %>% 
            rename(CPROV = prov_id, CMUN = mun_id, Party = candidature_acron, Votes = votes_candidature) %>% 
            group_by(CPROV, CMUN, Party) %>% 
            summarise_all(sum) %>% 
            arrange(CPROV, CMUN, -Votes)
        } else if (level == "district") {
          data <- data %>% 
            select(aut_id, prov_id, mun_id, dist_id, candidature_acron, votes_candidature) %>% 
            rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, Party = candidature_acron, Votes = votes_candidature) %>% 
            group_by(CAUT, CPROV, CMUN, CDIST, Party) %>% 
            summarise(Votes = sum(Votes)) %>% 
            arrange(CAUT, CPROV, CMUN, CDIST, -Votes)
        } else if (level == "section") {
          data <- data %>% 
            select(aut_id, prov_id, mun_id, dist_id, sec_id, candidature_acron, votes_candidature) %>% 
            rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CSEC = sec_id, Party = candidature_acron, Votes = votes_candidature) %>% 
            group_by(CAUT, CPROV, CMUN, CDIST, CSEC, Party) %>% 
            summarise(Votes = sum(Votes)) %>% 
            arrange(CAUT, CPROV, CMUN, CDIST, CSEC, -Votes)
        } else if (level == "ballot"){  
          data <- data %>% 
            select(aut_id, prov_id, mun_id, dist_id, sec_id, precint_id, candidature_acron, votes_candidature) %>% 
            rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CSEC = sec_id, CBALLOT = precint_id, Party = candidature_acron, Votes = votes_candidature) %>% 
            group_by(CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT, Party) %>% 
            summarise_all(sum) %>% 
            arrange(CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT, -Votes)
        }
          
        } else if (str_sub(meta$collect[1], str_length(meta$collect[1]) - 3, str_length(meta$collect[1])) == "data"){
          
          if (level == "national"){
            data <- data %>% 
              select(population, n_precint, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures, seats) %>% 
              rename(Population = population, Ballots = n_precint, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                     Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures, Seats = seats) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks)
          } else if (level == "regional"){ 
            data <- data %>% 
              select(aut_id, population, n_precint, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures, seats) %>% 
              rename(CAUT = aut_id, Population = population, Ballots = n_precint, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                     Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures, Seats = seats) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks)
            merge_vars <- c("CAUT")
          } else if (level == "province"){ 
            data <- data %>% 
              select(aut_id, prov_id, population, n_precint, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures, seats) %>% 
              rename(CAUT = aut_id, CPROV = prov_id, Population = population, Ballots = n_precint, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                     Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures, Seats = seats) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks)
            merge_vars <- c("CAUT", "CPROV")
          } else if (level == "island"){ 
            data <- data %>% 
              select(aut_id, prov_id, constituency_id, population, n_precint, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures, seats) %>% 
              rename(CAUT = aut_id, CPROV = prov_id, CISLAND = constituency_id, Population = population, Ballots = n_precint, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures, Seats = seats) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks)
            merge_vars <- c("CAUT", "CPROV", "CISLAND")
          } else if (level == "local"){
            data <- data %>% 
              select(prov_id, mun_id, population, n_precint, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures, seats) %>% 
              rename(CPROV = prov_id, CMUN = mun_id, Population = population, Ballots = n_precint, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                     Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures, Seats = seats) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks)
            merge_vars <- c("CPROV", "CMUN")
             } else if (level == "district") {
              data <- data %>% 
                select(aut_id, prov_id, mun_id, dist_id, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures) %>% 
                rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                       Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures) %>% 
                mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks) %>% 
                group_by(CAUT, CPROV, CMUN, CDIST) %>% 
                summarise_all(sum)
              merge_vars <- c("CAUT", "CPROV", "CMUN", "CDIST")
          } else if (level == "section") {
            data <- data %>% 
              select(aut_id, prov_id, mun_id, dist_id, sec_id, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures) %>% 
              rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CSEC = sec_id, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                     Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks) %>% 
              group_by(CAUT, CPROV, CMUN, CDIST, CSEC) %>% 
              summarise_all(sum)
            merge_vars <- c("CAUT", "CPROV", "CMUN", "CDIST", "CSEC")
          } else if (level == "ballot"){  
            data <- data %>% 
              select(aut_id, prov_id, mun_id, dist_id, sec_id, precint_id, census_INE, census_escrut, census_cere, turnout_pre_1, turnout_pre_2, votes_blank, votes_null, votes_candidatures) %>% 
              rename(CAUT = aut_id, CPROV = prov_id, CMUN = mun_id, CDIST = dist_id, CSEC = sec_id, CBALLOT = precint_id, CensusINE = census_INE, CensusCount = census_escrut, CensusCERE = census_cere,  
                     Turnout1 = turnout_pre_1, Turnout2 = turnout_pre_2, Blanks = votes_blank, Nulls = votes_null, Candidatures = votes_candidatures) %>% 
              mutate(TurnoutFinal = Candidatures + Blanks + Nulls, Valid = Candidatures + Blanks) %>% 
              group_by(CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT) %>% 
              summarise_all(sum)
            merge_vars <- c("CAUT", "CPROV", "CMUN", "CDIST", "CSEC", "CBALLOT")
        }
        
        }
          
 
## 4. swith from long to wide or wide to long ##
    
    if(format == "long" & str_sub(meta$collect[1], str_length(meta$collect[1]) - 3, str_length(meta$collect[1])) == "data"){
    
    if(level %in% c("national", "regional", "province", "island", "local")){  
      data <- data %>%
      gather(Variable, Value, Population:Valid)
    } else {
      data <- data %>%
        gather(Variable, Value, CensusINE:Valid)
    }
   
    } else if (format =="wide" & str_sub(meta$collect[1], str_length(meta$collect[1]) - 11, str_length(meta$collect[1])) == "candidatures" & meta$elec[1] != 3){
   
     data_col <- data %>% 
      group_by(Party) %>% 
      summarise(Votes = sum(Votes)) %>% 
      arrange(-Votes) 
    
     # extract colnames and select to save geo index
    if(level == "national"){
      colnames <- data_col$Party  
    } else {
    colnames <- c(colnames(data)[1:(length(colnames(data))-2)], data_col$Party)
    }
    
     # spread data
    data <-  data %>%
        spread(Party, Votes)
    
    # select columns  
    data <- data[ ,colnames]  
    
      }

## 5. replace original data by formated data for long and wide formats ##    
    query_list[[i]] <- data

## 6. prepare summary output ##          
    if(meta$data_orig == "summary" & i %% 2 == 0 & format == "long") {
      
      votes_df <- query_list[[i]] %>% 
        rename(Variable = Party, Value = Votes)
      
      particip_df <- query_list[[i-1]] %>% 
        filter(Variable %in% c("CensusCount", "TurnoutFinal", "Valid", "Seats"))
      
      summary_df <- bind_rows(particip_df, votes_df)
      
      switch (level,
        "regional" = summary_df <- arrange(summary_df, CAUT),
        "province" = summary_df <- arrange(summary_df, CAUT, CPROV),
        "island" = summary_df <- arrange(summary_df, CAUT, CPROV, CISLAND),
        "local" = summary_df <- arrange(summary_df, CPROV, CMUN),
        "district" = summary_df <- arrange(summary_df, CAUT, CPROV, CMUN, CDIST),
        "section" = summary_df <- arrange(summary_df, CAUT, CPROV, CMUN, CDIST, CSEC),
        "ballot" = summary_df <- arrange(summary_df, CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT)
      )
      
      summary_list[[length(summary_list)+1]] <- summary_df 
        
    } else if(meta$data_orig == "summary" & i %% 2 == 0 & format == "wide") {
      votes_df <- query_list[[i]]
      
      particip_df <- query_list[[i-1]] 
      colnames_particip <- colnames(particip_df)
      colnames_particip <- colnames_particip %in% c("CAUT", "CPROV", "CISLAND", "CMUN", "CSEC", "CDIST", "CSEC", "CBALLOT", "CensusCount", "TurnoutFinal", "Valid", "Seats")
      particip_df <- particip_df[, colnames_particip] 
      
      if(level == "national"){
        summary_df <- cbind(particip_df, votes_df)
      } else {
      summary_df <- left_join(particip_df, votes_df, by =  merge_vars) 
      }
      
      switch (level,
              "regional" = summary_df <- arrange(summary_df, CAUT),
              "province" = summary_df <- arrange(summary_df, CAUT, CPROV),
              "island" = summary_df <- arrange(summary_df, CAUT, CPROV, CISLAND),
              "local" = summary_df <- arrange(summary_df, CPROV, CMUN),
              "district" = summary_df <- arrange(summary_df, CAUT, CPROV, CMUN, CDIST),
              "section" = summary_df <- arrange(summary_df, CAUT, CPROV, CMUN, CDIST, CSEC),
              "ballot" = summary_df <- arrange(summary_df, CAUT, CPROV, CMUN, CDIST, CSEC, CBALLOT)
              )
      
      summary_list[[length(summary_list)+1]] <- summary_df 
      
      }

}

if(length(summary_list)>0){
  return_list <- summary_list
  
  nrows <- nrow(query_list$meta)
  nrows <- 1:nrows
  nrows <- nrows[nrows %% 2 == 0]
  meta <- query_list$meta[nrows, -which(names(meta) %in% c("data", "data_orig", "collect"))] 
  rownames(meta) <- 1:length(nrows)
  return_list[[length(return_list)+1]] <- meta
  return_list_names <- c(paste0("query", 1:length(nrows)), "meta")
  names(return_list) <- return_list_names
  return_list$meta$elec <- recode(return_list$meta$elec, `2` = "cong", `3` = "sen", `4` = "local", `1` = "ref", `6` = "cabildo", `7` = "euro")
  
} else {
  return_list <- query_list
  return_list$meta <- return_list$meta[, -which(names(return_list$meta) %in% c("data", "data_orig", "collect"))] 
  return_list$meta$elec <- recode(return_list$meta$elec, `2` = "cong", `3` = "sen", `4` = "local", `1` = "ref", `6` = "cabildo", `7` = "euro") 
}

  return(return_list)
    
}  



######### GLOBAL FUNCTION ########
# elec = "cong", "sen", "local", "ref", "euro", "cabildo"
# year = year in which the election took place
# format = "long", "wide"
# level =  "national", "regional", "province", "local", "district", "section", "ballot"
# data = "particip", "candidat", "all", "summary" 
# format = "long", "wide"

elections <- function(elec, year, 
                          caut = NULL, cprov = NULL, cisland = NULL, cmun = NULL, cdist = NULL, csec = NULL, cballot = NULL,
                          cmultiple = NULL,
                          format = "long", level = "national", data = "summary"){
 
 queries <- arrange_queries(elec = elec, year = year, 
                            caut = caut, cprov = cprov, cisland = NULL, cmun = cmun, cdist = cdist, csec = csec, cballot = cballot,
                            cmultiple = cmultiple,level = level, data = data, format = format)
 query_list <- extract_data(queries = queries)
 return_list <- format_output(query_list = query_list, format = format)
 return(return_list)
}

### get structure of multiple table

get_cmultiple_df <- function(){
  return_df <- data.frame(elec = integer(0), 
                                year = integer(0),
                                caut = integer(0),
                                cprov = integer(0), 
                                cmun = integer(0), 
                                cdist = integer(0), 
                                csec = character(0), 
                                cballot = character(0),
                                level = character(0),
                                data = character(0),
                                stringsAsFactors = FALSE) 
  return(return_df)
  
}


