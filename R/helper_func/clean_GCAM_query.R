#This function cleans a basic GCAM csv query that has been concatenated to more of its kind

get_csv_path_per_query <- function(QUERY, CSV_FOLDER, SPECIFIC_SCEN = NULL){
  list.files(path = file.path(CSV_FOLDER), recursive = T, full.names = T)[
    grepl(paste0(QUERY,".csv"), list.files(path = file.path(CSV_FOLDER), recursive = T, full.names = T))] ->
    Path_all

    if ( is.null(SPECIFIC_SCEN)) {
     return(Path_all)
   }
   Path_all[grepl(paste0("/", SPECIFIC_SCEN, "/", collapse = "|"), Path_all)]
  }





get_data_per_query <- function(QUERY, CSV_FOLDER, f_SPECIFIC_SCEN = NULL){

  # lapply(get_csv_path_per_query(QUERY, CSV_FOLDER),
  #        function(csvDir){clean_GCAM_query(csvDir)}) %>% bind_rows()

  clean_GCAM_query = clean_GCAM_query
  library(doParallel)
  myCluster <- makeCluster(4, # number of cores to use
                           type = "PSOCK") # type of cluster
  #detectCores()
  registerDoParallel(myCluster)

  foreach(csvDir = get_csv_path_per_query(QUERY, CSV_FOLDER, SPECIFIC_SCEN = f_SPECIFIC_SCEN),
          .combine=rbind,
          .packages = "dplyr" ,.errorhandling = "remove"
  ) %dopar% {
    clean_GCAM_query(csvDir)
  } -> df
  stopCluster(myCluster)
  return(df)

}

clean_GCAM_query <- function(CSV_PATH,
                             CSV = TRUE,
                             DATE_RM = TRUE,
                             GATHER_YEAR = TRUE){
  if(CSV == TRUE) file <- readr::read_csv(CSV_PATH, skip = 1)

  file %>%
    select(-matches("^X")) %>%
    na.omit() %>%
    dplyr::filter(scenario != "scenario") ->
    file1

  if (DATE_RM == TRUE) {
    file1 %>%
      mutate(scenario = gsub(",date.*$", "", scenario)) ->
      file1
  }

  if (GATHER_YEAR == TRUE) {
    file1 %>%
      gcamdata::gather_years() ->
      file1
  }

    return(file1)
}

remove_date <- function(.data){
  .data %>%
    mutate(scenario = gsub(",date.*$", "", scenario)) %>%
    return()
  }

gather_years <- function(.data){
  .data %>%
    gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
  }


clean_tech <- function(.data, tech = "LandLeaf", ...){
  .data %>%
    filter(str_detect(get(tech), paste(C_1, collapse = "|"))) %>%
    separate(col = get(tech), into = c("crop","basin")) %>%
    mutate(water = "NA", tech = "NA") %>%
    dplyr::select(scenario, region, basin, water,tech, crop, year, value, ...) %>%
    bind_rows(.data %>%
                filter(str_detect(get(tech), paste(C_2, collapse = "|"))) %>%
                separate(col = get(tech), into = c("crop","basin","water","tech")) %>%
                dplyr::select(scenario, region, basin, water,tech, crop, year, value, ...))%>%
    return()
}

clean_tech1 <- function(.data, tech = "LandLeaf"){
  .data %>%
    filter(str_detect(get(tech), paste(C_1, collapse = "|"))) %>%
    separate(col = get(tech), into = c("crop","basin")) %>%
    mutate(water = "NA", tech = "NA") %>%
    bind_rows(.data %>%
                filter(str_detect(get(tech), paste(C_2, collapse = "|"))) %>%
                separate(col = get(tech), into = c("crop","basin","water","tech"))) %>%
    bind_rows(.data %>%
                filter(str_detect(get(tech), paste(C_3, collapse = "|"))) %>%
                separate(col = get(tech), into = c("Crop1", "Crop2","basin","water","tech")) %>%
                unite(crop, c(Crop1,Crop2), remove = T) )%>%
    return()
}



