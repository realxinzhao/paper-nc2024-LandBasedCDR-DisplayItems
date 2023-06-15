# GCAM results
c(
  "BioUn_ProtLow_LCT0_FCT200p104_G1",
  "BioUn_ProtLow_LCT10_FCT182p483_G1",
  "BioUn_ProtLow_LCT50_FCT163p942_G1",
  "BioUn_ProtLow_LCT100_FCT160p365_G1",

  "BioUn_ProtLow_LCT100_FCT160p365_G1_oLUC",
  "BioUn_ProtLow_LCT0_FCT200p104_G1_oLUC",
  "BioUn_ProtLow_LCT10_FCT182p483_G1_oLUC",
  "BioUn_ProtLow_LCT50_FCT163p942_G1_oLUC"

) -> ss_Scen_1p5C

Load_GCAM <- function(VarNames,
                      Include_MainGScenarios = T,
                      Include_Ref = F,
                      Include_LCPForOnly = F,
                      Include_LowBio = F,
                      Include_Consequential = F,
                      Include_KeyOnly = F,
                      Include_NZscen = F,
                      RM_1p5C = F,
                      Folder_NAME = "Result_FINAL_GOOD_11052022"){

  # Load libs ----
  library(tidyr)
  library(dplyr)
  library(gcamdata)

  source("R/info.R")
  source("R/data_func.R");
  source("R/clean_GCAM_query.R")

  outdir <- "output/GCAM/"

#***********************************************************************
# Load data ----
DATA_FOLDER <- "data/GCAM_EMRE"


CSV_FINAL_GOOD <- file.path(DATA_FOLDER, Folder_NAME)


# Folder names
scenario_label <- list.files(path = CSV_FINAL_GOOD)
# All queries or results
#querynames <- gsub("_emre|.csv", "", list.files(path = file.path(CSV_FINAL_GOOD, scenario_label[1])))

querynames <- VarNames

#scenario_label[grepl("_B1$|_G1$", scenario_label)] %>% setdiff("ref_G1")-> Scen_Core
scenario_label[grepl("_G1$", scenario_label)] %>% setdiff("ref_G1")-> Scen_Core

if(Include_MainGScenarios == F) {
   c() -> Scen_Core
}

if (Include_Ref == T) {
  c("ref_G1", Scen_Core) -> Scen_Core
}

if (Include_LowBio == T) {

  scenario_label[grepl("_B1$", scenario_label)] %>% setdiff("ref_G1") -> scenB

  c(scenB, Scen_Core) -> Scen_Core
}

if (Include_Consequential == T) {
  scenario_label[grepl("_G1_oLUC$", scenario_label)]  -> scenCons

  c(scenCons, Scen_Core) -> Scen_Core
}

if (Include_LCPForOnly == T) {
  scenario_label[grepl("_G1F$", scenario_label)]  -> scenCons

  c(scenCons, Scen_Core) -> Scen_Core
}

if (Include_NZscen == T) {
  scenario_label[grepl("_N1$", scenario_label)]  -> scenCons

  c(scenCons, Scen_Core) -> Scen_Core
}


c(
  "BioUn_ProtLow_LCT0_FCT200p104_G1",
  "BioUn_ProtLow_LCT10_FCT182p483_G1",
  "BioUn_ProtLow_LCT50_FCT163p942_G1",
  "BioUn_ProtLow_LCT100_FCT160p365_G1",

  "BioUn_ProtLow_LCT100_FCT160p365_G1_oLUC",
  "BioUn_ProtLow_LCT0_FCT200p104_G1_oLUC",
  "BioUn_ProtLow_LCT10_FCT182p483_G1_oLUC",
  "BioUn_ProtLow_LCT50_FCT163p942_G1_oLUC"

  ) -> ss_Scen_1p5C

assign("ss_Scen_1p5C", ss_Scen_1p5C, envir = .GlobalEnv)

if(Include_KeyOnly == T){
  Scen_Core <- c(
    "BioUn_ProtLow_LCT0_FCT104p91_G1",
    "BioUn_ProtLow_LCT100_FCT87p968_G1",
    "BioUn_ProtLow_LCT100_FCT79p512_G1F",
    "Bio100_ProtLow_LCT100_FCT109p792_B1",
    "BioUn_ProtLow_LCT100_FCT160p365_G1"
  )
}



if (RM_1p5C == T) {
  setdiff(Scen_Core, ss_Scen_1p5C) -> Scen_Core
}


Scen_1p5C <- paste0("DB_", ss_Scen_1p5C )


  for (query in querynames) {
    assign(query,
           get_data_per_query(query, CSV_FINAL_GOOD, f_SPECIFIC_SCEN = Scen_Core) %>%
             mutate(target = if_else(scenario %in% Scen_1p5C, "1p5C", "2C"),
                    SDR = "p03")%>%
             mutate(scenario = gsub("ref_G1|Reference", "BioUn_ProtLow_LCT0_FCT0_REF", scenario)),
          envir = .GlobalEnv
    )
  }

}



proc_scen <- function(.df1){
  .df1 %>%
    mutate(scenario = gsub("DB_", "", scenario)) %>%
    separate(scenario, sep = "_",
             into = c("Biolimit", "Prot", "LCT", "FCT", "LandSupply"  ), remove = F) %>%
    mutate(LCT = if_else(grepl("LCT0_FCT0", scenario), "REF", LCT)) %>%
    mutate(ref = if_else(grepl("LCT0_FCT0", scenario), "Reference", "Mitigation")) %>%

    mutate(FCT = gsub("p", ".", FCT)) %>%
    mutate(FCT = round(as.numeric(gsub("FCT", "", FCT)), 0))  -> .df1


  .df1 %>%
    mutate(LCT = factor(LCT, levels = c("REF",paste0("LCT", c(0, 10, 50, 100)))
                        , labels = c("Reference", "No-LCP", "10%-LCP",
                                     "50%-LCP", "100%-LCP")
    )) %>%
    mutate(target = if_else(scenario %in% ss_Scen_1p5C,  "1p5C", "2C")) %>%
    mutate(target = factor(target, level = c("2C", "1p5C"))) %>%
    mutate(LandSupply = if_else(LandSupply == "G1" & target == "2C", "G1-2C", LandSupply)) %>%
    mutate(LandSupply = if_else(LandSupply == "G1" & target == "1p5C", "G1-1p5C", LandSupply)) %>%
    mutate(LandSupply = factor(LandSupply, levels = c("REF", "G1-2C", "G1F", "B1", "G1-1p5C", "N1"),
                               labels = c("Reference", "2C Main",
                                          "A/R-Focused", "Low-Bioenergy", "1.5C", "Net-Zero")) ) %>%

    mutate(Biolimit = factor(Biolimit, levels = c("BioUn", "Bio100"),
                             labels = c("UnlimitedBio", "LimitedBio100EJ")))-> .df1

  return(.df1)
}


Fill_annual <- function(.df, CUMULATIVE = FALSE,
                        CUM_YEAR_START = 2020,
                        CUM_OUT_STEP = 5){
  YEAR_START <- min(unique(.df$year))
  YEAR_END <- max(unique(.df$year))
  .df %>% mutate(year = as.integer(year)) -> .df


  .df %>% filter(year >= YEAR_START) %>%
    bind_rows(
      .df %>%
        #assuming YEAR_END has values for all
        filter(year == YEAR_END) %>% select(-year) %>%
        mutate(value = NA) %>%
        gcamdata::repeat_add_columns(tibble(year = setdiff(seq(YEAR_START,YEAR_END), unique(.df$year))))
    ) %>% arrange(year) %>%
    mutate(value = gcamdata::approx_fun(year, value, rule = 2)) -> .df1

  if (CUMULATIVE == TRUE ) {
    assertthat::assert_that(CUM_YEAR_START >= YEAR_START)
    .df1 %>% filter(year >= CUM_YEAR_START) %>%
      mutate(value = cumsum(value)) %>% filter(year >= CUM_YEAR_START) %>%
      filter(year %in% seq(YEAR_START,YEAR_END, CUM_OUT_STEP))-> .df1
  }
  return(.df1)
}


GCAM_EMs <- function(aInclude_Ref = F,
                     aInclude_MainGScenarios = T,
                     aInclude_Consequential = F,
                     aInclude_LowBio = F, aInclude_LCPForOnly = F,
                     aRM_1p5C = F, aInclude_NZscen = F){
  querynames = c("NCEM", "CLUC", "CSQ")
  Load_GCAM(querynames, Include_MainGScenarios = aInclude_MainGScenarios,
            Include_Ref = aInclude_Ref,
            Include_Consequential = aInclude_Consequential,
            Include_LowBio = aInclude_LowBio,
            Include_LCPForOnly = aInclude_LCPForOnly, RM_1p5C = aRM_1p5C,
            Include_NZscen = aInclude_NZscen)

  CLUC %>% Agg_reg(target) %>% mutate(GHG = "AFOLU" ) %>%
    bind_rows(
      NCEM  %>% filter(GHG == "CO2")%>% # avoid double counting
        Agg_reg(GHG, target) %>% mutate(GHG = "FFI" ) )  %>%
    bind_rows(
      CSQ %>%
        mutate(subsector = case_when(
          grepl("biomass", subsector) ~ "BECCS",
          grepl("refined liquids", subsector) ~ "Oil",
          grepl("coal", subsector) ~ "Coal",
          grepl("gas", subsector) ~ "NG",
          TRUE ~ "Other")  ) %>%
        Agg_reg(GHG = subsector, target) %>%
        filter(GHG == "BECCS")
    ) %>%
    mutate(value = value / 1000*44 /12) %>%
    filter(year >= 2015) %>% spread(GHG, value) %>%
    replace_na(list(BECCS = 0)) %>%
    mutate(BECCS = -BECCS, EMs = FFI + AFOLU) %>%
    mutate(`FFI, Non-BECCS` = FFI - BECCS) %>%
    gather(GHG, value, AFOLU:`FFI, Non-BECCS`) %>%
    group_by(scenario, target, GHG) %>%
    Fill_annual(CUMULATIVE = T) %>% ungroup() -> EMs

  saveRDS(EMs %>% proc_scen(), "output/AR6/RDS_temp/GCAM_EMs.rds")


  # EMs %>% proc_scen() %>%
  #   filter(GHG %in% c("AFOLU", "FFI", "BECCS")) %>%
  #   ggplot() + facet_wrap(Biolimit + target ~GHG, scales = "free") +
  #   geom_hline(yintercept = 0) +
  #   geom_line(aes(x = year, y = value, group = scenario, color = LCT, linetype = LandSupply), size = 1) +
  #   scale_x_continuous(expand = c(0, 0)) +
  #   scale_color_manual(values = mycol) +
  #   scale_linetype_manual(values = c(1, 5)) +
  #   labs(x = "Year", y = expression(paste(GtCO[2])), linetype = "Target",
  #        color = "Land mitigation policy") +
  #   theme_bw() + theme0 +
  #   theme(axis.text.x = element_text(angle = 90),
  #         legend.key.width = unit(1,"cm"),
  #         panel.grid = element_blank()) -> A; A
  #
  # A %>% Write_png("EMs_TS", h = 3200, w = 3800,  r = 300)


  return(EMs)

}

GCAM_CPs <- function(){
  outdir <- "output/GCAM/"
  querynames = c("CO2Price")
  #Load_GCAM(querynames)

  Load_GCAM(querynames,
            Include_LCPForOnly = T,
            Include_LowBio = T,
            Include_NZscen = F)

  CO2Price %>%
    mutate(value = value * gcamdata::gdp_deflator(2015, 1990) * 12 / 44) %>%
    #left_join(NZ_PeakYear, by = "scenario") %>%
    #filter(!(!is.na(NZyear) & year > NZyear)) %>% select(-NZyear) %>%
    proc_scen() %>%
    filter(market %in% c("USACO2_LUC", "globalCO2_LTG"), year >= 2025) %>%
    mutate(market = if_else(grepl("_LUC", market), "Land", "Global")) %>%
    filter(market == "Global")%>%
    ggplot() +
    facet_grid( ~ LandSupply) +
    geom_hline(yintercept = 500, linetype = 5) +
    geom_hline(yintercept = 1000, linetype = 5) +
    geom_hline(yintercept = 1500, linetype = 5) +
    #facet_wrap(~market) +
    geom_line(aes(x = year , y = value, color = LCT, linetype = LCT ), size = 1.1, alpha = 1) +
    labs(x = "Year", y = expression(paste("2015$ per tonne ", CO[2]))) +
    scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), limits = c(0, 1600)) +
    #scale_color_npg(name = "Scenario") +
    scale_color_manual(name = "LCT", values = mycol) +
    theme_bw() + theme0 + theme_leg +
    theme(axis.text.x = element_text(angle = 90))+
    theme(panel.spacing = unit(0.5, "lines"))-> A;A

  A %>% Write_png("GCAM_CPs",h = 1800, w = 4400,  r = 300)
}
