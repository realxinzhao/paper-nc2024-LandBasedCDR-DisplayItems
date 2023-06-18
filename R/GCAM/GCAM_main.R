
# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)

require(broom)


library(patchwork)
library(cowplot)
library(RColorBrewer)


source("R/info.R")
source("R/clean_GCAM_query.R")
source("R/data_func.R");
source("R/Figure_func.R")
source("R/GCAM/GCAM_module_funcs.R")

outdir <- "output/GCAM/"


source("R/GCAM/GCAM_MainFig2_CarbonPrices.R")
MainFig2_CarbonPrices()

source("R/GCAM/GCAM_MainFig3_LandCarbonPricingStrength.R")
MainFig3_LandCarbonPricingStrength()

source("R/GCAM/GCAM_MainFig4_PolicyScope.R")
MainFig4_PolicyScope()

source("R/GCAM/GCAM_MainFig6_AgPriceImplication.R")
MainFig6_AgPriceImplication()


# Carbon prices ----
GCAM_CPs()

# Cumulative emissions data ----
GCAM_EMs(aInclude_LCPForOnly = T, aInclude_LowBio = T, aRM_1p5C = F) -> EMs
GCAM_EMs(aRM_1p5C = F) -> EMs






# EMs decompose ----
source("R/GCAM/GCAM_module_funcs_EMsDecompose.R")
GCAM_EMsDecompose()


# GCAM_LandProd ----
source("R/GCAM/GCAM_module_funcs_LandProd.R")
GCAM_LandProd()


# GCAM_Prices ----
source("R/GCAM/GCAM_module_funcs_Price.R")
GCAM_Price()

# Forest only land policy
source("R/GCAM/GCAM_module_funcs_ForOnly.R")
GCAM_ForOnly()

# Low bio energy
source("R/GCAM/GCAM_module_funcs_BioLimit.R")
GCAM_BioLimit()


