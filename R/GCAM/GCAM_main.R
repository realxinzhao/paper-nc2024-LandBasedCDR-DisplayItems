
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


source("R/helper_func/info.R")
source("R/helper_func/clean_GCAM_query.R")
source("R/helper_func/data_func.R");
source("R/helper_func/Figure_func.R")
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



source("R/GCAM/GCAM_SIFigs_LandToEMs.R")
SIFigs_LandToEMs()

source("R/GCAM/GCAM_SIFigs_EmissionDecompose.R")
SIFigs_EmissionDecompose()

source("R/GCAM/GCAM_SIFigs_LandProd.R")
SIFigs_LandProd()

source("R/GCAM/GCAM_SIFigs_BioEnergy.R")
SIFigs_BioEnergy()






