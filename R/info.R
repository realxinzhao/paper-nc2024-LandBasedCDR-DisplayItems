library(ggthemes)
library(ggplot2)
library(ggsci)
mycol = c(pal_npg()(10)[1],
          excel_new_pal()(6)[6],
          pal_jco()(10)[7],
          excel_new_pal()(5), pal_npg()(10)[5:10])
# barplot(1:10, col = c(pal_npg()(10)[1],
#                       excel_new_pal()(6)[6],
#                       pal_jco()(10)[7],
#                       excel_new_pal()(5), pal_npg()(10)[5:10]))



All_scenario <-c()
All_quaryName <- c(
  "Agdemand",             #12 crops + forest and pasture
  "Agprod",               #14 crops + forest and pasture
  "Agland",
  "Agprices",
  "RegAgsource",
  "Detailedland",         #14 crops + forest and pasture  + all other land
  "POP",
  "GDP",
  "Agyield",              #14 crops + forest and pasture
  "Agrental",
  "Agexpectedprice",
  "Agexpectedyield"
)


nonyear <- c(1975, 1990, 2005)
resultsyear <- seq(2010, 2100, by = 5)
resultsyear_emissions <- seq(2010, 2100, by = 1)


#Regmap <- read.csv(paste0(getwd(),"/info/Regmapping.csv"), header=TRUE, sep=",",comment.char = "#")
#Filenames = list.files(pattern = "*.csv")

Cropmap <-
  read.csv("./data/maps/Cropmapping.csv", header=TRUE, sep=",",comment.char = "#") %>% mutate(crop = tolower(crop))
Regionmap <- read.csv("./data/maps/Regmapping.csv", header=TRUE, sep=",",comment.char = "#")
Regmap <- read.csv("./data/maps/Regmapping.csv", header=TRUE, sep=",",comment.char = "#")

gridcol_vol_mkt <- as.character(unlist(as.list(read.csv("./data/maps/gridcol_vol_mkt.csv",
                                                        header=F, sep=",")), use.names=FALSE))
gridcol_vol_trd <- as.character(unlist(as.list(read.csv("./data/maps/gridcol_vol_trd.csv",
                                                        header=F, sep=",")), use.names=FALSE))
gridcol_val1 <- as.character(unlist(as.list(read.csv("./data/maps/gridcol_val1.csv",
                                                        header=F, sep=",")), use.names=FALSE))


fontfamily = "Arial"
windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey75"),
  #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  #axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  #plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())


