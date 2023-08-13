Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(file.path(outdir, paste0(name,".png")), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}



line <- function(data, x = "year", y = "value", group, arearange = c(0.1,0.9),
                 areaalpha = 0.1, leg.key.size = 1.5, leg.text.size = 15){
  ggplot(data%>% filter(is.na(get(y)) == F) %>%
                group_by_at(vars(group,x))  %>%
                summarise(min = quantile(get(y), probs = arearange[1]),
                       median = median(get(y)),
                       mean = mean(get(y)),
                       max = quantile(get(y), probs = arearange[2])) %>% ungroup()
  ) +
    geom_ribbon(aes(x = get(x), ymin = min, ymax = max,group = get(group), fill = get(group)),alpha = areaalpha) +
    geom_line(aes(x = get(x), y = mean, group = get(group), color = get(group), linetype = get(group))) +
    geom_line(aes(x = get(x), y = median, group = get(group), color = get(group)), linetype = 5) +
    geom_hline(yintercept = 0, linetype= 1, color = "black") +
    #xscale +
    #sty_reg +
    theme_bw() + theme0 + theme_leg  +
    theme(plot.title = element_text(size=16, face="bold"),
          legend.key.size = unit(leg.key.size, "cm"),
          legend.text = element_text(size = leg.text.size))
}

mar.boxplot <- function(data, x = group, y = "value",  boxrange = c(0.1,0.25,0.5,0.75,0.9),
                        col = hue_pal()(10)){
  ggplot(data %>% group_by_at(vars(x)) %>%
           summarise(ymin = quantile(get(y), probs = boxrange[1]),
                     lower = quantile(get(y), probs = boxrange[2]),
                     middle = quantile(get(y), probs = boxrange[3]),
                     upper = quantile(get(y), probs = boxrange[4]),
                     ymax = quantile(get(y), probs = boxrange[5])) %>% ungroup()
         , aes(x = get(x))) +
    geom_boxplot(aes(ymin = ymin, lower = lower, middle = middle,
                     upper = upper, max = ymax, fill =  get(x)),
                 alpha=0.5, lwd=0.5, fatten = 1, stat = "identity") +
    geom_point(data = data %>%  group_by_at(vars(x)) %>%
                 summarise(mean = mean(get(y))) %>% ungroup()
               , aes(x = get(x), y = mean), shape=18, size= 2) +
    scale_fill_manual(values=c(col))  +
    theme_bw()+
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position="none",
      plot.margin = margin(t = 10, r = 10, b = 10, l =0))
}



line.box <- function(.data, leftmargin = 6, LP = "none", vv = "Price", AA = "", aa = c(0.1, 0.9), areaalpha = 0.1,
                     group = "scenario", boxwidth = 0.08, leg.key.size = 1.5, leg.text.size = 15){

  plt1_1 <- line(.data,
                 x = "year", y ="value", group = group, arearange = aa,
                 areaalpha = areaalpha, leg.key.size = leg.key.size, leg.text.size = leg.text.size)  +
    scale_y_continuous(expand = c(0, 0), limits = limit0, breaks =BK)  +
    labs(title = paste(AA, vv)) +
    theme(plot.margin = margin(t = 10, r = 0, b = 10, l = leftmargin), legend.position = LP) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())


  plt1_2 <- mar.boxplot(.data %>% filter(year == 2100),
                        x = group, y = "value", boxrange = c(aa[1],0.25,0.5,0.75,aa[2]),
                        col = color4) +
            scale_y_continuous(expand = c(0, 0),limits = limit0)

  ggarrange(plt1_1,plt1_2,
            nrow=1, ncol = 2,
            widths = c(0.9,boxwidth))
}





V.text <- function(text = "Percent change (2010 = 0)", size = 6){
  T1 <-ggplot() + annotate("text", x= 0, y=c(4), label= c(text),
                           angle = 90, size = size) +
    theme_bw() +  scale_y_continuous(expand = c(0,0),limits = c(0,8)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
          axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  return(T1)
}
H.text <- function(text = "Year", size = 6){
  T2 <-ggplot() + annotate("text", y = 0, x=c(1.58), label= c(text),angle = 0, size = size) +
    theme_bw() + scale_x_continuous(expand = c(0,0),limits = c(0,3)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
          axis.text.y = element_blank(),axis.text.x = element_blank(), axis.title.y = element_blank(),axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0));
  return(T2)
}


temp.func <- function(.data, LP ="none", group = "region"){

  x = "year"; y = "value"; arearange = c(0, 1); areaalpha = 0.1
  boxwidth = 0.08;leftmargin = 6

  plt1_1 <-  ggplot(.data%>% filter(is.na(get(y)) == F) %>%
                      group_by_at(vars(group,x))  %>%
                      summarise(min = quantile(get(y), probs = arearange[1]),
                                median = median(get(y)),
                                mean = mean(get(y)),
                                max = quantile(get(y), probs = arearange[2])) %>% ungroup()
  ) +
    geom_ribbon(aes(x = get(x), ymin = min, ymax = max,group = region, fill = region),alpha = areaalpha) +
    #geom_line(aes(x = get(x), y = mean, group = get(group), color = get(group), linetype = get(group))) +
    #geom_line(aes(x = get(x), y = median, group = get(group), color = get(group)), linetype = 5) +
    geom_line(data = .data, aes(x = year, y = value, group = interaction(region, crop),
                                color = crop, linetype = region), size = 1) +
    geom_hline(yintercept = 0, linetype= 1, color = "black") +
    xscale +
    theme_bw() + theme0 + theme_leg  +
    theme(plot.title = element_text(size=16, face="bold")) +
    labs(title = paste(AA, vv)) +
    theme(plot.margin = margin(t = 10, r = 0, b = 10, l = leftmargin), legend.position = LP) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    scale_color_npg(name = "Crop") +
    scale_fill_manual(values=c(color4), name = "Region") +
    scale_linetype_manual(values=c(1, 5), name = "Region") +
    scale_y_continuous(expand = c(0, 0), limits = limit0, breaks =BK)


  plt1_2 <-  mar.boxplot(.data %>% filter(year == 2100),
                         x = "region", y = "value", boxrange = c(arearange[1],0.25,0.5,0.75,arearange[2]),
                         col = color4) +
    scale_y_continuous(expand = c(0, 0), limits = limit0, breaks =BK)

  ggarrange(plt1_1,plt1_2,
            nrow=1, ncol = 2,
            widths = c(0.9,boxwidth))
}


Mapdata <- function(.data, crop, year, variable, scenario){
  mapdata0 <- map_424_sf %>%
    dplyr::select(region, basin) %>%
    mutate(scenario = "",
           crop = "",
           year = 2010,
           value = 1,
           variable = "") %>%
    filter(region == "")

  for (cc in crop) {
    for (yy in year) {
      for (vv in variable) {
        for (ss in scenario) {
          left_join(map_424_sf, .data %>%
                      filter(crop %in% cc,
                             year %in% yy,
                             variable %in% vv,
                             scenario %in% ss) ) %>%
            mutate(crop = cc,
                   year = yy,
                   variable = vv,
                   scenario = ss )-> mapdata1

          mapdata0 = rbind(mapdata0, mapdata1)

        }
      }
    }
  }
  return(mapdata0)
}



Chord <- function(.data, gridcol = gridcol){
  chordDiagram(as.data.frame(.data),
               transparency = 0.5,
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               diffHeight = -uh(2, "mm")
               ,link.arr.type = "big.arrow"
               #, col = linkcol
               , grid.col = gridcol
               ,annotationTrack = c("grid")
               ,preAllocateTracks = list(list(track.height = c(0.3))
                                         ,list(track.height = c(0.035))
               ))
  #title(main = "test figure")
  circos.track(track.index = 3, panel.fun = function(x, y) {
    circos.axis(h = 1, labels.cex = 0.8)
  }, bg.border = NA)

  circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")

    #make text label vertical when space is too small; cex to adjust font size

    if(abs(xplot[2] - xplot[1]) < 20 | abs(xplot[2] - xplot[1]) > 340 ) {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                  niceFacing = TRUE, adj = c(0, 0.5), col = "black",
                  cex = 1)
    } else{
      circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                  niceFacing = TRUE, adj = c(0.5, 0), col= "black",
                  cex = 1)
    }  }, bg.border = NA)

}


chordata <- function(.data, nn = 20, flow = "gross equilibrium", inflow1 = "consume", outflow1 = "prod"){

  dat_circular0 <- .data %>% mutate(world = "World", RegID = as.character(RegID)) %>%
    mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO))

  if (flow == "gross equilibrium"| flow == 1) {inflow = "consume"; outflow = "prod"
  dd <- .data %>% mutate(world = "world", RegID = as.character(RegID)) %>%
    mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO))  %>%
    mutate(topplayer = abs(get(outflow1)) + abs(get(inflow1))) %>%
    arrange(desc(topplayer)) %>% top_n(nn) %>%
    dplyr::select(REG) %>% add_row(REG = "World") %>%
    rename(RegID2 = REG) %>% mutate(RegID2 = as.character(RegID2))
  dd1 <- unique(dd$RegID2)

  dat_circular <- dat_circular0 %>% select(REG_ex = REG, REG_im = world, flow = export) %>%
    bind_rows(dat_circular0 %>% mutate(REG_ex = "World") %>% select(REG_ex, REG_im = REG, flow = import)) %>%
    bind_rows(dat_circular0 %>% mutate(REG_ex = REG) %>% select(REG_ex, REG_im = REG, flow = domestic)) %>%
    mutate(REG_ex = if_else(REG_ex %notin% dd$RegID2, "Other regions", REG_ex)) %>%
    mutate(REG_im = if_else(REG_im %notin% dd$RegID2, "Other regions", REG_im)) %>%
    group_by(REG_ex, REG_im) %>% summarise(flow = sum(flow))
  } else{
    if (flow == "net equilibrium"| flow == 2) {inflow = "consume"; outflow = "prod"} else
      if (flow == "gross trade" | flow == 3) {inflow = "import"; outflow = "export" } else
        if (flow == "net trade" | flow == 4) {inflow = "netimport"; outflow = "netexport" }


    dd <- .data %>% mutate(world = "world", RegID = as.character(RegID)) %>%
      mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO))  %>%
      mutate(topplayer = abs(get(outflow1)) + abs(get(inflow1))) %>%
      arrange(desc(topplayer)) %>% top_n(nn) %>%
      dplyr::select(REG) %>% add_row(REG = "World") %>%
      rename(RegID2 = REG) %>% mutate(RegID2 = as.character(RegID2))
    dd1 <- unique(dd$RegID2)

    dat_circular <- dat_circular0 %>% select(REG_ex = REG, REG_im = world, flow = outflow) %>%
      bind_rows(dat_circular0 %>% mutate(REG_ex = "World") %>% select(REG_ex, REG_im = REG, flow = inflow)) %>%
      mutate(REG_ex = if_else(REG_ex %notin% dd$RegID2, "Other regions", REG_ex)) %>%
      mutate(REG_im = if_else(REG_im %notin% dd$RegID2, "Other regions", REG_im)) %>%
      group_by(REG_ex, REG_im) %>% summarise(flow = sum(flow)) %>% ungroup()
  }
  return(dat_circular)
}



# Mapping tools ----

## Load sf lib and data ----

basin <- read.csv(file = "data/maps/basin_to_country_mapping.csv", header = T, comment.char = "#") %>%
  dplyr::select(GCAM_basin_ID, GLU_name)
basin_noise <- read.csv(file = "data/maps/basin_noise.csv", header = T, comment.char = "#")
library(sf)
map_424_sf <- st_read("data/maps/map_424.shp") %>%
  # Separate Taiwan
  mutate(cntry_n = if_else(basn_nm == "Taiwan", "Taiwan", cntry_n)) %>%
  left_join(basin %>% select(basn_ID = GCAM_basin_ID, GLU_name), by = "basn_ID") %>%
  rename(basin = GLU_name, region = cntry_n) %>%
  # remove Greenland
  filter(basin != "ArcticIsl") %>%
  # remove small noise regions
  anti_join(basin_noise) # 381 combinations ~10 small areas not included
summary(map_424_sf)

## Function to plot basin map directly ----


ggcamMapBasin <- function(.data,
                          Facet_Var = NULL,
                          cut = FALSE, # note that add on scale_fill_viridis_b is much better
                          nbreak = 10,
                          return_data_only = FALSE){

  windowsFonts("Arial" = windowsFont("Arial"))
  assertthat::assert_that(is.character(all_of(Facet_Var))|is.null(Facet_Var))

  # Join sf
  # adding in areas exist in map but not in .data as zero
  merge(map_424_sf %>% as_tibble() %>%  select(region, basin),
        .data %>% distinct_at( vars(Facet_Var)))  %>%
    # setdiff existing combinations
    setdiff(.data %>% distinct_at(vars("region", "basin", Facet_Var)) ) ->
    .data_needadd

  .data_needadd %>%
    merge(
      .data %>% select_at(vars(-"value", -"region", -"basin", -Facet_Var)) %>% distinct()
    ) %>%
    mutate(value = 0) %>%
    bind_rows(.data) ->
    .data

  map_424_sf %>%
    left_join(.data, by = c("region", "basin")) %>%
    replace_na(list(value = 0)) -> .data

  # using discrete data when cut with nbreak
  if (cut == TRUE) {
    Fr = floor(min(.data$value, na.rm = T)*10)/10
    Ce =  ceiling(max(.data$value, na.rm = T) *10)/10
    breaks = round(seq(Fr, Ce, (Ce - Fr)/nbreak), 1)

    .data %>%
      mutate(value = cut(value,unique(breaks))) -> .data
  }

  # return data only otherwise base map
  if (return_data_only == TRUE) {
    return(.data)
  }

  # mapping use ggplot
  .data %>%
    ggplot() +
    geom_sf(aes(fill = value), color = "black", size = 0.5) +
    theme_bw() +
    scale_y_continuous(expand = c(0.03, 0.03)) + scale_x_continuous(expand = c(0.01, 0.01)) +
    theme(panel.grid.major = element_line(colour = 'transparent'), #panel.border =element_blank(),
          panel.border = element_rect(color = "black", size =1),
          axis.text = element_blank(), axis.ticks = element_blank(),
          text = element_text(family= "Arial", size = 15),
          strip.background = element_blank(),
          strip.text = element_text(vjust = 0.5, margin = margin(0.4,0.4,0.4,0.4, "lines"),
                                    size = 16 #, face = "bold"
          ),
          panel.spacing.y = unit(0.8, "lines"),
          panel.spacing.x = unit(0.8, "lines") )

}


