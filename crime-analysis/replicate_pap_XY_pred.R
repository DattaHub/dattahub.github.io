rm(list = ls())

## Only R codes for repliacte PAP 

# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")

if(T){
  library("sf")            # Spatial data objects and methods
  library("mapview")       # Interactive Map Viewing
  library("ggmap")         # ggplot2 addon for base maps
  library("cowplot")
  library("spatstat")      # KDE and other spatial functions
  library("raster")        # cell-based spatial operations
  library("tidyverse")     # data manipulation framework
  library("Hmisc")         # using cut2() functions for ggplot legends
  library("fitdistrplus")  # Distribution fitting functions
  library("lubridate")     # Power tools for handling dates
  library("tidycensus")
  library("lwgeom")
  library("Hmisc")
  library("hrbrthemes")
  library("gridExtra")
  library("patchwork")
  library("spdep")         # KNN functions
  library("foreach")
  library("doParallel")
  library("corrplot")
  library("ranger")        # randomforest implimentation      
  library("glmnet")        # for Ridge and Lasso Regression
  library("knitr")         # for kable table
  library("kableExtra")
  library("FNN")           # KNN for CPS vs. NN plots
  library("groupdata2")
  library("htmltools")
  library("viridis")
  library("viridisLite")
  library(sjPlot)
  library(sjlabelled)
  library(sjmisc)
}


## Themes 

mapTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    panel.border = element_blank()
  )
}

plotTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0), 
    axis.title.x = element_text(size = 10, family = "sans", face = "plain", hjust = 1, vjust = -0.5),
    axis.title.y = element_text(size = 10, family = "sans", face = "plain", hjust = 1, vjust = 1),
    axis.text = element_text(size = 9, family = "sans", face = "plain"),
    panel.background = element_blank(),
    panel.grid.minor = element_line(colour = "gray"),
    panel.grid.major = element_line(colour = "gray"),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    axis.line = element_blank()
  )
}

## Options
if(T){
  mapviewOptions(basemaps = c("Stamen.TonerLite", "OpenStreetMap.DE"))
  base_dir = "C:/Users/jd033/Box/Child Maltreatment"
  fishnet_grid_dim = 1000
  k_direction = 8 # 4 = rook, 8 = queen
  k_nearest_neighbors = 5
  # Either k (e.g. 5 or 10) or "LOOCV"
  n_folds = "LOOCV"
  # threshold quntile for statArea grouping
  stat_area_quantile = 0.60
  # Number of simulations for CPS vs. NN
  simulations = 1000
  # Number of neighbors for CPS vs. NN
  k = 5
  # random seed
  set.seed(11235)
}

## Source 

source('C:/Users/jd033/Box/Child Maltreatment/R-codes/FUNCTIONS_VAPAP_LR.R', echo = FALSE, keep.source = TRUE)
source('C:/Users/jd033/Box/Child Maltreatment/R-codes/FEA_CREATE_VARIABLES_LR_2.R', echo = TRUE, keep.source = TRUE)

# load("full_results_line_1983_0718.RData")


# rm(list = ls())

setwd("C:/Users/jd033/Box/Child Maltreatment/R-codes/")
load("Rdata/source_file_objects.RData")

## LR tracts data
lr_tract = var_list[["LR_Tracts_Working51"]]

# idx_1 = which(var_names == "LR_Tracts_Working51")
# var_list[[idx_1]] <- NULL

attributes(lr_tract)
# Class
class(lr_tract)

# Dimensions
dim(lr_tract)
# Info in shapefile
names(lr_tract)

(p1 <- lr_tract %>%
    ggplot(aes(fill = Area)) + 
    geom_sf(color = NA) + 
    coord_sf(crs = 2765) + 
    scale_fill_viridis_c(option = "plasma") + 
    labs(title = "Area"))

lr_tract_diss <- lr_tract %>%
  mutate(dissolve = 1) %>%
  # get rid of slivers
  st_buffer(., dist = 0.1) %>%
  group_by(dissolve) %>%
  summarise()

lr_rast_SP <- raster(as(lr_tract_diss, "Spatial"), nrows = 2000, ncol = 2000)

#### CPS ACCEPTED 

var_list[["CPS_Accepted"]] <-  var_list[["CM_LR_Matched_Centerline_3857"]]

# idx_2 = which(var_names == "CM_LR_Matched_Centerline_3857")
# var_list[[idx_2]] <- NULL

### get CPS_Accepted values (add 1 column for dissolving)

cps_base_map   <- get_stamenmap(bbox = c(left = -92.52091, bottom = 34.62606, right = -92.15494, top = 34.82195),
                                    maptype = "toner-lite")

# ggmap(cps_base_map)

## Alternative 

lr_base_map <- st_union(lr_tract) %>%
  ggplot()+geom_sf(aes(), fill = "grey85", color = NA, size = 1) +
  mapTheme()

cps_dissolve <- var_list[["CPS_Accepted"]] %>%
  mutate(value = 1) %>%
  dplyr::select(value) #%>% st_transform(crs = 2765)

net <- st_make_grid(lr_tract, cellsize = fishnet_grid_dim) #%>%st_transform(2756)

cps_dissolve %>% st_crs() == lr_tract %>% st_crs() 

# count CPS incidents per net cell - really just to get net raster into sf polygon format
net_agg <- aggregate(cps_dissolve, net , sum) %>%
    tibble::rowid_to_column(.,"net_id")

net_agg_vals = net_agg$value[!is.na(net_agg$value)]
summary(net_agg_vals)

# list of net cells IDs that intersect with Little Rock
net_intersect <- st_intersects(lr_tract, net_agg)

# extract Little Rock net cells based on intersect ID
net_littlerock <- net_agg[unique(unlist(net_intersect)),]
net_hood <- st_join(net_littlerock, lr_tract, largest = TRUE)
listw <- nb2listw(poly2nb(as(net_littlerock, "Spatial"), queen = TRUE))


## Plot fishnet 

net_littlerock$value[is.na(net_littlerock$value)] <- 0

net_littlerock %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "cividis") + 
  labs(title = "CM per grid-cell")+ 
  plotTheme()

### Read ACS data

acs <- var_list[["LR_BG_Tracts_ACS_DataJoined"]]

# idx_3 = which(var_names == "LR_BG_Tracts_ACS_DataJoined")
# var_list[[idx_3]] <- NULL

acs = acs %>% dplyr::select(Incident_C, TotPopSize, NLTotPop, PopDensity,Perc_Under,
                            Perc_Black, Perc_NonWh, Perc_Hispa, Perc_NonMa,
                            Perc_FHH,Perc_SingP,PercLowEdu, Perc_Rente,
                            Perc_PopUn, Perc_PopSt, Perc_NotIn, Perc_Publi,
                            PercHighHH, PercCollEd, Perc_OwnHo)


acs <- acs %>% rename( Incident_Count_sum = Incident_C,
                       Perc_Under18 = Perc_Under,
                       Perc_NonMarr_Fam_HH = Perc_NonMa,
                       Perc_SingPrnt_HH = Perc_SingP,
                       Perc_RenterOcc = Perc_Rente,
                       Perc_PopUnder18inPov = Perc_PopUn,
                       Perc_PopStrugg = Perc_PopSt,
                       Perc_NotInsured = Perc_NotIn, 
                       Perc_PublicInsure = Perc_Publi)

acs_pop <- acs %>% dplyr::select(TotPopSize)

## the number 2.29568e-5 is sq ft to acre 

acs_pop <- acs_pop %>%
  mutate(acre = as.numeric(st_area(acs)*2.29568e-5),
         # acre = units::set_units(acre, acre), 
         pop_acre_rate = TotPopSize / acre) 


POP_ACRE_RATE_plot <- acs_pop %>%
  ggplot(aes(fill = pop_acre_rate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 2765) + 
  scale_fill_viridis_c(option = "magma") + 
  labs(title = "Population per acre")+ 
  plotTheme()

net_blocks_intersect <- st_intersection(acs_pop, net_littlerock)

# group by cell and calc block stats.
net_blocks_intersect <- net_blocks_intersect %>%
  mutate(intersect_area_acres = as.numeric(st_area(net_blocks_intersect)*2.29568e-5)) %>%
  group_by(net_id) %>%
  mutate(cnt = n(),
         pcnt_of_block = intersect_area_acres/acre,
         intersect_pop = TotPopSize * pcnt_of_block) %>%
  arrange(net_id)

## Summarize pop 
fishnet_pop <- net_blocks_intersect %>% # xcc
  group_by(net_id) %>%
  summarise(net_pop = sum(intersect_pop)) %>%
  filter(net_pop > 0)   # <-  zeros or no zeros!!!!

# fishnet_pop %>% plot(graticule = TRUE, axes = TRUE)

BASIC_FISHNET_plot <- fishnet_pop %>%
  ggplot(aes(fill = net_pop)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Net Pop")

######### MAKE NET AND RATE FOR ALL CPS VARS
CPS_vars <- grep("CPS_",names(var_list), value = TRUE)
CPS_agg <- NULL

for(i in seq_along(CPS_vars)){
  var_name <- paste0("net_",CPS_vars[i])
  cat(var_name,"\n")
  
  CPS_dat <- var_list[[CPS_vars[i]]] %>%
    mutate(value = 1) %>%
    dplyr::select(value)
  
  fishnet_CPS_var <- aggregate(x = CPS_dat, by = fishnet_pop, FUN = sum) %>%
    st_drop_geometry() %>%
    mutate(Feature = var_name) %>%
    dplyr::select(Feature,value)
  
  CPS_agg <- rbind(CPS_agg, fishnet_CPS_var)
}

CPS_agg <- CPS_agg %>%
  mutate(id = rep(seq(1:nrow(fishnet_pop)),length(CPS_vars))) %>%
  spread(Feature, value) %>%
  dplyr::select(-id) %>%
  mutate(geometry = fishnet_pop$geometry) %>%
  st_as_sf()

#### Spatial join of fishnet_pop and fishnet_cps to then calculate rate for all CPS features

fishnet_pop_cps <- st_join(fishnet_pop, CPS_agg, join = st_equals) %>%
  mutate_at(vars(paste0("net_",CPS_vars)), funs(rate = ./(net_pop/100)))  %>% # cps per 100 person
  # rename_at(vars( contains( "_rate")), .funs = list(paste("rate", gsub("net_|_rate", "", .), sep = "_"))) %>% 
  replace(is.na(.), 0) # replace NA with zero

fishnet_coords <- fishnet_pop_cps %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.matrix()

### 

fishnet_pop_cps_cut <- fishnet_pop_cps %>%
  mutate(net_CPS_Accepted = ifelse(is.na(net_CPS_Accepted), 0, net_CPS_Accepted)) %>% 
  make_cuts(., "net_CPS_Accepted", cuts = "breaks", n_breaks = 10)

CPS_COUNT_BY_FISHNET_PLOT <- lr_base_map + #ggplot() + #ggmap(cps_base_map) +
  geom_sf(data = ll(fishnet_pop_cps_cut), aes(fill = cut_val), inherit.aes = FALSE, color = alpha("white", 1/2), size = 0.2, alpha = 1) +
  labs(title = "CPS count per\nfishnet cell") +
  scale_fill_viridis_d(na.value = NA, option = "D", direction = 1, name = "CPS Count") +
  mapTheme() +
  theme(plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
        plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
        plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
        axis.line = element_blank(),
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 9, family = "sans"))

CPS_COUNT_BY_FISHNET_PLOT


fishnet_pop_cps <- fishnet_pop_cps %>% rename(rate_CPS_Accepted = rate)

fishnet_pop_cps_rate_cut <- fishnet_pop_cps %>%
  mutate(rate_CPS_Accepted = ifelse(is.na(rate_CPS_Accepted), 0, rate_CPS_Accepted)) %>% 
  make_cuts(., "rate_CPS_Accepted", cuts = "breaks", n_breaks = 10)

CPS_RATE_BY_FISHNET_PLOT <- ggplot() + #ggmap(cps_base_map) +
  geom_sf(data = ll(fishnet_pop_cps_rate_cut), aes(fill = cut_val), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  labs(title = "Child Protective Service rate\nper 100 people") +
  scale_fill_viridis_d(na.value = NA, option = "D", direction = 1, name = "CPS Rate\nper 100") +
  mapTheme() +
  theme(plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
        plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
        plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
        axis.line = element_blank(),
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 9, family = "sans"))

# CPS_RATE_BY_FISHNET_PLOT

CPS_Counts_Year_table  <- table(lubridate::year(var_list[["CPS_Accepted"]]$Referral_D))
CPS_Counts_Month_table <- table(lubridate::month(var_list[["CPS_Accepted"]]$Referral_D))

CPS_by_year <- lubridate::year(var_list[["CPS_Accepted"]]$Referral_D) %>%
  data.frame(year = .)
CPS_HIST_BY_DATE <- ggplot(CPS_by_year, aes(x = year)) +
  geom_histogram() +
  plotTheme()

# CPS_HIST_BY_DATE

months <- c("January", "February", "March", "April", 
            "May", "June", "July","August", 
            "September", "October", "November", "December")

## the variable cps is being made here 

cps <- var_list[["CPS_Accepted"]] %>%
  mutate(year  = lubridate::year(Referral_D),
         month = lubridate::month(Referral_D),
         month = months[month],
         month = fct_relevel(month, months))

CPS_POINT_BY_MONTH_plot <- ggplot() +
  geom_point(data = data.frame(st_coordinates(ll(cps)), year = cps$year), 
             aes(x=X, y=Y, color = as.factor(year)), size=1.5, alpha = 0.8) +
  scale_color_viridis_d(name = "Year") +
  labs(title = "CPS Accepted in Little Rock, AR by Year",
       caption = "source: **************") +
  facet_wrap(~year) +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11),
    legend.position = c(0.85, 0.25) # or "none
  )

# CPS_POINT_BY_MONTH_plot

variable = "year"
values <- unique(cps[[variable]])
year_dat <- list()
brks <- 9
window_cps <- get_window(cps, buff_dist = 10000)
for(i in seq_along(values)){
  dat <- filter(cps, !!as.name(variable) == values[i])
  points.ppp <- as.ppp(st_coordinates(ll(dat)),window_cps)
  densityRaster <- raster(density(points.ppp, scalekernel=TRUE, sigma = 0.005))
  dens_data <- gplot_data(densityRaster, maxpixels = 2500) %>%
    mutate(!!as.name(variable) := values[i])
  year_dat[[i]] <- dens_data
}
year_dat <- do.call(rbind, year_dat)

CPS_KDE_BY_YEAR_plot <- ggplot() + #ggmap(cps_base_map) +
  geom_tile(data = year_dat, 
            aes(x,y,fill = as.factor(ntile(value,brks)), 
                group = !!as.name(variable)), alpha=0.8) +
  scale_fill_viridis_d(name = variable) +
  labs(title = "CPS accepted in Richmond, VA by year",
       caption = "Figure 5.2") +
  facet_wrap(vars(!!as.name(variable))) +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11, hjust = 0),
    strip.background = element_rect(fill = "white"),
    legend.position = "none"
  )

# CPS_KDE_BY_YEAR_plot

CPS_by_year_month <- st_drop_geometry(var_list[["CPS_Accepted"]]) %>%
  mutate(month = lubridate::month(Referral_D),
         year  = lubridate::year(Referral_D))%>%
  dplyr::select(month, year) %>%
  group_by(month, year) %>%
  mutate(m_count = n()) %>%
  distinct() %>%
  ungroup()

# CPS_by_year_month

CPS_TREND_BY_MONTH_YEAR_plot <- ggplot(CPS_by_year_month, aes(x = year, y = m_count)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  labs(y="Incidents per month") +
  plotTheme()

# CPS_TREND_BY_MONTH_YEAR_plot

CPS_agg_by_month <- st_drop_geometry(var_list[["CPS_Accepted"]]) %>%
  mutate(month = lubridate::month(Referral_D),
         year  = lubridate::year(Referral_D))%>%
  group_by(month) %>%
  summarise(count = n())

CPS_LINE_AGG_BY_MOTNH_plot <- ggplot(CPS_agg_by_month, aes(x = month, y = count)) +
  scale_x_continuous(breaks = seq(1,12), labels = seq(1,12)) +
  geom_line() +
  plotTheme()

# CPS_LINE_AGG_BY_MOTNH_plot

CPS_normalized_by_month <- st_drop_geometry(var_list[["CPS_Accepted"]]) %>%
  mutate(month = lubridate::month(Referral_D),
         year  = lubridate::year(Referral_D)) %>%
  group_by(year, month) %>%
  summarise(m_total = n()) %>%
  arrange(month, year) %>%
  dplyr::select(month, year, m_total) %>%
  ungroup() %>%
  group_by(month) %>%
  mutate(m_mean = mean(m_total),
         m_sd   = sd(m_total),
         m_z    = (m_total - m_mean) / m_sd)

CPS_LINE_NORMALIZED_plot <- ggplot(CPS_normalized_by_month, aes(x = as.factor(month), 
                                                                y = m_z, group = year, 
                                                                color = as.factor(year))) +
  geom_line() +
  geom_hline(yintercept = 0, color = "gray20", linetype = "dashed") +
  scale_color_viridis_d(name = "year") +
  labs(x = "month") +
  scale_y_continuous(limits = c(-2,2)) +
  plotTheme()

# CPS_LINE_NORMALIZED_plot


CPS_agg_cal <- st_drop_geometry(var_list[["CPS_Accepted"]]) %>%
  mutate() %>%
  mutate(day = factor(weekdays(Referral_D,T),
                      levels = rev(c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun"))),
         week = week(Referral_D),
         month = month(Referral_D),
         year  = year(Referral_D)) %>%
  dplyr::select(day, week, month, year) %>%
  group_by(day, week, month, year) %>%
  summarise(day_cnt = n()) %>%
  complete(day, week, month, year) 

CPS_CALENDAR_plot <- ggplot(CPS_agg_cal, aes(x = week, y = day, fill = day_cnt)) +
  viridis::scale_fill_viridis(name="Incidents",
                              option = 'C',
                              direction = 1,
                              na.value = "gray90") +
  geom_tile(color = 'white', size = 0.1) +
  facet_wrap('year', ncol = 1) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_ipsum_rc()

# CPS_CALENDAR_plot

##
grid_seq <- c(500,1000,1500)
p_loc_l  <- vector(mode = "list", length = length(grid_seq))
p_hist_l <- vector(mode = "list", length = length(grid_seq))
for(i in seq_along(grid_seq)){
  cat(grid_seq[i], "\n")
  net_i <- st_make_grid(lr_tract, cellsize = grid_seq[i])
  net_agg_i <- aggregate(cps_dissolve, net_i, sum) %>% 
    mutate(value = ifelse(is.na(value),0,value))
  
  net_intersect_i <- st_intersects(lr_tract, net_agg_i) 
  # extract Richmonds net cells based on intersect ID
  net_littlerock_i <- net_agg_i[unique(unlist(net_intersect_i)),]
  
  net_littlerock_i$class <- Hmisc::cut2(net_littlerock_i$value, g = 9)
  p_loc <- ggplot() + #ggmap(cps_base_map) +
    geom_sf(data = ll(net_littlerock_i), aes(fill = class), 
            color = NA, inherit.aes = FALSE, size = 0.5, alpha = 0.8) +
    scale_fill_viridis_d(na.value=NA,
                         name   = paste0("Values","\n[quantiles]"),
                         breaks = levels(net_agg_i$class),
                         labels = levels(net_agg_i$class)) +
    mapTheme()
  
  p_loc_l[[i]] <- p_loc
  
  p_hist <- ggplot(net_littlerock_i, aes(x=value)) +
    geom_histogram(bins = 30) +
    # scale_x_continuous(limits = c(-1,100)) +
    # scale_y_continuous(limits = c(0,15)) +
    labs(title = paste0("Cell Dimensions =\n",grid_seq[i]," ft sq")) +
    plotTheme()
  
  p_hist_l[[i]] <- p_hist
}

CPS_COMPARE_FISHNET_GRID_SIZE_3x2_plot <- grid.arrange(p_hist_l[[1]], p_hist_l[[2]], p_hist_l[[3]], p_loc_l[[1]], p_loc_l[[2]], p_loc_l[[3]], ncol = 3)

CPS_COMPARE_FISHNET_GRID_SIZE_3x2_plot 

number <- as.numeric(na.omit(fishnet_pop_cps$net_CPS_Accepted))
fitp <- fitdist(number,"pois", discrete = TRUE)
fitnb <- fitdist(number,"nbinom", discrete = TRUE)
cdfcomp(list(fitp,fitnb)) # plot
gof <- gofstat(list(fitp,fitnb))

net_cell_dims <- seq(500,5000,50)
aic_results <- matrix(nrow=length(net_cell_dims), ncol = 3)
colnames(aic_results) <- c("cell_dim","pois","nbinom")
for(i in seq_along(net_cell_dims)){
  net <- st_make_grid(lr_tract,cellsize=net_cell_dims[i])
  
  cps_cnt <- aggregate(cps_dissolve, net, sum)
  
  number <- as.numeric(na.omit(cps_cnt$value))
  fitp <- fitdist(number,"pois", discrete = TRUE)
  fitnb <- fitdist(number,"nbinom", discrete = TRUE)
  gof <- gofstat(list(fitp,fitnb))
  aic_results[i,1] <- net_cell_dims[i]
  aic_results[i,2] <- as.numeric(gof$bic[1])
  aic_results[i,3] <- as.numeric(gof$bic[2])
}

AIC_LINE_FITDISTR_plot <- data.frame(aic_results) %>%
  gather(dist, aic, -cell_dim) %>%
  rename("Distribution" = dist) %>% 
  mutate(Distribution = case_when(
    Distribution == "nbinom" ~ "Negative Binomial",
    Distribution == "pois"   ~ "Poisson"
  )) %>% 
  ggplot(., aes(x = cell_dim, y = aic, group = Distribution, color = Distribution)) +
  geom_line() +
  labs(y = "AIC - goodness of fit",
       x = "Fishnet Cell Dimension (feet)") +
  plotTheme()

AIC_LINE_FITDISTR_plot


## Predictive modeling 

protective_names <-  c("Banks",
                       "GrocerySuperMarket",
                       "HighSchoolsPublic",
                       "HotelMotel",
                       "ChildCareServices",
                       "ChildYouthServices",
                       "CivilSocialOrgs",
                       "Hospitals",
                       "NeighborhoodResourceCenters",
                       "PoliceFacilities",
                       "ReligiousOrgs")

risk_names <- c( "CRIME_THEFT OF PROPERTY FELONY", 
                 "CRIME_BURGLARY - RESIDENTIAL",
                 "CRIME_AGGRAVATED ASSAULT", 
                 "CRIME_TERRORISTIC ACT",                              
                 "CRIME_THEFT OF PROPERTY MISD",                             
                 "CRIME_RAPE",                                               
                 "CRIME_BATTERY 2ND DEGREE",                                 
                 "CRIME_DOMESTIC BATTERING 2ND DEGREE",                      
                 "CRIME_BREAKING OR ENTERING VEHICLE" ,                      
                 "CRIME_AGGRAVATED ROBBERY (INDIVIDUAL)",                    
                 "CRIME_ROBBERY (INDIVIDUAL)" ,                              
                 "CRIME_AGGRAVATED ASSAULT ON AN FAMILY OR HOUSEHOLD MEMBER",
                 "CRIME_BURGLARY COMMERCIAL" ,                               
                 "CRIME_BATTERY 1ST DEGREE",
                 "BarberAndBeautyShops",
                 "BusStops",
                 "CheckCashingAndPawn",
                 "FastFoodAndBeverage",
                 "GasStationAndConvMart",
                 "HotelMotel",
                 "LiquorStores",
                 "MajorDeptRetailDiscount",
                 "MixedDrink_BarRestClub",
                 "Rental_MobileHomes",
                 "Rental_SingleToQuad",
                 "Rentals_Apts_LessThan100units",
                 "Rentals_Apts_Over100units",
                 "TattooPiercing",
                 "Unsafe_Vacant_BldgsNEW")

risk_var_list <- var_list[grep(paste(risk_names,collapse="|"), names(var_list), value = TRUE)]
protective_var_list <- var_list[grep(paste(protective_names,collapse="|"), names(var_list), value = TRUE)]


risk_plot_dat <- list()
brks <- 9
window_cps <- get_window(cps, buff_dist = 10000)
for(i in seq_along(risk_var_list)){
  var_dat <- risk_var_list[[i]]
  points.ppp <- as.ppp(st_coordinates(ll(var_dat)),window_cps)
  densityRaster <- raster(density(points.ppp, scalekernel=TRUE, sigma = 0.005))
  dens_data <- gplot_data(densityRaster, maxpixels = 2500) %>%
    mutate(variable = names(risk_var_list)[i])
  risk_plot_dat[[i]] <- dens_data
}
risk_plot_dat <- do.call(rbind, risk_plot_dat)

# one-liner to extract all 'geometry' cols from list and rbind
risk_compile <- sf::st_as_sf(data.table::rbindlist(lapply(risk_var_list, '[', "geometry")))
risk.points.ppp <- as.ppp(st_coordinates(ll(risk_compile)),window_cps)
risk_densityRaster <- raster(density(risk.points.ppp, scalekernel=TRUE, sigma = 0.005))
risk_aggregate_plot_data <- gplot_data(risk_densityRaster, maxpixels = 2500) %>%
  mutate(variable = "Risk")

## KDE Risk 
# risk_plot_xy <- risk_plot_dat %>% dplyr::select(x,y)
# library(proj4)
# proj4string <- "+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
# pj = project(risk_plot_xy, proj4string, inverse=TRUE) 
# risk_latlon <- data.frame(lat=pj$y, lon=pj$x)
# risk_plot_latlon = cbind(risk_plot_dat, risk_latlon)

RISK_KDE_FACET_PLOT <- ggplot() + #ggmap(cps_base_map) +
  geom_tile(data = risk_plot_dat, 
            aes(x,y, fill = as.factor(ntile(value,brks)), 
                group = variable), alpha=0.8) +
  scale_fill_viridis_d(name = variable) +
  facet_wrap(~variable) +
  labs(title = "Spatial density of risk factors",
       caption = "Figure 5.4") +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11, hjust = 0),
    legend.position = "none",
    strip.background = element_rect(fill = "white")
  )

RISK_KDE_FACET_PLOT

RISK_KDE_PLOT <- ggplot() + #ggmap(cps_base_map) +
  geom_tile(data = risk_aggregate_plot_data, 
            aes(x,y,fill = as.factor(ntile(value,brks)), 
                group = variable), alpha=0.6) +
  scale_fill_viridis_d(name = variable) +
  #facet_wrap(~variable) +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11),
    legend.position = "none"
  )

RISK_KDE_PLOT

protective_plot_dat <- list()
window_cps <- get_window(cps, buff_dist = 10000)
for(i in seq_along(protective_var_list)){
  var_dat <- protective_var_list[[i]]
  points.ppp <- as.ppp(st_coordinates(ll(var_dat)),window_cps)
  densityRaster <- raster(density(points.ppp, scalekernel=TRUE, sigma = 0.005))
  dens_data <- gplot_data(densityRaster, maxpixels = 2500) %>%
    mutate(variable = names(protective_var_list)[i])
  protective_plot_dat[[i]] <- dens_data
}
protective_plot_dat <- do.call(rbind, protective_plot_dat)

# one-liner to extract all 'geometry' cols from list and rbind
protective_compile <- sf::st_as_sf(data.table::rbindlist(lapply(protective_var_list, '[', "geometry")))
protective.points.ppp <- as.ppp(st_coordinates(ll(protective_compile)),window_cps)
protective_densityRaster <- raster(density(protective.points.ppp, scalekernel=TRUE, sigma = 0.005))
protective_aggregate_plot_data <- gplot_data(protective_densityRaster, maxpixels = 2500) %>%
  mutate(variable = "Protective")

PROTECTIVE_KDE_FACET_PLOT <- ggplot() + #ggmap(cps_base_map) +
  # geom_point(data = data.frame(st_coordinates(ll(cps)),
  #                              month = cps[[variable]]),
  #            aes(x=X, y=Y), size = 1, color = "gray30", alpha = 0.75) +
  geom_tile(data = protective_plot_dat, 
            aes(x,y,fill = as.factor(ntile(value,brks)), 
                group = variable), alpha=0.8) +
  scale_fill_viridis_d(name = variable) +
  facet_wrap(~variable) +
  labs(title = "Spatial density of protective factors",
       caption = "Figure 5.3") +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11, hjust = 0),
    legend.position = "none",
    strip.background = element_rect(fill = "white")
  )

PROTECTIVE_KDE_FACET_PLOT

PROTECTIVE_KDE_PLOT <- ggplot()+  #ggmap(cps_base_map) +
  geom_tile(data = protective_aggregate_plot_data, 
            aes(x,y,fill = as.factor(ntile(value,brks)), 
                group = variable), alpha=0.6) +
  scale_fill_viridis_d(name = variable) +
  #facet_wrap(~variable) +
  mapTheme() +
  theme(
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(face = "plain", size = 11),
    legend.position = "none"
  )

PROTECTIVE_KDE_PLOT

fishnet_knn <- knn2nb(knearneigh(fishnet_coords, k_direction))
fishnet_Weights <- nb2listw(fishnet_knn, style="W")
localMorans  <- as.data.frame(localmoran(fishnet_pop_cps$net_CPS_Accepted, fishnet_Weights))
globalMorans <- moran.mc(fishnet_pop_cps$net_CPS_Accepted, fishnet_Weights, nsim=999)

(GLOBAL_MORANS_PERMUTATION_plot <- ggplot(data.frame(res = globalMorans$res)[1:999,,0], aes(res)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = globalMorans$statistic), colour = "red",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I", 
       x = "Simulated Moran's I Value") +
  plotTheme())

fishnet_pop_cps_morans <- fishnet_pop_cps
fishnet_pop_cps_morans$Ii <- localMorans$Ii
fishnet_pop_cps_morans$pvalue <- localMorans$`Pr(z > 0)`
fishnet_pop_cps_morans <- cbind(fishnet_coords, fishnet_pop_cps_morans)


fishnet_pop_cps_morans_cut <- make_cuts(fishnet_pop_cps_morans, "net_CPS_Accepted",
                                        cuts = "breaks", n_breaks = 10)

## Next chunk 
plot_cps <- lr_base_map + #ggplot() + #ggmap(cps_base_map) +
  geom_sf(data = ll(fishnet_pop_cps_morans_cut), aes(fill = cut_val),
          color = NA, inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_viridis_d(na.value=NA, name = "Maltreatment\nEvents") +
  labs(title = "Panel 1",
       subtitle = "CPS count by fishnet") +
  mapTheme() +
  theme(plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
        plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
        plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
        axis.line = element_blank(),
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 9, family = "sans"))

Ii_cut <- fishnet_pop_cps_morans %>%
  mutate(Ii_cut_val = as.character(Hmisc::cut2(.$Ii, 
                                               cuts = as.numeric(quantile(round(fishnet_pop_cps_morans$Ii,2), 
                                                                          na.rm=T, p = seq(0,1,0.25))))))
plot_Ii <- lr_base_map + #ggplot() + #ggmap(cps_base_map) +
  geom_sf(data = ll(Ii_cut), aes(fill = Ii_cut_val),
          color = NA, inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_viridis_d(na.value=NA, name = "I value", option = "D") +
  labs(title = "Panel 2",
       subtitle = "Local Moran's I value") +
  mapTheme() +
  theme(plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
        plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
        plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
        axis.line = element_blank(),
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 9, family = "sans"))

p_cut <- fishnet_pop_cps_morans %>%
  mutate(pval_cut = ifelse(pvalue > 0.05, "Not\nSignificant", "Significant"))

plot_p <- lr_base_map + #ggplot() + #ggmap(cps_base_map) +
  geom_sf(data = ll(p_cut), aes(fill = pval_cut),
          color = NA, inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_viridis_d(na.value=NA, name = "p-value", option = "D") +
  labs(title = "Panel 3",
       subtitle = "Stastically significant\nmaltreatment clusters",
       caption = "Figure 5.5") +
  mapTheme() +
  theme(plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
        plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
        plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
        axis.line = element_blank(),
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 9, family = "sans"))

MORANS_I_P_plot <- cowplot::plot_grid(plot_cps, plot_Ii, plot_p, ncol =1, align = "hv", axis = "lrbt")
#cowplot::plot_grid(plot_cps, plot_Ii, plot_p,rel_widths = c(0.9,0.9,0.9),ncol = 1, align = "v")

MORANS_I_P_plot

## Now aggregate features will happen 

load("RData/source_file_objects.RData")

names(var_list)
var_list_all = var_list

var_list <- var_list %>% discard((names(var_list)=="LR_BG_Tracts_ACS_DataJoined"))
var_list <- var_list %>% discard((names(var_list)=="LR_Tracts_Working51"))
var_list <- var_list %>% discard((names(var_list)=="CM_LR_Matched_Centerline_3857"))
var_list <- var_list %>% discard((names(var_list)=="2015_Crime_Part1"))

names(var_list)

# var_list_small <- list(var_list[[1]], var_list[[2]])
# names(var_list_small) <- var_names[c(2,3)]

cl <- makePSOCKcluster(3)
registerDoParallel(cl)

ptm <-proc.time()

# agg_results <- Aggregate_points_Features(var_list, net_littlerock)
# save(agg_results, file = "RData/agg_results.RData")

# ED_results <- Euclidean_point_features(var_list, 
#                                        lr_rast_SP,
#                                        lr_tract_diss, 
#                                        net_littlerock)
# save(ED_results, file = "RData/ED_results.RData")

# NN_results <- NN_point_features(var_list, net_littlerock, k_nearest_neighbors)
# save(NN_results, file = "RData/NN_results.RData")

load("RData/ED_results.RData")
load("RData/NN_results.RData")
load("RData/agg_results.RData")

proc.time() - ptm





stopCluster(cl)

## Next chunk not needed - sf1_features_download
## Because we have ACS data downloaded 

sf1_tract <- acs %>% dplyr::select(-Incident_Count_sum, -TotPopSize, -NLTotPop)

sf1_tract <- sf1_tract %>%
  mutate(acre = as.numeric(st_area(acs)*2.29568e-5))

vars_sf1_desc <- sf1_tract %>% st_drop_geometry() %>% dplyr::select(starts_with("P"))%>% names()

net_blocks_intersect <- st_intersection(sf1_tract, net_littlerock) 

# group by cell and calc block stats.
net_blocks_intersect2 <- net_blocks_intersect %>%
  mutate(intersect_area_acres = as.numeric(st_area(net_blocks_intersect)*2.29568e-5)) %>%
  group_by(net_id) %>%
  mutate(cnt = n(),
         pcnt_of_block = intersect_area_acres/acre) %>%
  # intersect_pop = value * pcnt_of_block) %>%
  arrange(net_id) %>%
  mutate_at(vars(matches("^P|^T")), funs(.* pcnt_of_block))

### summarise intersect pops to each net cell and create pop rates for some

fishnet_sf1 <- net_blocks_intersect2 %>% # xcc
  group_by(net_id) %>%
  summarise_at(vars(matches("^P|^H")), funs(sum)) %>%
  dplyr::select(-pcnt_of_block) 

## cast data frame to list of variables
sf1_results <- fishnet_sf1 %>%
  gather(variable, value, -net_id, -geometry) %>%
  mutate(feature_name = paste0("SF1_",variable)) %>%
  group_by(variable) %>%
  nest() %>%
  pull(data)
names(sf1_results) <- paste0("SF1_",setdiff(colnames(fishnet_sf1), c("net_id","geometry")))

## 

fishnet_pop_cps_net <- fishnet_pop_cps %>%
  dplyr::select(net_id, net_pop, rate_CPS_Accepted, net_CPS_Accepted) %>%
  rename(cps_rate = rate_CPS_Accepted,
         cps_net  = net_CPS_Accepted)

## NN features combine 

features <- data.frame(net_id = NN_results[[1]]$net_id, stringsAsFactors = FALSE)
for(i in  seq_along(NN_results)){
  feat_i <- NN_results[[i]] %>%
    st_drop_geometry() %>%
    dplyr::select(net_id, feature_name, value) %>%
    spread(feature_name, value)
  features <- left_join(features, feat_i, by = "net_id")
}
# join features to our target of cps_rate
NN_features <- features %>%
  left_join(., st_drop_geometry(fishnet_pop_cps_net), by = "net_id") 

## 

features <- data.frame(net_id = ED_results[[1]][[1]]$net_id, stringsAsFactors = FALSE)
for(i in  seq_along(ED_results[[1]])){
  feat_i <- ED_results[[1]][[i]] %>%
    st_drop_geometry() %>%
    dplyr::select(net_id, feature_name, value = mean_dist ) %>% ### mean_dist  !!!
    spread(feature_name, value)
  features <- left_join(features, feat_i, by = "net_id")
}
# join features to our target of cps_rate
ED_features <- features %>%
  left_join(., st_drop_geometry(fishnet_pop_cps_net), by = "net_id")

## agg_feature_combine 

features <- data.frame(net_id = agg_results[[1]]$net_id, stringsAsFactors = FALSE)
for(i in  seq_along(ED_results[[1]])){
  feat_i <- agg_results[[i]] %>%
    st_drop_geometry() %>%
    dplyr::select(net_id, feature_name, value) %>%
    spread(feature_name, value)
  features <- left_join(features, feat_i, by = "net_id")
}
# join features to our target of cps_rate
agg_features <- features %>%
  left_join(., st_drop_geometry(fishnet_pop_cps_net), by = "net_id")

## sf1-features-combine 

features <- data.frame(net_id = sf1_results[[1]]$net_id, stringsAsFactors = FALSE)
for(i in  seq_along(sf1_results)){
  feat_i <- sf1_results[[i]] %>%
    st_drop_geometry() %>%
    dplyr::select(net_id, feature_name, value) %>%
    spread(feature_name, value)
  features <- left_join(features, feat_i, by = "net_id")
}
# join features to our target of cps_rate
sf1_features <- features %>%
  left_join(., st_drop_geometry(fishnet_pop_cps_net), by = "net_id")

## corr feature remove NA

cor_NN_features <- NN_features %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  dplyr::select(-net_id)

cor_agg_features <- agg_features %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  dplyr::select(-net_id)

cor_ED_features <- ED_features %>%
  mutate(cps_rate = ifelse(is.na(cps_rate),0,cps_rate),
         net_pop = ifelse(is.na(net_pop),0,net_pop)) %>%
  na.omit() %>%
  dplyr::select(-net_id)

cor_sf1_features <- sf1_features %>%
  mutate(cps_rate = ifelse(is.na(cps_rate),0,cps_rate),
         net_pop = ifelse(is.na(net_pop),0,net_pop)) %>%
  na.omit() %>%
  dplyr::select(-net_id)

## combine all features 

ALL_FEATURES <- full_join(NN_features, agg_features, by = "net_id") %>%
  full_join(.,ED_features, by = "net_id") %>%
  full_join(.,sf1_features, by = "net_id")
all.equal(ALL_FEATURES$cps_rate.x, ALL_FEATURES$cps_rate.y, 
          ALL_FEATURES$cps_rate.x.x, ALL_FEATURES$cps_rate.y.y)

NN_CPS_Accepted <- ALL_FEATURES$NN_CPS_Accepted

ALL_FEATURES <- ALL_FEATURES %>%
  dplyr::select(-cps_rate.y, -cps_rate.x.x, -cps_rate.y.y, 
                -cps_net.y, -cps_net.x.x, -cps_net.y.y,
                -net_pop.y, -net_pop.x.x, -net_pop.y.y) %>%
  dplyr::select(-contains("_CPS_")) %>%
  dplyr::rename(cps_net  = cps_net.x,
                cps_rate = cps_rate.x,
                net_pop  = net_pop.x) %>%
  mutate_all(funs(replace(., is.na(.), 0)))  %>%
  dplyr::rename_all(funs(make.names(.)))
## add NN_CPS_Accepted back in to ALL_FEATURES
ALL_FEATURES$NN_CPS_Accepted <- NN_CPS_Accepted


## Corr all plot 

cps_cor_ALL <- cor(ALL_FEATURES)
All_cors <- cps_cor_ALL[,"cps_net"]

p.mat_ALL <- cor.mtest(ALL_FEATURES)$p
p.mat_ALL <- p.mat_ALL[,which(colnames(cps_cor_ALL)=="cps_net")]

cor_ALL_plot <- data.frame(feature = names(All_cors), 
                           cor = as.numeric(All_cors),
                           p_value   = p.mat_ALL) %>%
  filter(!(feature %in% c("cps_rate","cps_net","net_pop","net_cps","net_id"))) %>%
  filter(!(feature %in% grep("CPS", names(All_cors),value=T))) %>%
  arrange(desc(cor)) %>% 
  mutate(p_value = ifelse(p_value >= 0.05, "Not Significant", "Significant"))

cor_ALL_plot$feature <- factor(cor_ALL_plot$feature,
                               levels=cor_ALL_plot[order(cor_ALL_plot$cor,
                                                         decreasing=F),]$feature)
## corr line positive feature 

CORR_LINE_POSITIVE_FEATURE_plot <- ggplot(dplyr::filter(cor_ALL_plot,cor >= 0), 
                                          aes(x = feature, y = cor, color = factor(p_value))) +
  geom_segment(aes(x = feature, y = 0, xend = feature, yend = cor), color = "grey50") +
  geom_point() +
  coord_flip() +
  scale_color_discrete(name = "p-value") +
  theme_bw()+
  theme(axis.text.y = element_text(size=8))

CORR_LINE_POSITIVE_FEATURE_plot

## corr line negative feature plot 

CORR_LINE_NEGATIVE_FEATURE_plot <- ggplot(dplyr::filter(cor_ALL_plot,cor <= 0), 
                                          aes(x = feature, y = cor, color = factor(p_value))) +
  geom_segment(aes(x = feature, y = 0, xend = feature, yend = cor), color = "grey50") +
  geom_point() +
  coord_flip() +
  scale_color_discrete(name = "p-value") +
  theme_bw()+
  theme(axis.text.y = element_text(size=6))

CORR_LINE_NEGATIVE_FEATURE_plot

## features corr strong 

features_cor <- cor_ALL_plot %>%
  mutate(feature = as.character(feature)) %>%
  arrange(desc(cor)) %>%
  pull(feature)
top_n <- head(features_cor,10)
bottom_n <- tail(features_cor,10)

features_strong_cor <- ALL_FEATURES %>%
  dplyr::select(top_n, bottom_n, cps_net, cps_rate, net_pop, net_id) %>%
  base::identity()


### Now this line has to be modified to meet our needs 
features_protective_all <- ALL_FEATURES %>%
  dplyr::select(contains("Banks"),
                contains("GrocerySuperMarket"),
                contains("HighSchoolsPublic"),
                contains("HotelMotel"),
                contains("ChildCareServices"),
                contains("ChildYouthServices"),
                contains("CivilSocialOrgs"),
                         contains("Hospitals"),
                         contains("NeighborhoodResourceCenters"),
                         contains("PoliceFacilities"),
                         contains("ReligiousOrgs"),
                NN_CPS_Accepted,
                cps_net, cps_rate, net_pop, net_id)

features_strong_protective_names <- cor_ALL_plot %>% 
  filter(feature %in% names(features_protective_all)) %>%
  mutate(prefix = str_extract(feature, "^[^_]+(?=_)"),
         suffix = str_extract(feature, "(?<=_)[^_].*"),
         feature = as.character(feature)) %>%
  group_by(suffix) %>%
  slice(which.max(abs(cor)))

features_protective_strong <- features_protective_all %>%
  dplyr::select(features_strong_protective_names$feature,
                NN_CPS_Accepted,
                cps_net, cps_rate, net_pop, net_id) %>%
  base::identity()


## risk features all

features_risk_all <- ALL_FEATURES %>%
  dplyr::select(contains("CRIME_THEFT.OF.PROPERTY.FELONY"),
                contains("CRIME_BURGLARY...RESIDENTIAL"),
                contains("CRIME_TERRORISTIC.ACT"),
                contains("NN_CRIME_THEFT.OF.PROPERTY.MISD"),
                contains("NN_CRIME_RAPE"),                                                
                contains("NN_CRIME_BATTERY.2ND.DEGREE" ),                                 
                contains("NN_CRIME_DOMESTIC.BATTERING.2ND.DEGREE"),                       
                contains("NN_CRIME_BREAKING.OR.ENTERING.VEHICLE" ),                       
                contains("NN_CRIME_AGGRAVATED.ROBBERY..INDIVIDUAL." ),                    
                contains("NN_CRIME_ROBBERY..INDIVIDUAL."  ),                              
                contains("NN_CRIME_AGGRAVATED.ASSAULT.ON.AN.FAMILY.OR.HOUSEHOLD.MEMBER" ),
                contains("NN_CRIME_BURGLARY.COMMERCIAL" ),                                
                contains("NN_CRIME_BATTERY.1ST.DEGREE"), 
                contains("BarberAndBeautyShops"),
                contains("BusStops"),
                contains("CheckCashingAndPawn"),
                contains("FastFoodAndBeverage"),
                contains("GasStationAndConvMart"),
                contains("HotelMotel"),
                contains("LiquorStores"),
                contains("MajorDeptRetailDiscount"),
                contains("MixedDrink_BarRestClub"),
                contains("Rental_MobileHomes"),
                contains("Rental_SingleToQuad"),
                contains("Rentals_Apts_LessThan100units"),
                contains("Rentals_Apts_Over100units"),
                contains("TattooPiercing"),
                contains("Unsafe_Vacant_BldgsNEW"),
                NN_CPS_Accepted,
                cps_net, cps_rate, net_pop, net_id)

## features_risk_strong

features_risk_strong_names <- cor_ALL_plot %>%
  filter(feature %in% names(features_risk_all)) %>%
  mutate(prefix = str_extract(feature, "^[^_]+(?=_)"),
         suffix = str_extract(feature, "(?<=_)[^_].*"),
         feature = as.character(feature)) %>%
  group_by(suffix) %>%
  slice(which.max(abs(cor)))

features_risk_strong <- features_risk_all %>%
  dplyr::select(features_risk_strong_names$feature,
                NN_CPS_Accepted,
                cps_net, cps_rate, net_pop, net_id) %>%
  base::identity()

# features_census_select

features_census_select <- ALL_FEATURES %>%
  dplyr::select(SF1_Perc_Under18,
                 SF1_Perc_Black, 
                 SF1_Perc_NonWh, 
                 SF1_Perc_Hispa,
                 SF1_Perc_NonMarr_Fam_HH,
                 SF1_Perc_FHH, 
                 SF1_Perc_SingPrnt_HH,
                 SF1_PercLowEdu, 
                 SF1_Perc_RenterOcc,
                 SF1_Perc_PopUnder18inPov,
                 SF1_Perc_PopStrugg,
                 SF1_Perc_NotInsured,
                 SF1_Perc_PublicInsure,
                 SF1_PercHighHH, 
                 SF1_PercCollEd,
                 SF1_Perc_OwnHo,   
                 cps_net, cps_rate, net_pop, net_id)

features_risk_strong_plot <- features_risk_strong %>%
  dplyr::select(-net_id)

CORR_RISK_FEATURES_plot <- feature_corrplot(features_risk_strong_plot, "Correlation of Risk Features")

features_protective_strong_plot <- features_protective_strong %>%
  dplyr::select(-net_id)
CORR_PROTECTIVE_FEATURES_plot <- feature_corrplot(features_protective_strong_plot, "Correlation of Protective Features")

## Line 1096 is Corr-protective-features-plot 
## Line 1560 is feature prep 
## We can safely ignore bunch of plots in between these two chunks 
## feature prep 

target_var <- "cps_net"
features_protective_strong2 <- dplyr::select(features_protective_strong, -cps_rate, -net_pop)
features_risk_strong2 <- dplyr::select(features_risk_strong, -cps_rate, -net_pop)
features_census_select2     <- dplyr::select(features_census_select, -cps_rate, -net_pop)


## model data prep 

full_join(features_risk_strong, features_census_select, by = "net_id") %>%
  full_join(., features_protective_strong, by = "net_id") %>% names()

og_dat <- full_join(features_risk_strong, features_census_select, by = "net_id") %>%
  full_join(., features_protective_strong, by = "net_id") %>% 
  dplyr::select(-net_pop.y, -cps_net.y, -cps_rate.y,
                -net_pop.x, -cps_net.x, -cps_rate.x) 
 

dat    <- og_dat %>% dplyr::select(-cps_rate, -net_pop, -net_id) %>%
  mutate_at(vars(-cps_net), scale_this) %>%
  identity() # line ender (does nothing)

net_hood <- st_join(net_littlerock, lr_tract, largest = TRUE)
all.equal(net_hood$net_id, og_dat$net_id)
og_dat$.block_id <- net_hood$NAME

## tract fixed effects 

hood_matrix <- model.matrix(cps_net~.block_id,og_dat)
hood_model <- lm(sqrt(og_dat$cps_net) ~ hood_matrix)
dat$hood_fixed <- predict(hood_model, type = "response")^2
og_dat$hood_fixed <- predict(hood_model, type = "response")^2


## create cv fold_tibble 

n_folds = 5

target_var <- "cps_net"

all_hoods <- length(unique(net_hood$name))
n_folds = ifelse(n_folds == "LOOCV", all_hoods, n_folds)
folds_index <- groupdata2::fold(og_dat, k = n_folds, id_col = '.block_id')$.folds

cv_tbl <- tibble(folds = seq_len(n_folds),
                 train = NA, train_y = NA, train_index = NA, train_net_id = NA,
                 test  = NA, test_y  = NA, test_index  = NA, test_net_id  = NA)

for(k in seq_len(n_folds)){
  fold_i  <- which(folds_index == k)
  cv_tbl[k,]$train         <- list(dat[-fold_i,])
  cv_tbl[k,]$test          <- list(dat[ fold_i,])
  cv_tbl[k,]$train_y       <- list(og_dat[-fold_i,target_var])
  cv_tbl[k,]$test_y        <- list(og_dat[ fold_i,target_var])
  cv_tbl[k,]$train_index   <- list(setdiff(seq(1:nrow(dat)),fold_i))
  cv_tbl[k,]$test_index    <- list(fold_i)
  cv_tbl[k,]$train_net_id  <- list(og_dat[-fold_i,"net_id"])
  cv_tbl[k,]$test_net_id   <- list(og_dat[ fold_i,"net_id"])
}

cv_sf <- left_join(og_dat, net_littlerock, by = "net_id") %>%
  st_as_sf() %>%
  dplyr::select(.block_id)
NEIGHBORHOOD_FOLDS_plot <- plot(cv_sf)

## Poisson regression 

po_cv_tbl <- cv_tbl %>%
  mutate(fit   = map(train, glm_fit, 
                     formula =  paste("cps_net ~ ."), 
                     family = "poisson"),
         pred  = map2(fit, test, lm_predict, sqrt = FALSE),
         mdl_nam = "GLM - Poisson") %>% 
  score_model()

ponet_cv_tbl <- cv_tbl %>%
  mutate(fit   = map(train, glmnet_fit, 
                     formula =  paste("cps_net ~ ."), 
                     family = "poisson"),
         pred  = map2(fit, test, lm_predict, sqrt = FALSE),
         mdl_nam = "Bayes GLM - Poisson") %>% 
  score_model()

cat("Test Set MAE:",mean(po_cv_tbl$MAE),"\n")
cat("Test Set logdev:",mean(po_cv_tbl$logdev, na.rm=TRUE),"\n")

## Poisson Regression fit plot 

POISSON_REGRESSION_FIT_plot <- plot_fold_pred(po_cv_tbl$pred, po_cv_tbl$test_y, type = "fit")

POISSON_REGRESSION_FIT_plot

## Poisson Coefficients 

POISSON_GLM_COEFF_plot <- plot_model(po_cv_tbl$fit[[1]], sort.est = TRUE)

## Random Forest 

rf_cv_tbl <- cv_tbl %>%
  mutate(fit   = map(train, rf_fit, formula = "cps_net ~ .", mtry_add = 2, importance = "impurity"),
         pred  = map2(fit, test, lm_predict),
         mdl_nam = "Random Forest") %>% 
  score_model()
cat("Test Set MAE:",mean(rf_cv_tbl$MAE),"\n")
cat("Test Set logdev:",mean(rf_cv_tbl$logdev, na.rm=TRUE),"\n")


## Random Forest Var Imp Plot 

varimp_dat <- data.frame(importance = rf_cv_tbl$fit[[1]]$variable.importance) %>% 
  rownames_to_column("variable")

RF_VARIMP_PLOT <- ggplot(varimp_dat, aes(x=reorder(variable,importance), y=importance, fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  labs(y = "Variable Importance",
       x = " ", 
       title = "Feature importance",
       subtitle = "Random Forest sub-model",
       caption = "Figure 6.4") +
  guides(fill=F)+
  scale_fill_viridis_c() +
  plotTheme()+
  theme(axis.text.y = element_text(size = 6))

RF_VARIMP_PLOT

ggsave(file = "RF-Var-Imp-Plot.png",RF_VARIMP_PLOT, height = 9, width = 7)

RANDOM_FOREST_FIT_plot <- plot_fold_pred(rf_cv_tbl$pred, rf_cv_tbl$test_y, type = "fit")

RANDOM_FOREST_FIT_plot

## Spatial Error Regression

spat_durbin <- errorsarlm(sqrt(cps_net) ~ ., data = dat, listw, etype ="emixed")
spat_durbin_tbl <- tibble(
  fit   = list(spat_durbin),
  pred  = map(fit, sar_pred),
  test_y= list(dat$cps_net),
  test_net_id = list(og_dat$net_id),
  mdl_nam = "Spatial Durbin - sqrt") %>% 
  score_model()
cat("Test Set MAE:",mean(spat_durbin_tbl$MAE),"\n")
cat("Test Set logdev:",mean(spat_durbin_tbl$logdev, na.rm=TRUE),"\n")

SPATIAL_ERROR_FIT_plot <- plot_fold_pred(spat_durbin_tbl$pred, dat$cps_net, type = "fit")

POISSON_REGRESSION_FIT_plot+RANDOM_FOREST_FIT_plot+SPATIAL_ERROR_FIT_plot


## Gather oof prediction 

po_pred_dat <- po_cv_tbl %>%
  unnest(pred) %>%
  mutate(test_y = po_cv_tbl %>% unnest(test_y) %>% pull(test_y),
         test_net_id = po_cv_tbl %>% unnest(test_net_id) %>% pull(test_net_id))

po_pred_geoplot <- model_pred_geoplot(po_pred_dat$pred,
                                      po_pred_dat$test_y,
                                      po_pred_dat$test_net_id,
                                      net_littlerock, cps_base_map, "po")

rf_pred_dat <- rf_cv_tbl %>%
  unnest(pred) %>%
  mutate(test_y = rf_cv_tbl %>% unnest(test_y) %>% pull(test_y),
         test_net_id = rf_cv_tbl %>% unnest(test_net_id) %>% pull(test_net_id))

rf_pred_geoplot <- model_pred_geoplot(rf_pred_dat$pred,
                                      rf_pred_dat$test_y,
                                      rf_pred_dat$test_net_id,
                                      net_littlerock, cps_base_map,
                                      "Rand
                                      om Forest")
### One special 
if(T){
  pred_dat <- data.frame(pred = rf_pred_dat$pred,
                         obs  = rf_pred_dat$test_y,
                         net_id = rf_pred_dat$test_net_id)
  
  MAE_geoplot <- net_littlerock %>%
    left_join(., pred_dat, by = "net_id") %>% 
    mutate(MAE = round(abs(pred - obs),2),
           feature_name = paste0("RF"," ", "MAE")) %>%
    make_cuts(., "MAE")
  
  MAE_geoplot %>%  ggplot(aes(fill = MAE)) + 
    geom_sf(color = NA) + 
    scale_fill_viridis_d() + 
    labs(title = "MAE")
}


###
sarlm_pred_dat <- spat_durbin_tbl %>%
  unnest(pred) %>%
  mutate(test_y = spat_durbin_tbl %>% unnest(test_y) %>% pull(test_y),
         test_net_id = spat_durbin_tbl %>% unnest(test_net_id) %>% pull(test_net_id))

sarlm_pred_geoplot <- model_pred_geoplot(sarlm_pred_dat$pred,
                                         sarlm_pred_dat$test_y,
                                         sarlm_pred_dat$test_net_id,
                                         net_littlerock, cps_base_map,
                                         "SARLM")


cps_preds <- og_dat %>% 
  dplyr::select(net_id, cps_net) %>% 
  left_join(., dplyr::select(po_pred_dat,
                             net_id = test_net_id,
                             pred_lm = pred), by = "net_id") %>%
  left_join(., dplyr::select(rf_pred_dat, 
                             net_id = test_net_id,
                             pred_rf = pred), by = "net_id") %>% 
  left_join(., dplyr::select(sarlm_pred_dat, 
                             net_id = test_net_id,
                             pred_sarlm = pred), by = "net_id") %>% 
  mutate_if(is.double, round, 2)


## Meta model stacking 

if(all.equal(cps_preds$net_id, net_hood$net_id)){
  cat("Predictions and spatial data are in same order, GOOD to go!", "\n")
} else {
  cat("There is a PROBLEM with order of predictions and spatial data; Likely Errors!","\n")
}

cps_preds_cv_dat <- dplyr::select(cps_preds, -net_id)
ens_cv_tbl <- tibble(folds = seq_len(n_folds),
                     train = NA, train_y = NA, train_index = NA, train_net_id = NA,
                     test  = NA, test_y  = NA, test_index  = NA, test_net_id  = NA)
for(k in seq_len(n_folds)){
  fold_i  <- which(folds_index == k)
  ens_cv_tbl[k,]$train         <- list(cps_preds_cv_dat[-fold_i,])
  ens_cv_tbl[k,]$test          <- list(cps_preds_cv_dat[ fold_i,])
  ens_cv_tbl[k,]$train_y       <- list(cps_preds_cv_dat[-fold_i,target_var])
  ens_cv_tbl[k,]$test_y        <- list(cps_preds_cv_dat[ fold_i,target_var])
  ens_cv_tbl[k,]$train_index   <- list(setdiff(seq(1:nrow(cps_preds_cv_dat)),fold_i))
  ens_cv_tbl[k,]$test_index    <- list(fold_i)
  ens_cv_tbl[k,]$train_net_id  <- list(cps_preds[-fold_i,"net_id"])
  ens_cv_tbl[k,]$test_net_id   <- list(cps_preds[ fold_i,"net_id"])
}

ens_cv_tbl <- ens_cv_tbl %>%
  mutate(fit   = map(train, rf_fit, formula = "cps_net ~ pred_rf + pred_sarlm"),
         pred  = map2(fit, test, lm_predict),
         # pred  = map(pred, round),
         mdl_nam = "Meta-Model") %>% 
  score_model()

cat("Test Set MAE:",mean(ens_cv_tbl$MAE),"\n")
cat("Test Set logdev:",mean(ens_cv_tbl$logdev),"\n")

## Meta model fit plot 

META_MODEL_FIT_plot <- plot_fold_pred(ens_cv_tbl$pred, ens_cv_tbl$test_y, type = "fit") +
  labs(x = "Observed Maltreatment Counts",
       y = "Predicted Maltreatment Counts",
       title = "Predicted vs. observed maltreatment counts",
       caption = "Figure 1.7") +
  plotTheme() +
  theme(panel.border = element_blank())

## join meta moodel predictions

ens_pred_dat <- ens_cv_tbl %>% 
  unnest(pred) %>% 
  mutate(test_y = ens_cv_tbl %>% unnest(test_y) %>% pull(test_y),
         test_net_id = ens_cv_tbl %>% unnest(test_net_id) %>% pull(test_net_id)) 

ens_pred_geoplot <- model_pred_geoplot(ens_pred_dat$pred, 
                                       ens_pred_dat$test_y, 
                                       ens_pred_dat$test_net_id,
                                       net_littlerock, cps_base_map, 
                                       "Meta-Model")
cps_preds2 <- cps_preds %>% 
  left_join(., dplyr::select(ens_pred_dat, 
                             net_id = test_net_id,
                             pred_ens = pred) %>% 
              mutate(pred_ens = round(pred_ens,2)), by = "net_id") 

## PREDICTION_MAP_plots

POISSON_MODEL_PREDICTION_MAP_plot <- cowplot::plot_grid(po_pred_geoplot[[2]] + 
                                                          labs(title = "Poisson Regression",
                                                               subtitle = "Predicted Maltreatment Count"), 
                                                        po_pred_geoplot[[1]] + 
                                                          labs(subtitle = "MAE") +
                                                          scale_fill_viridis_d(name = "MAE"), 
                                                        align = "h")

POISSON_MODEL_PREDICTION_MAP_plot

RF_MODEL_PREDICTION_MAP_plot <- cowplot::plot_grid(rf_pred_geoplot[[2]] + 
                                                     labs(title = "Random Forest",
                                                          subtitle = "Predicted Maltreatment Count"),
                                                   rf_pred_geoplot[[1]] + 
                                                     labs(subtitle = "MAE") +
                                                     scale_fill_viridis_d(name = "MAE"), 
                                                   align = "h")

RF_MODEL_PREDICTION_MAP_plot


SARLM_MODEL_PREDICTION_MAP_plot <- cowplot::plot_grid(sarlm_pred_geoplot[[2]] + 
                                                        labs(title = "Spatial Durbin Model",
                                                             subtitle = "Predicted Maltreatment Count") +
                                                        mapTheme() + 
                                                        theme(panel.border = element_blank()), 
                                                      sarlm_pred_geoplot[[1]] + 
                                                        labs(subtitle = "MAE") +
                                                        scale_fill_viridis_d(name = "MAE") +
                                                        mapTheme() + 
                                                        theme(panel.border = element_blank()), 
                                                      align = "h")

META_MODEL_PREDICTION_MAP_plot <- cowplot::plot_grid(ens_pred_geoplot[[2]] + 
                                                       labs(title = "Meta-Model",
                                                            subtitle = "Predicted Maltreatment Count",
                                                            caption = "Figure 6.2") +
                                                       mapTheme() + 
                                                       theme(panel.border = element_blank()), 
                                                     ens_pred_geoplot[[1]] + 
                                                       labs(subtitle = "MAE") +
                                                       scale_fill_viridis_d(name = "MAE") +
                                                       mapTheme() + 
                                                       theme(panel.border = element_blank()), 
                                                     align = "h")

### r model_error_by_decile
models <- bind_rows(rf_cv_tbl, spat_durbin_tbl, ens_cv_tbl, po_cv_tbl)

CV_preds_long <- models %>%
  group_by(mdl_nam) %>%
  unnest(pred, test_y) 

## map over all quantiles to get error metrics
quantile_errors <- CV_preds_long %>%
  nest(-mdl_nam) %>%
  mutate(q      = list(seq(0,1,0.01)),
         pred   = map(data, "pred"),
         test_y = map(data, "test_y")) %>%
  dplyr::select(-data) %>%
  unnest(q, .preserve = c(pred, test_y)) %>%
  filter(q != 0) %>% 
  mutate(q_dat  = pmap(list(pred, test_y, q), quantile_error),
         q_pred = map(q_dat, "pred"),
         q_obs  = map(q_dat, "obs"),
         q_RMSE = map2_dbl(q_pred, q_obs, rmse),
         q_MAE  = map2_dbl(q_pred, q_obs, mae),
         q_logdev  = map2_dbl(q_pred, q_obs, logdev_p),
         y_max  = quantile(seq(0,max(dat$cps_net)), q),
         q_cnt  = nrow(og_dat) - map_int(q_dat, nrow))

q_error_plotdat <- quantile_errors %>%
  dplyr::select(mdl_nam, q, q_RMSE, q_MAE, q_logdev)
q_cnt_plotdat <- quantile_errors %>% 
  dplyr::select(mdl_nam, q, y_max, q_cnt) %>% 
  filter(q != 0) %>%
  mutate(q_pcnt = (q_cnt / nrow(og_dat)))
q_error_mean <- q_error_plotdat %>%
  group_by(mdl_nam) %>%
  summarise(mean_RMSE = mean(q_RMSE, na.rm = TRUE),
            mean_MAE  = mean(q_MAE, na.rm = TRUE),
            mean_logdev  = mean(q_logdev, na.rm = TRUE)) %>%
  arrange(desc(mean_logdev))
print(q_error_mean)


## Appendix 4e - Model Error Table 


# Helper function for quantile error
quantile_error <- function(pred,obs,quant){
  preds <- data.frame(pred = pred, obs = obs) %>%
    filter(quantile(seq(0,max(obs)), quant)>obs)
  return(preds)
}
# Join/bind model prediction tables
models <- bind_rows(rf_cv_tbl, spat_durbin_tbl, ens_cv_tbl, po_cv_tbl)
# Unnest predictions by model
CV_preds_long <- models %>%
  group_by(mdl_nam) %>%
  unnest(pred, test_y) 
## Map over all quantiles to get error metrics
quantile_errors <- CV_preds_long %>%
  nest(-mdl_nam) %>%
  mutate(q      = list(seq(0,1,0.01)),
         pred   = map(data, "pred"),
         test_y = map(data, "test_y")) %>%
  dplyr::select(-data) %>%
  unnest(q, .preserve = c(pred, test_y)) %>%
  filter(q != 0) %>% 
  mutate(q_dat  = pmap(list(pred, test_y, q), quantile_error),
         q_pred = map(q_dat, "pred"),
         q_obs  = map(q_dat, "obs"),
         q_RMSE = map2_dbl(q_pred, q_obs, rmse),
         q_MAE  = map2_dbl(q_pred, q_obs, mae),
         q_logdev  = map2_dbl(q_pred, q_obs, logdev_p),
         y_max  = quantile(seq(0,max(dat$cps_net)), q),
         q_cnt  = nrow(og_dat) - map_int(q_dat, nrow))

# Map over all predictions grouped by model to calculate mean and sd for error metrics
model_results <- models %>%
  dplyr::select("Model Name" = mdl_nam, R2, RMSE, MAE, logdev) %>%
  group_by(`Model Name`) %>%
  arrange(`Model Name`) %>%
  summarise(R2_mean      = mean(R2, na.rm=TRUE),
            R2_sd        = sd(R2, na.rm=TRUE),
            MAE_mean     = mean(MAE, na.rm=TRUE),
            MAE_sd       = sd(MAE, na.rm=TRUE),
            RMSE_mean    = mean(RMSE, na.rm=TRUE),
            RMSE_sd      = sd(RMSE, na.rm=TRUE),
            logdev_mean  = mean(logdev, na.rm=TRUE),
            logdev_sd    = sd(logdev, na.rm=TRUE)) 
Model_Error_Results_table <- model_results %>%
  kable(., format = "html", digits = 3) %>%
  kable_styling()

# load("full_results_070619.RData")

## Line 1929 of Richmond PAP Report R markdown 

meta_log_mean <- model_results[which(model_results$`Model Name` == "Meta-Model"),"logdev_mean",drop=TRUE]
meta_log_sd <- model_results[which(model_results$`Model Name` == "Meta-Model"),"logdev_sd",drop=TRUE]
meta_log_error <- qnorm(0.975)*meta_log_sd/sqrt(nrow(ens_cv_tbl))
meta_log_error_lower <- round(meta_log_mean - meta_log_error,3)
meta_log_error_upper <- round(meta_log_mean + meta_log_error,3)

meta_MAE_mean <- model_results[which(model_results$`Model Name` == "Meta-Model"),"MAE_mean",drop=TRUE]
meta_MAE_sd <- model_results[which(model_results$`Model Name` == "Meta-Model"),"MAE_sd",drop=TRUE]
meta_MAE_error <- qnorm(0.975)*meta_MAE_sd/sqrt(nrow(ens_cv_tbl))
meta_MAE_error_lower <- round(meta_MAE_mean - meta_MAE_error,3)
meta_MAE_error_upper <- round(meta_MAE_mean + meta_MAE_error,3)


## chunk aggregate_model_errors_to_neighborhood

error_geoplot <-  net_littlerock %>%
  left_join(., ens_pred_dat, by = c("net_id" = "test_net_id"),
            feature_name = paste0("Meta-Model", "dev")) %>%
  score_model() %>%
  mutate(dev_p_inv = 1 - logdev) %>% 
  make_cuts(., "logdev", cuts = "breaks", n_breaks = 5)

# error metrics to points
error_points <- st_centroid(error_geoplot) %>%
  dplyr::select(logdev, MAE, test_y) ##add net_id

# aggreate mean errors to neighborhoods
neighborhood_metric_logdev <- error_points %>%
  aggregate(., lr_tract, mean) %>%
  dplyr::select(logdev) %>% 
  make_cuts(., "logdev")  ## lr_tract replaces nbr

neighborhood_metric_MAE<- error_points %>%
  aggregate(., lr_tract, mean) %>%
  dplyr::select(MAE) %>% 
  mutate(MAE = round(MAE,2)) %>% 
  make_cuts(., "MAE")

## model error by decile 

models <- bind_rows(rf_cv_tbl, spat_durbin_tbl, ens_cv_tbl, po_cv_tbl)

CV_preds_long <- models %>%
  group_by(mdl_nam) %>%
  unnest(pred, test_y) 

## map over all quantiles to get error metrics
quantile_errors <- CV_preds_long %>%
  nest(-mdl_nam) %>%
  mutate(q      = list(seq(0,1,0.01)),
         pred   = map(data, "pred"),
         test_y = map(data, "test_y")) %>%
  dplyr::select(-data) %>%
  unnest(q, .preserve = c(pred, test_y)) %>%
  filter(q != 0) %>% 
  mutate(q_dat  = pmap(list(pred, test_y, q), quantile_error),
         q_pred = map(q_dat, "pred"),
         q_obs  = map(q_dat, "obs"),
         q_RMSE = map2_dbl(q_pred, q_obs, rmse),
         q_MAE  = map2_dbl(q_pred, q_obs, mae),
         q_logdev  = map2_dbl(q_pred, q_obs, logdev_p),
         y_max  = quantile(seq(0,max(dat$cps_net)), q),
         q_cnt  = nrow(og_dat) - map_int(q_dat, nrow))

q_error_plotdat <- quantile_errors %>%
  dplyr::select(mdl_nam, q, q_RMSE, q_MAE, q_logdev)
q_cnt_plotdat <- quantile_errors %>% 
  dplyr::select(mdl_nam, q, y_max, q_cnt) %>% 
  filter(q != 0) %>%
  mutate(q_pcnt = (q_cnt / nrow(og_dat)))
q_error_mean <- q_error_plotdat %>%
  group_by(mdl_nam) %>%
  summarise(mean_RMSE = mean(q_RMSE, na.rm = TRUE),
            mean_MAE  = mean(q_MAE, na.rm = TRUE),
            mean_logdev  = mean(q_logdev, na.rm = TRUE)) %>%
  arrange(desc(mean_logdev))
print(q_error_mean)

### Error decile plots 

LOGDEV_MODEL_ERROR_BY_DECILE_plot <- ggplot(data = q_error_plotdat, aes(x=q, y=q_logdev, group = mdl_nam, color = factor(mdl_nam))) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Model") +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Logarithmic Score",
       caption = "Figure 6.2 - Goodness of fit by decile") +
  plotTheme() +
  theme(legend.position = "right")

MAE_MODEL_ERROR_BY_DECILE_plot <- ggplot(data = q_error_plotdat, aes(x=q, y=q_MAE, group = mdl_nam, color = factor(mdl_nam))) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Model") +
  labs(y = "MAE") +
  plotTheme()  +
  theme(legend.position = "right")

COUNT_BY_DECILE_plot <- ggplot(data = q_cnt_plotdat, 
                               aes(x=q, y=q_cnt, group = mdl_nam, color = factor(mdl_nam))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks=seq(0,1,0.1), labels = seq(0,1,0.1)) +
  scale_color_viridis_d(name = "Model") +
  labs(y = "Number of Predictions in Each Decile",
       x = "Decile") +
  plotTheme()  +
  theme(legend.position = "right")


legend <- get_legend(LOGDEV_MODEL_ERROR_BY_DECILE_plot + plotTheme() + theme(legend.position = "right"))

### Model_Error_Results_table

model_results <- models %>%
  dplyr::select("Model Name" = mdl_nam, R2, RMSE, MAE, logdev) %>%
  group_by(`Model Name`) %>%
  arrange(`Model Name`) %>%
  summarise(R2_mean      = mean(R2, na.rm=TRUE),
            R2_sd        = sd(R2, na.rm=TRUE),
            MAE_mean     = mean(MAE, na.rm=TRUE),
            MAE_sd       = sd(MAE, na.rm=TRUE),
            RMSE_mean    = mean(RMSE, na.rm=TRUE),
            RMSE_sd      = sd(RMSE, na.rm=TRUE),
            logdev_mean  = mean(logdev, na.rm=TRUE),
            logdev_sd    = sd(logdev, na.rm=TRUE)) 
Model_Error_Results_table <- model_results %>%
  kable(., format = "html", digits = 3) %>%
  kable_styling()

meta_log_mean <- model_results[which(model_results$`Model Name` == "Meta-Model"),"logdev_mean",drop=TRUE]
meta_log_sd <- model_results[which(model_results$`Model Name` == "Meta-Model"),"logdev_sd",drop=TRUE]
meta_log_error <- qnorm(0.975)*meta_log_sd/sqrt(nrow(ens_cv_tbl))
meta_log_error_lower <- round(meta_log_mean - meta_log_error,3)
meta_log_error_upper <- round(meta_log_mean + meta_log_error,3)

meta_MAE_mean <- model_results[which(model_results$`Model Name` == "Meta-Model"),"MAE_mean",drop=TRUE]
meta_MAE_sd <- model_results[which(model_results$`Model Name` == "Meta-Model"),"MAE_sd",drop=TRUE]
meta_MAE_error <- qnorm(0.975)*meta_MAE_sd/sqrt(nrow(ens_cv_tbl))
meta_MAE_error_lower <- round(meta_MAE_mean - meta_MAE_error,3)
meta_MAE_error_upper <- round(meta_MAE_mean + meta_MAE_error,3)

## Line 1944 : change nbr to lr_tract
## Chunk aggregate_model_errors_to_neighborhood

error_geoplot <-  net_littlerock %>%
  left_join(., ens_pred_dat, by = c("net_id" = "test_net_id"),
            feature_name = paste0("Meta-Model", "dev")) %>%
  score_model() %>%
  mutate(dev_p_inv = 1 - logdev) %>% 
  make_cuts(., "logdev", cuts = "breaks", n_breaks = 5)

# error metrics to points
error_points <- st_centroid(error_geoplot) %>%
  dplyr::select(logdev, MAE, test_y)

# aggreate mean errors to neighborhoods
neighborhood_metric_logdev <- error_points %>%
  aggregate(., lr_tract, mean) %>%
  dplyr::select(logdev) %>% 
  make_cuts(., "logdev")

neighborhood_metric_MAE<- error_points %>%
  aggregate(., lr_tract, mean) %>%
  dplyr::select(MAE) %>% 
  mutate(MAE = round(MAE,2)) %>% 
  make_cuts(., "MAE")

##MODEL_ERROR_BY_NEIGHBORHOOD_plots

LOGDEV_BY_NEIGHBORHOOD_plot <- make_fishnet_dist_plot(neighborhood_metric_logdev, cps_base_map, legend = "right", 
                                                      direction = 1, var_name = "Deviance", 
                                                      title = "Out-of-Fold error by Census Tract") + 
  labs(caption = "Figure 6.3",
       subtitle = "Logarithmic score") +
  mapTheme()

MAE_BY_NEIGHBORHOOD_plot <- make_fishnet_dist_plot(neighborhood_metric_MAE, cps_base_map, legend = "right", 
                                                   direction = 1, var_name = "MAE") +
  labs(subtitle = "MAE") +
  mapTheme()

MAE_BY_NEIGHBORHOOD_plot

## The following lines are inside the main text - not loaded chunks 
## See lines 3540-3547 of Richmond-PAP-Report Rmd

plot_fold_pred(ens_cv_tbl$pred, ens_cv_tbl$test_y, type = "fit") +
  labs(x = "Observed Maltreatment Counts",
       y = "Predicted Maltreatment Counts",
       title = "Predicted vs. observed maltreatment counts",
       caption = "Figure 6.1") +
  plotTheme() +
  theme(panel.border = element_blank())


save.image(file = "full_results_line_1983_0718.RData")

load("full_results_line_1983_0718.RData")


## Next chunk: line 1983: {r statistical_area_download}

## Below we calculate poverty and nonWhite rates by neighborhood by converting tracts to centroids and spatial joining with 
#neighborhoods statistical areas. 

#get statarea
# nbr_statAreas <- read_sf("https://data.richmondgov.com/resource/8kyq-v9j2.geojson") %>%
#   st_transform(crs = 102747) %>% 
#   mutate(stat_area_id = id)

lr_tract_statAreas <- lr_tract %>% mutate(stat_area_id = GEOID)

## acs data (we have this already)

tract10 <- var_list[["LR_BG_Tracts_ACS_DataJoined"]]

tract10 <- tract10 %>% dplyr::select(TotPopSize, Perc_NonWh, Perc_PopSt, 
                                     Perc_NotIn, Perc_PopUn,geometry)

tract10 <- tract10 %>% dplyr::rename( Perc_PopUnder18inPov = Perc_PopUn,
            Perc_PopStrugg = Perc_PopSt,
            Perc_NotInsured = Perc_NotIn,
            TotalPop = TotPopSize) %>% 
            dplyr::mutate(tract_id = dplyr::row_number(),
                          NumberWhites = ifelse(TotalPop > 0, (TotalPop*(1-Perc_NonWh)),0),
                          TotalPoverty = TotalPop*Perc_PopStrugg) 
            

tract10$tract_area <- st_area(tract10)


## Line 2015 ## chunk: census_statistical_area_spatial_intersection

#do the spatial join, create poverty and non whites rates by district. Create a dummy for rates >= stat_area_quantile percentile
# create intersection of tract10 and statareas
lr_tract_statAreas.intersect <- st_intersection(tract10, lr_tract_statAreas)


# get % tract in statares and mulitply by pop totals from each tract
# result is the total tract pops distributed to the statarea by % of tract in statare
lr_tract_statAreas.spJoin <- lr_tract_statAreas.intersect %>% 
  mutate(intersect_area = st_area(lr_tract_statAreas.intersect)) %>% 
  # get % of tract and multiply totals by percent area of tract in statarea
  group_by(tract_id) %>% 
  mutate(intersect_pcnt_of_tract = as.numeric(intersect_area) / as.numeric(tract_area),
         intersect_TotalPop = round(TotalPop * intersect_pcnt_of_tract, 1),
         intersect_NumberWhites = round(NumberWhites * intersect_pcnt_of_tract, 1),
         intersect_TotalPoverty = round(TotalPoverty * intersect_pcnt_of_tract, 1)) %>%
  ungroup() %>% 
  # sum the fraction of pop totals up to statarea
  group_by(stat_area_id) %>%
  summarise(statarea_TotalPop = sum(intersect_TotalPop),
            statarea_NumberWhites = sum(intersect_NumberWhites),
            statarea_TotalPoverty = sum(intersect_TotalPoverty)) %>% 
  # make quantites of interest
  mutate(percentNonWhite = ifelse(statarea_TotalPop > 0, 
                                  ((statarea_TotalPop - statarea_NumberWhites) / statarea_TotalPop),0),
         percentPoverty = ifelse(statarea_TotalPop > 0, 
                                 statarea_TotalPoverty / statarea_TotalPop, 0))

# classify by quantile and make dummy variable
lr_tract_statAreas.spJoin <- lr_tract_statAreas.spJoin %>% 
  mutate(poverty.percentile = ifelse(percentPoverty >=
                                       quantile(lr_tract_statAreas.spJoin$percentPoverty, 
                                                p = stat_area_quantile, na.rm=T),"1",0),
         nonWhite.percentile = ifelse(percentNonWhite >=
                                        quantile(lr_tract_statAreas.spJoin$percentNonWhite, 
                                                 p = stat_area_quantile, na.rm=T),1,0))

### r STAT_AREA_CATEGORY_plot - line 2051

STAT_AREA_CATEGORY_plot <- lr_tract_statAreas.spJoin %>%
  dplyr::select(poverty.percentile,nonWhite.percentile) %>%
  gather(var,val,-geometry) %>%
  ggplot() +
  geom_sf(aes(fill=factor(val))) +
  facet_wrap(~var) +
  theme_bw()

### r aggregate_model_area_to_statistical_area: line 2061-2102

# aggreate mean errors to statareas
stat_area_metric_logdev <- error_points %>%
  aggregate(., lr_tract_statAreas.spJoin, mean) %>%
  dplyr::select(logdev) %>% 
  mutate(logdev = round(logdev, 3)) %>% 
  make_cuts(., "logdev")

stat_area_metric_MAE<- error_points %>%
  aggregate(., lr_tract_statAreas.spJoin, mean) %>%
  dplyr::select(MAE) %>% 
  mutate(MAE = round(MAE, 3)) %>% 
  make_cuts(., "MAE")

# aggregate sum of CPS incidents to statarea
stat_area_cps <- error_points %>%
  aggregate(., lr_tract_statAreas.spJoin, sum) %>%
  dplyr::select(test_y)
stat_area_errors <- stat_area_metric_logdev %>% 
  st_join(., stat_area_metric_MAE, join = st_equals) %>% 
  st_join(., stat_area_cps, join = st_equals) %>% 
  st_join(., lr_tract_statAreas.spJoin, join = st_equals)

# group by poverty and get median of statarea aggregate errors
poverty_aggregate <- stat_area_errors %>% 
  group_by(poverty.percentile) %>% 
  summarise(med_dev = round(median(logdev),3),
            med_MAE = round(median(MAE),3),
            med_CPS = sum(test_y)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(poverty.percentile, med_dev, med_MAE, med_CPS)

# group by nonwhite and get median of statarea aggregate errors
nonwhite_aggregate <- stat_area_errors %>% 
  group_by(nonWhite.percentile) %>% 
  summarise(med_dev = round(median(logdev),3),
            med_MAE = round(median(MAE),3),
            med_CPS = sum(test_y)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(nonWhite.percentile, med_dev, med_MAE, med_CPS)

print(poverty_aggregate)
print(nonwhite_aggregate)

### r STATISTICAL_AREA_MODEL_ERROR_plot: line 2105

logdev_stat_area_plot <- make_fishnet_dist_plot(stat_area_metric_logdev, cps_base_map, 
                                                legend = "right", 
                                                direction = -1, var_name = "Deviance",
                                                title = "Out-of-Fold-Error by Statistical Area")

MAE_stat_area_plot <- make_fishnet_dist_plot(stat_area_metric_MAE, cps_base_map, 
                                             legend = "right", 
                                             direction = 1, var_name = "MAE",
                                             title = "Out-of-Fold-Error by Statistical Area")
STATISTICAL_AREA_MODEL_ERROR_plot <- cowplot::plot_grid(logdev_stat_area_plot , MAE_stat_area_plot, 
                                                        align = "h", labels = "Out-of-Fold Error by Statistical Area")


STATISTICAL_AREA_MODEL_ERROR_plot 

### r prediction_to_bin_class: line 2119-2128

error_geoplot$pred_bin_class <- bin_class(error_geoplot, "pred")

p.summ <- error_geoplot %>%
  group_by(pred_bin_class) %>%
  dplyr::summarize(obs.total = sum(test_y),
                   obs.cnt = n()) %>% 
  rename(sens_group = pred_bin_class) %>%
  filter(!is.na(sens_group)) %>%
  identity()

### r compute_KDE

cps_ppp <- as.ppp(st_coordinates(cps_dissolve), W = st_bbox(net_littlerock))
cps_KDE <- spatstat::density.ppp(cps_ppp)

cps_KDE_tbl <- as.data.frame(cps_KDE) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(net_littlerock))%>%
  aggregate(., net_littlerock, mean)%>%
  mutate(net_id = net_littlerock$net_id)

#### r KDE_to_bin_class (chunk: 2141-2156)

if(all.equal(error_geoplot$net_id, cps_KDE_tbl$net_id)){
  cat("Good to go!")
} else {
  cat("Join will be an error, Net_id index does not match")
}

error_geoplot$kde_bin_class  <- bin_class(cps_KDE_tbl, "value")

kde.summ <- error_geoplot %>%
  group_by(kde_bin_class) %>%
  dplyr::summarize(kde.total = sum(test_y),
                   kde.cnt = n()) %>% 
  rename(sens_group = kde_bin_class) %>%
  filter(!is.na(sens_group)) %>%
  identity()

#### r REALTIVE_SENSITIVITY_plot: lines 2159-2178


REALTIVE_SENSITIVITY_KDE <- ggplot() +
  geom_sf(data = ll(kde.summ), aes(fill = factor(sens_group)), 
          color = NA, alpha = 0.85, inherit.aes = FALSE) +
  geom_sf(data = ll(cps_dissolve), inherit.aes = FALSE, size = 1) +
  scale_fill_viridis_d(na.value = NA, option = "D", direction = 1,
                       name = "Risk\nCategory") +
  labs(title = "Risk categories from KDE",
       caption = "Figure 1.6") +
  mapTheme()

REALTIVE_SENSITIVITY_PREDICTIONS <- ggplot() +
  geom_sf(data = ll(p.summ), aes(fill = factor(sens_group)), 
          color = NA, alpha = 0.85, inherit.aes = FALSE) +
  geom_sf(data = ll(cps_dissolve), inherit.aes = FALSE, size = 1) +
  scale_fill_viridis_d(na.value = NA, option = "D", direction = 1,
                       name = "Risk\nCategory") +
  labs(title = "Risk categories from meta-model",
       caption = "Figure 1.5") +
  mapTheme()

REALTIVE_SENSITIVITY_PREDICTIONS

### r REALTIVE_RISK_BARPLOT_COMPARE_plot: chunk lines 2181-2201

countComparisons <- merge(st_drop_geometry(p.summ), st_drop_geometry(kde.summ)) %>%
  mutate_if(is.double, round, 3) %>% 
  mutate(Category = rev(c("90% - 100%", "70% - 89%", "50% - 69%", 
                          "30% - 49%", "1% - 29%"))) %>%
  dplyr::mutate(kernelPct = round(kde.total / sum(kde.total),4),
                fittedPct = round(obs.total / sum(obs.total), 4))

countComparisonsLong <- countComparisons %>% 
  gather(Variable, Value, kernelPct:fittedPct)

REALTIVE_RISK_BARPLOT_COMPARE_plot <- ggplot(data=countComparisonsLong, aes(Category,Value)) +
  geom_bar(aes(fill = Variable), position = "dodge", stat="identity", color = NA) +
  scale_fill_viridis_d(name = " ",
                       labels=c("Meta-model", "Kernel Density")) +
  labs(x= "Predicted Risk Levels",
       y="Percent of Test Set Cases",
       title= "Goodness of fit: Spatial risk model vs. Kernel Density",
       caption = "Figure 1.6") +
  plotTheme() +
  theme(axis.line = element_blank())

REALTIVE_RISK_BARPLOT_COMPARE_plot

## August 07, 2019 
# save.image("full_results_line_2102_0807.RData")
## 
setwd("C:/Users/jd033/Box/Child Maltreatment/R-codes")
load("full_results_line_2102_0807.RData")


### Next is align phase 
### Line 2204 
if(F){
  st_write(error_geoplot, "error_geoplot.shp")
  
  # writeSpatialShape(error_geoplot, "error_geoplot")
  # writeOGR(obj=error_geoplot, dsn="meta-predict", layer="error_geoplot", driver="ESRI Shapefile") # this is in geographical projection
  
  error_geoplot_read <- read_sf(file.path(base_dir,"/R-codes/meta-predict/","error_geoplot.shp"))
  
  error_geoplot_read$pred_bin_class <- bin_class(error_geoplot_read, "pred")
  
  p.summ <- error_geoplot_read %>%
    group_by(pred_bin_class) %>%
    dplyr::summarize(obs.total = sum(test_y),
                     obs.cnt = n()) %>% 
    rename(sens_group = pred_bin_class) %>%
    filter(!is.na(sens_group)) %>%
    identity()
  
  ggplot() +
    geom_sf(data = ll(p.summ), aes(fill = factor(sens_group)), 
            color = NA, alpha = 0.85, inherit.aes = FALSE) +
    scale_fill_viridis_d(na.value = NA, option = "D", direction = 1,
                         name = "Risk\nCategory") +
    labs(title = "Risk categories from meta-model",
         caption = "Figure 1") +
    mapTheme()
} ## Dyann's request for shapefiles 

##In this section change "sensitivty class" to "risk category"

## r alignPhase

#the final prediction map
predMap <- 
  error_geoplot #%>%
#st_transform(102747)

## Lines 2211-2233 not executable: we don't have these data-sets
if(F){
  #removals
  removals <- 
    read.csv("C:/projects/PAP_Virginia/data/z_alignPhase/removals.csv") %>%
    st_as_sf(coords = c("X", "Y"), crs = 102747, agr = "constant")
  #service visits
  visits <- 
    read.csv("C:/projects/PAP_Virginia/data/z_alignPhase/healthFamilyServices.csv") %>%
    filter(!is.na(X)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
    st_transform(102747) %>%
    #some are outside of the the study area, so select by those that itersect
    .[st_union(predMap),]
  #SCAN data - 'stop child abuse now' network centers. geocode. There are only 27 in the study area
  scan <- read.csv("C:/projects/PAP_Virginia/data/z_alignPhase/scanPreventionResources.csv") %>%
    mutate(Street.Address = as.character(Street.Address))  %>%
    mutate(Street.Address = paste(Street.Address, "Richmond, Va."))
  scan <-
    scan %>%
    bind_cols(scan,geocode(scan$Street.Address, source="dsk")) %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
    st_transform(102747) %>%
    .[st_union(predMap),]
}

## Lines 2234-2336: Poverty and Prediction risk map 

#7.1 - population per risk category
popID <- ALL_FEATURES %>% 
  dplyr::select(net_id, net_pop) %>% 
  as.data.frame()

binID <- error_geoplot %>% 
  dplyr::select(net_id, pred_bin_class) %>% 
  as.data.frame()

popBin <- left_join(popID, binID, by = "net_id") %>% 
  group_by(pred_bin_class) %>% 
  summarise(sumPop = sum(net_pop)) %>%
  mutate(pctPop = sumPop/sum(sumPop),
         Category = rev(c("90% - 100%", "70% - 89%", "50% - 69%", 
                          "30% - 49%", "1% - 29%")))

popPerRiskPlot <- ggplot(data=popBin, aes(Category, sumPop)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(x= "Predicted Risk Levels",
       y= "Number of People",
       title = "Population per risk category",
       caption = "Figure 2.1") +
  plotTheme()

#7.2 - poverty rate correlation with predicted maltreatment count
#take the net id and geometry for the fishnet 
geom_fishnet <- error_geoplot %>% 
  dplyr::select(net_id, geometry)

#get population and poverty info at tract level
poverty_tract <- tract10 %>% 
  dplyr::select(TotalPoverty, tract_id, geometry) %>% 
  mutate(tract_acre = as.numeric(st_area(.)*2.29568e-5),
         pov_acre_rate = TotalPoverty/tract_acre)

population_tract <- tract10 %>% 
  dplyr::select(TotalPop, tract_id, geometry) %>% 
  mutate(tract_acre = as.numeric(st_area(.)*2.29568e-5),
         pop_acre_rate = TotalPop/tract_acre)

#intersect tract poverty and fishnet
pov_tracts_intersect <- st_intersection(poverty_tract, geom_fishnet)

pov_tracts_intersect <- pov_tracts_intersect %>%
  mutate(int_area_acres = as.numeric(st_area(pov_tracts_intersect)*2.29568e-5)) %>%
  group_by(net_id) %>%
  mutate(cnt = n(),
         pcnt_of_block = int_area_acres/tract_acre,
         int_pov =TotalPoverty * pcnt_of_block) %>%
  arrange(net_id)

fishnet_poverty <- pov_tracts_intersect %>% # xcc
  group_by(net_id) %>%
  summarise(net_pov = sum(int_pov)) %>% 
  as.data.frame()

#intersect tract population and fishnet
pop_tracts_intersect <- st_intersection(population_tract, geom_fishnet)

pop_tracts_intersect <- pop_tracts_intersect %>%
  mutate(int_area_acres = as.numeric(st_area(pop_tracts_intersect)*2.29568e-5)) %>%
  group_by(net_id) %>%
  mutate(cnt = n(),
         pcnt_of_block = int_area_acres/tract_acre,
         int_pop =TotalPop * pcnt_of_block) %>%
  arrange(net_id)

fishnet_population <- pop_tracts_intersect %>% # xcc
  group_by(net_id) %>%
  summarise(net_pop = sum(int_pop)) %>% 
  as.data.frame()

pov_pop_fishnet <- left_join(fishnet_poverty, fishnet_population, by = "net_id") %>% 
  mutate(povRate = net_pov/net_pop) %>% 
  dplyr::select(-geometry.x) %>% 
  rename(geometry = geometry.y) %>% 
  st_sf() %>% 
  st_transform(102747)

#map it:
povertyRateMap <- ggplot() + #ggmap(cps_base_map) +
  geom_sf(data=ll(pov_pop_fishnet), aes(fill=factor(ntile(povRate, 5))), inherit.aes = FALSE, alpha = 0.8, color = NA) +
  scale_fill_viridis_d(labels = as.character(round(quantile(pov_pop_fishnet$povRate,
                                                            c(.1,.2,.4,.6,.8),na.rm=T), 4)),
                       name="Poverty\nRate") +
  labs(title = "Weighted poverty rate",
       caption = "Figure 2.2") +
  mapTheme()

povertyRateMap

pov_pop_fishnet_pred <- left_join(pov_pop_fishnet, error_geoplot %>% 
                                    dplyr::select(net_id, pred) %>% 
                                    #mutate(pred = round(pred)) %>% 
                                    as.data.frame(), by = "net_id") %>% 
  filter(pred > 0)

povRatePredPlot <- ggplot(pov_pop_fishnet_pred, aes(x=povRate, y=pred)) + 
  geom_point() +
  labs(x = "Poverty Rate",
       y = "Predicted\nMaltreatment Counts",
       title = "Relationship between predicted maltreatment counts\nand poverty rate",
       caption = "Figure 2.3") +
  plotTheme()

povRatePredPlot

CORR_between_poverty_pred <- cor(pov_pop_fishnet_pred$povRate, pov_pop_fishnet_pred$pred)

## Line 2338: removals / risk over prediction map 
#7.3 - map of risk categoeries with removals overlayed
## Line 2349: #7.4 - count of removals by risk categoery
## ** We don't have removals data as of now. **

## Line 2372: #7.4 - map of protective land uses by type 
## Line 2407: #table of top protective places
## Line 2420: #7.6 - visits as a function of risk levels
## Line 2431: #7.7 visits by risk category
## Line 2453: #7.8 scan centers
## Line 2464: #7.9 scan buffers and risk
## Line 2469: #map average predicted event by quarter mile buffer
## Line 2481: #find top 3 Scan centers
## Line 2491: #Gap #get neighborhoods
## Line 2513: #calculate number of protective centers within 
## Line 2523: #put demand and supply together and look at difference
## Line 2491-2591: #Gap Analysis 
#### 1. Get statistical areas. 
#### 2. create a dummy variable field for where risk category == 5
#### 3. count 5th risk quintile per neighborhood
#### 4. #calculate number of protective centers within (not possible)
#### 5. #put demand and supply together and look at difference
#### 6. #map average predicted event by neighborhoods
#### Line 2532 - will create gapMap (not possible right now)

### Lines 2550 - 2604 : childRiskAreas
### They used get_acs with a bunch of variables (without any descriptions)
### No way of knowing what this was for. 
## Code: 
## child_tract10 <- get_acs(geography = "tract", 
## variables = c("B01001_003E", "B01001_004E", "B01001_005E", 
## "B01001_006E","B01001_027E", "B01001_028E", "B01001_029E", "B01001_030E"), 
##                         year = 2010, state=51, county=760, geometry=T)

### Lines 2606 - 2621 : CPSCOUNT_by_nbr
## Recall we have lr_tract instead of nbr and NAME instead of name in lr_tract

lr_tract_CPSCount <- st_join(fishnet_pop_cps, lr_tract, largest = TRUE) %>% 
  dplyr::select(NAME, geometry, net_CPS_Accepted, net_id) %>% 
  group_by(NAME) %>% 
  summarise(count = sum(net_CPS_Accepted)) %>% 
  filter(NAME != "NA")

CPSCOUNT_by_lr_tract_plot <- ggplot() + #ggmap(cps_base_map) +
  geom_sf(data = ll(lr_tract_CPSCount), aes(fill = factor(ntile(count, 5))), color = NA, alpha = 0.8, inherit.aes = FALSE) +
  scale_fill_viridis_d(labels = as.character(quantile(lr_tract_CPSCount$count,
                                                      c(.1,.2,.4,.6,.8),na.rm=T)),
                       direction = 1,
                       name="Count\nQuantile\nBreaks") +
  labs(title = "Number of Child Maltreatment events reported\nby census tract",
       caption = "Figure 1.1") +
  mapTheme()

CPSCOUNT_by_lr_tract_plot

## Line 2624 - 2639: ```{r GLOBAL_MORANS_TEST_MAE}

#matrix of coordinates
nbrCoords <- neighborhood_metric_MAE %>% 
  st_centroid() %>% 
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(X, Y) %>% 
  as.matrix()


#make weights for test - neighborhood level
neighborNbrs <- knn2nb(knearneigh(nbrCoords, 5))
spatialWeights <- nb2listw(neighborNbrs, style="W")

moranTest_MAE <- moran.mc(neighborhood_metric_MAE$MAE, spatialWeights, nsim = 999)

moranTest_MAE

# save.image("full_results_line_2642_0827.RData")

# load("full_results_line_2642_0827.RData")

## Line 2642: ```{r childFatality}

## We don't read CSV files 
# deaths <- 
#   read.csv("C:/projects/PAP_Virginia/data/z_alignPhase/deaths.csv") %>%
#   st_as_sf(coords = c("X", "Y"), crs = 102747, agr = "constant") %>% 
#   filter(ChildFatality == "Y")

deaths <- var_list[["CPS_Accepted"]] %>% filter(Death == 1)

fatalitiesMap <- ggplot() +
  geom_sf(data=ll(predMap), aes(fill=factor(pred_bin_class)), inherit.aes = FALSE, alpha = 0.8, color = NA) +
  geom_sf(data=ll(deaths),aes(colour="ChildFatality"),colour="red", inherit.aes = FALSE, size = 2) +
  scale_fill_viridis_d(name = "Risk\nCategory") +
  labs(title="Predicted risk levels and child fatalities",
       subtitle="Fatalities in red",
       caption = "Figure 2.6") +
  mapTheme()

fatalitiesMap

fatalitiesPlot <- deaths %>%
  mutate(counter=1) %>%
  aggregate(predMap,FUN=length) %>%
  dplyr::select(counter) %>%
  mutate(counter = ifelse(is.na(counter),0,counter)) %>%
  bind_cols(predMap) %>%
  mutate(Category = case_when(pred_bin_class == 1 ~ "1% - 29%",
                              pred_bin_class == 2 ~ "30% - 49%%",
                              pred_bin_class == 3 ~ "50% - 69%",
                              pred_bin_class == 4 ~ "70% - 89%",
                              pred_bin_class == 5 ~ "90% - 100%")) %>%
  group_by(Category) %>%
  dplyr::summarize(percentCount = sum(counter)/nrow(deaths)) %>%
  ggplot(aes(Category,percentCount)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(x= "Predicted Risk Levels",
       y="Percent of Child Fatalities",
       title= "Percent of child fatalities by risk category") +
  plotTheme()

fatalitiesPlot

save(deaths, fatalitiesMap, fatalitiesPlot, file = "child_fatality_data_plot_0829.RData")


