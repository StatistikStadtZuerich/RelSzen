# header ------------------------------------------------------------------
# general: packages, functions, levels, colors


# packages ----------------------------------------------------------------

library(plyr) #round_any function, e.g. round to 20
library(tidyverse) # tidyverse functionality
library(dvmisc) # for expand_grid
library(gridExtra) # for ggplot on multiple pages
library(nlme) # needed for mgcv, see below
library(mgcv) # for gam
library(ggrepel) # to label lines (when too many for a normal legend)
library(zoo) # moving average (no confusion with filter function in base R)
library(ckanr) # download of open data directly from ckan
library(scales) # for pretty axis breaks
library(rlang) # for functions: symbols, quasiquotation (!!, !!!, :=)
library(gtools) # invalid function (to check for NA, NULL, NaN), e.g. in constrained regression
library(this.path) # to extract the current file name
library(modelr) # add_predictions
library(readxl) # import excel files




# paths, import parameters ------------------------------------------------

# path for code
code_path <- "1_Code/"

# path for data
data_path <- "2_Daten/"

# path for future rates
exp_path <- "2_Daten/rates/"

# path for output (outputs of the model by cdistrict)
out_path <- "2_Daten/output/"

# path for the results (final results by idistrict)
fin_path <- "2_Daten/results/"


# import an assign parameters
para <- read_delim(paste0(data_path, "parameter/parameter.csv"), ";", lazy = FALSE) %>%
  select(parameter, value)

  for (i_para in 1:nrow(para)) {
    assign(para$parameter[i_para], para$value[i_para], envir = .GlobalEnv)
  }



# general functions -------------------------------------------------------

# functions
sum_NA <- function(x) {
  sum(x, na.rm = TRUE)
}
max_NA <- function(x) {
  max(x, na.rm = TRUE)
}


# options -----------------------------------------------------------------

# no scientic notation
options(scipen = 999)


# lookup tables -----------------------------------------------------------

# lookup church districts (according to population scenarios, based on districts)
look_c <- tibble(cnum = 1:9, 
                   cdistrict = c("Kirchenkreis 1+6", "Kirchenkreis 2", 
                               "Kirchenkreis 3", "Kirchenkreis 4+5", 
                               "Kirchenkreis 7+8", "Kirchenkreis 9", 
                               "Kirchenkreis 10", "Kirchenkreis 11", 
                               "Kirchenkreis 12"))

# lookup church districts (real church districts)
look_i <- tibble(inum = 1:10, 
                   idistrict = c("Kirchenkreis 1", "Kirchenkreis 2", 
                               "Kirchenkreis 3", "Kirchenkreis 4+5", 
                               "Kirchenkreis 6", 
                               "Kirchenkreis 7+8", "Kirchenkreis 9", 
                               "Kirchenkreis 10", "Kirchenkreis 11", 
                               "Kirchenkreis 12"))

# lookup church districts (combination of both classes)


# 10 idistricts (real church districts)
# 9 cdistricts (based on population scenarios)
# combination: per idistrict: which cdistrict is used to calculate the transformation factor?

look_ic <- look_i %>% 
  mutate(cdistrict = c(look_c$cdistrict[1:4], look_c$cdistrict[1], 
                       look_c$cdistrict[5:9]))





# unique levels -----------------------------------------------------------

# church districts (9 church districts, based on population scenarios)
text_c <- look_c$cdistrict
uni_c <- factor(text_c, levels = text_c)

# church districts (10 church districts, real districts)
text_i <- look_i$idistrict
uni_i <- factor(text_i, levels = text_i)

# sex
text_s <- c("male", "female")
uni_s <- factor(text_s, levels = text_s)

# sex
text_s <- c("male", "female")
uni_s <- factor(text_s, levels = text_s)

# religion
text_r <- c("reformed", "other")
uni_r <- factor(text_r, levels = text_r)

# year: base period
uniy_bir_base <- bir_base_begin:bir_base_end

# year: future
uniy_scen <- scen_begin:scen_end




# categories (t = text) and associated lookup tables ----------------------

# age category 1
age_1 <- c(30, 40)
age_1t <- c("15-29", "30-39", "40-49")

look_a1 <- tibble(age = 15:49) %>%
  mutate(age_1 = factor(if_else(age < age_1[1], age_1t[1],
    if_else(age < age_1[2], age_1t[2], age_1t[3])
  ), levels = age_1t))

# age category 2
age_2 <- seq(25, 40, by = 5)
age_2t <- c("15-24", "25-29", "30-34", "35-39", "40-49")

look_a2 <- tibble(age = 15:49) %>%
  mutate(age_2 = factor(if_else(age < age_2[1], age_2t[1],
    if_else(age < age_2[2], age_2t[2],
      if_else(age < age_2[3], age_2t[3],
        if_else(age < age_2[4], age_2t[4], age_2t[5])
      )
    )
  ), levels = age_2t))


# age category 3
age_3 <- seq(0, 90, by = 10)

age_3t <- c(
  "0-9", "10-19", "20-29", "30-39", "40-49",
  "50-59", "60-69", "70-79", "80-89", "90+"
)

look_a3 <- tibble(age = 0:120) %>%
  mutate(age_3 = factor(case_when(
    age < age_3[2] ~ age_3t[1],
    age < age_3[3] ~ age_3t[2],
    age < age_3[4] ~ age_3t[3],
    age < age_3[5] ~ age_3t[4],
    age < age_3[6] ~ age_3t[5],
    age < age_3[7] ~ age_3t[6],
    age < age_3[8] ~ age_3t[7],
    age < age_3[9] ~ age_3t[8],
    age < age_3[10] ~ age_3t[9],
    TRUE ~ age_3t[10]
  ), levels = age_3t))

# age category 4
age_4 <- seq(0, 80, by = 20)

age_4t <- c("0-19", "20-39", "40-59", "60-79", "80+")

look_a4 <- tibble(age = 0:120) %>%
  mutate(age_4 = factor(case_when(
    age < age_4[2] ~ age_4t[1],
    age < age_4[3] ~ age_4t[2],
    age < age_4[4] ~ age_4t[3],
    age < age_4[5] ~ age_4t[4],
    TRUE ~ age_4t[5]
  ), levels = age_4t))



# age category 5
age_5 <- seq(0, 80, by = 10)

age_5t <- c(
  "0-9", "10-19", "20-29", "30-39", "40-49",
  "50-59", "60-69", "70-79", "80+"
)

look_a5 <- tibble(age = 0:120) %>%
  mutate(age_5 = factor(case_when(
    age < age_5[2] ~ age_5t[1],
    age < age_5[3] ~ age_5t[2],
    age < age_5[4] ~ age_5t[3],
    age < age_5[5] ~ age_5t[4],
    age < age_5[6] ~ age_5t[5],
    age < age_5[7] ~ age_5t[6],
    age < age_5[8] ~ age_5t[7],
    age < age_5[9] ~ age_5t[8],
    TRUE ~ age_5t[9]
  ), levels = age_5t))

look_a5_num <- tibble(num = 1:9) %>% 
  mutate(age_5 = factor(age_5t, levels = age_5t))

uni_a5 <- sort(unique(look_a5$age_5))






# colors, graphics --------------------------------------------------------

# colors (e.g. for sex, origin)
col_6 <- c("#005CA9", "#83072A", "#EB5E04", "#FBBA00", "#007229", "#9B9B9B")
# plot(1:6, 1:6, col = col_6, pch = 16, cex = 3)

# grey
col_grey <- "grey90"

# color for region
col_r <- colorRampPalette(col_6)(length(text_r))
# plot(1:length(text_r), 1:length(text_r), col = col_r, pch = 16, cex = 3)

# color for sex
col_s <- col_6[c(1, 3)]
# plot(1:2, 1:2, col = col_s, pch = 16, cex = 3)

# color for religion
col_r <- col_6[c(3, 2)]
# plot(1:2, 1:2, col = col_s, pch = 16, cex = 3)

# themes: neutral design for ggplot
neutral <- theme_bw() + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_rect(colour = "grey85"),
  panel.border = element_rect(colour = "grey85")
)


# specific functions ------------------------------------------------------

# plot functions
source(paste0(code_path, "Modell/91_plot-functions.r"))

# constrained regression
source(paste0(code_path, "Modell/92_constrained-regression.r"))

# life expectancy
source(paste0(code_path, "Modell/93_life-expectancy.r"))





