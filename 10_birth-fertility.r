# header ------------------------------------------------------------------
# birth: fertility rate




# paths, general ----------------------------------------------------------

# paths, general functions
  setwd("V:/RelSzen")
  res_path <- "3_Resultate/10_Birth/"
  source("1_Code/Modell/90_general.r")



# import and data preparation ---------------------------------------------

# birth
# age: only births of women at 'fertile age'
# rel: religion of the mother

bir <- read_excel(paste0(data_path, "/input/Geb.xlsx")) %>% 
  rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) %>% 
  filter((age >= bir_age_begin) & (age <= bir_age_end)) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>% 
  mutate(rel = factor(if_else(RelMutter == 1, uni_r[1], uni_r[2]), uni_r), 
    cdistrict = factor(cdistrict, uni_c)) %>% 
  select(cdistrict, year, age, rel, bir) %>% 
  group_by(cdistrict, year, age, rel) %>%
  summarize(
    bir = sum(bir),
    .groups = "drop"
  )


# population
# year: begin of year population (therefore: StichtagDatJahr + 1)
# age: only women at 'fertile age'

pop <- read_excel(paste0(data_path, "/input/BesTodZuzWeg.xlsx")) %>%  
  rename(age = AlterVCd, pop = AnzBestWir) %>% 
  filter((SexCd == 2) & (age >= bir_age_begin) & (age <= bir_age_end)) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>% 
  mutate(
    year = StichtagDatJahr + 1,
    cdistrict = factor(cdistrict, uni_c),
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r)
  ) %>% 
  select(cdistrict, year, age, rel, pop) %>% 
  group_by(cdistrict, year, age, rel) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )


# fertility ---------------------------------------------------------------

# values for all possible cases

# WHY with age categories? to calculate TFR by age category

cas <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):date_end,
  age = bir_age_begin:bir_age_end,
  rel = uni_r
)) %>% 
  left_join(pop, by = c("cdistrict", "year", "age", "rel")) %>%
  left_join(bir, by = c("cdistrict", "year", "age", "rel")) %>% 
  replace_na(list(pop = 0, bir = 0)) %>%
  left_join(look_a1, by = "age") %>%
  left_join(look_a2, by = "age")


# fertility by year, age
fer_ya <- cas %>% 
  group_by(year, age) %>%
  summarize(
    pop = sum(pop),
    bir = sum(bir),
    .groups = "drop"
  ) %>%
  mutate(fer_ya = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>%
  select(year, age, fer_ya)

# fertility by year, age, rel
fer_yar <- cas %>% 
  group_by(year, age, rel) %>%
  summarize(
    pop = sum(pop),
    bir = sum(bir),
    .groups = "drop"
  ) %>%
  mutate(fer_yar = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate))) %>%
  select(year, age, rel, fer_yar)

# fertility by cdistrict, year, age, rel
# is already aggregated by cdistrict, year, age, rel
fer_cyar <- cas %>% 
  mutate(fer_cyar = if_else(pop == 0, NA_real_, round(bir / pop * 100, round_rate)))


# TFR (total fertility rate) ----------------------------------------------

# WHY? plots to define the base period for fertility prediction

# TFR by year
tfr_y <- group_by(fer_ya, year) %>%
  summarize(
    tfr_y = sum_NA(fer_ya / 100),
    .groups = "drop"
  )

sszplot(tfr_y,
  aes_x = "year", aes_y = "tfr_y",
  labs_y = "TFR", i_x = "5",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),    
  name = "1000_TFR_by-year"
)

# TFR by yr
tfr_yr <- group_by(fer_yar, year, rel) %>%
  summarize(
    tfr_yr = sum_NA(fer_yar / 100),
    .groups = "drop"
  )

sszplot(tfr_yr,
  aes_x = "year", aes_y = "tfr_yr", aes_col = "rel",
  labs_y = "TFR", i_x = "5",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),    
  name = "1001_TFR_by-year-rel"
)

# TFR by year, age1
tfr_ya1 <- left_join(fer_ya, look_a1, by = "age") %>%
  group_by(year, age_1) %>%
  summarize(
    tfr_ya1 = sum_NA(fer_ya / 100),
    .groups = "drop"
  ) %>%
  rename(age = age_1)

sszplot(tfr_ya1,
  aes_x = "year", aes_y = "tfr_ya1", aes_col = "age",
  i_x = "5", labs_y = "TFR",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),   
  name = "1002_TFR_by-year-age1"
)


# TFR by year, age2
tfr_ya2 <- left_join(fer_ya, look_a2, by = "age") %>%
  group_by(year, age_2) %>%
  summarize(
    tfr_ya2 = sum_NA(fer_ya / 100),
    .groups = "drop"
  ) %>%
  rename(age = age_2)

sszplot(tfr_ya2,
  aes_x = "year", aes_y = "tfr_ya2", aes_col = "age",
  i_x = "5", labs_y = "TFR",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),   
  name = "1003_TFR_by-year-age2"
)


# TFR by year, age1, rel
tfr_ya1r <- left_join(fer_yar, look_a1, by = "age") %>%
  group_by(year, age_1, rel) %>%
  summarize(
    tfr_ya1r = sum_NA(fer_yar / 100),
    .groups = "drop"
  ) %>%
  rename(age = age_1)

sszplot(tfr_ya1r,
  aes_x = "year", aes_y = "tfr_ya1r", aes_col = "rel",
  wrap = "age", ncol = nlevels(tfr_ya1r$age),
  labs_y = "TFR", i_x = "5",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),   
  name = "1004_TFR_by-year-age1-rel",
  width = 16, height = 5
)


# TFR by year, age2, origin
tfr_ya2r <- left_join(fer_yar, look_a2, by = "age") %>%
  group_by(year, age_2, rel) %>%
  summarize(
    tfr_ya2r = sum_NA(fer_yar / 100),
    .groups = "drop"
  ) %>%
  rename(age = age_2)

sszplot(tfr_ya2r,
  aes_x = "year", aes_y = "tfr_ya2r", aes_col = "rel",
  wrap = "age", ncol = nlevels(tfr_ya2r$age),
  labs_y = "TFR", i_x = "5",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),   
  name = "1005_TFR_by-year-age2-rel",
  width = 16, height = 5
)


# TFR by cdistrict, year, rel
tfr_cyr <- fer_cyar %>% 
  group_by(cdistrict, year, rel) %>%  
  summarize(
    tfr_cyr = sum_NA(fer_cyar / 100),
    .groups = "drop"
  )

sszplot(tfr_cyr,
  aes_x = "year", aes_y = "tfr_cyr", aes_col = "rel",
  wrap = "cdistrict", ncol = 3,
  labs_y = "TFR", i_x = "5",
  geom = c("line", "point"),
  quotes = quote(expand_limits(y = 0)),   
  name = "1006_TFR_by-cdistrict-year-rel",
  width = 14, height = 10
)



# fertility, birth, population (before any corrections) --------------------------------

# base years only
fer_cyar_base <- fer_cyar %>% 
  filter((year >= bir_base_begin) & (year <= bir_base_end))
  
# plot: fertility by district, year, age, rel
  sszplot(fer_cyar_base, aes_x = "age", aes_y = "fer_cyar", aes_col = "rel",
    wrap = "as.factor(year)", labs_y = "fertility rate (in % per year)",
    name = "1010_fertility_by-cdistrict-year-age-rel",
    width = 11, height = 5,
    multi = uni_c
  )
  
# why high fertility rate in Kirchenkreis* 4+5 in 2013, reformed?
# 4 women at age 16, and 1 birth
fer_cyar_base %>% 
  filter((cdistrict == "Kirchenkreis 4+5") & (year == 2013) & (age < 17))

# similar in 2014
# 8 women at age 19, and 1 birth
fer_cyar_base %>% 
  filter((cdistrict == "Kirchenkreis 4+5") & (year == 2014) & (age < 20))


# plot: births by district, year, age, rel
  sszplot(fer_cyar_base, aes_x = "age", aes_y = "bir", aes_col = "rel",
    wrap = "as.factor(year)", labs_y = "births (per year)",
    name = "1011_births_by-cdistrict-year-age-rel",
    width = 11, height = 5,
    multi = uni_c
  )
  
# same plot (but free y-scale: since category 'reformed' is much lower)
  sszplot(fer_cyar_base, aes_x = "age", aes_y = "bir", aes_col = "rel",
    labs_y = "births (per year)",
    name = "1012_births_by-cdistrict-year-age-rel_scales",
    width = 16, height = 6,
    multi = uni_c,
    quotes = quote(facet_grid(rel ~ as.factor(year), scales = "free_y"))
  )  
    
# plot: population by district, year, age, rel
  sszplot(fer_cyar_base, aes_x = "age", aes_y = "pop", aes_col = "rel",
    wrap = "as.factor(year)", labs_y = "population",
    name = "1013_population_by-cdistrict-year-age-rel",
    width = 11, height = 5,
    multi = uni_c
  ) 
  


# smooth fertility rate ---------------------------------------------------

# smoothing fertility rate with loess
fer_fit <- fer_cyar_base %>%
  arrange(cdistrict, year, rel, age) %>%
  group_by(cdistrict, year, rel) %>%
  mutate(
    fer_fit = pmax(0, predict(loess(fer_cyar ~ age,
      span = bir_fer_span, degree = 1,
      na.action = na.aggregate
    )))) %>%
  ungroup()

  

# plot preparation
fit_lev <- c("initial", "smoothed")  
  
fit_dat <- fer_fit %>% 
  rename(fer = fer_cyar) %>% 
  select(cdistrict, year, age, rel, fer, fer_fit) %>%
  pivot_longer(c(fer, fer_fit), names_to = "category", values_to = "fer") %>%
  mutate(cat = factor(if_else(category == "fer",
    fit_lev[1], fit_lev[2]
  ), levels = fit_lev)) %>%
  select(cdistrict, year, age, rel, cat, fer)

# plot
sszplot(fit_dat,
  aes_x = "age", aes_y = "fer", aes_col = "rel", aes_ltyp = "cat",
  wrap = "as.factor(year)",
  labs_y = "fertility rate (in % per year)",
  name = "1014_fertility_fit",
  width = 13, height = 8,
  multi = uni_c
)


# prediction --------------------------------------------------------------

# constrained regression
# (proportion of linear model and mean, within bandwidth)

fer_pred <- con_reg(
  data = fer_fit, x = "year", y = "fer_fit",
  group_cols = c("cdistrict", "age", "rel"),
  window = bir_window_thres, base_t0 = bir_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = bir_prop_trend, thres_percent = bir_thres_percent,
  lower_thres = bir_lower_thres, upper_thres = bir_upper_thres
) %>%
  # with the input data (WHY? to assess the regression)
  left_join(select(fer_fit, cdistrict, year, age, rel, fer_fit),
    by = c("cdistrict", "year", "age", "rel")
  ) %>%
  # output generation, one rate variable for both future and past
  left_join(select(fer_cyar, cdistrict, year, age, rel, fer_cyar),
    by = c("cdistrict", "year", "age", "rel")
  ) %>%
  mutate(fer_all = if_else(year <= bir_base_end, fer_cyar, pred_roll))


# plot the predictions: age distribution by district and year -------------

# plot
sszplot(fer_pred,
  aes_x = "age", aes_y = "fer_all", aes_col = "year",
  wrap = "cdistrict",
  labs_y = "fertility rate (in % per year)",
  scale_y = c(0, bir_plot_lim),
  name = "1015_fertility-prediction_by-cdistrict",
  width = 12, height = 14,
  multi = uni_r
)


# smooth the future fertility rates ---------------------------------------

# smoothed (only the prediction years)
pred_fit <- fer_pred %>% 
  filter(year >= scen_begin) %>%
  arrange(cdistrict, year, rel, age) %>%
  group_by(cdistrict, year, rel) %>%
  mutate(pred_fit = pmax(0, predict(
    loess(pred_roll ~ age, span = bir_fer_span_pred, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()

# plot prediction for selected years
sel_years <- uniy_scen[(uniy_scen %% 10) == 0]

sel_lev <- c("initial", "smoothed")

sel_dat <- pred_fit %>%
  pivot_longer(c(pred_fit, pred_roll), names_to = "category", values_to = "fer") %>%
  mutate(cat = factor(if_else(category == "pred_roll",
    sel_lev[1], sel_lev[2]
  ), levels = sel_lev)) %>%
  filter(year %in% sel_years)

# plot
sszplot(sel_dat,
  aes_x = "age", aes_y = "fer", aes_col = "cat",
  grid = c("rel", "year"),
  labs_y = "fertility rate (in % per year)",
  name = "1016_fertility-prediction_before-after-fit",
  width = 10, height = 6,
  multi = uni_c
)


# export fertility rates --------------------------------------------------

# prepare the export data
fer_ex <- mutate(pred_fit, fer = round(pred_fit, round_rate)) %>%
  filter(year >= scen_begin) %>%
  select(cdistrict, year, age, rel, fer) %>%
  arrange(cdistrict, year, age, rel)

# export
write_csv(fer_ex, paste0(exp_path, "birth_fertility_future.csv"))



# TFR (total fertility rate) ----------------------------------------------

# fertility rate of past and future
fer_ex_past <- rename(fer_cyar, fer = fer_cyar) %>%
  select(cdistrict, year, age, rel, fer) %>%
  arrange(cdistrict, year, age, rel)

# export the data of the past (for model evaluation later on)
write_csv(fer_ex_past, paste0(exp_path, "birth_fertility_past.csv"))

# TFR
tfr_cyr <- bind_rows(fer_ex_past, fer_ex) %>%
  group_by(cdistrict, year, rel) %>%
  summarize(
    TFR = sum_NA(fer / 100),
    .groups = "drop"
  )

# plot
sszplot(tfr_cyr,
  aes_x = "year", aes_y = "TFR", aes_col = "rel",
  i_x = c(bir_base_begin, scen_begin),
  wrap = "cdistrict", ncol = 4,
  name = "1017_TFR_by-cdistrict-rel",
  width = 12, height = 7
)



# TFR by age class --------------------------------------------------------

# TFR by age class
tfr_a1 <- bind_rows(fer_ex_past, fer_ex) %>%
  left_join(look_a1, by = "age") %>%
  group_by(cdistrict, year, rel, age_1) %>%
  summarize(
    TFR = sum_NA(fer / 100),
    .groups = "drop"
  ) %>%
  rename(age = age_1)

# plot
sszplot(tfr_a1,
  aes_x = "year", aes_y = "TFR", aes_col = "age",
  i_x = c(bir_base_begin, scen_begin),
  labs_col = "age",
  wrap = "cdistrict", ncol = 4,
  name = "1018_TFR_by-cdistrict-rel-age1",
  width = 12, height = 7,
  multi = uni_r
)


# TFR by age class (more detailled) ---------------------------------------

# TFR by age class
tfr_a2 <- bind_rows(fer_ex_past, fer_ex) %>%
  left_join(look_a2, by = "age") %>%
  group_by(cdistrict, year, rel, age_2) %>%
  summarize(
    TFR = sum_NA(fer / 100),
    .groups = "drop"
  ) %>%
  rename(age = age_2)

# plot
sszplot(tfr_a2,
  aes_x = "year", aes_y = "TFR", aes_col = "age",
  i_x = c(bir_base_begin, scen_begin),
  labs_col = "age",
  wrap = "cdistrict", ncol = 4,
  name = "1019_TFR_by-cdistrict-rel-age2",
  width = 12, height = 7,
  multi = uni_r
)



# cleanup -----------------------------------------------------------------

# remove variables without further use
rm(list = c(
  "bir", "cas", "fer_cyar", "fer_ya", "fer_yar", 
  "fer_fit", "fer_pred",
  "pred_fit", "pop", "sel_dat", "tfr_a1", "tfr_a2",
  "tfr_y", "tfr_ya1", "tfr_ya1r", "tfr_ya2", "tfr_ya2r",
  "tfr_yr", "fer_cyar_base", "fer_ex", "fer_ex_past"
))


