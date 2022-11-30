# header ------------------------------------------------------------------
# conversion


# paths, general ----------------------------------------------------------

# paths, general functions
  main_path <- "V:/RelSzen/"
  res_path <- paste0(main_path, "3_Resultate/50_Conversion/")
  source("90_general.r")


# data import -------------------------------------------------------------

# conversion
# religion: previous religion (before the conversion)

con <- read_excel(paste0(data_path, "/input/Kon.xlsx")) %>%
  rename(year = EreignisDatJahr, age = AlterVCd, con = AnzKonvWir) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    rel = factor(if_else(RelBisher == 1, uni_r[1], uni_r[2]), uni_r),
    cdistrict = factor(cdistrict, uni_c)
  ) %>%
  select(cdistrict, year, age, sex, rel, con) %>%
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(
    con = sum(con),
    .groups = "drop"
  ) 


# population
# year: begin of year population (therefore, StichtagDatJahr + 1)

pop <- read_excel(paste0(data_path, "/input/BesTodZuzWeg.xlsx")) %>%
  rename(age = AlterVCd, pop = AnzBestWir) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    year = StichtagDatJahr + 1,
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    cdistrict = factor(cdistrict, uni_c),
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r)
  ) %>%
  select(cdistrict, year, age, sex, rel, pop) %>%
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )


# con and pop (based on all possible cases)
con_pop <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):date_end,
  age = age_min:age_max,
  sex = uni_s,
  rel = uni_r  
)) %>%
left_join(pop, by = c("cdistrict", "year", "age", "sex", "rel")) %>% 
left_join(con, by = c("cdistrict", "year", "age", "sex", "rel")) %>% 
replace_na(list(pop = 0, con = 0))


# plots (past) ------------------------------------------------------------

# WHY not only reformed? WHY also category 'other'?
# To check if relevant for a certain group (e.g. age, or church district)


# years of the past (for plot)
year_past <- (date_start + 1):date_end
year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))


#yr 
con_yr <- con_pop %>% 
  group_by(year, rel) %>%
  summarize(
    con = sum(con),
    pop = sum(pop),
    .groups = "drop"
  ) %>% 
mutate(con_rate_yr = if_else(pop == 0, NA_real_, round(con / pop * 100, round_rate)))

sszplot(con_yr,
  aes_x = "year", aes_y = "con_rate_yr", aes_col = "rel", 
  i_x = year_past_5,
  labs_y = "conversion rate (in % per year)",
  name = "5000_conversion-rate_by-year-rel",
  width = 8, height = 5
)

#yasr 
con_yasr <- con_pop %>% 
  group_by(year, age, sex, rel) %>%
  summarize(
    con = sum(con),
    pop = sum(pop),
    .groups = "drop"
  ) %>% 
mutate(con_rate_yasr = if_else(pop == 0, NA_real_, round(con / pop * 100, round_rate)))

con_yasr %>% 
  filter(year %in% year_past_5) %>% 
sszplot(
  aes_x = "age", aes_y = "con_rate_yasr", aes_col = "sex", 
  grid = c("rel", "year"),
  labs_y = "conversion rate (in % per year)",
  name = "5001_conversion-rate_by-year-age-sex-rel",
  width = 15, height = 6
)

#cyr 
con_cyr <- con_pop %>% 
  group_by(cdistrict, year, rel) %>%
  summarize(
    con = sum(con),
    pop = sum(pop),
    .groups = "drop"
  ) %>% 
mutate(con_rate_cyr = if_else(pop == 0, NA_real_, round(con / pop * 100, round_rate)))

sszplot(con_cyr,
  aes_x = "year", aes_y = "con_rate_cyr", aes_col = "rel", 
  i_x = year_past_5,
  wrap = "cdistrict", ncol = 5,
  labs_y = "conversion rate (in % per year)",
  name = "5002_conversion-rate_by-cdistrict-year-rel",
  width = 13, height = 7
)


#cyasr 
con_cyasr <- con_pop %>% 
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(
    con = sum(con),
    pop = sum(pop),
    .groups = "drop"
  ) %>% 
mutate(con_rate_cyasr = if_else(pop == 0, NA_real_, round(con / pop * 100, round_rate)))

con_cyasr %>% 
  filter(year %in% year_past_5) %>% 
sszplot(
  aes_x = "age", aes_y = "con_rate_cyasr", aes_col = "sex", 
  grid = c("rel", "year"),
  labs_y = "proportion in %", labs_col = "year",
  name = "5003_conversion-rate_by-cdistrict-year-age-sex-rel",
  width = 14, height = 6,
  multi = uni_c
)


# model: preparation ------------------------------------------------------

# Only for category 'reformed'
# WHY? 'other' is very low in all groups as of 2017
# overall values (2017 until 2021): 
# 'other': between 3 and 17 per year
# 'reformed': between 1099 and 1783 per year
# neglect 'other'

# WHY 'other' conversions not subtracted from 'reformed' conversions?
# Then we should allow for negative values
# Hard to differentiate between numerical problems (in fit) and real negative values

# data: 'reformed' only (religion before the conversion)
con_cyas <- con_pop %>% 
  filter(rel == uni_r[1]) %>% 
  select(-rel)

# check
nrow(con_cyas)
length(uni_c) * (date_end - (date_start + 1) + 1) * (age_max - age_min + 1) * length(uni_s)



# model: smoothing conversions with LOESS over years (by cas) -------------

# smoothing conversions
con_t0 <- Sys.time()

con_smooth <- con_cyas %>%
  arrange(cdistrict, age, sex, year) %>%
  group_by(cdistrict, age, sex) %>%
  mutate(con_smooth = pmax(0, predict(
    loess(con ~ year, span = con_span_y, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()

con_t1 <- Sys.time()
con_t1 - con_t0


# plot preparation

fit_lev <- c("initial", "smoothed")

con_smooth_plot <- con_smooth %>%
  pivot_longer(c(con, con_smooth), names_to = "category", values_to = "con") %>%
  mutate(cat = factor(if_else(category == "con",
    fit_lev[1], fit_lev[2]
  ), levels = fit_lev)) %>%
  select(cdistrict, year, age, sex, cat, con)

# plot: focus age distribution
con_smooth_plot %>%
  filter(year %in% year_past_5) %>%
  sszplot(
    aes_x = "age", aes_y = "con", aes_col = "cat",
    grid = c("as.factor(year)", "sex"),
    labs_y = "conversions per year",
    name = "5010_conversion_smoothed-over-year_focus-age",
    width = 9, height = 9,
    multi = uni_c
  )

# plot: focus years
# age (subjectively selected)
age_plot_smooth <- seq(0, 80, by = 10)

con_smooth_plot %>% 
filter(age %in% age_plot_smooth) %>% 
sszplot(
  aes_x = "year", aes_y = "con", aes_col = "sex", aes_ltyp = "cat",
  wrap = "as.factor(age)", ncol = 5,  
  labs_y = "conversions per year",
  name = "5011_conversion_smoothed-over-year_focus-years",
  width = 12, height = 6,
  multi = uni_c
)


# model: smoothing rate by age with LOESS (by cys) ------------------------

# conversion rate (based on smoothed conversions)
con_rate_cyas <- con_smooth %>% 
  replace_na(list(pop = 0, con_smooth = 0)) %>%
  mutate(rate_temp = if_else(pop < con_min_pop, NA_real_, round(con_smooth / pop * 100, round_rate)),
    con_rate_cyas = if_else(age > con_age_max, con_age_value, rate_temp))        

# smoothing rate by age
rate_fit <- con_rate_cyas %>% 
  arrange(cdistrict, year, sex, age) %>%
  group_by(cdistrict, year, sex) %>%
  mutate(rate_fit = pmax(0, predict(
    loess(con_rate_cyas ~ age, span = con_span_a, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()


# plot preparation
fit_lev <- c("initial", "smoothed")

con_fit_plot <- rate_fit %>%
  pivot_longer(c(con_rate_cyas, rate_fit), names_to = "category", values_to = "rate") %>%
  mutate(cat = factor(if_else(category == "con_rate_cyas",
    fit_lev[1], fit_lev[2]
  ), levels = fit_lev)) %>%
  select(cdistrict, year, age, sex, cat, rate)


# plot: focus age distribution
con_fit_plot %>% 
  filter(year %in% year_past_5) %>% 
sszplot(
  aes_x = "age", aes_y = "rate", aes_col = "cat",
  grid = c("sex", "as.factor(year)"),
  labs_y = "conversion rate (in % per year)",
  name = "5012_conversion-rate_smoothed-over-age",
  width = 15, height = 6,
  multi = uni_c
)


# model: constrained regression -------------------------------------------

# years: base period
years_base <- con_base_begin:con_base_end

# data for base period
rate_base <- rate_fit %>% 
  filter(year %in% years_base)

# prediction (duration: approx. 10 seconds)

con_rate_t0 <- Sys.time()

rate_pred <- con_reg(
  data = rate_base, x = "year", y = "rate_fit",
  group_cols = c("cdistrict", "age", "sex"),
  window = con_window_thres, base_t0 = min(years_base),
  scen_t0 = max(years_base) + 1, scen_t1 = scen_end,
  prop_trend = con_prop_trend, thres_percent = con_thres_percent,
  lower_thres = con_lower_thres, upper_thres = NA
)

con_rate_t1 <- Sys.time()
con_rate_t1 - con_rate_t0


# past and prediction
con_past_pred <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):scen_end,
  age = age_min:age_max,
  sex = uni_s
)) %>%
  left_join(select(con_rate_cyas, cdistrict, year, age, sex, con_rate_cyas),
    by = c("cdistrict", "year", "age", "sex")
  ) %>%
  left_join(rate_pred, by = c("cdistrict", "year", "age", "sex")) %>%
  mutate(rate_all = if_else(year <= con_base_end, con_rate_cyas, pred_roll))


sszplot(con_past_pred,
  aes_x = "age", aes_y = "rate_all", aes_col = "year",
  grid = c(".", "sex"),
  labs_y = "conversion rate (in % per year)",
  name = "5013_conversion-rate__by-cdistrict-year-age-sex_past-future",
  width = 11, height = 5,
  multi = uni_c
)

# export preparation
ex_con_rate_cyas <- mutate(con_past_pred,
  rate = round(rate_all, round_rate)
) %>%
  filter(year >= scen_begin) %>%
  select(cdistrict, year, age, sex, rate) %>%
  arrange(cdistrict, year, age, sex)

# export
write_csv(ex_con_rate_cyas, paste0(exp_path, "conversion_rate-cyas_future.csv"))




