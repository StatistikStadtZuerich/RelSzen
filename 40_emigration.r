# header ------------------------------------------------------------------
# emigration*


# paths, general ----------------------------------------------------------

# paths, general functions
main_path <- "V:/RelSzen/"
res_path <- paste0(main_path, "3_Resultate/40_Emigration/")
source("90_general.r")


# 0. data import -------------------------------------------------------------

# emigration
emi <- read_excel(paste0(data_path, "/input/BesTodZuzWeg.xlsx")) %>%
  rename(year = StichtagDatJahr, age = AlterVCd, emi = Wegzug) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r),
    cdistrict = factor(cdistrict, uni_c)
  ) %>%
  select(cdistrict, year, age, sex, rel, emi) %>%
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(
    emi = sum(emi),
    .groups = "drop"
  )

# relocation
# only migration from a certain district
# WHY? this is needed to calculate emigration* (i.e. migration from a certain district)

reo <- read_excel(paste0(data_path, "/input/BesTodZuzWeg.xlsx")) %>%
  rename(year = StichtagDatJahr, age = AlterVCd, reo = Umwegzug) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r),
    cdistrict = factor(cdistrict, uni_c)
  ) %>%
  select(cdistrict, year, age, sex, rel, reo) %>%
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(
    reo = sum(reo),
    .groups = "drop"
  )


# emigration* (i.e. emigration and relocation from a certain district)
ems <- bind_rows(
  rename(emi, ems = emi),
  rename(reo, ems = reo)
) %>%
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(
    ems = sum(ems),
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

# ems and pop: aggregate
ems_cy <- group_by(ems, cdistrict, year) %>%
  summarize(
    ems = sum(ems),
    .groups = "drop"
  )

pop_cy <- group_by(pop, cdistrict, year) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )

# emigration* rate (based on all possible cases)
ems_rate_cy <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):date_end
)) %>%
  left_join(pop_cy, by = c("cdistrict", "year")) %>%
  left_join(ems_cy, by = c("cdistrict", "year")) %>%
  replace_na(list(pop = 0, ems = 0)) %>%
  mutate(ems_rate_cy = if_else(pop == 0, NA_real_, round(ems / pop * 100, round_rate)))

# years of the past (for plot)
year_past <- (date_start + 1):date_end
year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

# plot
sszplot(ems_rate_cy,
  aes_x = "year", aes_y = "ems_rate_cy",
  i_x = year_past_5,
  wrap = "cdistrict", ncol = 5,
  labs_y = "emigration* rate (in % per year)",
  name = "4000_emigration-star-rate_by-cdistrict-year",
  width = 13, height = 7
)


# 1. future emigration (cy) ----------------------------------------------

# base years
ems_base <- filter(
  ems_rate_cy,
  (year >= ems_base_begin) & (year <= ems_base_end)
)

# prediction: constrained regression
ems_pred <- con_reg(
  data = ems_base, x = "year", y = "ems_rate_cy",
  group_cols = "cdistrict",
  window = ems_rate_window_thres, base_t0 = ems_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = ems_rate_prop_trend, thres_percent = ems_rate_thres_percent,
  lower_thres = ems_rate_lower_thres, upper_thres = NA
)

# past and prediction
ems_past_pred <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):scen_end
)) %>%
  left_join(select(ems_rate_cy, cdistrict, year, ems_rate_cy),
    by = c("cdistrict", "year")
  ) %>%
  left_join(ems_pred, by = c("cdistrict", "year")) %>%
  mutate(rate_all = if_else(year <= ems_base_end, ems_rate_cy, pred_roll))


# plot

# levels
time_lev <- c("past", "future")

# plot data
plot_dat_ems_pred <- select(ems_past_pred, cdistrict, year, rate_all) %>%
  mutate(time = factor(if_else(year <= ems_base_end,
    time_lev[1], time_lev[2]
  ), levels = time_lev))

sszplot(plot_dat_ems_pred,
  aes_x = "year", aes_y = "rate_all", aes_ltyp = "time",
  i_x = c(ems_base_begin, ems_base_end),
  wrap = "cdistrict", ncol = 5,
  labs_y = "emigration* rate (in % per year)",
  name = "4001_emigration-star-rate_by-cdistrict-year_predicition",
  width = 13, height = 7
)


# plot (more detailed: regression and limits)
sszplot(ems_past_pred,
  aes_x = "year", aes_y = "ems_rate_cy",
  geom = "point",
  i_x = c(ems_base_begin, ems_base_end),
  wrap = "cdistrict", ncol = 5,
  labs_y = "emigration* rate (in % per year)",
  name = "4002_emigration-star-rate_by-cdistrict-year_predicition-details",
  width = 13, height = 7,
  quotes = c(
    quote(geom_line(aes(x = year, y = pred), linetype = 2)),
    quote(geom_line(aes(x = year, y = pred_mean), linetype = 3)),
    quote(geom_line(aes(x = year, y = pred_roll)))
  )
)

# export preparation
ex_ems_rate_cy <- mutate(ems_past_pred,
  rate = round(rate_all, round_rate)
) %>%
  filter(year >= scen_begin) %>%
  select(cdistrict, year, rate) %>%
  arrange(cdistrict, year)

# export
write_csv(ex_ems_rate_cy, paste0(exp_path, "emigration-star_rate-cy_future.csv"))



# 2. future proportion (sr) ------------------------------------------------------

# possible cases
cas_cysr <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):date_end,
  sex = uni_s,
  rel = uni_r
))

# distribution of sex and religion per cdistrict and year
# comment: a proportion not a rate

ems_cysr <- group_by(ems, cdistrict, year, sex, rel) %>%
  summarize(
    ems_cysr = sum(ems),
    .groups = "drop"
  ) %>%
  left_join(rename(ems_cy, ems_cy = ems),
    by = c("cdistrict", "year")
  ) %>%
  right_join(cas_cysr, by = c("cdistrict", "year", "sex", "rel")) %>%
  replace_na(list(ems_cysr = 0, ems_cy = 0)) %>%
  mutate(ems_prop_cysr = if_else(ems_cy == 0, NA_real_, round(ems_cysr / ems_cy * 100, round_prop)))

# years of the past (for plot)
year_past <- (date_start + 1):date_end
year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

# plot
sszplot(ems_cysr,
  aes_x = "year", aes_y = "ems_prop_cysr", aes_col = "sex", aes_ltyp = "rel",
  i_x = year_past_5,
  wrap = "cdistrict", ncol = 5,
  labs_y = "proportion in % (per church district and year, in emigration*)",
  name = "4003_emigration_proportion-sex-rel_by-cdistrict-year",
  width = 13, height = 7
)

# base years
ems_sr_base <- filter(
  ems_cysr,
  (year >= ems_sr_base_begin) & (year <= ems_sr_base_end)
)

# prediction: constrained regression

# Why no upper threshold?
# proportions per cdistrict/year will be standardized to 100 percent anyways

ems_sr_pred <- con_reg(
  data = ems_sr_base, x = "year", y = "ems_prop_cysr",
  group_cols = c("cdistrict", "sex", "rel"),
  window = ems_sr_window_thres, base_t0 = ems_sr_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = ems_sr_prop_trend, thres_percent = ems_sr_thres_percent,
  lower_thres = ems_sr_lower_thres, upper_thres = NA
)

# standardize proportions per cdistrict and year to 100 percent
ems_sr_pred_stand <- group_by(ems_sr_pred, cdistrict, year) %>%
  mutate(
    pred_roll_sum = sum_NA(pred_roll),
    pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_,
      round(
        pred_roll / pred_roll_sum * 100,
        round_prop
      )
    )
  ) %>%
  ungroup()

# past and prediction
ems_sr_past_pred <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):scen_end,
  sex = uni_s,
  rel = uni_r
)) %>%
  left_join(select(ems_cysr, cdistrict, year, sex, rel, ems_prop_cysr),
    by = c("cdistrict", "year", "sex", "rel")
  ) %>%
  left_join(ems_sr_pred_stand, by = c("cdistrict", "year", "sex", "rel")) %>%
  mutate(prop_all = if_else(year <= ems_sr_base_end, ems_prop_cysr, pred_roll_stand))

# plot
sszplot(ems_sr_past_pred,
  aes_x = "year", aes_y = "prop_all", aes_col = "sex", aes_ltyp = "rel",
  i_x = c(ems_sr_base_begin, ems_sr_base_end),
  wrap = "cdistrict", ncol = 5,
  labs_y = "proportion in % (per district and year, in emigration*)",
  name = "4004_emigration_proportion-sex-rel_by-cdistrict-year_prediction",
  width = 13, height = 7
)

# export preparation
ex_ems_prop_cy <- mutate(ems_sr_past_pred,
  prop = round(prop_all, round_prop)
) %>%
  filter(year >= scen_begin) %>%
  select(cdistrict, year, sex, rel, prop) %>%
  arrange(cdistrict, year, sex, rel)

# export
write_csv(ex_ems_prop_cy, paste0(exp_path, "emigration-star_prop-sr-cy_future.csv"))



# 3. proportion (a) -------------------------------------------------------

# WHY? to see where the denominator of the age proportion is low

# emigration*: cysr
ems_cysr <- group_by(ems, cdistrict, year, sex, rel) %>%
  summarize(
    ems_cysr = sum(ems),
    .groups = "drop"
  ) %>%
  right_join(cas_cysr, by = c("cdistrict", "year", "sex", "rel")) %>%
  replace_na(list(ems_cysr = 0))

# years of the past (for plot)
year_past <- date_start:date_end
year_past_5 <- sort(unique(year_past[year_past %% 5 == 0]))

# plot
sszplot(ems_cysr,
  aes_x = "year", aes_y = "ems_cysr", aes_col = "sex", aes_ltyp = "rel",
  i_x = year_past_5,
  wrap = "cdistrict", ncol = 5,
  labs_y = "emigration* per year",
  name = "4010_emigration_star_per-district-year-sex-rel",
  width = 14, height = 7
)

# plot
sszplot(ems_cysr,
  aes_x = "year", aes_y = "ems_cysr", aes_col = "sex", aes_ltyp = "rel",
  i_x = year_past_5,
  wrap = "cdistrict", ncol = 5, gridscale = "free",
  labs_y = "emigration* per year",
  name = "4011_emigration_star_per-district-year-sex-rel_free-scales",
  width = 14, height = 7
)


# emigration*: cyasr (already summarized before)
ems_cyasr <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = date_start:date_end,
  age = age_min:age_max,
  sex = uni_s,
  rel = uni_r
)) %>%
  left_join(ems, by = c("cdistrict", "year", "age", "sex", "rel")) %>%
  rename(ems_cyasr = ems) %>%
  replace_na(list(ems_cyasr = 0)) %>%
  left_join(ems_cysr, by = c("cdistrict", "year", "sex", "rel")) %>%
  arrange(cdistrict, year, sex, rel, age) %>%
  mutate(ems_prop_a = if_else(ems_cysr == 0, NA_real_, round(ems_cyasr / ems_cysr * 100, round_prop)))


# plot: focus age distribution
# years (subjectively selected)
# WHY with rev? To have the last year in the plot
years_plot <- rev(seq(date_end, date_start, by = -8))

sszplot(filter(ems_cyasr, year %in% years_plot),
  aes_x = "age", aes_y = "ems_prop_a", aes_col = "year",
  grid = c("sex", "rel"),
  labs_y = "proportion in %", labs_col = "year",
  name = "4012_emigration_star_age-proportion_per-cdistrict-year-sex-rel_focus-age",
  width = 11, height = 8,
  multi = uni_c
)


# plot: focus years
# age (subjectively selected)
age_plot <- seq(0, 60, by = 20)

sszplot(filter(ems_cyasr, age %in% age_plot),
  aes_x = "year", aes_y = "ems_prop_a", aes_col = "age",
  grid = c("sex", "rel"),
  labs_y = "proportion in %", labs_col = "age",
  name = "4013_emigration_star_age-proportion_per-cdistrict-year-sex-rel_focus-years",
  width = 11, height = 8,
  multi = uni_c
)


# 3.1 smoothing emigration* with LOESS over years (by casr) --------------

ems_smooth <- ems_cyasr %>%
  arrange(cdistrict, age, sex, rel, year) %>%
  group_by(cdistrict, age, sex, rel) %>%
  mutate(ems_smooth = pmax(0, predict(
    loess(ems_cyasr ~ year, span = ems_span_y, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()


# plot preparation

fit_lev <- c("initial", "smoothed")

ems_smooth_plot <- ems_smooth %>%
  pivot_longer(c(ems_cyasr, ems_smooth), names_to = "category", values_to = "ems") %>%
  mutate(cat = factor(if_else(category == "ems_cyasr",
    fit_lev[1], fit_lev[2]
  ), levels = fit_lev)) %>%
  select(cdistrict, year, age, sex, rel, cat, ems)


# plot: focus age distribution
sszplot(filter(ems_smooth_plot, year %in% year_past_5),
  aes_x = "age", aes_y = "ems", aes_col = "cat",
  grid = c("as.factor(year)", "rel*sex"),
  labs_y = "emigration* per year",
  name = "4014_emigration_star_smoothed-over-year_focus-age",
  width = 11, height = 8,
  multi = uni_c
)

# plot: focus years
# age (subjectively selected)
age_plot_smooth <- seq(0, 60, by = 20)

sszplot(filter(ems_smooth_plot, age %in% age_plot_smooth),
  aes_x = "year", aes_y = "ems", aes_col = "cat",
  grid = c("as.factor(age)", "rel*sex"),
  labs_y = "emigration* per year",
  name = "4015_emigration_star_smoothed-over-year_focus-years",
  width = 11, height = 8,
  multi = uni_c
)

# age proportion (after smoothing emigration* over years)

# preparation
ems_smooth_prep <- ems_smooth %>%
  select(cdistrict, year, age, sex, rel, ems_smooth) %>%
  rename(ems_cyasr = ems_smooth)

# age proportion
ems_age_prop_smooth <- group_by(ems_smooth_prep, cdistrict, year, sex, rel) %>%
  mutate(
    ems_cysr = sum_NA(ems_cyasr),
    prop_a_smooth = if_else(ems_cysr == 0, NA_real_, round(ems_cyasr / ems_cysr * 100, round_prop))
  ) %>%
  ungroup() %>%
  select(cdistrict, year, age, sex, rel, prop_a_smooth) %>%
  arrange(cdistrict, year, sex, rel, age)

# plot: focus age distribution
sszplot(filter(ems_age_prop_smooth, year %in% year_past_5),
  aes_x = "age", aes_y = "prop_a_smooth", aes_col = "year",
  grid = c("sex", "rel"),
  labs_y = "proportion in %",
  name = "4016_emigration_star_age-proportion_after-smoothing_focus-age",
  width = 11, height = 8,
  multi = uni_c
)

# plot: focus years
# age (subjectively selected)
age_plot_smooth_prop <- seq(0, 60, by = 20)

sszplot(filter(ems_age_prop_smooth, age %in% age_plot_smooth_prop),
  aes_x = "year", aes_y = "prop_a_smooth", aes_col = "age",
  grid = c("sex", "rel"),
  labs_y = "proportion in %", labs_col = "age",
  name = "4017_emigration_star_age-proportion_after-smoothing_focus-years",
  width = 11, height = 8,
  multi = uni_c
)


# 3.2 smoothing proportion by age with LOESS (by cysr) --------------------

prop_fit <- arrange(ems_age_prop_smooth, cdistrict, year, sex, rel, age) %>%
  group_by(cdistrict, year, sex, rel) %>%
  mutate(prop_fit = pmax(0, predict(
    loess(prop_a_smooth ~ age, span = ems_span_a, degree = 1, na.action = na.aggregate)
  ))) %>%
  ungroup()


# plot preparation
fit_lev <- c("initial", "smoothed")

ems_fit_plot <- prop_fit %>%
  pivot_longer(c(prop_a_smooth, prop_fit), names_to = "category", values_to = "prop") %>%
  mutate(cat = factor(if_else(category == "prop_a_smooth",
    fit_lev[1], fit_lev[2]
  ), levels = fit_lev)) %>%
  select(cdistrict, year, age, sex, rel, cat, prop)

# plot: focus age distribution

sszplot(filter(ems_fit_plot, year %in% year_past_5),
  aes_x = "age", aes_y = "prop", aes_col = "cat",
  grid = c("as.factor(year)", "rel*sex"),
  labs_y = "proportion in %",
  name = "4018_emigration_star_proportion_smoothed_focus-age",
  width = 11, height = 8,
  multi = uni_c
)


# 3.3. constrained regression ---------------------------------------------

# years: base period
years_base <- ems_age_base_begin:ems_age_base_end

# data for base period
prop_base <- filter(prop_fit, year %in% years_base)

# prediction (duration: approx. 30 seconds)
# Why no upper threshold?
# proportions will be standardized to 100 percent anyways

prop_pred <- con_reg(
  data = prop_base, x = "year", y = "prop_fit",
  group_cols = c("cdistrict", "age", "sex", "rel"),
  window = ems_age_window_thres, base_t0 = min(years_base),
  scen_t0 = max(years_base) + 1, scen_t1 = scen_end,
  prop_trend = ems_age_prop_trend, thres_percent = ems_age_thres_percent,
  lower_thres = ems_age_lower_thres, upper_thres = NA
)

# limit prediction period
prop_pred_begin <- filter(prop_pred, year >= scen_begin)

# standardize the sum of the proportions to 100 percent
prop_stand <- group_by(prop_pred_begin, cdistrict, year, sex, rel) %>%
  summarize(
    pred_roll_sum = sum_NA(pred_roll),
    .groups = "drop"
  ) %>%
  right_join(prop_pred_begin, by = c("cdistrict", "year", "sex", "rel")) %>%
  mutate(pred_roll_stand = if_else(pred_roll_sum == 0, NA_real_,
    round(
      pred_roll / pred_roll_sum * 100,
      round_prop
    )
  ))

# past and prediction
ems_a_past_pred <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = (date_start + 1):scen_end,
  age = age_min:age_max,
  sex = uni_s,
  rel = uni_r
)) %>%
  left_join(select(ems_cyasr, cdistrict, year, age, sex, rel, ems_prop_a),
    by = c("cdistrict", "year", "age", "sex", "rel")
  ) %>%
  left_join(select(prop_stand, cdistrict, year, age, sex, rel, pred_roll_stand),
    by = c("cdistrict", "year", "age", "sex", "rel")
  ) %>%
  mutate(prop_a = if_else(year <= ems_age_base_end, ems_prop_a, pred_roll_stand))

# export preparation
ex_ems_prop_a_cysr <- mutate(ems_a_past_pred,
  prop = round(prop_a, round_prop)
) %>%
  filter(year >= scen_begin) %>%
  select(cdistrict, year, age, sex, rel, prop) %>%
  arrange(cdistrict, year, sex, rel, age)

# export the data
write_csv(
  ex_ems_prop_a_cysr,
  paste0(exp_path, "emigration-star_prop-a-cysr_future.csv")
)

sszplot(ems_a_past_pred,
  aes_x = "age", aes_y = "prop_a", aes_col = "year",
  grid = c("rel", "sex"),
  labs_y = "proportion in %",
  name = "4019_emigration_star_age-proportion_by-district-year-sex-rel_past-future",
  width = 11, height = 8,
  multi = uni_c
)


# plot levels
time_lev <- c("past", "future")

# plot data
plot_a_past_pred <- mutate(ems_a_past_pred,
  time = factor(
    if_else(year <= ems_age_base_end, time_lev[1], time_lev[2]),
    levels = time_lev
  )
)

# plot: focus age distribution
# WHY this plot: it is recommendable to look precisely at the age plots over years
# therefore, a plot that focuses on certain years

# years
year_plot <- seq(date_start + 1, scen_end, by = 8)

sszplot(filter(plot_a_past_pred, year %in% year_plot),
  aes_x = "age", aes_y = "prop_a", aes_col = "year",
  grid = c("sex", "rel"),
  labs_y = "proportion in %", labs_col = "year",
  fix_size = 1,
  name = "4020_emigration_star_age-proportion_by-cdistrict-year-sex-rel_past-future_focus-age",
  width = 11, height = 8,
  multi = uni_c
)

# plot: focus years

# age (subjectively selected)
age_plot_pred <- seq(0, 60, by = 20)

sszplot(filter(plot_a_past_pred, age %in% age_plot_pred),
  aes_x = "year", aes_y = "prop_a", aes_col = "age",
  grid = c("sex", "rel"),
  labs_y = "proportion in %", labs_col = "age",
  name = "4021_emigration_star_age-proportion_by-cdistrict-year-sex-rel_past-future_focus-years",
  width = 11, height = 8,
  multi = uni_c
)
