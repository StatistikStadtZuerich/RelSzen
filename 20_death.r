# header ------------------------------------------------------------------
# death: mortality rate


# paths, general ----------------------------------------------------------

# paths, general functions
  main_path <- "V:/RelSzen/"
  res_path <- paste0(main_path, "3_Resultate/20_Death/")
  source("90_general.r")  
  
  

# import and data preparation ---------------------------------------------

# import: population and death
imp_pop_dea <- read_excel(paste0(data_path, "/input/BesTodZuzWeg.xlsx"))    
  
# death
dea <- imp_pop_dea %>%
  rename(year = StichtagDatJahr, age = AlterVCd, dea = AnzSterWir) %>%
  mutate(sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
         rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r)) %>% 
  group_by(year, age, sex, rel) %>%
  summarize(
    dea = sum(dea),
    .groups = "drop"
  )

  
# birth (needed to calculate life expectancy)  
bir <- read_excel(paste0(data_path, "/input/Geb.xlsx")) %>% 
  rename(year = EreignisDatJahr, bir = AnzGebuWir) %>%   
  mutate(
    age = 0,
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s), 
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r) 
  ) %>% 
  group_by(year, age, sex, rel) %>%
  summarize(
    B = sum(bir),
    .groups = "drop"
  )

# population
# year: begin of year population
pop <- imp_pop_dea %>%  
  rename(age = AlterVCd, pop = AnzBestWir) %>%
  mutate(
    year = StichtagDatJahr + 1,
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r)
  ) %>%
  group_by(year, age, sex, rel) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  )


# mortality ---------------------------------------------------------------

# mortality (calculation based on all possible cases)
mor_yasr <- as_tibble(expand_grid(
  year = (date_start + 1):date_end,
  age = age_min:age_max,
  sex = uni_s,
  rel = uni_r)) %>% 
  left_join(pop, by = c("year", "age", "sex", "rel")) %>%
  left_join(dea, by = c("year", "age", "sex", "rel")) %>%
  replace_na(list(pop = 0, dea = 0)) %>%
  mutate(mor_yasr = if_else(pop == 0, NA_real_, round(dea / pop * 100, round_rate)))



# mortality: age plots ----------------------------------------------------

# years (past only)
year_past <- (date_start + 1):date_end
year_5 <- sort(unique(year_past[year_past %% 5 == 0]))

# focus: difference in mortality by sex
sszplot(filter(mor_yasr, year %in% year_5),
  aes_x = "age", aes_y = "mor_yasr", aes_col = "sex",
  labs_y = "mortality rate (in % per year)",
  scale_y = "log",
  grid = c("rel", "year"),
  name = "2000_mortality_by-year-age-sex-rel_focus-age-sex",
  width = 12, height = 7
)

# focus: difference in mortality by region
sszplot(filter(mor_yasr, year %in% year_5),
  aes_x = "age", aes_y = "mor_yasr", aes_col = "rel",
  labs_y = "mortality rate (in % per year)",
  scale_y = "log",
  grid = c("sex", "year"),
  name = "2001_mortality_by-year-age-sex-rel_focus-age-rel",
  width = 12, height = 7
)


# mortality: year plots ---------------------------------------------------

# age
age_select <- c(0, 20, 40, 50, 60, 70, 80)

# focus: difference in mortality by sex
sszplot(filter(mor_yasr, age %in% age_select),
  aes_x = "year", aes_y = "mor_yasr", aes_col = "sex",
  labs_y = "mortality rate (in % per year)",
  scale_y = "log",
  grid = c("rel", "age"),
  name = "2002_mortality_by-year-age-sex-rel_focus-year-sex",
  width = 14, height = 7,
  angle = 90
)

# focus: difference in mortality by region
sszplot(filter(mor_yasr, age %in% age_select),
  aes_x = "year", aes_y = "mor_yasr", aes_col = "rel",
  labs_y = "mortality rate (in % per year)",
  scale_y = "log",
  grid = c("sex", "age"),
  name = "2003_mortality_by-year-age-sex-rel_focus-year-region",
  width = 14, height = 7,
  angle = 90
)


# life expectancy: based on deaths, births, population --------------------
# no function, since only used once

# age capped
pop_capped <- pop %>%
  mutate(age_capped = if_else(age >= dea_age_max_le, dea_age_max_le, age)) %>%
  group_by(year, age_capped, sex, rel) %>%
  summarize(
    pop = sum(pop),
    .groups = "drop"
  ) %>%
  rename(age = age_capped)

dea_capped <- dea %>%
  mutate(age_capped = if_else(age >= dea_age_max_le, dea_age_max_le, age)) %>%
  group_by(year, age_capped, sex, rel) %>%
  summarize(
    dx = sum(dea),
    .groups = "drop"
  ) %>%
  rename(age = age_capped)

# population at the end of the year
pop_end_year <- mutate(pop_capped, end_of_year = year - 1) %>%
  select(end_of_year, age, sex, rel, pop) %>%
  rename(year = end_of_year, pop_end = pop)

# mean population per year
pop_mean <- as_tibble(expand_grid(
  year = (date_start + 1):date_end,
  age = age_min:dea_age_max_le,
  sex = uni_s,
  rel = uni_r
)) %>%
  left_join(pop_capped, by = c("year", "age", "sex", "rel")) %>%
  left_join(pop_end_year, by = c("year", "age", "sex", "rel")) %>%
  replace_na(list(pop = 0, pop_end = 0)) %>%
  mutate(Px = (pop + pop_end) / 2) %>%
  left_join(dea_capped, by = c("year", "age", "sex", "rel")) %>%
  left_join(bir, by = c("year", "age", "sex", "rel")) %>%
  replace_na(list(dx = 0, B = 0))


# preparation (for life expectancy calculation)

le_mult <- mutate(pop_mean,
  # mx: age-specific mortality rate, Yusuf et al. (2014), eq 7.1
  mx = if_else(Px > 0, dx / Px, NA_real_),
  # qx1: probability to die between age x and age x+1, Yusuf et al. (2014), eq 7.4, 7.5, 7.6
  # the last two age-values: qx should be 1
  # why? otherwise after the lag, some people 'survive' the last age
  qx1 = pmin(1, if_else(age == 0, if_else(B > 0, dx / B, NA_real_),
    if_else((age > 0) & (age < (dea_age_max_le - 1)), 2 * mx / (2 + mx), 1)
  )),
  # if there is no one at a certain age in the population (e.g. no 96 year old men),
  # then qx1 is NA. However, a value is needed to multiply the subsequent survival probabilities
  qx = if_else(is.na(qx1), dea_qx_NA_le, qx1),
  # survival
  px_ = 1 - qx
) %>%
  # cumulative product
  arrange(year, sex, rel, age) %>%
  group_by(year, sex, rel) %>%
  mutate(
    mult = cumprod(px_),
    multlag = lag(mult, default = 1)
  ) %>%
  ungroup() %>%
  mutate(
    lx = dea_radix * multlag,
    # expected deaths dx_, Yusuf et al. (2014), eq 7.8
    dx_ = lx * qx,
    # number of survivors lxp1, Yusuf et al. (2014), eq 7.9
    lxp1 = lx - dx_,
    # person-years lived, Yusuf et al. (2014), eq 7.12, 7.14, 7.15
    Lx_ = if_else(age == 0, 0.3 * lx + 0.7 * lxp1,
      if_else((age > 0) & (age < dea_age_max_le), 0.5 * lx + 0.5 * lxp1, dx / mx)
    )
  )

# check subgroups
le_mult_sub <- filter(le_mult, (year == 2021) & (sex == "female") & (rel == "reformed"))
plot(le_mult_sub$age, le_mult_sub$px_, type = "o")
plot(le_mult_sub$age, le_mult_sub$mult)

# life expectancy at certain age (e.g. birth)
le <- le_mult %>% 
  group_by(year, sex, rel) %>%
  summarize(
    life_years = sum(Lx_[age >= dea_age_at], na.rm = TRUE),
    start_pop = min(lx[age == dea_age_at], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(life = dea_age_at + life_years / start_pop) %>%
  select(year, sex, rel, life)


# life expectancy: based on mortality rate --------------------------------

# life expectancy
le_ysr <- life_exp(
  data = mor_yasr, mor = "mor_yasr",
  age = "age", group_cols = c("year", "sex", "rel"),
  age_max = dea_age_max_le, qx_NA = dea_qx_NA_le,
  age_at = dea_age_at, radix = dea_radix
)


# plot: life expectancy ---------------------------------------------------

# years
year_all <- (date_start + 1):date_end
year_all_5 <- sort(unique(year_all[year_all %% 5 == 0]))

# focus: differences by sex
sszplot(le_ysr,
  aes_x = "year", aes_y = "life_exp", aes_col = "sex",
  i_x = year_all_5,
  labs_y = "life expectancy at birth",
  grid = c(".", "rel"),
  name = "2004_life-expectancy-at-birth_by-year-sex-rel_focus-sex",
  width = 10, height = 6
)


# focus: differences by rel
sszplot(le_ysr,
  aes_x = "year", aes_y = "life_exp", aes_col = "rel",
  i_x = year_all_5,
  labs_y = "life expectancy at birth",
  grid = c(".", "sex"),
  name = "2005_life-expectancy-at-birth_by-year-sex-rel_focus-rel",
  width = 10, height = 6
)



