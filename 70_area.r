# header ------------------------------------------------------------------
# area factors

# cdistrict (KiK): areas according population scenarios (district clusters)
# idistrict (Ki): real areas (based on GIS, including Oberengstringen)


# paths, general ----------------------------------------------------------

# paths, general functions
main_path <- "V:/RelSzen/"
res_path <- paste0(main_path, "3_Resultate/70_Area/")
source("90_general.r")


# 0. data import -------------------------------------------------------------

# population (past, both church district definitions)
pop_past <- read_excel(paste0(data_path, "/input/Kik.xlsx")) %>%
  rename(year = StichtagDatJahr, pop = AnzBestWir) %>%
  left_join(look_a5_num, by = c("Alter" = "num")) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  left_join(look_i, by = c("KI" = "inum")) %>%
  mutate(
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r),
    cdistrict = factor(cdistrict, uni_c),
    idistrict = factor(idistrict, uni_i),
  ) %>%
  select(cdistrict, idistrict, year, age_5, rel, pop) %>%
  group_by(cdistrict, idistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

# population (future)
pop_future <- read_csv(paste0(out_path, "/population_future.csv"), lazy = FALSE) %>%
  mutate(
    cdistrict = factor(cdistrict, levels = uni_c),
    rel = factor(rel, levels = uni_r)
  ) %>%
  left_join(look_a5, by = "age") %>%
  group_by(cdistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()


# check: cdistricts -------------------------------------------------------

# past and future
# past with the data set with both church district definitions

# use only the records with a church district value
# WHY? in the past data there are people from 'idistrict 12'
# however, they do not belong to cdistrict (since they live in Hirzenbach)

pop_cya5r <- pop_past %>%
  group_by(cdistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pop_future) %>%
  filter(cdistrict %in% look_c$cdistrict)

# plot y
pop_cya5r %>%
  group_by(year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  sszplot(
    aes_x = "year", aes_y = "pop",
    i_x = scen_begin,
    labs_y = "population", name = "7000_popscen_y",
    quotes = quote(expand_limits(y = 0)),
    width = 7, height = 5
  )

# plot yr
pop_cya5r %>%
  group_by(year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  sszplot(
    aes_x = "year", aes_y = "pop", aes_col = "rel",
    i_x = scen_begin,
    labs_y = "population", name = "7001_popscen_yr",
    quotes = quote(expand_limits(y = 0)),
    width = 7, height = 5
  )


# plot cyr
pop_cya5r %>%
  group_by(cdistrict, year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  sszplot(
    aes_x = "year", aes_y = "pop", aes_col = "rel",
    i_x = scen_begin,
    labs_y = "population",
    wrap = "cdistrict", ncol = 3,
    name = "7002_popscen_cyr",
    quotes = quote(expand_limits(y = 0)),
    width = 12, height = 9
  )


# plot cya5r
pop_cya5r %>%
  group_by(cdistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  sszplot(
    aes_x = "year", aes_y = "pop", aes_col = "rel",
    i_x = scen_begin,
    labs_y = "population",
    wrap = "age_5", ncol = 3,
    name = "7003_popscen_cyar",
    quotes = quote(expand_limits(y = 0)),
    width = 12, height = 9,
    multi = uni_c
  )



# check: idistricts (past only) ------------------------------------------

# plot iyr
pop_past %>%
  group_by(idistrict, year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  sszplot(
    aes_x = "year", aes_y = "pop", aes_col = "rel",
    labs_y = "population",
    wrap = "idistrict", ncol = 4,
    name = "7004_popscen_iyr",
    quotes = quote(expand_limits(y = 0)),
    width = 11, height = 8
  )


# plot iya5r
pop_past %>%
  group_by(idistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  sszplot(
    aes_x = "year", aes_y = "pop", aes_col = "rel",
    labs_y = "population",
    wrap = "age_5", ncol = 3,
    name = "7005_popscen_iyar",
    quotes = quote(expand_limits(y = 0)),
    width = 12, height = 8,
    multi = uni_i
  )



# area factors ------------------------------------------------------------

# preparation
pop_c <- pop_past %>%
  group_by(cdistrict, year, age_5, rel) %>%
  summarize(pop_c = sum(pop)) %>%
  ungroup()

pop_i <- pop_past %>%
  group_by(idistrict, year, age_5, rel) %>%
  summarize(pop_i = sum(pop)) %>%
  ungroup()

# combination

# for every idistrict: which cdistrict is used to calculation the factor?
# e.g. idistrict 'Kirchenkreis 1': cdistrict 'Kirchenkreis 1+6' is used

pop_ic <- pop_i %>%
  left_join(look_ic, by = "idistrict") %>%
  left_join(pop_c, by = c("cdistrict", "year", "age_5", "rel")) %>%
  select(idistrict, year, age_5, rel, pop_i, pop_c) %>%
  arrange(idistrict, year, age_5, rel) %>%
  mutate(idistrict = factor(idistrict, levels = uni_i))

# factor: iy
pop_ic %>%
  group_by(idistrict, year) %>%
  summarize(
    pop_i = sum(pop_i),
    pop_c = sum(pop_c)
  ) %>%
  ungroup() %>%
  mutate(area_factor = round(pop_i / pop_c, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "area_factor",
    labs_y = "area factor",
    i_y = 1,
    wrap = "idistrict", ncol = 4,
    name = "7010_area-factor_iy",
    quotes = quote(expand_limits(y = 0)),
    width = 11, height = 8
  )


# factor: iyr
pop_ic %>%
  group_by(idistrict, year, rel) %>%
  summarize(
    pop_i = sum(pop_i),
    pop_c = sum(pop_c)
  ) %>%
  ungroup() %>%
  mutate(area_factor = round(pop_i / pop_c, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "area_factor", aes_col = "rel",
    labs_y = "area factor",
    i_y = 1,
    wrap = "idistrict", ncol = 4,
    name = "7011_area-factor_iyr",
    quotes = quote(expand_limits(y = 0)),
    width = 11, height = 8
  )


# factor: iya5r

# WHY not piped to the plot?
# the result is used for predictions

factor_iya5r <- pop_ic %>%
  group_by(idistrict, year, age_5, rel) %>%
  summarize(
    pop_i = sum(pop_i),
    pop_c = sum(pop_c)
  ) %>%
  ungroup() %>%
  mutate(area_factor = round(pop_i / pop_c, round_prop))


sszplot(factor_iya5r,
  aes_x = "year", aes_y = "area_factor", aes_col = "rel",
  labs_y = "area factor",
  i_y = 1,
  wrap = "age_5", ncol = 3,
  name = "7012_area-factor_iyar",
  quotes = quote(expand_limits(y = 0)),
  width = 12, height = 8,
  multi = uni_i
)


# prediction: area factor -------------------------------------------------

# preparation
factor_prep <- factor_iya5r %>%
  select(idistrict, year, age_5, rel, area_factor)

# base years
factor_base <- factor_prep %>%
  filter(year >= are_base_begin, year <= are_base_end)

# prediction

# no upper threshold
# WHY? the factor could exceed one etc.
# therefore, the upper range is controlled by 'are_thres_percent'

factor_pred <- con_reg(
  data = factor_base, x = "year", y = "area_factor",
  group_cols = c("idistrict", "age_5", "rel"),
  window = are_window_thres, base_t0 = are_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = are_prop_trend, thres_percent = are_thres_percent,
  lower_thres = are_lower_thres, upper_thres = NA
) %>%
  # output generation, one rate variable for both future and past
  full_join(factor_prep, by = c("idistrict", "year", "age_5", "rel")) %>%
  mutate(factor_all = if_else(year <= are_base_end, area_factor, pred_roll))

# plot
sszplot(factor_pred,
  aes_x = "year", aes_y = "factor_all", aes_col = "rel",
  labs_y = "area factor",
  i_x = scen_begin,
  i_y = 1,
  wrap = "age_5", ncol = 3,
  name = "7020_area-factor_iyar_past-future",
  quotes = quote(expand_limits(y = 0)),
  width = 12, height = 8,
  multi = uni_i
)

# plot
sszplot(factor_pred,
  aes_x = "year", aes_y = "factor_all", aes_col = "rel",
  labs_y = "area factor",
  i_x = scen_begin,
  i_y = 1,
  wrap = "idistrict", ncol = 5,
  name = "7021_area-factor_iyar_past-future_focus-i",
  quotes = quote(expand_limits(y = 0)),
  width = 17, height = 6,
  multi = uni_a5
)





# prediction: idistrict ---------------------------------------------------

# future
pred_i <- as_tibble(expand_grid(
  idistrict = uni_i,
  year = scen_begin:scen_end,
  age_5 = look_a5_num$age_5,
  rel = uni_r
)) %>%
  left_join(select(look_ic, idistrict, cdistrict), by = "idistrict") %>%
  left_join(pop_future, by = c("cdistrict", "year", "age_5", "rel")) %>%
  left_join(select(factor_pred, idistrict, year, age_5, rel, factor_all),
    by = c("idistrict", "year", "age_5", "rel")
  ) %>%
  mutate(pop_i = pop * factor_all) %>%
  group_by(idistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop_i)) %>%
  ungroup()

# past and future (the result!)
res_iyar <- pop_past %>%
  group_by(idistrict, year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pred_i) %>%
  mutate(idistrict = factor(idistrict, levels = uni_i)) %>%
  arrange(idistrict, year, age_5, rel)

tail(res_iyar)




# result plots ------------------------------------------------------------

# result y
res_y <- res_iyar %>%
  group_by(year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(res_y,
  aes_x = "year", aes_y = "pop",
  i_x = scen_begin,
  labs_y = "population", name = "7030_popscen_y",
  quotes = quote(expand_limits(y = 0)),
  width = 7, height = 5
)

# result yr
res_yr <- res_iyar %>%
  group_by(year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(res_yr,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population", name = "7031_popscen_yr",
  quotes = quote(expand_limits(y = 0)),
  width = 7, height = 5
)

res_prop_yr <- res_yr %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop))

sszplot(res_prop_yr,
  aes_x = "year", aes_y = "prop_ref",
  i_x = scen_begin,
  labs_y = "proportion reformed (in %)", name = "7032_propref_yr",
  quotes = quote(expand_limits(y = 0)),
  width = 7, height = 5
)


# result yar
res_yar <- res_iyar %>%
  group_by(year, age_5, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(res_yar,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "age_5", ncol = 3,
  name = "7033_popscen_yar",
  quotes = quote(expand_limits(y = 0)),
  width = 13, height = 8
)

res_prop_yar <- res_yar %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop))

sszplot(res_prop_yar,
  aes_x = "year", aes_y = "prop_ref",
  i_x = scen_begin,
  labs_y = "proportion reformed (in %)",
  wrap = "age_5", ncol = 3,
  name = "7034_propref_yar",
  quotes = quote(expand_limits(y = 0)),
  width = 13, height = 8
)

# result iyr
res_iyr <- res_iyar %>%
  group_by(idistrict, year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

sszplot(res_iyr,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "idistrict", ncol = 4,
  name = "7035_popscen_iyr",
  quotes = quote(expand_limits(y = 0)),
  width = 13, height = 8
)

res_prop_iyr <- res_iyr %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop))

sszplot(res_prop_iyr,
  aes_x = "year", aes_y = "prop_ref",
  i_x = scen_begin,
  labs_y = "proportion reformed (in %)",
  wrap = "idistrict", ncol = 4,
  name = "7036_propref_iyr",
  quotes = quote(expand_limits(y = 0)),
  width = 13, height = 8
)

# result iyar

sszplot(res_iyar,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  labs_y = "population",
  i_x = scen_begin,
  wrap = "age_5", ncol = 3,
  name = "7037_popscen_iyar",
  quotes = quote(expand_limits(y = 0)),
  width = 12, height = 8,
  multi = uni_i
)

res_prop_iyar <- res_iyar %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop))

sszplot(res_prop_iyar,
  aes_x = "year", aes_y = "prop_ref",
  labs_y = "proportion reformed (in %)",
  i_x = scen_begin,
  wrap = "age_5", ncol = 4,
  name = "7038_propref_iyar",
  quotes = quote(expand_limits(y = 0)),
  width = 12, height = 8,
  multi = uni_i
)



# export the final results ------------------------------------------------------

# round: different by aggregation level

# selected years
fin_years <- c(2021, seq(2025, 2040, by = 5))


# yr (round: 100 persons)
res_prop_yr %>%
  filter(year %in% fin_years) %>%
  mutate(total = reformed + other) %>%
  select(year, total, reformed, other, prop_ref) %>%
  arrange(year) %>%
  mutate(
    total = if_else(year >= scen_begin, round_any(total, 100), total),
    reformed = if_else(year >= scen_begin, round_any(reformed, 100), reformed),
    other = if_else(year >= scen_begin, round_any(other, 100), other),
    prop_ref = round_any(prop_ref, 0.1)
  ) %>%
  write_csv(paste0(fin_path, "/1_yr.csv"))


# yar (round: 50 persons)
res_prop_yar %>%
  filter(year %in% fin_years) %>%
  mutate(total = reformed + other) %>%
  select(year, age_5, total, reformed, other, prop_ref) %>%
  arrange(year, age_5) %>%
  mutate(
    total = if_else(year >= scen_begin, round_any(total, 50), total),
    reformed = if_else(year >= scen_begin, round_any(reformed, 50), reformed),
    other = if_else(year >= scen_begin, round_any(other, 50), other),
    prop_ref = round_any(prop_ref, 0.1)
  ) %>%
  write_csv(paste0(fin_path, "/2_yar.csv"))


# iyr (round: 50 persons)
res_prop_iyr %>%
  filter(year %in% fin_years) %>%
  mutate(total = reformed + other) %>%
  select(year, idistrict, total, reformed, other, prop_ref) %>%
  arrange(year, idistrict) %>%
  mutate(
    total = if_else(year >= scen_begin, round_any(total, 50), total),
    reformed = if_else(year >= scen_begin, round_any(reformed, 50), reformed),
    other = if_else(year >= scen_begin, round_any(other, 50), other),
    prop_ref = round_any(prop_ref, 0.1)
  ) %>%
  write_csv(paste0(fin_path, "/3_iyr.csv"))


# iyar (round: 10 persons)
res_prop_iyar %>%
  filter(year %in% fin_years) %>%
  mutate(total = reformed + other) %>%
  select(year, idistrict, age_5, total, reformed, other, prop_ref) %>%
  arrange(year, idistrict, age_5) %>%
  mutate(
    total = if_else(year >= scen_begin, round_any(total, 10), total),
    reformed = if_else(year >= scen_begin, round_any(reformed, 10), reformed),
    other = if_else(year >= scen_begin, round_any(other, 10), other),
    prop_ref = round_any(prop_ref, 0.1)
  ) %>%
  write_csv(paste0(fin_path, "/4_iyar.csv"))
