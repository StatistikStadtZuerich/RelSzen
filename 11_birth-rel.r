
# header ------------------------------------------------------------------

# birth: rel of the babies



# paths, general ----------------------------------------------------------

# paths, general functions
main_path <- "V:/RelSzen/"
res_path <- paste0(main_path, "3_Resultate/10_Birth/")
source("90_general.r")


# import and data preparation ---------------------------------------------

# birth
# age: only births of women at 'fertile age'
# relb: rel of baby
# rel: rel of mother

bir <- read_excel(paste0(data_path, "/input/Geb.xlsx")) %>%
  rename(year = EreignisDatJahr, age = AlterVMutterCd, bir = AnzGebuWir) %>%
  filter((age >= bir_age_begin) & (age <= bir_age_end)) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    relb = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r),
    rel = factor(if_else(RelMutter == 1, uni_r[1], uni_r[2]), uni_r),
    cdistrict = factor(cdistrict, uni_c)
  ) %>%
  select(cdistrict, year, age, relb, rel, bir) %>%
  group_by(cdistrict, year, age, relb, rel) %>%
  summarize(
    bir = sum(bir),
    .groups = "drop"
  )


# rel change: plots ----------------------------------------------------

# WHY plots?
# find out the relevant processes/variables

# rel change: data preparation
cha <- bir %>%
  pivot_wider(names_from = "relb", values_from = "bir") %>%
  replace_na(list(reformed = 0, other = 0)) %>%
  mutate(
    change = if_else(rel == "reformed", other, reformed),
    nochange = if_else(rel == "reformed", reformed, other),
    total = change + nochange
  )

# change by year, rel
cha_yr <- group_by(cha, year, rel) %>%
  summarize(
    change = sum(change),
    total = sum(total),
    .groups = "drop"
  ) %>%
  mutate(cha_yr = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))

year5 <- cha_yr$year[cha_yr$year %% 5 == 0]

sszplot(cha_yr,
  aes_x = "year", aes_y = "cha_yr", aes_col = "rel",
  geom = c("line", "point"),
  i_x = year5,
  labs_y = "religion change in %",
  name = "1100_rel-change_by-year-rel"
)


# change by year, age1, rel
cha_ya1r <- left_join(cha, look_a1, by = "age") %>%
  group_by(year, age_1, rel) %>%
  summarize(
    change = sum(change),
    total = sum(total),
    .groups = "drop"
  ) %>%
  mutate(cha_ya1r = if_else(total == 0, NA_real_, round(change / total * 100, round_rate))) %>%
  rename(age = age_1)

sszplot(cha_ya1r,
  aes_x = "year", aes_y = "cha_ya1r", aes_col = "rel",
  geom = c("line", "point"),
  i_x = year5,
  labs_y = "religion change in %",
  wrap = "age",
  name = "1101_rel-change_by-year-age1-rel",
  width = 12
)


# change by year, age2, rel
cha_ya2r <- left_join(cha, look_a2, by = "age") %>%
  group_by(year, age_2, rel) %>%
  summarize(
    change = sum(change),
    total = sum(total),
    .groups = "drop"
  ) %>%
  mutate(cha_ya2r = if_else(total == 0, NA_real_, round(change / total * 100, round_rate))) %>%
  rename(age = age_2)

sszplot(cha_ya2r,
  aes_x = "year", aes_y = "cha_ya2r", aes_col = "rel",
  geom = c("line", "point"),
  i_x = year5,
  labs_y = "religion change in %",
  wrap = "age", ncol = nlevels(cha_ya2r$age),
  name = "1102_rel-change_by-year-age2-rel",
  width = 12
)


# change by district, year, rel
cha_cyr <- group_by(cha, cdistrict, year, rel) %>%
  summarize(
    change = sum(change),
    total = sum(total),
    .groups = "drop"
  ) %>%
  mutate(cha_dyr = if_else(total == 0, NA_real_, round(change / total * 100, round_rate)))

sszplot(cha_cyr,
  aes_x = "year", aes_y = "cha_dyr", aes_col = "rel",
  geom = c("line", "point"),
  i_x = year5,
  labs_y = "religion change in %",
  wrap = "cdistrict", ncol = 4,
  name = "1103_rel-change_by-cdistrict-year-rel",
  width = 12, height = 10
)


# change by cdistrict, year, age1 (rel of 'reformed-mother' only, since few cases for 'other-mother' to 'reformed-baby')
cha_cya1r <- left_join(cha, look_a1, by = "age") %>%
  filter(rel == "reformed") %>%
  group_by(cdistrict, year, age_1) %>%
  summarize(
    change = sum(change),
    total = sum(total),
    .groups = "drop"
  ) %>%
  mutate(cha_cya1r = if_else(total == 0, NA_real_, round(change / total * 100, round_rate))) %>%
  rename(age = age_1)


sszplot(cha_cya1r,
  aes_x = "year", aes_y = "cha_cya1r", aes_col = "age",
  geom = c("line", "point"),
  i_x = year5,
  labs_y = "religion change in %",
  wrap = "cdistrict", ncol = 4,
  name = "1104_rel-change_by-cdistrict-year-age1_mother-reformed",
  width = 12, height = 8
)


# origin change (cha): model ----------------------------------------------

# base years
cha_base <- filter(cha_yr, (year >= bir_cha_base_begin) & (year <= bir_cha_base_end))

# constrained regression
# (proportion of linear model and mean, within bandwidth)

cha_pred <- con_reg(
  data = cha_base, x = "year", y = "cha_yr",
  group_cols = "rel",
  window = bir_cha_window_thres, base_t0 = bir_cha_base_begin,
  scen_t0 = scen_begin, scen_t1 = scen_end,
  prop_trend = bir_cha_prop_trend, thres_percent = bir_cha_thres_percent,
  lower_thres = bir_cha_lower_thres, upper_thres = bir_cha_upper_thres
) %>%
  # with the input data (WHY? to assess the regression)
  full_join(select(cha_yr, year, rel, cha_yr),
    by = c("year", "rel")
  ) %>%
  # output generation, one rate variable for both future and past
  mutate(cha_all = if_else(year <= bir_cha_base_end, cha_yr, pred_roll)) %>%
  arrange(year, rel)

sszplot(cha_pred,
  aes_x = "year", aes_y = "cha_all", aes_col = "rel",
  geom = c("line", "point"),
  i_x = c(bir_cha_base_begin, bir_cha_base_end),
  labs_y = "religion change in %",
  name = "1105_rel-change_by-year-rel_past-future"
)




# export the results ------------------------------------------------------

# prepare the export data
cha_ex <- mutate(cha_pred, cha = round(cha_all, round_rate)) %>%
  filter(year >= scen_begin) %>%
  select(year, rel, cha) %>%
  arrange(year, rel)

# export
write_csv(cha_ex, paste0(exp_path, "/birth_rel-change_future.csv"))



# cleanup -----------------------------------------------------------------

# remove variables without further use
rm(list = c(
  "bir", "cha", "cha_yr", "cha_ya1r", "cha_ya2r",
  "cha_cyr", "cha_cya1r", "cha_base", "cha_pred"
))
