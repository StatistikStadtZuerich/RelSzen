# header ------------------------------------------------------------------
# demography and population scenarios



# paths, general ----------------------------------------------------------

# paths, general functions
main_path <- "V:/RelSzen/"
res_path <- paste0(main_path, "3_Resultate/60_Demography-Popscen/")
source("90_general.r")


# import, data preparation ------------------------------------------------

# population: import

# in contrast to rate calculations: population at the end of the year
# WHY: the rates will be applied here to the population of the previous year

pop_import <- read_excel(paste0(data_path, "/input/BesTodZuzWeg.xlsx")) %>%
  rename(year = StichtagDatJahr, age = AlterVCd, pop = AnzBestWir) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    cdistrict = factor(cdistrict, uni_c),
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
    rel = factor(if_else(Rel == 1, uni_r[1], uni_r[2]), uni_r)
  ) %>%
  select(cdistrict, year, age, sex, rel, pop) %>%
  group_by(cdistrict, year, age, sex, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

# population: all cases
pop <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = date_start:date_end,
  age = age_min:age_max,
  sex = uni_s,
  rel = uni_r
)) %>%
  left_join(pop_import, by = c("cdistrict", "year", "age", "sex", "rel")) %>%
  replace_na(list(pop = 0))


# birth: fertility rate (rel = rel of mother)
fer <- read_csv(paste0(exp_path, "/birth_fertility_future.csv"), lazy = FALSE) %>%
  mutate(
    cdistrict = factor(cdistrict, levels = uni_c),
    sex = uni_s[2],
    rel = factor(rel, levels = uni_r)
  ) %>%
  select(cdistrict, year, age, sex, rel, fer)

# birth: religion change (rel = rel of mother)
cha <- read_csv(paste0(exp_path, "/birth_rel-change_future.csv"), lazy = FALSE) %>%
  mutate(
    rel = factor(rel, levels = uni_r)
  )

# birth: proportion male
pro_male <- read_csv(paste0(exp_path, "/birth_sex-ratio_future.csv"), lazy = FALSE)

# death: mortality rate
mor <- read_csv(paste0(exp_path, "/mortality_future.csv"), lazy = FALSE) %>%
  mutate(sex = factor(sex, levels = uni_s))

# immigration*: immigration* rate
ims_rate <- read_csv(paste0(exp_path, "/immigration-star_rate-cy_future.csv"), lazy = FALSE) %>%
  mutate(cdistrict = factor(cdistrict, levels = uni_c))

# immigration*: sex and rel proportion
ims_prop_sr <- read_csv(paste0(exp_path, "/immigration-star_prop-sr-cy_future.csv"), lazy = FALSE) %>%
  mutate(
    cdistrict = factor(cdistrict, levels = uni_c),
    sex = factor(sex, levels = uni_s),
    rel = factor(rel, levels = uni_r)
  )

# immigration*: age proportion
ims_prop_a <- read_csv(paste0(exp_path, "/immigration-star_prop-a-cysr_future.csv"), lazy = FALSE) %>%
  mutate(
    cdistrict = factor(cdistrict, levels = uni_c),
    sex = factor(sex, levels = uni_s),
    rel = factor(rel, levels = uni_r)
  )

# emigration*: emigration* rate
ems_rate <- read_csv(paste0(exp_path, "/emigration-star_rate-cy_future.csv"), lazy = FALSE) %>%
  mutate(cdistrict = factor(cdistrict, levels = uni_c))

# emigration*: sex and rel proportion
ems_prop_sr <- read_csv(paste0(exp_path, "/emigration-star_prop-sr-cy_future.csv"), lazy = FALSE) %>%
  mutate(
    district = factor(cdistrict, levels = uni_c),
    sex = factor(sex, levels = uni_s),
    rel = factor(rel, levels = uni_r)
  )

# emigration*: age proportion
ems_prop_a <- read_csv(paste0(exp_path, "/emigration-star_prop-a-cysr_future.csv"), lazy = FALSE) %>%
  mutate(
    cdistrict = factor(cdistrict, levels = uni_c),
    sex = factor(sex, levels = uni_s),
    rel = factor(rel, levels = uni_r)
  )

# conversion
con_rate <- read_csv(paste0(exp_path, "conversion_rate-cyas_future.csv"), lazy = FALSE) %>%
  mutate(
    cdistrict = factor(cdistrict, levels = uni_c),
    sex = factor(sex, levels = uni_s),
    rel = uni_r[1]
  ) %>%
  rename(rate_con = rate) %>%
  select(cdistrict, year, age, sex, rel, rate_con)

# population scenarios
sce_import <- read_excel(paste0(data_path, "/input/Sze.xlsx")) %>%
  rename(year = Jahr, age = AlterVNum, sce = AnzBestWir) %>%
  left_join(look_c, by = c("KIK" = "cnum")) %>%
  mutate(
    cdistrict = factor(cdistrict, uni_c),
    sex = factor(if_else(SexCd == 1, uni_s[1], uni_s[2]), uni_s),
  ) %>%
  select(cdistrict, year, age, sex, sce) %>%
  group_by(cdistrict, year, age, sex) %>%
  summarize(sce = sum(sce)) %>%
  ungroup()

# population scenarios: all cases
sce <- as_tibble(expand_grid(
  cdistrict = uni_c,
  year = date_start:scen_end,
  age = age_min:age_max,
  sex = uni_s
)) %>%
  left_join(sce_import, by = c("cdistrict", "year", "age", "sex")) %>%
  replace_na(list(sce = 0))


# loop over years ---------------------------------------------------------

# last year of data,
pop_last <- filter(pop, year == date_end)

# years in the future
future <- scen_begin:scen_end

# outputs
out_pop <- NULL
out_bir <- NULL
out_dem <- NULL
out_ref <- NULL

# loop over years
for (iyear in future) {

  # iyear <- 2022

  # population at the begin of the year = population at the end of the previous year
  if (iyear == min(future)) {
    popu <- select(pop_last, -year)
  } else {
    popu <- select(pop_end_year, -year)
  }

  # births (fertility rate * women in the population)
  bir_cr <- fer %>%
    filter(year == iyear) %>%
    left_join(popu, by = c("cdistrict", "age", "sex", "rel")) %>%
    replace_na(list(fer = 0, pop = 0)) %>%
    mutate(bir = pop * fer / 100) %>%
    group_by(cdistrict, rel) %>%
    summarize(bir = sum(bir)) %>%
    ungroup()

  # sum(bir_cr$bir)


  # births: rel changes (from mother to baby)
  bir_cr_new <- cha %>%
    filter(year == iyear) %>%
    select(-year) %>%
    right_join(bir_cr, by = "rel") %>%
    mutate(
      change = bir * cha / 100,
      keep = bir - change
    ) %>%
    select(cdistrict, rel, change, keep) %>%
    pivot_longer(
      cols = c("change", "keep"),
      names_to = "category", values_to = "bir"
    ) %>%
    mutate(new_rel = case_when(
      category == "keep" ~ rel,
      rel == uni_r[1] ~ uni_r[2],
      TRUE ~ uni_r[1]
    )) %>%
    select(cdistrict, new_rel, bir) %>%
    rename(rel = new_rel) %>%
    group_by(cdistrict, rel) %>%
    summarize(bir = sum(bir)) %>%
    ungroup()

  # sum(bir_cr$bir)
  # sum(bir_cr_new$bir)


  # births: with variable 'sex'

  # why age -1? still perspective at the begin of the year
  # the newborn babies are one year younger than the population at age zero
  # aging of the population is executed at the very end of the code

  pro_male_value <- filter(pro_male, year == iyear)

  bir <- bir_cr_new %>%
    mutate(
      pro_male = pro_male_value$pro_male,
      male = bir * pro_male / 100,
      female = bir - male
    ) %>%
    select(-c(bir, pro_male)) %>%
    pivot_longer(
      cols = uni_s,
      names_to = "sex", values_to = "bir"
    ) %>%
    mutate(
      age = -1,
      sex = factor(sex, levels = uni_s)
    ) %>%
    select(cdistrict, age, sex, rel, bir) %>%
    arrange(cdistrict, sex, rel)

  # sum(bir_cr_new$bir)
  # sum(bir$bir)

  # deaths (mortality rate * population)
  dea <- mor %>%
    filter(year == iyear) %>%
    right_join(popu, by = c("age", "sex")) %>%
    replace_na(list(mor = 0, pop = 0)) %>%
    mutate(dea = pop * mor / 100) %>%
    select(cdistrict, age, sex, rel, dea)

  # sum(dea$dea)

  # immigration*
  ims <- popu %>%
    group_by(cdistrict) %>%
    summarize(pop = sum(pop)) %>%
    ungroup() %>%
    left_join(filter(ims_rate, year == iyear), by = "cdistrict") %>%
    mutate(ims_c = pop * rate / 100) %>%
    # here: keep the year in order to join the right year below
    select(-c(pop, rate)) %>%
    # until here: immigration* by cdistrict (9 rows)
    left_join(ims_prop_sr, by = c("cdistrict", "year")) %>%
    mutate(ims_csr = ims_c * prop / 100) %>%
    select(-c(ims_c, prop)) %>%
    # until here: immigration* by cdistrict, sex, rel (9*2*2 = 36 rows)
    left_join(ims_prop_a, by = c("cdistrict", "year", "sex", "rel")) %>%
    mutate(ims = ims_csr * prop / 100) %>%
    select(cdistrict, age, sex, rel, ims)
  # result: immigration* by district, age, sex, rel (9*121*2*2 = 4356 rows)


  # sum(ims$ims)

  # emigration*
  ems <- popu %>%
    group_by(cdistrict) %>%
    summarize(pop = sum(pop)) %>%
    ungroup() %>%
    left_join(filter(ems_rate, year == iyear), by = "cdistrict") %>%
    mutate(ems_c = pop * rate / 100) %>%
    # here: keep the year in order to join the right year below
    select(-c(pop, rate)) %>%
    # until here: emigration* by cdistrict (9 rows)
    left_join(ems_prop_sr, by = c("cdistrict", "year")) %>%
    mutate(ems_csr = ems_c * prop / 100) %>%
    select(-c(ems_c, prop)) %>%
    # until here: emigration* by cdistrict, sex, rel (9*2*2 = 36 rows)
    left_join(ems_prop_a, by = c("cdistrict", "year", "sex", "rel")) %>%
    mutate(ems = ems_csr * prop / 100) %>%
    select(cdistrict, age, sex, rel, ems)
  # result: emigration* by district, age, sex, rel (9*121*2*2 = 4356 rows)

  # sum(ems$ems)

  # conversions

  # first, splitted into reformed and other
  # WHY? easier in the process balance

  # conversions: reformed (no more reformed = negative value for this group)
  con_reformed <- popu %>%
    filter(rel == uni_r[1]) %>%
    group_by(cdistrict, age, sex, rel) %>%
    summarize(pop = sum(pop)) %>%
    ungroup() %>%
    left_join(filter(con_rate, year == iyear),
      by = c("cdistrict", "age", "sex", "rel")
    ) %>%
    mutate(con = pop * rate_con / 100 * (-1)) %>%
    select(cdistrict, age, sex, rel, con)

  # sum(con_reformed$con)

  # conversions: other (no more reformed = positive value for this group)
  con_other <- con_reformed %>%
    mutate(
      rel = uni_r[2],
      con_new = abs(con)
    ) %>%
    select(-con) %>%
    rename(con = con_new)

  # sum(con_other$con)

  con <- con_reformed %>%
    bind_rows(con_other) %>%
    arrange(cdistrict, age, sex, rel)

  # sum(con$con)



  # combine the processes: end of year population, aging

  dem <- as_tibble(expand_grid(
    cdistrict = uni_c,
    age = (-1):age_max,
    sex = uni_s,
    rel = uni_r
  )) %>%
    left_join(popu, by = c("cdistrict", "age", "sex", "rel")) %>%
    left_join(bir, by = c("cdistrict", "age", "sex", "rel")) %>%
    left_join(dea, by = c("cdistrict", "age", "sex", "rel")) %>%
    left_join(ims, by = c("cdistrict", "age", "sex", "rel")) %>%
    left_join(ems, by = c("cdistrict", "age", "sex", "rel")) %>%
    left_join(con, by = c("cdistrict", "age", "sex", "rel")) %>%
    replace_na(list(pop = 0, bir = 0, dea = 0, ims = 0, ems = 0, con = 0)) %>%
    # population cannot be negative
    mutate(
      pop_end = pmax(0, pop + bir - dea + ims - ems + con),
      age_new = age + 1
    ) %>%
    select(cdistrict, age_new, sex, rel, pop, bir, dea, ims, ems, con, pop_end) %>%
    rename(age = age_new)


  # sum(dem$pop)
  # sum(dem$pop_end)

  # calculate proportion 'reformed' (constant value for age above 'prop_thres_age')
  prop_ref_thres <- dem %>%
    select(cdistrict, age, sex, rel, pop_end) %>%
    mutate(age_new = if_else(age >= prop_thres_age, prop_thres_age, age)) %>%
    group_by(cdistrict, age_new, sex, rel) %>%
    summarize(pop_end = sum_NA(pop_end)) %>%
    ungroup() %>%
    pivot_wider(names_from = rel, values_from = pop_end) %>%
    mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop)) %>%
    select(cdistrict, age_new, sex, prop_ref)

  # all age years, and smoothing over age (by c, s)
  prop_ref <- as_tibble(expand_grid(
    cdistrict = uni_c,
    age = as.double(age_min:age_max),
    sex = uni_s
  )) %>%
    mutate(age_new = if_else(age >= prop_thres_age, prop_thres_age, age)) %>%
    left_join(prop_ref_thres, by = c("cdistrict", "age_new", "sex")) %>%
    select(-age_new) %>%
    arrange(cdistrict, sex, age) %>%
    group_by(cdistrict, sex) %>%
    mutate(prop_fit = pmax(0, predict(
      loess(prop_ref ~ age, span = dem_span, degree = 1, na.action = na.aggregate)
    ))) %>%
    ungroup()

  # new population (based on population scenarios and proportion 'reformed')
  pop_end_year <- sce %>%
    filter(year == iyear) %>%
    left_join(prop_ref, by = c("cdistrict", "age", "sex")) %>%
    mutate(
      reformed = sce * prop_fit / 100,
      other = sce - reformed
    ) %>%
    pivot_longer(cols = uni_r, names_to = "rel_text", values_to = "pop") %>%
    mutate(rel = factor(rel_text, levels = uni_r)) %>%
    select(cdistrict, year, age, sex, rel, pop)

  # sum(dem$pop)
  # sum(dem$pop_end)

  # dem %>%
  #     select(pop, bir, dea, ims, ems, con, pop_end) %>%
  #     summarize_all(list(sum))

  # outputs: birth (separate, since no 'age' variable), with variable 'year'
  out_bir <- bir %>%
    mutate(year = iyear) %>%
    select(cdistrict, year, sex, rel, bir) %>%
    bind_rows(out_bir)

  # outputs: demographic processes and population (before scaling to popscen)
  out_dem <- dem %>%
    mutate(year = iyear) %>%
    select(cdistrict, year, age, sex, rel, bir, dea, ims, ems, con, pop_end) %>%
    rename(pop_unscaled = pop_end) %>%
    bind_rows(out_dem)

  # outputs: proportion 'reformed' (before and after smoothing)
  out_ref <- prop_ref %>%
    mutate(year = iyear) %>%
    select(cdistrict, year, age, sex, prop_ref, prop_fit) %>%
    arrange(cdistrict, year, age, sex) %>%
    bind_rows(out_ref)

  # outputs: end of year population (after scaline to population scenarios)
  out_pop <- pop_end_year %>%
    bind_rows(out_pop)

  # end of loop over years
}


# plots -------------------------------------------------------------------


# check smoothing over age (proportion reformed) in the model

# plot prediction for selected years
sel_years <- uniy_scen[(uniy_scen %% 10) == 0]

sel_lev <- c("initial", "smoothed")

sel_dat <- out_ref %>%
  filter(year %in% sel_years) %>%
  pivot_longer(c(prop_ref, prop_fit), names_to = "category", values_to = "prop") %>%
  mutate(cat = factor(if_else(category == "prop_ref",
    sel_lev[1], sel_lev[2]
  ), levels = sel_lev))


# plot
sszplot(sel_dat,
  aes_x = "age", aes_y = "prop", aes_col = "cat",
  grid = c("sex", "year"),
  labs_y = "proportion reformed (in %)",
  name = "6000_proportion_reformed_before-after-fit",
  width = 10, height = 6,
  multi = uni_c
)



# scen by y

# comment: is popscen without Witikon

pop_y_past <- pop %>%
  group_by(year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

out_pop %>%
  group_by(year) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pop_y_past) %>%
  sszplot(
    aes_x = "year", aes_y = "pop",
    i_x = scen_begin,
    labs_y = "population", name = "6001_popscen_y",
    quotes = quote(expand_limits(y = 0)),
    width = 7, height = 5
  )


# scen by yr

pop_yr_past <- pop %>%
  group_by(year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

dat_yr <- out_pop %>%
  group_by(year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pop_yr_past)
# why not piped to the plot?
# data is used to calculate the proportion 'reformed'

sszplot(dat_yr,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population", name = "6002_popscen_yr",
  quotes = quote(expand_limits(y = 0)),
  width = 7, height = 5
)

# proportion 'reformed' by yr
dat_yr %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "prop_ref", i_x = scen_begin,
    labs_y = "proportion reformed (in %)", name = "6003_propref_y",
    quotes = quote(expand_limits(y = 0)),
    width = 7, height = 5
  )


# scen by ya3r

pop_ya3r_past <- pop %>%
  left_join(look_a3, by = "age") %>%
  group_by(year, age_3, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

dat_ya3r <- out_pop %>%
  left_join(look_a3, by = "age") %>%
  group_by(year, age_3, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pop_ya3r_past)

sszplot(dat_ya3r,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "age_3", ncol = 4,
  name = "6004_popscen_yar",
  quotes = quote(expand_limits(y = 0)),
  width = 14, height = 8
)

sszplot(dat_ya3r,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "age_3", ncol = 4,
  gridscale = "free_y",
  name = "6005_popscen_yar_free",
  quotes = quote(expand_limits(y = 0)),
  width = 14, height = 8
)

# proportion 'reformed' by yar
dat_ya3r %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "prop_ref",
    i_x = scen_begin,
    labs_y = "proportion reformed (in %)",
    wrap = "age_3", ncol = 4,
    name = "6006_propref_ya",
    quotes = quote(expand_limits(y = 0)),
    width = 14, height = 8
  )



# scen by cyr

pop_cyr_past <- pop %>%
  group_by(cdistrict, year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

dat_cyr <- out_pop %>%
  group_by(cdistrict, year, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pop_cyr_past)

sszplot(dat_cyr,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "cdistrict", ncol = 3,
  name = "6007_popscen_cyr",
  quotes = quote(expand_limits(y = 0)),
  width = 11, height = 8
)

# proportion 'reformed' by cyr
dat_cyr %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "prop_ref",
    i_x = scen_begin,
    labs_y = "proportion reformed (in %)",
    wrap = "cdistrict", ncol = 3,
    name = "6008_propref_cyr",
    quotes = quote(expand_limits(y = 0)),
    width = 11, height = 8
  )




# scen by cya3r
pop_cya3r_past <- pop %>%
  left_join(look_a3, by = "age") %>%
  group_by(cdistrict, year, age_3, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup()

dat_cya3r <- out_pop %>%
  left_join(look_a3, by = "age") %>%
  group_by(cdistrict, year, age_3, rel) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  bind_rows(pop_cya3r_past)

sszplot(dat_cya3r,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "age_3", ncol = 4,
  name = "6009_popscen_cyar",
  quotes = quote(expand_limits(y = 0)),
  width = 14, height = 8,
  multi = uni_c
)

sszplot(dat_cya3r,
  aes_x = "year", aes_y = "pop", aes_col = "rel",
  i_x = scen_begin,
  labs_y = "population",
  wrap = "age_3", ncol = 4,
  gridscale = "free_y",
  name = "6010_popscen_cyar_free",
  quotes = quote(expand_limits(y = 0)),
  width = 14, height = 8,
  multi = uni_c
)

# proportion 'reformed' by yar
dat_cya3r %>%
  pivot_wider(names_from = "rel", values_from = pop) %>%
  mutate(prop_ref = round(reformed / (reformed + other) * 100, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "prop_ref",
    i_x = scen_begin,
    labs_y = "proportion reformed (in %)",
    wrap = "age_3", ncol = 4,
    name = "6011_propref_cya",
    quotes = quote(expand_limits(y = 0)),
    width = 14, height = 8,
    multi = uni_c
  )


# conversions (from 'reformed' to 'other')
con_y_past <- read_excel(paste0(data_path, "/input/Kon.xlsx")) %>%
  rename(year = EreignisDatJahr, con = AnzKonvWir) %>%
  filter(RelBisher == 1) %>%
  group_by(year) %>%
  summarize(con = sum(con)) %>%
  ungroup()

dat_con_y <- out_dem %>%
  filter(rel == uni_r[1]) %>%
  select(year, con) %>%
  group_by(year) %>%
  summarize(con = sum(abs(con))) %>%
  ungroup() %>%
  bind_rows(con_y_past)

sszplot(dat_con_y,
  aes_x = "year", aes_y = "con",
  i_x = scen_begin,
  labs_y = "conversions per year", name = "6030_con_y",
  quotes = quote(expand_limits(y = 0)),
  width = 7, height = 5
)

# conversion rate (from 'reformed' to 'other')
dat_yr %>%
  filter(rel == uni_r[1]) %>%
  left_join(dat_con_y, by = "year") %>%
  mutate(con_rate = round(con / pop * 100, round_prop)) %>%
  sszplot(
    aes_x = "year", aes_y = "con_rate",
    i_x = scen_begin,
    labs_y = "conversion rate (in % per year)", name = "6031_con-rate_y",
    quotes = quote(expand_limits(y = 0)),
    width = 7, height = 5
  )



# export the results ------------------------------------------------------

# population (end of year)
out_pop %>%
  arrange(cdistrict, year, age, sex, rel) %>%
  write_csv(paste0(out_path, "/population_future.csv"))

# births
out_bir %>%
  arrange(cdistrict, year, sex, rel) %>%
  write_csv(paste0(out_path, "/births_future.csv"))

# demographic processes
out_dem %>%
  select(cdistrict, year, age, sex, rel, bir, dea, ims, ems, con) %>%
  arrange(cdistrict, year, age, sex, rel) %>%
  write_csv(paste0(out_path, "/demographic-processes_future.csv"))
