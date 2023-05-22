# introduction ####
# R code for the paper Age reporting for the oldest old in the Brazilian COVID-19
# vaccination database: what can we learn from it?

# To avoid scientific notation on data frames
options(scipen = 100)

# load libraries ####
library(DemoTools)

library(systemfonts)

library(styler)
library(here)
library(glue)
library(janitor)

library(bigreadr)
library(rio)

library(lubridate)
library(dtplyr)
library(vroom)
library(tidyverse)
library(purrr)

library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

# factors ####
## agegrp_levels ####
agegrp_levels <-
  as_factor(
    c(
      "0", "1-4", "0-4", "5-9", "10-14", "15-19",
      "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
      "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
      "80-84", "85-89", "90-94", "95-99", "100-104", "105-109",
      "80+", "85+", "90+", "95+", "100+"
    )
  ) %>%
  forcats::fct_na_value_to_level()

## brasil_regions_levels_eng ####
brasil_regions_levels_eng <-
  as_factor(
    c("Brazil", "North", "Northeast", "Midwest", "Southeast", "South")
  ) %>%
  forcats::fct_na_value_to_level()

## brasil_state_codes_names ####
brasil_state_names <-
  vroom::vroom(
    here("data-raw", "br-state-codes.csv"),
    .name_repair = "universal"
  ) %>%
  filter(is.na(extinction)) %>%
  select(state_code = idIBGE, state = subdivision, state_name = name, region) %>%
  mutate(
    region_name =
      factor(
        case_when(
          region == "NE" ~ "Northeast",
          region == "N" ~ "North",
          region == "CO" ~ "Midwest",
          region == "SE" ~ "Southeast",
          region == "S" ~ "South",
          TRUE ~ NA_character_
        ),
        levels = brasil_regions_levels_eng
      )
  )

## brasil_region_names ####
brasil_region_names <-
  brasil_state_names %>%
  select(region, region_name) %>%
  group_by(across(.cols = everything())) %>%
  slice_head() %>%
  ungroup() %>%
  bind_rows(
    tibble(region = "BR", region_name = "Brazil")
  ) %>%
  mutate(region_name = factor(region_name, levels = brasil_regions_levels_eng))

# functions ####
# function to estimate age heaping for vaccination data
age_heaping_check_vaccinated <- function(df) {
  residence_region <- quo(residence_region)
  year <- 2021
  regions <- c("BR", "N", "NE", "CO", "SE", "S")

  df_result <- regions %>%
    map_dfr(~ {
      temp_df <- df %>% filter(!!residence_region == .x)

      myers <- DemoTools::check_heaping_myers(temp_df$vaccination_total,
        temp_df$age_calculated,
        ageMin = 80, ageMax = 99, details = FALSE
      )
      spoorenberg <- DemoTools::check_heaping_spoorenberg(temp_df$vaccination_total,
        temp_df$age_calculated,
        ageMin = 80, ageMax = 99
      )
      bachi <- DemoTools::check_heaping_bachi(temp_df$vaccination_total,
        temp_df$age_calculated,
        ageMin = 80, ageMax = 99
      )
      tibble::tibble(
        region = .x,
        year = year,
        index = c("myers", "spoorenberg", "bachi"),
        value = c(myers, spoorenberg, bachi)
      )
    })

  df_result
}

# to estimate age heaping for IPUMS Census data
age_heaping_check_ipums <- function(df) {
  df_result <-
    df %>%
    select(residence_region, year, population_total, age_calculated) %>%
    group_by(residence_region, year) %>%
    summarise(
      myers = DemoTools::check_heaping_myers(population_total,
        age_calculated,
        ageMin = 80,
        ageMax = 99,
        details = FALSE
      ),
      spoorenberg = DemoTools::check_heaping_spoorenberg(population_total,
        age_calculated,
        ageMin = 80,
        ageMax = 99
      ),
      bachi = DemoTools::check_heaping_bachi(population_total,
        age_calculated,
        ageMin = 80,
        ageMax = 99
      )
    ) %>%
    pivot_longer(cols = c("myers", "spoorenberg", "bachi"), names_to = "index", values_to = "value") %>%
    ungroup()

  df_result
}


# first read of vaccination database - 387,750,333 records
vaccination_first_dose_80_plus_read <- function(df) {
  janitor::clean_names(df) %>%
    select(
      -sistema_origem,
      -vacina_lote, -vacina_fabricante_referencia,
      -estalecimento_no_fantasia, -estabelecimento_valor,
      -estabelecimento_razao_social,
      -all_of(c(contains("_grupo_atendimento")))
    ) %>%
    dtplyr::lazy_dt(.) %>%
    dplyr::rename(
      date_birth = paciente_data_nascimento,
      age = paciente_idade,
      sex = paciente_enum_sexo_biologico,
      race_code = paciente_raca_cor_codigo,
      race = paciente_raca_cor_valor,
      residence_city_code = paciente_endereco_co_ibge_municipio,
      residence_country_code = paciente_endereco_co_pais,
      residence_city = paciente_endereco_nm_municipio,
      residence_country = paciente_endereco_nm_pais,
      residence_state = paciente_endereco_uf,
      residence_zip_code = paciente_endereco_cep,
      nationality = paciente_nacionalidade_enum_nacionalidade,
      vaccination_city_code = estabelecimento_municipio_codigo,
      vaccination_city = estabelecimento_municipio_nome,
      vaccination_state = estabelecimento_uf,
      vaccination_category_code = vacina_categoria_codigo,
      vaccination_category = vacina_categoria_nome,
      vaccine_developer = vacina_fabricante_nome,
      vaccination_date = vacina_data_aplicacao,
      vaccination_dose = vacina_descricao_dose,
      vaccine_code = vacina_codigo,
      vaccine_name = vacina_nome
    ) %>%
    mutate(
      vaccination_dose = case_when(
        str_detect(vaccination_dose, "1ª") ~ "first dose",
        str_detect(vaccination_dose, "Inicial") ~ "first dose",
        str_detect(vaccination_dose, "2ª") ~ "second dose",
        str_detect(vaccination_dose, "3ª") ~ "third dose",
        str_detect(vaccination_dose, "4ª") ~ "fourth dose",
        str_detect(vaccination_dose, "Única") ~ "single dose",
        str_detect(vaccination_dose, "Reforço") ~ "booster",
        str_detect(vaccination_dose, "Adicional") ~ "booster",
        TRUE ~ "first dose" # for "Dose" which does not indicate which - if second, will be deduplicated later on
      ),
      date_birth = as_date(date_birth),
      vaccination_date = as_date(vaccination_date),
      age_calculated = trunc(interval(date_birth, date4) / years(1))
    ) %>%
    # only first dose - let's filter at the beginning of the pipe
    filter(vaccination_dose %in% c("first dose", "single dose")) %>%
    # only age above or equal to 80
    filter(age_calculated >= 80) %>%
    dplyr::collect()
}

# 0 dates ####
date0 <- lubridate::ymd("2020-01-01") # begin of 2020
date1 <- lubridate::ymd("2020-07-01") # projection date 2020
date2 <- lubridate::ymd("2020-07-04") # closet CONASS date for excess deaths - 27th epiweek
date3 <- lubridate::ymd("2021-01-01") # begin of 2021
date4 <- lubridate::ymd("2021-07-01") # projection date 2021
date5 <- lubridate::ymd("2021-07-03") # closet CONASS date for excess deaths - 26th epiweek
date6 <- lubridate::ymd("2022-01-01") # begin of 2022
date7 <- lubridate::ymd("2022-07-01") # projection date 2022

# interpolation_factor <- years(interval(date3, date5)) / years(interval(date3, date6))

# 2020
pro_rata_mort_2020 <- years(interval(date0, date1)) / years(interval(date0, date2))

# 2021
pro_rata_mort_2021 <- years(interval(date3, date4)) / years(interval(date3, date5))

# 1 excess mortality from CONASS ####
## 1.1 conass_excess_mortality_2020 ####
conass_excess_mortality_2020 <-
  rio::import(here("data-raw", "conass-excess-mortality-2020.csv")) %>%
  pivot_longer(cols = South:Brazil, names_to = "state", values_to = "over_deaths") %>%
  # estimate proportion of deaths until date1
  group_by(state) %>%
  summarise(
    year = 2020,
    over_deaths_1 = sum(over_deaths[which(epiweek <= lubridate::isoweek(date2))]),
    over_deaths_2 = sum(over_deaths[which(epiweek > lubridate::isoweek(date2))]),
    prop_over_deaths = (over_deaths_1 * pro_rata_mort_2020) / (over_deaths_1 + over_deaths_2)
  ) %>%
  ungroup() %>%
  rename(state_name = state)

## 1.2 conass_excess_mortality_2021 ####
conass_excess_mortality_2021 <-
  rio::import(here("data-raw", "conass-excess-mortality-2021.csv")) %>%
  pivot_longer(cols = South:Brazil, names_to = "state", values_to = "over_deaths") %>%
  # estimate proportion of deaths until date4
  group_by(state) %>%
  summarise(
    year = 2021,
    over_deaths_1 = sum(over_deaths[which(epiweek <= lubridate::isoweek(date5))]),
    over_deaths_2 = sum(over_deaths[which(epiweek > lubridate::isoweek(date5))]),
    prop_over_deaths = (over_deaths_1 * pro_rata_mort_2021) / (over_deaths_1 + over_deaths_2)
  ) %>%
  ungroup() %>%
  rename(state_name = state)

## 1.3 conass_excess_mortality_2020_2021 ####
conass_excess_mortality_2020_2021 <-
  bind_rows(
    conass_excess_mortality_2020,
    conass_excess_mortality_2021
  )

## 1.4 conass_overmortality ####
conass_overmortality <-
  rio::import(here("data-raw", "conass-overmortality-2020-2021.csv")) %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  left_join(brasil_region_names, by = c("state" = "region")) %>%
  select(-state) %>%
  rename(state_name = region_name) %>%
  arrange(state_name, year, sex)

# 2 IBGE population projections ####
## 2.0 ibge_2020_2021_pop_initial ####
ibge_2020_2021_pop_initial <-
  rio::import(here("data-raw", "ibge-pop-source.csv")) %>%
  tibble::as_tibble() %>%
  rename(
    state = area_id,
    year = ref_year,
    sex = sexid,
    age_groups = agegrp,
    population_total = pop
  ) %>%
  filter(sex != "total" & year %in% c(2020, 2021)) %>%
  arrange(state, sex, age_groups, agegrpstart, year) %>%
  drop_na() %>%
  # only great regions
  filter(!state %in% c(brasil_state_names$state)) %>%
  mutate(
    state_name =
      factor(
        case_when(
          state == "Nordeste" ~ "Northeast",
          state == "Norte" ~ "North",
          state == "Centro-Oeste" ~ "Midwest",
          state == "Sudeste" ~ "Southeast",
          state == "Sul" ~ "South",
          state == "Brasil" ~ "Brazil",
          TRUE ~ NA_character_
        ),
        levels = brasil_regions_levels_eng
      ),
    state =
      case_when(
        state_name == "Northeast" ~ "NE",
        state_name == "North" ~ "N",
        state_name == "Midwest" ~ "CO",
        state_name == "Southeast" ~ "SE",
        state_name == "South" ~ "S",
        state_name == "Brazil" ~ "BR",
        TRUE ~ NA_character_
      )
  ) %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 2.1 ibge_2020_2021_lifetable_initial ####
ibge_2020_2021_lifetable_initial <-
  rio::import(here("data-raw", "ibge-lifetable-source.csv")) %>%
  tibble::as_tibble() %>%
  rename(
    state = area_id,
    year = ref_year,
    sex = sexid
  ) %>%
  filter(sex != "total" & year %in% c(2020, 2021)) %>%
  arrange(state, sex, agegrpstart, year) %>%
  drop_na() %>%
  # great regions
  filter(!state %in% c(brasil_state_names$state)) %>%
  mutate(
    state_name =
      factor(
        case_when(
          state == "Nordeste" ~ "Northeast",
          state == "Norte" ~ "North",
          state == "Centro-Oeste" ~ "Midwest",
          state == "Sudeste" ~ "Southeast",
          state == "Sul" ~ "South",
          state == "Brasil" ~ "Brazil",
          TRUE ~ NA_character_
        ),
        levels = brasil_regions_levels_eng
      ),
    state =
      case_when(
        state_name == "Northeast" ~ "NE",
        state_name == "North" ~ "N",
        state_name == "Midwest" ~ "CO",
        state_name == "Southeast" ~ "SE",
        state_name == "South" ~ "S",
        state_name == "Brazil" ~ "BR",
        TRUE ~ NA_character_
      )
  )

## 2.2 ibge_2021_lifetable_pivot ####
ibge_2021_lifetable_pivot <-
  ibge_2020_2021_lifetable_initial %>%
  filter(year == 2021) %>%
  select(state, state_name, year, sex, agegrpstart, ex) %>%
  filter(agegrpstart >= 50 & agegrpstart <= 80) %>%
  pivot_wider(
    values_from = ex,
    names_from = agegrpstart,
    names_glue = "ex_{agegrpstart}"
  )

## 2.3 ibge deaths and covid-19 overmortality ####
## 2.3.1 ibge_2020_2021_deaths #####
## estimate excess deaths between 2020/07/01 and 2021/07/01
## these excess deaths will be subtracted from the 2021 projection
ibge_2020_2021_deaths <-
  ibge_2020_2021_pop_initial %>%
  filter(agegrpstart >= 80) %>% # filter age 80+
  left_join(ibge_2020_2021_lifetable_initial) %>%
  select(state, year:nmx) %>%
  left_join(conass_overmortality) %>%
  left_join(conass_excess_mortality_2020_2021) %>%
  select(-c(over_deaths_1, over_deaths_2)) %>%
  arrange(state, year, sex, agegrpstart) %>%
  # nDx is expected deaths with overmortality
  # nDx_over is overmortality
  # 2020 second semester, 2021 first semester
  mutate(
    nDx =
      case_when(
        year == 2020 ~ population_total * nmx * (1 + overmortality_conass) * (1 - prop_over_deaths),
        year == 2021 ~ population_total * nmx * (1 + overmortality_conass) * prop_over_deaths,
        TRUE ~ NA_real_
      ),
    nDx_over =
      case_when(
        year == 2020 ~ population_total * nmx * (overmortality_conass) * (1 - prop_over_deaths),
        year == 2021 ~ population_total * nmx * (overmortality_conass) * prop_over_deaths,
        TRUE ~ NA_real_
      )
  ) %>%
  select(-population_total, -nmx, -overmortality_conass, -prop_over_deaths)

## 2.3.2 ibge_2020_2021_deaths_summary #####
ibge_2020_2021_deaths_summary <-
  ibge_2020_2021_deaths %>%
  group_by(state, state_name, year, sex) %>%
  summarise(
    nDx = sum(nDx),
    nDx_over = sum(nDx_over),
    age_groups = factor("80+", levels = agegrp_levels)
  ) %>%
  ungroup() %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 2.3.3 ibge_2020_2021_deaths_over #####
ibge_2020_2021_deaths_over <-
  ibge_2020_2021_deaths %>%
  group_by(state, state_name, sex, agegrpstart, age_groups) %>%
  summarise(
    nDx_over = sum(nDx_over),
    year = decimal_date(date4)
  ) %>%
  ungroup() %>%
  bind_rows(
    group_by(., state, state_name, sex, year) %>%
      summarise(nDx_over = sum(nDx_over)) %>%
      ungroup() %>%
      mutate(
        age_groups = factor("80+", levels = agegrp_levels),
        agegrpstart = 80
      ) %>%
      drop_na()
  ) %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels)) %>%
  arrange(state, sex, year, age_groups)

## 2.4 ibge_2021_population ####
## ibge 2021 population corrected for covid-19 overmortality
ibge_2021_population <-
  ibge_2020_2021_pop_initial %>%
  filter(agegrpstart >= 80 & year == 2021) %>%
  left_join(ibge_2020_2021_deaths_over,
    by = c(
      "state", "state_name", "sex",
      "age_groups", "agegrpstart"
    )
  ) %>%
  rename(year = year.x) %>%
  mutate(
    population = population_total - nDx_over
  ) %>%
  select(-population_total, -nDx_over, -year.y) %>%
  bind_rows(
    filter(., age_groups %in% c("80-84", "85-89", "90+")) %>%
      group_by(state, state_name, year, sex) %>%
      summarise(population = sum(population)) %>%
      mutate(
        age_groups = factor("80+", levels = agegrp_levels),
        agegrpstart = 80
      ) %>%
      ungroup()
  ) %>%
  arrange(state, year, sex, age_groups)

# 3 UN projections - World Population Prospects (WPP) ####
## 3.0 wpp_2020_2021_pop_initial ####
wpp_2020_2021_pop_initial <-
  # data from UN WPP 2019 revision
  rio::import(here("data-raw", "wpp-pop-source.csv")) %>%
  tibble::as_tibble() %>%
  select(-c(area_id, sexid, agegrpspan)) %>%
  rename(
    year = ref_year,
    country = area_iso3,
    country_name = area_name,
    age_groups = agegrp,
    population_total = pop
  ) %>%
  filter(year %in% c(2020, 2021)) %>%
  arrange(country, country_name, sex, age_groups, agegrpstart, year) %>%
  mutate(
    age_groups = factor(age_groups, levels = agegrp_levels),
    state = "BR", state_name = "Brazil",
    sex = stringr::str_to_lower(sex),
    population_total = population_total * 1000 # WPP is in thousands
  ) %>%
  filter(sex != "total")

## 3.1 wpp_2020_2025_lifetable_initial ####
wpp_2020_2025_lifetable_initial <-
  # life table from UN WPP 2019 revision
  rio::import(here("data-raw", "wpp-life-table-source.csv")) %>%
  tibble::as_tibble() %>%
  select(-c(area_id, sexid, agegrpspan, nax)) %>%
  rename(
    year = ref_year,
    country = area_iso3,
    country_name = area_name,
    age_groups = agegrp
  ) %>%
  mutate(
    state = "BR", state_name = "Brazil",
    sex = stringr::str_to_lower(sex)
  ) %>%
  filter(sex != "total") %>%
  arrange(country, sex, agegrpstart, year)

## 3.2 wpp_2020_2025_lifetable_pivot ####
wpp_2020_2025_lifetable_pivot <-
  wpp_2020_2025_lifetable_initial %>%
  select(year, sex, agegrpstart, ex, country, country_name, state, state_name) %>%
  filter(agegrpstart >= 50 & agegrpstart <= 80) %>%
  pivot_wider(
    values_from = ex,
    names_from = agegrpstart,
    names_glue = "ex_{agegrpstart}"
  )

## 3.3 wpp deaths and covid-19 overmortality ####
## 3.3.1 wpp_2020_2021_deaths #####
wpp_2020_2021_deaths <-
  wpp_2020_2021_pop_initial %>%
  filter(agegrpstart >= 80) %>% # filter age 80+
  left_join(wpp_2020_2025_lifetable_initial, by = c("country", "sex", "age_groups", "agegrpstart")) %>%
  select(year = year.x, sex:population_total, country, nmx, country_name = country_name.y) %>%
  left_join(conass_overmortality, by = c("year", "sex", "country_name" = "state_name")) %>%
  left_join(conass_excess_mortality_2020_2021, by = c("year", "country_name" = "state_name")) %>%
  select(-c(over_deaths_1, over_deaths_2)) %>%
  arrange(country_name, year, sex, agegrpstart) %>%
  # nDx is expected deaths with overmortality
  # nDx_over is overmortality
  # 2020 second semester, 2021 first semester
  mutate(
    nDx =
      case_when(
        year == 2020 ~ population_total * nmx * (1 + overmortality_conass) * (1 - prop_over_deaths),
        year == 2021 ~ population_total * nmx * (1 + overmortality_conass) * prop_over_deaths,
        TRUE ~ NA_real_
      ),
    nDx_over =
      case_when(
        year == 2020 ~ population_total * nmx * (overmortality_conass) * (1 - prop_over_deaths),
        year == 2021 ~ population_total * nmx * (overmortality_conass) * prop_over_deaths,
        TRUE ~ NA_real_
      )
  ) %>%
  select(-population_total, -nmx, -overmortality_conass, -prop_over_deaths)

## 3.3.2 wpp_2020_2021_deaths_summary #####
wpp_2020_2021_deaths_summary <-
  wpp_2020_2021_deaths %>%
  group_by(country, country_name, year, sex) %>%
  summarise(
    nDx = sum(nDx),
    nDx_over = sum(nDx_over),
    age_groups = factor("80+", levels = agegrp_levels)
  ) %>%
  ungroup() %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 3.3.3 wpp_2020_2021_deaths_over #####
wpp_2020_2021_deaths_over <-
  wpp_2020_2021_deaths %>%
  group_by(
    country, country_name, sex, agegrpstart, age_groups
  ) %>%
  summarise(
    nDx_over = sum(nDx_over),
    year = decimal_date(date4)
  ) %>%
  ungroup() %>%
  bind_rows(
    filter(., age_groups %in% c("80-84", "85-89", "90-94", "95-99", "100+")) %>%
      group_by(
        country, country_name, sex, year
      ) %>%
      summarise(
        nDx_over = sum(nDx_over),
        year = decimal_date(date4)
      ) %>%
      ungroup() %>%
      mutate(
        age_groups = factor("80+", levels = agegrp_levels),
        agegrpstart = 80
      ) %>%
      drop_na()
  ) %>%
  bind_rows(
    filter(., age_groups %in% c("90-94", "95-99", "100+")) %>%
      group_by(
        country, country_name, sex, year
      ) %>%
      summarise(
        nDx_over = sum(nDx_over),
        year = decimal_date(date4)
      ) %>%
      ungroup() %>%
      mutate(
        age_groups = factor("90+", levels = agegrp_levels),
        agegrpstart = 90
      ) %>%
      drop_na()
  ) %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels)) %>%
  arrange(
    country, country_name, sex, year, age_groups
  )

## 3.4. wpp_2021_population ####
## wpp population corrected for covid-19 overmortality
wpp_2021_population <-
  wpp_2020_2021_pop_initial %>%
  filter(agegrpstart >= 80 & year == 2021) %>%
  left_join(wpp_2020_2021_deaths_over,
    by = c("country", "country_name", "sex", "age_groups", "agegrpstart")
  ) %>%
  rename(year = year.x) %>%
  mutate(
    population = population_total - nDx_over
  ) %>%
  select(-population_total, -nDx_over, -year.y) %>%
  bind_rows(
    filter(., age_groups %in% c("80-84", "85-89", "90-94", "95-99", "100+")) %>%
      group_by(year, country, country_name, state, state_name, sex) %>%
      summarise(population = sum(population)) %>%
      ungroup() %>%
      mutate(
        age_groups = factor("80+", levels = agegrp_levels),
        agegrpstart = 80
      ) %>%
      drop_na()
  ) %>%
  bind_rows(
    filter(., age_groups %in% c("90-94", "95-99", "100+")) %>%
      group_by(year, country, country_name, state, state_name, sex) %>%
      summarise(population = sum(population)) %>%
      ungroup() %>%
      mutate(
        age_groups = factor("90+", levels = agegrp_levels),
        agegrpstart = 90
      ) %>%
      drop_na()
  ) %>%
  mutate(
    age_groups = factor(age_groups, levels = agegrp_levels)
  ) %>%
  arrange(state, year, sex, age_groups)

## 3.5 wpp_2021_100_90_80_brasil ####
wpp_2021_100_90_80_brasil <-
  wpp_2021_population %>%
  filter(age_groups %in% c("80+", "90+", "100+")) %>%
  arrange(state_name, country, country_name, year, sex, age_groups) %>%
  group_by(state_name, country, country_name, year, sex) %>%
  mutate(
    population_80_plus = first(population),
    population_90_plus = nth(population, 2),
    population_100_plus = nth(population, 3),
    population_90_80 = population_90_plus / population_80_plus,
    population_100_90 = population_100_plus / population_90_plus
  ) %>%
  ungroup() %>%
  select(-age_groups, -agegrpstart, -population) %>%
  group_by(across()) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(wpp_2020_2025_lifetable_pivot, by = c("country", "sex", "country_name", "state", "state_name")) %>%
  select(everything(), year = year.x, -year.y)

# 4 IPUMS census data - Brazil 1960 to 2010 ####
## 4.1 br_censuses_ipums_1960_2010_initial ####
br_censuses_ipums_1960_2010_initial <-
  vroom::vroom(
    here("data-raw", "ipums-br-1960-2010-pop-80-plus.csv"),
    .name_repair = "universal",
    escape_backslash = TRUE
  ) %>%
  dplyr::rename(
    age_calculated = age
  ) %>%
  pivot_longer(N:CO, names_to = "residence_region", values_to = "population_total") %>%
  mutate(
    residence_region_name =
      factor(
        case_when(
          residence_region == "NE" ~ "Northeast",
          residence_region == "N" ~ "North",
          residence_region == "CO" ~ "Midwest",
          residence_region == "SE" ~ "Southeast",
          residence_region == "S" ~ "South",
          residence_region == "BR" ~ "Brazil",
          TRUE ~ NA_character_
        ),
        levels = brasil_regions_levels_eng
      )
  ) %>%
  relocate(year, residence_region, residence_region_name, age_calculated, sex, population_total) %>%
  arrange(year, residence_region, age_calculated, sex)

## 4.2 br_censuses_ipums_1960_2010_br ####
br_censuses_ipums_1960_2010_br <-
  br_censuses_ipums_1960_2010_initial %>%
  group_by(year, age_calculated, sex) %>%
  summarise(
    population_total = sum(population_total, na.rm = TRUE),
    residence_region = "BR",
    residence_region_name = factor("Brazil", levels = brasil_regions_levels_eng)
  ) %>%
  ungroup()

## 4.3 br_censuses_ipums_2010_sex_ratio ####
br_censuses_ipums_1960_2010_sex_ratio <-
  br_censuses_ipums_1960_2010_initial %>%
  bind_rows(br_censuses_ipums_1960_2010_br) %>%
  group_by(year, residence_region_name, age_calculated) %>%
  mutate(sex_ratio = lead(population_total) / population_total) %>%
  ungroup() %>%
  select(-sex, -population_total) %>%
  drop_na()

## 4.4 br_censuses_ipums_1960_2010_age_heaping ####
br_censuses_ipums_1960_2010_age_heaping <-
  br_censuses_ipums_1960_2010_initial %>%
  bind_rows(br_censuses_ipums_1960_2010_br) %>%
  group_by(year, residence_region, age_calculated) %>%
  summarise(population_total = sum(population_total))

## 4.5 br_censuses_ipums_1960_2010_age_groups ####
br_censuses_ipums_1960_2010_age_groups <-
  bind_rows(
    br_censuses_ipums_1960_2010_initial %>%
      mutate(
        age_groups = factor(
          case_when(
            age_calculated >= 80 ~ "80+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(
        year, residence_region, residence_region_name, age_groups
      ) %>%
      summarise(population_total = sum(population_total, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  bind_rows(
    br_censuses_ipums_1960_2010_initial %>%
      mutate(
        age_groups = factor(
          case_when(
            age_calculated >= 80 & age_calculated < 85 ~ "80-84",
            age_calculated >= 85 & age_calculated < 90 ~ "85-89",
            age_calculated >= 90 ~ "90+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(
        year, residence_region, residence_region_name, age_groups
      ) %>%
      summarise(population_total = sum(population_total, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  bind_rows(
    br_censuses_ipums_1960_2010_initial %>%
      mutate(
        age_groups = factor(
          case_when(
            age_calculated >= 90 & age_calculated < 95 ~ "90-94",
            age_calculated >= 95 & age_calculated < 100 ~ "95-99",
            age_calculated >= 100 ~ "100+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(
        year, residence_region, residence_region_name, age_groups
      ) %>%
      summarise(population_total = sum(population_total, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  drop_na() %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 4.6 br_censuses_ipums_1960_2010_100_90_80 ####
br_censuses_ipums_1960_2010_100_90_80 <-
  br_censuses_ipums_1960_2010_age_groups %>%
  filter(age_groups %in% c("80+", "90+", "100+")) %>%
  arrange(year, residence_region, residence_region_name, age_groups) %>%
  group_by(year, residence_region, residence_region_name) %>%
  mutate(
    population_80_plus = first(population_total),
    population_90_plus = nth(population_total, 2),
    population_100_plus = nth(population_total, 3),
    population_90_80 = population_90_plus / population_80_plus,
    population_100_90 = population_100_plus / population_90_plus,
    population_100_80 = population_100_plus / population_80_plus,
  ) %>%
  ungroup() %>%
  select(-age_groups, -population_total) %>%
  group_by(across()) %>%
  slice_head() %>%
  ungroup()

# 5 OpenDataSUS vaccination data ####
## 5.0 vaccination_first_dose_80_plus_initial ####
## check the total number of lines of input file
bigreadr::nlines(here("data-raw", "opendatasus-br-vaccination-2022-03-14.csv"))

vaccination_first_dose_80_plus_initial <-
  bigreadr::big_fread1(
    here("data-raw", "opendatasus-br-vaccination-2022-03-14.csv"), 10e6,
    .transform = vaccination_first_dose_80_plus_read, skip = 0
  ) %>%
  as_tibble() %>%
  mutate(
    date_birth = as_date(date_birth),
    vaccination_date = as_date(vaccination_date)
  ) %>%
  write_rds(here("data-treated", "vaccination_first_dose_80_plus_initial.Rds"))

## 5.1 vaccination_first_dose_80_plus_treated ####
## to treat vaccination database already filtered for first doses and 80+
vaccination_first_dose_80_plus_treated <-
  vaccination_first_dose_80_plus_initial %>%
  mutate(
    vaccine_name = case_when(
      str_detect(str_to_lower(vaccine_name), "astrazeneca") ~ "Oxford-AstraZeneca",
      str_detect(str_to_lower(vaccine_name), "covishield") ~ "Oxford-AstraZeneca",
      str_detect(str_to_lower(vaccine_name), "janssen") ~ "Johnson & Johnson",
      str_detect(str_to_lower(vaccine_name), "butantan") ~ "CoronaVac",
      str_detect(str_to_lower(vaccine_name), "pfizer") ~ "Pfizer–BioNTech",
      TRUE ~ NA_character_
    ),
    sex_name = case_when(
      sex == "M" ~ "male",
      sex == "F" ~ "female",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(brasil_state_names, by = c("residence_state" = "state")) %>%
  rename(
    residence_state_name = state_name, residence_state_code = state_code,
    residence_region_name = region_name, residence_region = region
  ) %>%
  left_join(brasil_state_names, by = c("vaccination_state" = "state")) %>%
  rename(
    vaccination_state_name = state_name, vaccination_state_code = state_code,
    vaccination_region_name = region_name, vaccination_region = region
  ) %>%
  arrange(residence_state_name, vaccination_date, age_calculated, sex, vaccination_dose) %>%
  mutate(
    across(all_of(c("vaccination_city", "residence_city")), ~ str_remove_all(all_of(.), "\\[0]")),
    across(all_of("race"), ~ str_to_lower(.)),
    across(all_of(c("residence_city", "vaccination_city", "vaccine_developer")), ~ str_to_title(.)),
    across(all_of(c("residence_country", "vaccination_category")), ~ str_to_sentence(.))
  ) %>%
  relocate(
    residence_state_code, residence_state, residence_state_name,
    residence_region, residence_region_name,
    vaccination_date, age, date_birth, age_calculated,
    sex, sex_name, race_code, race
  ) %>%
  write_rds(here("data-treated", "vaccination_first_dose_80_plus_treated.Rds"))

## 5.1 vaccination_multiple_records ####
# find pacient_ids with multiple records
vaccination_multiple_records <-
  vaccination_first_dose_80_plus_treated %>%
  janitor::get_dupes(paciente_id) %>%
  write_rds(here("data-treated", "vaccination_multiple_records.Rds"))

## 5.2 vaccination_multiple_records_earliest ####
# for each paciente_id multiple record, select the record with the earliest vaccination date
vaccination_multiple_records_earliest <-
  vaccination_multiple_records %>%
  group_by(paciente_id) %>%
  slice_min(vaccination_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  write_rds(here("data-treated", "vaccination_multiple_records_earliest.Rds"))

## 5.3 vaccination_first_dose_80_plus_cleaned ####
# "deduplicate" vaccination_first_dose_80_plus_treated
vaccination_first_dose_80_plus_cleaned <-
  vaccination_first_dose_80_plus_treated %>%
  anti_join(vaccination_multiple_records_earliest, by = c("paciente_id" = "paciente_id")) %>%
  bind_rows(
    vaccination_multiple_records_earliest %>% select(-dupe_count)
  ) %>%
  write_rds(here("data-treated", "vaccination_first_dose_80_plus_cleaned.Rds"))

## 5.4 vaccination_first_dose_80_plus_filtered ####
# filter for our study criteria
vaccination_first_dose_80_plus_filtered <-
  vaccination_first_dose_80_plus_cleaned %>%
  # alocate state and region of vaccination to state and region of residence when missing
  mutate(
    residence_state =
      case_when(
        is.na(residence_state) ~ vaccination_state,
        residence_state == "XX" ~ vaccination_state,
        residence_state == "" ~ vaccination_state,
        TRUE ~ residence_state
      ),
    residence_state_name =
      case_when(
        is.na(residence_state_name) ~ vaccination_state_name,
        TRUE ~ residence_state_name
      ),
    residence_state_code =
      case_when(
        is.na(residence_state_code) ~ vaccination_state_code,
        TRUE ~ residence_state_code
      ),
    residence_region =
      case_when(
        is.na(residence_region) ~ vaccination_region,
        TRUE ~ residence_region
      ),
    residence_region_name =
      case_when(
        is.na(residence_region_name) ~ vaccination_region_name,
        TRUE ~ residence_region_name
      )
  ) %>%
  # not missing sex
  filter(!is.na(sex_name)) %>%
  # remove date of birth 1899-12-30 & vaccination_category_code == 9 (Trabalhadores de saúde)
  # remove date of birth 1899-12-30 & vaccination_category_code is missing
  # 1899-12-30 is probably system default for data not entered
  filter(!(date_birth == as_date("1899-12-30") & is.na(vaccination_category_code))) %>%
  filter(!(date_birth == as_date("1899-12-30") & vaccination_category_code == 9)) %>%
  # only after the official beginning of vaccination campaign 2021-01-18
  filter(vaccination_date >= as_date("2021-01-18")) %>%
  write_rds(here("data-treated", "vaccination_first_dose_80_plus_filtered.Rds"))

## 5.5 vaccination_filtered_how_many ####
vaccination_filtered_how_many_sex_name <-
  vaccination_first_dose_80_plus_cleaned %>%
  filter(
    is.na(sex_name)
  )

vaccination_filtered_how_many_1899_NA <-
  vaccination_first_dose_80_plus_cleaned %>%
  filter((date_birth == as_date("1899-12-30") & is.na(vaccination_category_code)))

vaccination_filtered_how_many_1899_9 <-
  vaccination_first_dose_80_plus_cleaned %>%
  filter((date_birth == as_date("1899-12-30") & vaccination_category_code == 9))

vaccination_filtered_how_many_vaccin_date <-
  vaccination_first_dose_80_plus_cleaned %>%
  filter(
    !vaccination_date >= as_date("2021-01-18")
  )

## 5.6 vaccination_by_day ####
vaccination_by_day <-
  bind_rows(
    vaccination_first_dose_80_plus_filtered %>%
      mutate(
        residence_region = "BR",
        residence_region_name = "Brazil"
      ) %>%
      group_by(
        residence_region, residence_region_name, vaccination_date, sex_name
      ) %>%
      tally(name = "vaccination_total") %>%
      ungroup()
  ) %>%
  bind_rows(
    vaccination_first_dose_80_plus_filtered %>%
      group_by(
        residence_region, residence_region_name, vaccination_date, sex_name
      ) %>%
      tally(name = "vaccination_total") %>%
      ungroup()
  ) %>%
  group_by(
    residence_region, residence_region_name, sex_name
  ) %>%
  mutate(
    vaccination_accumulated = cumsum(vaccination_total),
    vaccination_growth_rate = (vaccination_total * 100) / lag(vaccination_accumulated)
  ) %>%
  ungroup() %>%
  mutate(residence_region_name = factor(residence_region_name, levels = brasil_regions_levels_eng))

## 5.7 vaccination_single_age ####
vaccination_single_age <-
  bind_rows(
    vaccination_first_dose_80_plus_filtered %>%
      mutate(
        residence_region = "BR",
        residence_region_name = "Brazil"
      ) %>%
      group_by(
        residence_region, residence_region_name, age_calculated, sex_name
      ) %>%
      tally(name = "vaccination_total") %>%
      ungroup() %>%
      filter(age_calculated < 110)
  ) %>%
  bind_rows(
    vaccination_first_dose_80_plus_filtered %>%
      mutate(
        residence_region = "BR",
        residence_region_name = "Brazil"
      ) %>%
      group_by(
        residence_region, residence_region_name, age_calculated, sex_name
      ) %>%
      tally(name = "vaccination_total") %>%
      ungroup() %>%
      group_by(
        residence_region, residence_region_name, sex_name
      ) %>%
      summarise(vaccination_total = sum(vaccination_total[age_calculated >= 110])) %>%
      mutate(age_calculated = 110) %>%
      ungroup()
  ) %>%
  bind_rows(
    vaccination_first_dose_80_plus_filtered %>%
      group_by(
        residence_region, residence_region_name, age_calculated, sex_name
      ) %>%
      tally(name = "vaccination_total") %>%
      ungroup() %>%
      filter(age_calculated < 110)
  ) %>%
  bind_rows(
    vaccination_first_dose_80_plus_filtered %>%
      group_by(
        residence_region, residence_region_name, age_calculated, sex_name
      ) %>%
      tally(name = "vaccination_total") %>%
      ungroup() %>%
      group_by(
        residence_region, residence_region_name, sex_name
      ) %>%
      summarise(vaccination_total = sum(vaccination_total[age_calculated >= 110])) %>%
      mutate(age_calculated = 110) %>%
      ungroup()
  ) %>%
  # proportion within state, sex
  group_by(residence_region, residence_region_name, sex_name) %>%
  mutate(vaccination_proportion = vaccination_total / sum(vaccination_total)) %>%
  ungroup() %>%
  arrange(residence_region, residence_region_name, age_calculated, sex_name) %>%
  mutate(residence_region_name = factor(residence_region_name, levels = brasil_regions_levels_eng))

## 5.8 vaccination_single_age_sex_ratio ####
vaccination_single_age_sex_ratio <-
  vaccination_single_age %>%
  group_by(residence_region, residence_region_name, age_calculated) %>%
  mutate(
    sex_ratio = lead(vaccination_total) / vaccination_total,
    vaccination_total = sum(vaccination_total)
  ) %>%
  ungroup() %>%
  select(-sex_name, -vaccination_proportion) %>%
  drop_na()

## 5.9 vaccination_single_age_px ####
vaccination_single_age_px <-
  vaccination_single_age %>%
  arrange(residence_region, residence_region_name, sex_name, age_calculated) %>%
  group_by(residence_region, residence_region_name, sex_name) %>%
  mutate(
    dif_age = lead(age_calculated) - age_calculated,
    px = case_when(
      dif_age == 1 ~ lead(vaccination_total) / vaccination_total,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  select(-dif_age, -vaccination_proportion) %>%
  drop_na()

## 5.10 vaccination_single_age_heaping ####
vaccination_single_age_heaping <-
  vaccination_single_age %>%
  group_by(residence_region, age_calculated) %>%
  summarise(vaccination_total = sum(vaccination_total))

## 5.11 vaccination_age_groups ####
vaccination_age_groups <-
  bind_rows(
    vaccination_single_age %>%
      mutate(
        age_groups = factor(
          case_when(
            age_calculated >= 80 ~ "80+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(
        residence_region, residence_region_name, age_groups, sex_name
      ) %>%
      summarise(vaccination_total = sum(vaccination_total)) %>%
      ungroup()
  ) %>%
  bind_rows(
    vaccination_single_age %>%
      mutate(
        age_groups = factor(
          case_when(
            age_calculated >= 80 & age_calculated < 85 ~ "80-84",
            age_calculated >= 85 & age_calculated < 90 ~ "85-89",
            age_calculated >= 90 ~ "90+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(
        residence_region, residence_region_name, age_groups, sex_name
      ) %>%
      summarise(vaccination_total = sum(vaccination_total)) %>%
      ungroup()
  ) %>%
  bind_rows(
    vaccination_single_age %>%
      mutate(
        age_groups = factor(
          case_when(
            age_calculated >= 90 & age_calculated < 95 ~ "90-94",
            age_calculated >= 95 & age_calculated < 100 ~ "95-99",
            age_calculated >= 100 ~ "100+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(
        residence_region, residence_region_name, age_groups, sex_name
      ) %>%
      summarise(vaccination_total = sum(vaccination_total)) %>%
      ungroup()
  ) %>%
  drop_na() %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 5.12 vaccination_population_age_groups ####
vaccination_population_age_groups <-
  vaccination_age_groups %>%
  # join with ibge projection for 2021
  left_join(ibge_2021_population,
    by = c(
      "residence_region" = "state", "residence_region_name" = "state_name",
      "age_groups" = "age_groups", "sex_name" = "sex"
    )
  ) %>%
  mutate(
    year = decimal_date(date4),
    vaccination_accu_ratio = vaccination_total / population,
    agegrpstart = case_when(
      age_groups %in% c("80-84", "85-89", "80+") ~ 80,
      age_groups %in% c("90-94", "95-99", "90+") ~ 90,
      age_groups == "100+" ~ 100,
      TRUE ~ agegrpstart
    )
  )

## 5.13 vaccination_100_90_80 ####
vaccination_100_90_80 <-
  vaccination_population_age_groups %>%
  filter(age_groups %in% c("80+", "90+", "100+")) %>%
  select(-vaccination_accu_ratio) %>%
  arrange(residence_region, residence_region_name, sex_name, age_groups) %>%
  group_by(residence_region, residence_region_name, sex_name, year) %>%
  mutate(
    population_80_plus = first(population),
    population_90_plus = nth(population, 2),
    population_90_80 = population_90_plus / population_80_plus,
    vaccination_80_plus = first(vaccination_total),
    vaccination_90_plus = nth(vaccination_total, 2),
    vaccination_100_plus = nth(vaccination_total, 3),
    vaccination_90_80 = vaccination_90_plus / vaccination_80_plus,
    vaccination_100_90 = vaccination_100_plus / vaccination_90_plus
  ) %>%
  ungroup() %>%
  select(-age_groups, -agegrpstart, -vaccination_total, -population) %>%
  group_by(across()) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(ibge_2021_lifetable_pivot,
    by = c(
      "residence_region" = "state",
      "residence_region_name" = "state_name",
      "sex_name" = "sex"
    )
  ) %>%
  select(everything(), year = year.x, -year.y) %>%
  mutate(residence_region_name = factor(residence_region_name, levels = brasil_regions_levels_eng))

# 6 HMD data for Sweden ####
## 6.0 hmd_country_codes ####
hmd_country_codes <-
  vroom::vroom(
    here("data-raw", "hmd-country-codes.csv"),
    .name_repair = "universal"
  )

## 6.1 hmd_population ####
## 6.1.1 hmd_population_SWE_tible ####
hmd_population_SWE_tible_temp <-
  vroom::vroom(
    here("data-raw", "hmd-pop-swe.csv"),
    .name_repair = "universal",
    col_select = list(
      -Area, -AgeInterval, -Type, -Day, -Month, -Access, -LDB,
      -all_of(c(contains("NoteCode")))
    ),
    escape_backslash = TRUE
  ) %>%
  janitor::clean_names() %>%
  mutate(
    sex = case_when(
      sex == "m" ~ "male",
      sex == "f" ~ "female",
      TRUE ~ NA_character_
    )
  ) %>%
  # filter duplicated sources - favor most recent from Statistics Sweden
  filter(!(year == 1860 & ref_code %in% c(31, 32))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1870, 1880, 1890, 1900) & ref_code %in% c(32))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1910, 1920) & ref_code %in% c(33))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1930) & ref_code %in% c(34))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1935) & ref_code %in% c(35))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1940, 1945, 1950) & ref_code %in% c(36))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1960) & ref_code %in% c(37))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1965, 1966, 1967) & ref_code %in% c(38))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1970, 1971) & ref_code %in% c(40))) %>%
  # keeps ref_code 60
  filter(!(year >= 1972 & year <= 1989 & ref_code %in% c(40))) %>%
  # keeps ref_code 60
  filter(!(year %in% c(1990, 1991) & ref_code %in% c(39))) %>%
  # keeps ref_code 60
  rename(country = pop_name) %>%
  left_join(hmd_country_codes) %>%
  relocate(country, country_name) %>%
  arrange(country, year, age, sex) %>%
  type_convert() %>%
  drop_na()

hmd_population_SWE_tible <-
  hmd_population_SWE_tible_temp %>%
  filter(age >= 110) %>%
  group_by(country, country_name, year, sex) %>%
  summarise(
    population = sum(population, na.rm = TRUE),
    ref_code = last(ref_code)
  ) %>%
  mutate(age = 110) %>%
  ungroup() %>%
  bind_rows(hmd_population_SWE_tible_temp %>% filter(age < 110)) %>%
  arrange(country, year, age, sex)

## 6.2 hmd_population_sex_ratio ####
## 6.2.1 hmd_population_SWE_sex_ratio ####
hmd_population_SWE_sex_ratio <-
  hmd_population_SWE_tible %>%
  filter(year >= 1992 & year <= 2019) %>%
  filter(sex %in% c("male", "female")) %>%
  filter(age >= 80) %>%
  # filter age 80+
  group_by(year, age) %>%
  mutate(sex_ratio = lead(population) / population) %>%
  ungroup() %>%
  select(-sex, -population, -ref_code) %>%
  drop_na()

## 6.3 hmd_age_groups_SWE ####
hmd_age_groups_SWE_temp <-
  hmd_population_SWE_tible %>%
  filter(year >= 1992 & year <= 2019) %>%
  filter(age >= 80) %>%
  # filter age 80+
  select(-ref_code)

hmd_age_groups_SWE <-
  hmd_age_groups_SWE_temp %>%
  mutate(
    age_groups = factor(
      case_when(
        age >= 80 ~ "80+",
        TRUE ~ NA_character_
      ),
      levels = agegrp_levels
    )
  ) %>%
  group_by(country, country_name, year, age_groups, sex) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  bind_rows(
    hmd_age_groups_SWE_temp %>%
      mutate(
        age_groups = factor(
          case_when(
            age >= 80 & age < 85 ~ "80-84",
            age >= 85 & age < 90 ~ "85-89",
            age >= 90 ~ "90+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(country, country_name, year, age_groups, sex) %>%
      summarise(population = sum(population)) %>%
      ungroup()
  ) %>%
  bind_rows(
    hmd_age_groups_SWE_temp %>%
      mutate(
        age_groups = factor(
          case_when(
            age >= 90 & age < 95 ~ "90-94",
            age >= 95 & age < 100 ~ "95-99",
            age >= 100 ~ "100+",
            TRUE ~ NA_character_
          ),
          levels = agegrp_levels
        )
      ) %>%
      group_by(country, country_name, year, age_groups, sex) %>%
      summarise(population = sum(population)) %>%
      ungroup()
  ) %>%
  drop_na() %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 6.4 hmd_lifetable_initial ####
hmd_lifetable_initial <-
  rio::import(here("data-raw", "hmd-lt-source.csv")) %>%
  arrange(country, sex, age_groups, year) %>%
  filter(
    (country == "SWE" & year >= 1992 & year <= 2019)
  ) %>%
  mutate(age_groups = factor(age_groups, levels = agegrp_levels))

## 6.5 hmd_lifetable_pivot ####
hmd_lifetable_pivot <-
  hmd_lifetable_initial %>%
  filter(age_groups %in% c(
    "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80-84"
  )) %>%
  pivot_wider(
    values_from = ex,
    names_from = age_groups,
    names_glue = "ex_{age_groups}"
  ) %>%
  rename(
    "ex_50" = `ex_50-54`, "ex_55" = `ex_55-59`, "ex_60" = `ex_60-64`, "ex_65" = `ex_65-69`,
    "ex_70" = `ex_70-74`, "ex_75" = `ex_75-79`, "ex_80" = `ex_80-84`
  )

## 6.6 hmd_100_90_80_SWE ####
hmd_100_90_80_SWE <-
  hmd_age_groups_SWE %>%
  filter(age_groups %in% c("80+", "90+", "100+")) %>%
  arrange(country, country_name, year, sex, age_groups) %>%
  group_by(country, country_name, year, sex) %>%
  mutate(
    population_80_plus = first(population),
    population_90_plus = nth(population, 2),
    population_100_plus = nth(population, 3),
    population_90_80 = population_90_plus / population_80_plus,
    population_100_90 = population_100_plus / population_90_plus
  ) %>%
  ungroup() %>%
  select(-age_groups, -population) %>%
  drop_na() %>%
  group_by(across()) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(hmd_lifetable_pivot)

# Combine ####
## 7.0 hmd_ibge_vaccine_100_90_80 ####
hmd_ibge_vaccine_100_90_80 <-
  hmd_100_90_80_SWE %>%
  select(-population_100_90, -population_90_plus, -population_80_plus) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(
    vaccination_100_90_80 %>%
      rename(sex = sex_name, country_name = residence_region_name) %>%
      filter(country_name == "Brazil") %>% # only Brazil male and female
      select(
        -vaccination_100_90, -vaccination_90_80, -vaccination_90_plus, -population_90_plus,
        -vaccination_80_plus, -population_80_plus, -residence_region, -vaccination_100_plus
      ) %>%
      mutate(year = as.character(year)) %>%
      mutate(country = "IBGE")
  ) %>%
  bind_rows(
    vaccination_100_90_80 %>%
      rename(sex = sex_name, country_name = residence_region_name) %>%
      filter(country_name == "Brazil") %>% # only Brazil male and female
      select(
        -vaccination_100_90, -population_90_80, -vaccination_90_plus, -population_90_plus,
        -vaccination_80_plus, -population_80_plus, -residence_region
      ) %>%
      mutate(year = as.character(year)) %>%
      mutate(country = "Vaccinated") %>%
      rename(population_90_80 = vaccination_90_80, population_100_plus = vaccination_100_plus)
  )

## 7.1 hmd_ibge_vaccine_90_80_summary ####
hmd_ibge_vaccine_90_80_summary <-
  hmd_ibge_vaccine_100_90_80 %>%
  group_by(country_name, sex) %>%
  summarise(
    median_90_80 = median(population_90_80),
    mean_90_80 = mean(population_90_80),
    min_90_80 = min(population_90_80),
    max_90_80 = max(population_90_80),
    year_max = year[which.max(population_90_80)],
    year_min = year[which.min(population_90_80)]
  ) %>%
  ungroup()

# 7 Evaluate age heaping ####
## 7.1 age_heaping_results ####
age_heaping_results <- bind_rows(
  age_heaping_check_vaccinated(vaccination_single_age_heaping),
  age_heaping_check_ipums(br_censuses_ipums_1960_2010_age_heaping)
)

