
# Wrap some of the data cleaning codes for the second project
raw_data <- Les_diffe_rents_accents_du_franc_ais_Asians_Whites_October_26_2022_08_24

data <- raw_data[-1, ]

data <- data %>% 
  drop_na(`1A_1`)

# Rename the columns
data <- data %>%
  select(-c(IPAddress, Finished, ResponseId, QID18_1, QID18_2, QID18_3,
            QID18_5, QID18_6, QID18_7, QID18_8, QID18_9, QID18_10, QID19, `10B`, Courriel,
            `1B`, `2B`, `3B`, `4B`, `5B`, `6B`, `7B`, `8B`, `9B`, `10B`)) %>% 
  rename(gender = Q1,
         gender_other = Q1_4_TEXT, 
         age = Q2,
         country_of_birth = Q3,
         grow_up_province = Q41,
         grow_up_city = Q4,
         field_of_study = Q5,
         ethnic_group = Q6,
         ethnic_group_other = Q6_13_TEXT, 
         num_of_language = Q7...123, 
         spoken_language = Q7...124, 
         spoken_language_other = Q7_11_TEXT,
         mother_tongue = Q8,
         mother_tongue_other = Q8_11_TEXT,
         year_of_french = Q9,
         french_speaking_place = Q10,
         french_speaking_place_other = Q10_6_TEXT,
         month_live_in_fr_env = Q10B,
         pref_french = Q11,
         pref_french_other = Q11_3_TEXT,
         teacher_origin = Q12, 
         teacher_origin_other = Q12_3_TEXT
  ) %>% 
  mutate(age = as.numeric(age),
         num_of_language = as.numeric(num_of_language),
         year_of_french = as.numeric(year_of_french),
  )

# convert rating 
data <- data %>%  
  mutate_at(vars(1:90), as.numeric)

data <- data %>% 
  mutate_if(is.character, as.factor)

# categorize the country of birth column 
data <- data %>%  
  mutate(continent_of_birth = case_when(country_of_birth %in% c('China',
                                                       'India',
                                                       'South Korea',
                                                       'Japan',
                                                       'Pakistan',
                                                       'Philippines') ~ 'Asia',
                               country_of_birth %in% c('Mauritius', 'Egypt', 'Nigeria') ~ 'Africa',
                               country_of_birth %in% c(
                                 'United Kingdom of Great Britain and Northern Ireland',
                                 'Czech Republic',
                                 'Italy',
                                 'Germany'
                               ) ~ 'Europe',
                               country_of_birth %in% c('United States of America', 'Brazil', 'Mexico') ~ 'America',
                               country_of_birth == 'Canada' ~ 'Canada'))


# Grow up in Canada or not 
data <- data %>%  
  mutate(grew_up_canada = ifelse(str_detect(grow_up_country, "Canada|canada"), TRUE, FALSE), )



# Append the language number pyramid 
data <- data %>%
  mutate(
    year_interval = case_when(
      year_of_french <= 5 ~ '0-5',
      year_of_french > 5 &
        year_of_french <= 10 ~ '5-10',
      year_of_french > 10 &
        year_of_french <= 15 ~ '10-15',
      year_of_french > 15 ~ '15+'
    )
  )
















