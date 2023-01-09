
# Wrap some of the data cleaning codes for the second project


# Clean the socio-demographic columns 
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


# Born in Canada or not 
data <- data %>%  
  mutate(born_in_canada = ifelse(str_detect(country_of_birth, "Canada|canada"), TRUE, FALSE) )


# Country of birth - by continent
data <- data %>%  
  mutate(continent = case_when(country_of_birth %in% c('China', 'India', 'South Korea', 'Japan', 'Pakistan', 'Philippines') ~ 'Asia', 
                               country_of_birth %in% c('Mauritius', 'Egypt', 'Nigeria') ~ 'Africa',
                               country_of_birth %in% c('United Kingdom of Great Britain and Northern Ireland', 'Czech Republic', 'Italy', 'Germany') ~ 'Europe',
                               country_of_birth %in% c('United States of America', 'Brazil', 'Mexico') ~ 'America', 
                               country_of_birth == 'Canada' ~ 'Canada')) 


# Append the years of studying French pyramid 
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




# ethnicity of participant
data <- data %>% 
  mutate(
    ethnic_group_cate = case_when(
      ethnic_group %in% c('Latino', 'Latino-Américain', 'Noir') ~ 'Latio or Black',
      ethnic_group == 'White' ~ 'White',
      ethnic_group == 'Other. Specify:' &
        ethnic_group_other == 'Taïwanaise' ~ 'Asian',
      ethnic_group == 'Other. Specify:' &
        ethnic_group_other == 'Slavic (like Russian, Ukranian)	' ~ 'White',
      ethnic_group == 'Other. Specify:' &
        ethnic_group_other %notin% c('Taïwanaise', 'Slavic (like Russian, Ukranian') ~ 'Mixed',
      TRUE ~ 'Asian',
    )
  ) %>%  
  mutate(ethnic_group_Asian_or_not = case_when(ethnic_group_cate == 'Asian' ~ 'Pure Asian', 
                                               ethnic_group_other %in% c('Mix between Korean and Canadian.', 
                                                                         'Metis: White and Chinese.', 
                                                                         'White and Chinese', 
                                                                         'White, Korean and Caribbean', 
                                                                         'Black and Southeast Asian', 
                                                                         'White/Chinese', 
                                                                         'Blanc, Japonais') ~ 'Mixed Asian', 
                                               TRUE ~ 'Non-Asian')) 



# months of living in French-speaking place  
levels(data$month_live_in_fr_env) <- c("1 - 3", "4 - 6", "7 - 12", "< 1", "> 12")
data$month_live_in_fr_env <- factor(data$month_live_in_fr_env, levels = c("< 1", "1 - 3", "4 - 6", "7 - 12", "> 12")) 


# year of speaking French
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


# Live in French-speaking place 

data <- data %>%
  mutate(
    french_speaking_place_cate =  case_when((
      french_speaking_place == 'France' |
        french_speaking_place == 'France, Elsewhere? if so please specify:'
    ) ~ 'France',
    (
      french_speaking_place == 'Quebec' |
        french_speaking_place == 'Quebec, Elsewhere? if so please specify:'
    ) ~ 'Quebec',
    str_detect(
      french_speaking_place,
      'I have never stayed in a French-speaking context.'
    ) ~ 'never',
    french_speaking_place == "Elsewhere? if so please specify:" ~
      "other",
    french_speaking_place %in% c('Quebec and France', 'Quebec, France') ~ 'both'
    )
  ) 


# Sum up the score 
data <- data %>%  
  mutate(
    rec_1 = rowSums(.[1:9]),
    rec_2 = rowSums(.[10:18]),
    rec_3 = rowSums(.[19:27]),
    rec_4 = rowSums(.[28:36]),
    rec_5 = rowSums(.[37:45]),
    rec_6 = rowSums(.[46:54]),
    rec_7 = rowSums(.[55:63]),
    rec_8 = rowSums(.[64:72]),
    rec_9 = rowSums(.[73:81]),
    rec_10 = rowSums(.[82:90])
  ) %>%  
  mutate(id = 1:nrow(data)) %>% 
  relocate(id, .before = rec_1) 



# convert to long format
data_agg_long <- data %>%  
  select(c(120:130)) %>%  
  pivot_longer(-c(id), values_to = "score", names_to = "rec_index") %>% 
  left_join(data %>% select(- c(1:90, 121:130)), by = 'id') %>% 
  mutate(speaker_race = ifelse(rec_index %in% c('rec_1', 'rec_4', 'rec_7', 'rec_8', 'rec_9'), 'white', 'asian'), 
         speaker_accent = case_when(rec_index %in% c('rec_1', 'rec_6') ~ 'Quebec', 
                                    rec_index %in% c('rec_2', 'rec_9') ~ 'European',
                                    rec_index %in% c('rec_3', 'rec_8') ~ 'Acadian',
                                    rec_index %in% c('rec_4', 'rec_10') ~ 'L2',
                                    rec_index %in% c('rec_5', 'rec_7') ~ 'African'))




# helper function: calculate mean by factor
# factor_mean <- function(df, col_name){
#   group_var <- enquo(col_name)
#   res <- df %>% 
#     group_by(speaker_race, !!group_var) %>% 
#     summarise(mean_val = round(mean(score, na.rm = TRUE), 2))
#   return(res)
# }

factor_mean <- function(df, col_name){
  group_var <- col_name
  res <- df %>% 
    drop_na(!!sym(group_var)) %>%  
    group_by(speaker_race, !!sym(group_var)) %>%  
    summarise(mean_val = round(mean(score, na.rm = TRUE), 2))
  return(res)
}


factor_ts_test <- function(df, col_name) {
  group_var <- col_name
  res <- df %>%
    drop_na(!!sym(group_var)) %>%
    group_by(!!sym(group_var)) %>%
    wilcox_test(score ~ speaker_race)
  return(res)
}







