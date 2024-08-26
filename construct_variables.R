pacman::p_load(tidyverse, arrow)

horse_race_df <- read_parquet("data/dev/horse_race_info_cleaned_all.parquet")
place_df <- read.csv("data/dev/place_name.csv")

horse_count_df <- horse_race_df %>% 
  group_by(race_date, race_id, place) %>% 
  summarise(horse_count = n()) %>% 
  ungroup()

horse_race_df2 <- horse_race_df %>% 
  mutate(
    course_type_en = case_when(
      course_type == "ダ" ~ "dirt",
      course_type == "芝" ~ "grass"
    ),
    course_dir_en = case_when(
      course_dir == "右" ~ "right",
      course_dir == "左" ~ "left"
    ),
    weather_en = case_when(
      weather == "曇" ~ "cloudy",
      weather == "雨" ~ "rainy",
      weather == "晴" ~ "sunny",
      weather == "小雨" ~ "little_rainy",
      weather == "雪" ~ "snowy",
      weather == "小雪" ~ "little_snowy"
    ),
    dirt_status_en = case_when(
      dirt_status == "重" ~ "heavy",
      dirt_status == "稍重" ~ "little_heavy",
      dirt_status == "不良" ~ "bad",
      dirt_status == "良" ~ "good"
    ),
    order_top_1 = if_else(arrival_order <= 1, 1, 0),
    order_top_2 = if_else(arrival_order <= 2, 1, 0),
    order_top_3 = if_else(arrival_order <= 3, 1, 0)
  ) %>%
  left_join(place_df, by = c("place" = "place_id"))%>%
  left_join(horse_count_df, by = c("place", "race_date", "race_id"))



write_parquet(horse_race_df2, "data/dev/horse_race_info_cleaned_all2.parquet")
