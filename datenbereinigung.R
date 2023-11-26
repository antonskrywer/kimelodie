library(tidyverse)


# import data -------------------------------------------------------------


df_csv <- read.csv2("rdata_kimelodiefortsetzung_2023-07-27_22-25.csv", sep = "\t")


# transform data with pivot_longer(); drop NAs ---------------

df_longer <- df_csv |> 
  select(1, starts_with("F"), starts_with("SD")) |> 
  pivot_longer(
    cols = starts_with("F"),
    names_to = c("question", "rating_item"),
    names_sep = "_",
    values_to = "rating",
    values_drop_na = T
  ) |> 
  relocate(2:4, .after = "rating")

# Add variables "composer" and "probe_position" --------------------


df_longer <- df_longer |> 
  mutate(composer = if_else(str_starts(question, "F1") | str_starts(question, "F3"), "ai", "hum")) |> 
  mutate(probe_position = if_else(str_starts(question, "F1") | str_starts(question, "F2"), "1", "2")) |> 
  relocate(probe_position, composer, .before = 3) |> 
  filter(question != "FINISHED") |>
  mutate(gender = if_else(str_starts(SD05, "1"), "f", "m")) %>% 
  mutate(musical_identity = factor(SD07, levels = 1:3, labels = c("<6", "6-10", ">10"))) |> 
           rename(age = SD06_01) |> 
           select(-SD05, -SD07)
           
# Seperate variables with pivot_wider() --------------------------


df_longer <- df_longer |> 
  pivot_wider(
    names_from = rating_item,
    values_from = rating
  ) |> 
  rename("participant" = 01,
         "überzeugend" = "01",
         "logisch und sinnvoll" = "02",
         "interessant" = "03",
         "gefällt mir" = "04") |> 
    relocate(8:11, .after = composer)

# Add variable "composer_specific"  -----------------------------------


df_final <- df_longer |> 
  mutate(composer_specified = case_when(
    between(question, "F101", "F125") | between(question, "F301", "F336") ~ "gpt",
    between(question, "F126", "F150") | between(question, "F337", "F361") ~ "magenta",
    TRUE ~ "hum"
  )) |> 
  relocate(12, .after = 4) |> 
  filter(!is.na(musical_identity))

df_final <- df_final |> 
  relocate(11:12, .after = composer_specified)

# List and function to label items ---------------------------------


item_names <- list(
  '01' = "überzeugend",
  '02' = "logisch und sinnvoll",
  '03' = "interessant",
  '04' = "gefällt mir"
)

item_labeller <- function(variable, value){
  return(item_names[value])
}

# separate df for each compser -----------------------------------------------

df_gpt <- df_final |> 
  filter(composer_specified == "gpt")

df_magenta <- df_final |> 
  filter(composer_specified == "magenta")

df_hum <- df_final |> 
  filter(composer_specified == "hum")

df_ai <- df_final |> 
  filter(composer == "ai")

# Export df_final ---------------------------------------------------------


write.csv(df_final, "df_kimelodiefortsetzung_n71_neu.csv", sep = ",")
