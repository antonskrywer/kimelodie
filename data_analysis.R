<<<<<<< HEAD
library(tidyverse)
#library(emmeans)
#library(boot)
#library(rstatix)
#library(afex)
#library(broom)
library(effectsize)
#library(magrittr)
library(ggpubr)


# Load data ---------------------------------------------------------------

df_final <- read.csv("df_kimelodiefortsetzung_n71.csv", row.names = 1)

# Descriptives ---------------------------------------------------

n_musical_identity <- df_final |> 
  filter(CASE != "345") |> 
  group_by(SD07) |> 
  summarise(n = n())

n_gender <- df_final |> 
  group_by(SD05) |> 
  summarise(n = n())

# prepare data for MANOVA -----------------------------------------------------------------

data_manova <- df_final %>%
  group_by(participant, composer, probe_position) %>%
  summarise(überzeugend = mean(überzeugend),
            logisch.und.sinnvoll = mean(logisch.und.sinnvoll),
            interessant = mean(interessant),
            gefällt.mir = mean(gefällt.mir))
data_effectsize <- df_final %>%
  group_by(participant, composer) %>%
  summarise(überzeugend = mean(überzeugend),
            logisch.und.sinnvoll = mean(logisch.und.sinnvoll),
            interessant = mean(interessant),
            gefällt.mir = mean(gefällt.mir))

data_manova$musical_identity <-
  factor(
    pmap_vec(list(p = data_manova$participant,
                  c = data_manova$composer,
                  pp = data_manova$probe_position),
             function(p, c, pp) {
               df_final %>% filter(participant == p,
                                   composer == c,
                                   probe_position == pp) %$%
                 musical_identity %>%
                 .[[1]]
             }),
    levels = c("<6", "6-10", ">10")
  )




# Calculate MANOVA --------------------------------------------------------

stats_manova_model <- data_manova %>%
  drop_na() %>%
  manova(
    cbind(überzeugend, logisch.und.sinnvoll, interessant, gefällt.mir) ~
      factor(composer) * factor(probe_position) * musical_identity +
      Error(factor(participant)/(factor(composer) * factor(probe_position))),
    data = .
  )

summary(stats_manova_model)
summary_manova <- as.data.frame(summary(stats_manova_model))

# Calculate effectsizes ---------------------------------------------------

data_effsize_ai <- data_effectsize |> 
  filter(composer == "ai")
data_effsize_hum <- data_effectsize |> 
  filter(composer == "hum")

cohens_d(x = interessant ~ composer, data = data_effectsize, paired = TRUE)
cohens_d(x = logisch.und.sinnvoll ~ composer, data = data_effectsize, paired = TRUE)
cohens_d(x = gefällt.mir ~ composer, data = data_effectsize, paired = TRUE)
cohens_d(x = überzeugend ~ composer, data = data_effectsize, paired = TRUE)

# Mean and SD -----------------------------------------------------

df_final |> 
  mutate(composer = as.factor(composer)) |> 
  group_by(composer) |> 
  summarise(mean_überzeugend = mean(überzeugend),
            sd_überzeugend = sd(überzeugend),
            mean_interessant = mean(interessant),
            sd_interessant = sd(interessant))  

# errobar plot ----------------------------------------------------

my_data_long <- df_final |> 
  pivot_longer(cols = 6:9,
               names_to = "dependent_variable",
               values_to = "rating") |> 
  mutate(musical_identity = factor(musical_identity, levels = c("<6", "6-10", ">10"), labels = c("<6", "6-10", ">10"))) |> 
  drop_na()

musical_identity_order <- c("<6", "6-10", ">10")
my_data_long$musical_identity <- factor(my_data_long$musical_identity, levels = musical_identity_order)
my_data_long$musical_identity <- forcats::fct_relevel(my_data_long$musical_identity, "<6", ">10", after = 2)
my_data_long$composer_specified <- factor(my_data_long$composer_specified, levels = c("human", "gpt", "magenta"))

mean_ci_musical_identity <- my_data_long |> 
  group_by(dependent_variable, composer, musical_identity) |> 
  summarize(mean_rating = mean(rating),
            ci_low = mean_rating - qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()),
            ci_high = mean_rating + qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()))

mean_ci_composer <- my_data_long |> 
  group_by(dependent_variable, composer) |> 
  summarize(mean_rating = mean(rating),
            ci_low = mean_rating - qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()),
            ci_high = mean_rating + qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()))

mean_ci_composer$composer <- as.factor(mean_ci_composer$composer, levels = c("hum", "ai"))

mean_ci_specific <- my_data_long |> 
  group_by(dependent_variable, composer_specified, musical_identity) |> 
  summarize(mean_rating = mean(rating),
            ci_low = mean_rating - qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()),
            ci_high = mean_rating + qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()))

mean_ci_specific$composer_specified[is.na(mean_ci_specific$composer_specified)] <- "human"
  
labeller <- list("überzeugend" = "convincing",
             "logisch.und.sinnvoll" = "logical and meaningful",
             "interessant" = "interesting",
             "gefällt.mir" = "liking")

dep_var_labeller <- function(variable, value){
  return(labeller[value])
}

ggplot(mean_ci_specific, aes(x = composer_specified, y = mean_rating, group = musical_identity, color = musical_identity, shape = musical_identity)) +
  geom_point(size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.5, position = position_dodge(width = 0.9), size = 0.5) +
  facet_wrap(~dependent_variable, scales = "free_y", labeller = dep_var_labeller) +
  scale_color_manual(values = c("<6" = "#231f20", "6-10" = "#f47920", ">10" = "#e42485"), 
                     limits = c("<6", "6-10", ">10"), labels = c("<6", "6-10", ">10")) +
  scale_shape_manual(values = c("<6" = 1, "6-10" = 2, ">10" = 3),
                     limits = c("<6", "6-10", ">10"), 
                     labels = c("<6", "6-10", ">10")) +
  scale_x_discrete(labels = c("human", "GPT", "Magenta"), limits = c("human", "gpt", "magenta")) +
  labs(x = "composer", y = "mean rating", color = "musical experience in years", shape = "musical experience in years") +
  theme_pubr() +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),  
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
=======
library(tidyverse)
#library(emmeans)
#library(boot)
#library(rstatix)
#library(afex)
#library(broom)
library(effectsize)
#library(magrittr)
library(ggpubr)


# Load data ---------------------------------------------------------------

df_final <- read.csv("df_kimelodiefortsetzung_n71.csv", row.names = 1)

# Descriptives ---------------------------------------------------

n_musical_identity <- df_final |> 
  filter(CASE != "345") |> 
  group_by(SD07) |> 
  summarise(n = n())

n_gender <- df_final |> 
  group_by(SD05) |> 
  summarise(n = n())

# prepare data for MANOVA -----------------------------------------------------------------

data_manova <- df_final %>%
  group_by(participant, composer, probe_position) %>%
  summarise(überzeugend = mean(überzeugend),
            logisch.und.sinnvoll = mean(logisch.und.sinnvoll),
            interessant = mean(interessant),
            gefällt.mir = mean(gefällt.mir))
data_effectsize <- df_final %>%
  group_by(participant, composer) %>%
  summarise(überzeugend = mean(überzeugend),
            logisch.und.sinnvoll = mean(logisch.und.sinnvoll),
            interessant = mean(interessant),
            gefällt.mir = mean(gefällt.mir))

data_manova$musical_identity <-
  factor(
    pmap_vec(list(p = data_manova$participant,
                  c = data_manova$composer,
                  pp = data_manova$probe_position),
             function(p, c, pp) {
               df_final %>% filter(participant == p,
                                   composer == c,
                                   probe_position == pp) %$%
                 musical_identity %>%
                 .[[1]]
             }),
    levels = c("<6", "6-10", ">10")
  )




# Calculate MANOVA --------------------------------------------------------

stats_manova_model <- data_manova %>%
  drop_na() %>%
  manova(
    cbind(überzeugend, logisch.und.sinnvoll, interessant, gefällt.mir) ~
      factor(composer) * factor(probe_position) * musical_identity +
      Error(factor(participant)/(factor(composer) * factor(probe_position))),
    data = .
  )

summary(stats_manova_model)
summary_manova <- as.data.frame(summary(stats_manova_model))

# Calculate effectsizes ---------------------------------------------------

data_effsize_ai <- data_effectsize |> 
  filter(composer == "ai")
data_effsize_hum <- data_effectsize |> 
  filter(composer == "hum")

cohens_d(x = interessant ~ composer, data = data_effectsize, paired = TRUE)
cohens_d(x = logisch.und.sinnvoll ~ composer, data = data_effectsize, paired = TRUE)
cohens_d(x = gefällt.mir ~ composer, data = data_effectsize, paired = TRUE)
cohens_d(x = überzeugend ~ composer, data = data_effectsize, paired = TRUE)

# Mean and SD -----------------------------------------------------

df_final |> 
  mutate(composer = as.factor(composer)) |> 
  group_by(composer) |> 
  summarise(mean_überzeugend = mean(überzeugend),
            sd_überzeugend = sd(überzeugend),
            mean_interessant = mean(interessant),
            sd_interessant = sd(interessant))  

# errobar plot ----------------------------------------------------

my_data_long <- df_final |> 
  pivot_longer(cols = 6:9,
               names_to = "dependent_variable",
               values_to = "rating") |> 
  mutate(musical_identity = factor(musical_identity, levels = c("<6", "6-10", ">10"), labels = c("<6", "6-10", ">10"))) |> 
  drop_na()

musical_identity_order <- c("<6", "6-10", ">10")
my_data_long$musical_identity <- factor(my_data_long$musical_identity, levels = musical_identity_order)
my_data_long$musical_identity <- forcats::fct_relevel(my_data_long$musical_identity, "<6", ">10", after = 2)
my_data_long$composer_specified <- factor(my_data_long$composer_specified, levels = c("human", "gpt", "magenta"))

mean_ci_musical_identity <- my_data_long |> 
  group_by(dependent_variable, composer, musical_identity) |> 
  summarize(mean_rating = mean(rating),
            ci_low = mean_rating - qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()),
            ci_high = mean_rating + qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()))

mean_ci_composer <- my_data_long |> 
  group_by(dependent_variable, composer) |> 
  summarize(mean_rating = mean(rating),
            ci_low = mean_rating - qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()),
            ci_high = mean_rating + qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()))

mean_ci_composer$composer <- as.factor(mean_ci_composer$composer, levels = c("hum", "ai"))

mean_ci_specific <- my_data_long |> 
  group_by(dependent_variable, composer_specified, musical_identity) |> 
  summarize(mean_rating = mean(rating),
            ci_low = mean_rating - qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()),
            ci_high = mean_rating + qt(0.975, df = n() - 1) * sd(rating) / sqrt(n()))

mean_ci_specific$composer_specified[is.na(mean_ci_specific$composer_specified)] <- "human"
  
labeller <- list("überzeugend" = "convincing",
             "logisch.und.sinnvoll" = "logical and meaningful",
             "interessant" = "interesting",
             "gefällt.mir" = "liking")

dep_var_labeller <- function(variable, value){
  return(labeller[value])
}

ggplot(mean_ci_specific, aes(x = composer_specified, y = mean_rating, group = musical_identity, color = musical_identity, shape = musical_identity)) +
  geom_point(size = 2, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.5, position = position_dodge(width = 0.9), size = 0.5) +
  facet_wrap(~dependent_variable, scales = "free_y", labeller = dep_var_labeller) +
  scale_color_manual(values = c("<6" = "#231f20", "6-10" = "#f47920", ">10" = "#e42485"), 
                     limits = c("<6", "6-10", ">10"), labels = c("<6", "6-10", ">10")) +
  scale_shape_manual(values = c("<6" = 1, "6-10" = 2, ">10" = 3),
                     limits = c("<6", "6-10", ">10"), 
                     labels = c("<6", "6-10", ">10")) +
  scale_x_discrete(labels = c("human", "GPT", "Magenta"), limits = c("human", "gpt", "magenta")) +
  labs(x = "composer", y = "mean rating", color = "musical experience in years", shape = "musical experience in years") +
  theme_pubr() +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),  
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
>>>>>>> eb6045c7175344ee0d4b48aae3e3dc94a576fc8a
        legend.position = "bottom")