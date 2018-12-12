library(tidyverse)
library(readxl)
Sheff_derby_R <- read_excel("C:/Filestore/Stats work/Sheff_derby plots documentation etc/Sheff_derby_R.xlsx", 
                            col_types = c("text", "date", "text", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "text"))

#add WDL
Sheff_derby_WDL <-
  Sheff_derby_R %>%
  select(Season : Score) %>%
  mutate(U_WDL = ifelse(SUFC > SWFC, "W", ifelse(SUFC == SWFC, "D", "L"))) %>%
  mutate(W_WDL = ifelse(SWFC > SUFC, "W", ifelse(SWFC == SUFC, "D", "L")))

# coloured bar graph of (U_WDL)
ggplot(data = Sheff_derby_WDL)+
  geom_bar(mapping = aes(x = U_WDL, fill = U_WDL))

# coord flip of above graph
ggplot(data = Sheff_derby_WDL)+
  geom_bar(mapping = aes(x = U_WDL, fill = U_WDL))+
  coord_flip()

# add relative league positions
Rel_lge_pos <-
  Sheff_derby_WDL %>%
  select(Season : W_WDL) %>%
  mutate(U_rel_lge_pos = W_lge_pos - U_lge_pos) %>%
  mutate(W_rel_lge_pos = U_lge_pos - W_lge_pos)

# Who is higher, lower, level?
Sheff_derby_higher <-
  Rel_lge_pos %>%
  select(Season : W_rel_lge_pos) %>%
  mutate(Highest = ifelse(U_rel_lge_pos > 0, "Blades", ifelse(U_rel_lge_pos < 0, "Owls", "Level")))

#avg league position
Sheff_derby_higher %>%
  summarise(Avg_U_lge_pos = mean(U_lge_pos, na.rm = TRUE))

# total goals scored
Sheff_derby_higher %>%
  summarise(SUFC_G = sum(SUFC))

# average goals scored
Sheff_derby_higher %>%
  summarise(SUFC_G = mean(SUFC))

#distribution of goals scored by team
ggplot(data = Sheff_derby_higher)+
  geom_bar(mapping = aes(x = SUFC))

# filter specific events
filter(Sheff_derby_higher, SUFC == 4)

# group_by (eg, a team) and summarise
Sheff_derby_higher %>%
  group_by(Home_Team) %>%
  summarise(H_team_G = mean(SUFC))

# Works but doesn't make sense with current data structure
# A tibble: 2 x 2 
# Home_Team           H_team_G
# <chr>                  <dbl>
# 1 Sheffield United        1.53
# 2 Sheffield Wednesday     1.19

# tally() of distinct values, eg to find scores distribution
Sheff_derby_higher %>%
  group_by(Score) %>%
  tally()

# or

Sheff_derby_higher %>%
  count(Score)

# and to View
Scores_dist <- 
  Sheff_derby_higher %>%
  group_by(Score) %>%
  tally()
View(Scores_dist)

# dist chart of Score
ggplot(data = Sheff_derby_higher)+
  geom_bar(mapping = aes(x = Score, fill = Score))

# without colour
ggplot(data = Sheff_derby_higher)+
  geom_bar(mapping = aes(x = Score))

# dist of goals by team
ggplot(data = Sheff_derby_higher)+
  geom_bar(mapping = aes(x = SUFC))

#dist of team league position
ggplot(data = Sheff_derby_higher)+
  geom_bar(mapping = aes(x = U_lge_pos, na.rm = TRUE))

# Mutate to give Win when higher col
Sheff_derby_W_when_higher <-
  Sheff_derby_higher %>%
  mutate(U_W_higher = ifelse(Sheff_derby_higher$U_WDL == "W" &
                               Sheff_derby_higher$Highest == "Blades", "Yes", "No")) %>%
  mutate(W_W_higher = ifelse(Sheff_derby_higher$W_WDL == "W" &
                               Sheff_derby_higher$Highest == "Owls", "Yes", "No"))

# Plot W when higher
ggplot(data = Sheff_derby_W_when_higher)+
  geom_bar(mapping = aes(x = U_W_higher, na.rm = TRUE))

# Count W when higher
Sheff_derby_W_when_higher %>%
  count(U_W_higher)

# Mutate D when higher
Sheff_derby_D_when_higher <-
  Sheff_derby_W_when_higher %>%
  mutate(U_D_higher = ifelse(Sheff_derby_higher$U_WDL == "D" &
                               Sheff_derby_higher$Highest == "Blades", "Yes", "No")) %>%
  mutate(W_D_higher = ifelse(Sheff_derby_higher$W_WDL == "D" &
                               Sheff_derby_higher$Highest == "Owls", "Yes", "No"))
# Plot D when higher
ggplot(data = Sheff_derby_D_when_higher)+
  geom_bar(mapping = aes(x = U_D_higher, na.rm = TRUE))

# Count D when higher
Sheff_derby_D_when_higher %>%
  count(U_D_higher)

# path needs changing at start