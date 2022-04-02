library(tidyverse)
library(estimatr)
library(patchwork)


# Load the data
# You can download the Gujarat election results data from here:
# https://lokdhaba.ashoka.edu.in/browse-data?et=AE&st=Gujarat&an=10%2C11%2C12%2C13
# I saved this file as "gujarat.csv"

dat <- read_csv("gujarat.csv")

# Define two new variables for the analysis

dat <- dat %>%
  mutate(Win = case_when(Position == 1 ~ 1, Position != 1 ~ 0),) %>%
  group_by(Year) %>%
  mutate(Above_Avg_Turnout = case_when(
    Turnout_Percentage >= mean(Turnout_Percentage) ~ 1,
    Turnout_Percentage < mean(Turnout_Percentage) ~ 0
  )) %>%
  ungroup()

# Predictors of vote share 

voteshare_det_1 = dat %>%
  filter(Party %in% c("BJP", "INC")) %>%
  filter(Year %in% c(2002, 2007)) %>%
  group_by(Party, Year) %>%
  summarise(tidy(
    lm_robust(
      Vote_Share_Percentage ~ Above_Avg_Turnout + Constituency_Type + N_Cand + Contested + Turncoat + Incumbent,
      data = cur_data()
    )
  )) %>%
  filter(term != "(Intercept)")

voteshare_det_2 = dat %>%
  filter(Party %in% c("BJP", "INC")) %>%
  filter(Year %in% c(2012, 2017)) %>%
  group_by(Party, Year) %>%
  summarise(tidy(
    lm_robust(
      Vote_Share_Percentage ~ Above_Avg_Turnout + Constituency_Type + N_Cand + Contested + Turncoat + Incumbent + Sub_Region,
      data = cur_data()
    )
  )) %>%
  filter(term != "(Intercept)")

voteshare_det = bind_rows(voteshare_det_1, voteshare_det_2) %>%
  mutate(
    Variable = case_when(
      term == "Constituency_TypeSC" ~ "SC Constituency",
      term == "Constituency_TypeST" ~ "ST Constituency",
      term == "Contested" ~ "No. of Elections\nContested",
      term == "IncumbentTRUE" ~ "Incumbent",
      term == "N_Cand" ~ "No. of Cand\n in Constituency",
      term == "Sub_RegionNorth Gujarat" ~ "Region (North)",
      term == "Sub_RegionSouth Gujarat" ~ "Region (South)",
      term == "Sub_RegionSaurashtra-Kutch" ~ "Region (Saurashtra)",
      term == "TurncoatTRUE" ~ "Party Switcher",
      term == "Above_Avg_Turnout" ~ "Above Avg. Turnout"
    )
  )


fig1a <- ggplot(voteshare_det,
                aes(x = estimate, y = Variable, color = Party)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax =  conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("darkorange2", "dodgerblue1")) +
  facet_wrap( ~ Year, nrow = 1) +
  ggtitle("Party Voteshare") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

# Predictors of winning a constituency

win_det_1 = dat %>%
  filter(Party %in% c("BJP", "INC")) %>%
  filter(Year %in% c(2002, 2007)) %>%
  group_by(Party, Year) %>%
  summarise(tidy(
    lm_robust(
      Win ~ Above_Avg_Turnout + Constituency_Type + N_Cand + Contested + Turncoat + Incumbent,
      data = cur_data()
    )
  )) %>%
  filter(term != "(Intercept)")

win_det_2 = dat %>%
  filter(Party %in% c("BJP", "INC")) %>%
  filter(Year %in% c(2012, 2017)) %>%
  group_by(Party, Year) %>%
  summarise(tidy(
    lm_robust(
      Win ~ Above_Avg_Turnout + Constituency_Type + N_Cand + Contested + Turncoat + Incumbent + Sub_Region,
      data = cur_data()
    )
  )) %>%
  filter(term != "(Intercept)")

win_det = bind_rows(win_det_1, win_det_2) %>%
  mutate(
    Variable = case_when(
      term == "Constituency_TypeSC" ~ "SC Constituency",
      term == "Constituency_TypeST" ~ "ST Constituency",
      term == "Contested" ~ "No. of Elections\nContested",
      term == "IncumbentTRUE" ~ "Incumbent",
      term == "N_Cand" ~ "No. of Cand\n in Constituency",
      term == "Sub_RegionNorth Gujarat" ~ "Region (North)",
      term == "Sub_RegionSouth Gujarat" ~ "Region (South)",
      term == "Sub_RegionSaurashtra-Kutch" ~ "Region (Saurashtra)",
      term == "TurncoatTRUE" ~ "Party Switcher",
      term == "Above_Avg_Turnout" ~ "Above Avg. Turnout"
    )
  )


fig1b <- ggplot(win_det,
                aes(x = estimate, y = Variable, color = Party)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax =  conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("darkorange2", "dodgerblue1")) +
  facet_wrap( ~ Year, nrow = 1) +
  ggtitle("Winning a Constituency") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

# Overall Figure

fig = fig1a / fig1b

ggsave(fig, filename = "guj_correlates_of_success.png", width = 8.5, height = 11, units= "in")
