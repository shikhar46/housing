library(tidyverse)
library(ggthemes)
library(ggrepel)

# Load the data
# You can download the 2017 UP election results data from here:
# https://lokdhaba.ashoka.edu.in/browse-data?et=AE&st=Uttar_Pradesh&an=17
# I saved this file as "uttar_pradesh.csv"

# Recode sub-region since the election data has more regions than the CVoter survey
# Define alliances
dat <- read_csv("uttar_pradesh.csv") %>%
  mutate(
    Sub_Region = case_when(
      Sub_Region == "NORTH-EAST" ~ "EAST",
      Sub_Region == "ROHILKHAND" ~ "WEST",
      Sub_Region == "DOAB" &
        District_Name %in% c(
          "ALIGARH",
          "MAHAMAYA NAGAR",
          "MATHURA",
          "AGRA",
          "FIROZABAD",
          "KANSHIRAM NAGAR",
          "ETAH",
          "MAINPURI"
        ) ~ "WEST",
      Sub_Region == "DOAB" &
        District_Name %in% c(
          "FARRUKHABAD",
          "KANNAUJ",
          "ETAWAH",
          "AURAIYA",
          "RAMABAI NAGAR",
          "KANPUR GRAMEEN",
          "KANPUR NAGAR"
        ) ~ "AVADH",
      Sub_Region == "DOAB" &
        District_Name %in% c("FATEHPUR", "KAUSHAMBI",
                             "ALLAHABAD", "MIRZAPUR") ~ "EAST",
      TRUE ~ Sub_Region
    ),
    alliances = case_when(
      Party %in% c("BJP", "ADAL", "NINSHAD") ~ "BJP",
      Party %in% c("SP", "RLD", "SBSP", "NCP") ~ "SP",
      Party == "BSP" ~ "BSP",
      Party == "INC" ~ "INC",
      TRUE ~ "Other"
    ),
    ID = paste0(Sub_Region, "_", alliances)
  )

# Calculate the total votes polled by each alliance in a region
votes_2017 <- dat %>%
  group_by(Sub_Region, alliances) %>%
  summarise(party.total = sum(Votes)) %>%
  ungroup()

# Calculate the total votes polled in every region
totalvotes2017 <- dat %>%
  filter(Position == 1) %>%
  group_by(Sub_Region) %>%
  summarise(total.votes.polled = sum(Valid_Votes)) %>%
  ungroup()

# Merge the party totals and region vote totals
# Create a new variable calculating regional vote share
# Input CVoter's regional vote share projections
# Calculate the swing for each party in each region

voteshare_2017 = left_join(votes_2017, totalvotes2017, by = "Sub_Region") %>%
  mutate(
    voteshare = (party.total / total.votes.polled) * 100 %>% round(1),
    forecast = case_when(
      Sub_Region == "AVADH" & alliances == "BJP" ~ 43.9,
      Sub_Region == "AVADH" & alliances == "SP" ~ 33.6,
      Sub_Region == "AVADH" & alliances == "BSP" ~ 12.3,
      Sub_Region == "AVADH" & alliances == "INC" ~ 7.1,
      Sub_Region == "AVADH" & alliances == "Other" ~ 3,
      Sub_Region == "BUNDELKHAND" & alliances == "BJP" ~ 43.9,
      Sub_Region == "BUNDELKHAND" & alliances == "SP" ~ 30.9,
      Sub_Region == "BUNDELKHAND" & alliances == "BSP" ~ 14.6,
      Sub_Region == "BUNDELKHAND" & alliances == "INC" ~ 8.4,
      Sub_Region == "BUNDELKHAND" & alliances == "Other" ~ 2.2,
      Sub_Region == "EAST" & alliances == "BJP" ~ 40.2,
      Sub_Region == "EAST" & alliances == "SP" ~ 35.9,
      Sub_Region == "EAST" & alliances == "BSP" ~ 14.4,
      Sub_Region == "EAST" & alliances == "INC" ~ 6.8,
      Sub_Region == "EAST" & alliances == "Other" ~ 2.7,
      Sub_Region == "WEST" & alliances == "BJP" ~ 39.3,
      Sub_Region == "WEST" & alliances == "SP" ~ 36,
      Sub_Region == "WEST" & alliances == "BSP" ~ 15.6,
      Sub_Region == "WEST" & alliances == "INC" ~ 7.0,
      Sub_Region == "WEST" & alliances == "Other" ~ 2.1,
    ),
    swing = forecast - voteshare,
    ID =  paste0(Sub_Region, "_", alliances
    )
  )

# Write a function that calculates seats applying the regional swing to 2017 results. 
# Allow that swing to be adjusted: say, we can take 1 percentage-point from BJP and give it to SP or vice-versa
forecaster <- function(bjp_to_sp, bsp_to_sp){
  temp_votes <- voteshare_2017 %>%
    mutate(
      swing = case_when(
        alliances == "BJP" ~ swing - bjp_to_sp,
        alliances == "SP" ~ swing + bjp_to_sp + bsp_to_sp,
        alliances == "BSP" ~ swing - bsp_to_sp,
        TRUE ~ swing
      )
    )
  hypothetical_results <-
    left_join(dat, temp_votes %>% select(ID, swing), by = "ID") %>%
    select(Sub_Region,
           Constituency_Name,
           Party,
           Vote_Share_Percentage,
           swing) %>%
    mutate(projected_voteshare = Vote_Share_Percentage + swing) %>%
    group_by(Constituency_Name) %>%
    arrange(desc(projected_voteshare), .by_group = TRUE) %>%
    mutate(position = row_number()) %>%
    ungroup()
  
  results = hypothetical_results %>%
    filter(position == 1) %>%
    group_by(Party) %>%
    summarise(N = n()
    ) %>%
    ungroup() %>%
    mutate(
      alliances = case_when(
        Party %in% c("BJP", "ADAL", "NINSHAD") ~ "BJP",
        Party %in% c("SP", "RLD", "SBSP", "NCP") ~ "SP",
        Party == "BSP" ~ "BSP",
        Party == "INC" ~ "INC",
        TRUE ~ "Other"
      ),
      `Further Swing (BJP to SP)` = bjp_to_sp,
      `Further Swing (BSP to SP)` = bsp_to_sp
    )
  return(results)
}
    
# Estimate seats for different swings from BJP to SP, fixing BSP to SP swings, 
# 0 = applying CVoter's regional swing to 2017 results.
sims_bjp_sp <- bind_rows(
  forecaster(0, 0),
  forecaster(0.5, 0),
  forecaster(1, 0),
  forecaster(1.5, 0),
  forecaster(2, 0),
  forecaster(2.5, 0),
  forecaster(3, 0),
  forecaster(-0.5, 0),
  forecaster(-1, 0),
  forecaster(-1.5, 0),
  forecaster(-2, 0),
  forecaster(-2.5, 0),
  forecaster(-3, 0),
) 
  
# Consolidate seats by alliances
sims_bjp_sp_alliances <- sims_bjp_sp %>%
  group_by(alliances, `Further Swing (BJP to SP)`) %>%
  summarise(Seats = sum(N)) %>%
  ungroup()

# Plot the results
fig1 <- ggplot(
  subset(sims_bjp_sp_alliances, alliances %in% c("BJP", "SP")),
  aes(
    x = `Further Swing (BJP to SP)`,
    y = Seats,
    color = alliances,
    label = Seats
  )
) +
  geom_hline(yintercept = 202, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text",
           x = -2.5,
           y = 190,
           label = "Majority=202") +
  annotate("text",
           x = -0.75,
           y = 380,
           label = "Based on CVoter's\nvoteshare estimates\nfrom 6 Feb 2022") +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 4, alpha = 0.4) +
  geom_text_repel() +
  ylim(c(0, 403)) +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  scale_color_manual(values = c("#ee8105", "darkgreen")) +
  xlab("Further Swing from BJP+ to SP+") +
  theme_solarized() +
  labs(
    caption = str_wrap(
      "Note: I use CVoter's regional vote share estimates from Feb 6, 2022 to calculate a swing for each major party, using the 2017 election as a baseline. The resulting seat projection is shown at 0. Then I adjust this regional swing and recalculate the seat projection. Positive values indicate a further swing from BJP to SP. Negative values indicate a further swing from SP to BJP. These calculations fix the BSP's current vote share estimates.",
      width = 120
    )
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  fig1,
  filename = "up_seat_projection_1.png",
  width = 8.5,
  height = 5,
  units = "in"
)


##### For BSP-SP swings

sims_bsp_sp <- bind_rows(
  forecaster(0, 0),
  forecaster(0, 0.5),
  forecaster(0, 1),
  forecaster(0, 1.5),
  forecaster(0, 2),
  forecaster(0, 2.5),
  forecaster(0, 3),
  forecaster(0, -0.5),
  forecaster(0, -1),
  forecaster(0, -1.5),
  forecaster(0, -2),
  forecaster(0, -2.5),
  forecaster(0, -3),
  forecaster(0, -3.5),
  forecaster(0, -4),
  forecaster(0, -4.5),
  forecaster(0, -5),
  forecaster(0, -5.5),
  forecaster(0, -6),
  forecaster(0, -6.5),
  forecaster(0, -7),
  forecaster(0, -7.5),
  forecaster(0, -8),
  forecaster(0, -8.5),
  forecaster(0, -9),
  forecaster(0, -9.5),
  forecaster(0, -10)
)

sims_bsp_sp_alliances <- sims_bsp_sp %>%
  group_by(alliances, `Further Swing (BSP to SP)`) %>%
  summarise(Seats = sum(N)) %>%
  ungroup()

# Plot the results
fig2 <- ggplot(
  subset(sims_bsp_sp_alliances, alliances %in% c("BJP", "SP", "BSP")),
  aes(
    x = `Further Swing (BSP to SP)`,
    y = Seats,
    color = alliances,
    label = Seats
  )
) +
  geom_hline(yintercept = 202, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text",
           x = -7.5,
           y = 190,
           label = "Majority=202") +
  annotate("text",
           x = -1.5,
           y = 380,
           label = "Based on CVoter's\nvoteshare estimates\non 6 Feb 2022") +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 4, alpha = 0.4) +
  geom_text(aes(y = Seats + 15)) +
  ylim(c(0, 403)) +
  scale_x_continuous(breaks = seq(-10, 3, by = 0.5)) +
  scale_color_manual(values = c("#ee8105", "darkblue", "darkgreen")) +
  xlab("Further Swing from BSP to SP+") +
  theme_solarized() +
  labs(
    caption = str_wrap(
      "Note: I use CVoter's regional vote share estimates from Feb 6, 2022 to calculate a swing for each major party, using the 2017 election as a baseline. The resulting seat projection is shown at 0. Then I adjust this regional swing and recalculate the seat projection. Positive values indicate a further swing from BSP to SP. Negative values indicate a reverse swing from SP to BSP. The calculations fix BJPs vote share estimates.",
      width = 120
    )
  ) +
  theme(
    legend.position = "none",
    #axis.text.x = element_text(angle = 45),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  fig2,
  filename = "up_seat_projection_2.png",
  width = 8.5,
  height = 5,
  units = "in"
)
