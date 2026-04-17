#Mitch Marner Game Logs
library(gt)
library(dplyr)
library(glue)
library(sportyR)
library(ggplot2)
library(ggplotify)
library(tidyr)

MitchMarnerPlayoffGameLog

MitchMarnerPlayoffGameLog$Giveaways <- c(4,0,1,0,1,0,0,1,0,0,2)
MitchMarnerPlayoffGameLog$Takeaways <- c(1,0,0,0,0,0,0,0,0,1,0)

MitchMarnerPlayoffGameLog <- MitchMarnerPlayoffGameLog %>%
  mutate(
    TOI_formatted = sprintf(
      "%d:%02d",
      floor(round(TOI * 60) / 60),
      round(TOI * 60) %% 60
    )
  )


MitchMarnerPlayoffGameLog %>%
  select(Game, TOI_formatted, xGF.,HDCF., PDO, Giveaways, Takeaways) %>%
  gt() |>
  cols_label(
    Game = "Game",
    TOI_formatted = "Time on Ice",
    xGF. = "xGoalsFor %",
    HDCF. = "High Danger Chances For %",
    PDO = "PDO",
    Giveaways = "Giveaways",
    Takeaways = "Takeaways"
  ) |>
  tab_header(
    title = md(glue::glue("<img src='{logo_url}' height='20'> **Mitch Marner 5v5 Playoff Game Log**"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) |>
  cols_align(
    align = "center"
  )

#Connor Hellebuyck Grids
HellebuyckPlayoffHomeGames <- ConnorHellebuyckPlayoffGameLog %>%
  filter(grepl("WPG at", Game, fixed = TRUE) == FALSE) %>%
  mutate(GSAx = xG.Against - Goals.Against) %>%
  mutate(
    TOI_formatted = sprintf(
      "%d:%02d",
      floor(round(TOI * 60) / 60),
      round(TOI * 60) %% 60
    )
  )



HellebuyckPlayoffHomeGames %>%
  select(Game, TOI_formatted, SV., Goals.Against, xG.Against, GSAx, GAA, Rebound.Attempts.Against) %>%
  gt() |>
  cols_label(
    Game = "Game",
    TOI_formatted = "Time on Ice",
    SV. = "Save %",
    Goals.Against = "Goals Against",
    xG.Against = "xGoals Against",
    GSAx = "Goals Saved Above Expected",
    GAA = "Goals Against Average",
    Rebound.Attempts.Against = "Rebound Attempts Against"
  ) |>
  tab_header(
    title = md(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/en/9/93/Winnipeg_Jets_Logo_2011.svg' height='20'> **Connor Hellebuyck Home Playoff Game Log**"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) |>
  cols_align(
    align = "center"
  )

homeGoals <- shots_2024.5.16 %>%
  filter(isPlayoffGame == 1 & goal == 1 & homeTeamCode == "WPG" & goalieNameForShot == "Connor Hellebuyck") %>%
  slice(rep(1:n(), each = round((1 / 451) * 60 * 100)))
geom_hockey(league = "NHL") +
  stat_density_2d(
    data = homeGoals,
    aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.3
  ) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Connor Hellebuyck Home Playoff Games \n Goals Against Density") +
  theme(
    plot.title = element_text(vjust = -0.5, hjust = 0.5),
    legend.position = "none"
  )

HellebuyckPlayoffAwayGames <- ConnorHellebuyckPlayoffGameLog %>%
  filter(grepl("WPG at", Game, fixed = TRUE) == TRUE) %>%
  mutate(GSAx = xG.Against - Goals.Against) %>%
  mutate(
    TOI_formatted = sprintf(
      "%d:%02d",
      floor(round(TOI * 60) / 60),
      round(TOI * 60) %% 60
    )
  )


HellebuyckPlayoffAwayGames %>%
  select(Game, TOI_formatted, SV., Goals.Against, xG.Against, GSAx, GAA, Rebound.Attempts.Against) %>%
  gt() |>
  cols_label(
    Game = "Game",
    TOI_formatted = "Time on Ice",
    SV. = "Save %",
    Goals.Against = "Goals Against",
    xG.Against = "xGoals Against",
    GSAx = "Goals Saved Above Expected",
    GAA = "Goals Against Average",
    Rebound.Attempts.Against = "Rebound Attempts Against"
  ) |>
  tab_header(
    title = md(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/en/9/93/Winnipeg_Jets_Logo_2011.svg' height='20'> **Connor Hellebuyck Away Playoff Game Log**"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) |>
  cols_align(
    align = "center"
  )

awayGoals <- shots_2024.5.16 %>%
  filter(isPlayoffGame == 1 & goal == 1 & awayTeamCode == "WPG" & goalieNameForShot == "Connor Hellebuyck") %>%
  slice(rep(1:n(), each = round((1 / 246.65) * 60 * 100)))
geom_hockey(league = "NHL") +
  stat_density_2d(
    data = awayGoals,
    aes(x = arenaAdjustedXCord, y = arenaAdjustedYCord, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.3
  ) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Connor Hellebuyck Away Playoff Games \n Goals Against Density") +
  theme(
    plot.title = element_text(vjust = -0.5, hjust = 0.5),
    legend.position = "none"
  ) 



McKennaStats <- data.frame(
  TOI = 392,
  Goals = 9,
  Primary_Assists = 19,
  Controlled_Entries = 166,
  Controlled_Breakouts = 104
)

McKennaStatsPer60 <- McKennaStats %>%
  mutate(GoalsPer60 = (Goals/TOI)*60,
         PrimaryAssistsPer60 = (Primary_Assists/TOI)*60,
         PrimaryPointsPer60 = ((Goals+Primary_Assists)/TOI)*60,
         CEntriesPer60 = (Controlled_Entries/TOI)*60,
         CBreakoutsPer60 = (Controlled_Breakouts/TOI)*60)

McKennaStatsPer60 %>%
  # Select only the columns that represent stats you want in the "long" format
  select(GoalsPer60, PrimaryAssistsPer60, PrimaryPointsPer60, CEntriesPer60, CBreakoutsPer60) %>%
  # Convert to long format
  pivot_longer(
    cols = everything(), # Select all columns for pivoting
    names_to = "Statistic", # New column for the original column names (e.g., "GoalsPer60")
    values_to = "Value" # New column for the values (e.g., 0.23, 0.45)
  ) %>%
  # Round the 'Value' column after pivoting
  mutate(Value = round(Value, 2)) %>%
  # Create a new column with more readable labels for the 'Statistic' column
  mutate(
    Statistic_Label = case_when(
      Statistic == "GoalsPer60" ~ "Goals/60",
      Statistic == "PrimaryAssistsPer60" ~ "Primary Assists/60",
      Statistic == "PrimaryPointsPer60" ~ "Primary Points/60",
      Statistic == "CEntriesPer60" ~ "Controlled Entries/60",
      Statistic == "CBreakoutsPer60" ~ "Controlled Breakouts/60",
      TRUE ~ Statistic # Keep original if no match (for safety)
    )
  ) %>%
  # Select and reorder columns for the gt table
  select(Statistic_Label, Value) %>%
  gt() %>%
  cols_label(
    Statistic_Label = "", # No label for the "row name" column
    Value = "Stat Value" # Label for the stats column
  ) %>%
  tab_header(
    title = md(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/en/a/af/Medicine_Hat_Tigers_Logo.svg' height='20'> **Gavin McKenna WHL Playoff Stats**"))
  ) %>%
  # Add any other gt formatting you want (e.g., borders, colors)
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "lightgray",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "center",
    columns = Value # Center only the numeric values
  ) %>%
  cols_align(
    align = "left",
    columns = Statistic_Label # Left-align the stat names
  )


#Halak 2010 Playoffs Grids
Halak2010Round1PlayoffsGames <- Halak2010Playoffs %>%
  filter(grepl("WSH", Game, fixed = TRUE) == TRUE) %>%
  mutate(GSAx = xG.Against - Goals.Against) %>%
  mutate(
    TOI_formatted = sprintf(
      "%d:%02d",
      floor(round(TOI * 60) / 60),
      round(TOI * 60) %% 60
    )
  )



Halak2010Round1PlayoffsGames %>%
  select(Game, TOI_formatted, SV., Goals.Against, xG.Against, GSAx, GAA, HDSV., Rebound.Attempts.Against) %>%
  gt() |>
  cols_label(
    Game = "Game",
    TOI_formatted = "Time on Ice",
    SV. = "Save %",
    Goals.Against = "Goals Against",
    xG.Against = "xGoals Against",
    GSAx = "Goals Saved Above Expected",
    GAA = "Goals Against Average",
    HDSV. = "High Danger Save Percentage",
    Rebound.Attempts.Against = "Rebound Attempts Against",
  ) |>
  tab_header(
    title = md(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/commons/6/69/Montreal_Canadiens.svg' height='20'> **Jaroslav Halák 2010 Stanley Cup Playoffs Round 1**"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) |>
  cols_align(
    align = "center"
  )
  

Halak2010Round2PlayoffsGames <- Halak2010Playoffs %>%
  filter(grepl("PIT", Game, fixed = TRUE) == TRUE) %>%
  mutate(GSAx = xG.Against - Goals.Against) %>%
  mutate(
    TOI_formatted = sprintf(
      "%d:%02d",
      floor(round(TOI * 60) / 60),
      round(TOI * 60) %% 60
    )
  )



Halak2010Round2PlayoffsGames %>%
  select(Game, TOI_formatted, SV., Goals.Against, xG.Against, GSAx, GAA, HDSV., Rebound.Attempts.Against) %>%
  gt() |>
  cols_label(
    Game = "Game",
    TOI_formatted = "Time on Ice",
    SV. = "Save %",
    Goals.Against = "Goals Against",
    xG.Against = "xGoals Against",
    GSAx = "Goals Saved Above Expected",
    GAA = "Goals Against Average",
    HDSV. = "High Danger Save Percentage",
    Rebound.Attempts.Against = "Rebound Attempts Against",
  ) |>
  tab_header(
    title = md(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/commons/6/69/Montreal_Canadiens.svg' height='20'> **Jaroslav Halák 2010 Stanley Cup Playoffs Round 2**"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) |>
  cols_align(
    align = "center"
  )


Halak2010Round3PlayoffsGames <- Halak2010Playoffs %>%
  filter(grepl("PHI", Game, fixed = TRUE) == TRUE) %>%
  mutate(GSAx = xG.Against - Goals.Against) %>%
  mutate(
    TOI_formatted = sprintf(
      "%d:%02d",
      floor(round(TOI * 60) / 60),
      round(TOI * 60) %% 60
    )
  )



Halak2010Round3PlayoffsGames %>%
  select(Game, TOI_formatted, SV., Goals.Against, xG.Against, GSAx, GAA, HDSV., Rebound.Attempts.Against) %>%
  gt() |>
  cols_label(
    Game = "Game",
    TOI_formatted = "Time on Ice",
    SV. = "Save %",
    Goals.Against = "Goals Against",
    xG.Against = "xGoals Against",
    GSAx = "Goals Saved Above Expected",
    GAA = "Goals Against Average",
    HDSV. = "High Danger Save Percentage",
    Rebound.Attempts.Against = "Rebound Attempts Against",
  ) |>
  tab_header(
    title = md(glue::glue("<img src='https://upload.wikimedia.org/wikipedia/commons/6/69/Montreal_Canadiens.svg' height='20'> **Jaroslav Halák 2010 Stanley Cup Playoffs Conference Finals**"))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "lightgray",
      weight = px(1)
    ),
    locations = cells_body(columns = everything())
  ) |>
  cols_align(
    align = "center"
  )
