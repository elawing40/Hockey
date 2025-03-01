memorial_model1 <- brm(
  MemorialCupWinner ~ PCT + GF + GA + PP. + PK. + (1 | League) + (1 | Team),
  data = CHLData,
  family = bernoulli(link = "logit"),
  chains = 4, cores = 4, iter = 4000, seed = 123
)

summary(memorial_model1)
plot(memorial_model1)
pp_check(memorial_model1)
pp_check(memorial_model1, type='error_scatter_avg')
loo(memorial_model1)
posterior_summary(memorial_model1)



memorial_model2 <- brm(
  MemorialCupWinner ~ PCT + GF + GA + PP. + PK. + (League | MemorialCupYear) + (1 | Team),
  data = CHLData,
  family = bernoulli(link = "logit"),
  chains = 4, cores = 4, iter = 4000, seed = 123
)

summary(memorial_model2)
plot(memorial_model2)
pp_check(memorial_model2)

loo(memorial_model1, memorial_model2)

team_preds <- CHLData %>%
  mutate(pred_prob = posterior_epred(memorial_model1) %>% colMeans())

# View top teams by predicted probability
team_preds %>%
  arrange(desc(pred_prob)) %>%
  select(TeamYearID, pred_prob) 
str(posterior_epred(memorial_model1))
dim(posterior_epred(memorial_model1)) 

top_teams <- team_preds %>%
  arrange(desc(pred_prob)) %>%
  select(TeamYearID, pred_prob)

head(top_teams, 10)

team_log_odds <- data.frame(
  TeamYearID = CHLData$TeamYearID,
  log_odds = colMeans(posterior_linpred(memorial_model1))
) %>%
  arrange(desc(log_odds))

head(team_log_odds, 10)

team_log_odds <- CHLData %>%
  mutate(
    log_odds_mean = colMeans(posterior_linpred(memorial_model1)),
    log_odds_lower = apply(posterior_linpred(memorial_model1), 2, quantile, probs = 0.025),
    log_odds_upper = apply(posterior_linpred(memorial_model1), 2, quantile, probs = 0.975)
  ) %>%
  arrange(desc(log_odds_mean))

head(team_log_odds, 10)

#Convert to Probability
team_log_odds %>%
  mutate(prob = exp(log_odds_mean) / (1 + exp(log_odds_mean)))

winners_log_odds <- team_log_odds %>%
  filter(TeamYearID %in% CHLData$TeamYearID[CHLData$MemorialCupWinner == 1]) %>%
  arrange(log_odds_mean)

# Plot
ggplot(winners_log_odds, aes(x = reorder(TeamYearID, log_odds_mean), y = log_odds_mean)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Ranking of Memorial Cup Winning Teams",
       x = "Team & Year",
       y = "Estimated Log-Odds") +
  theme_minimal()


top_teams_log_odds <- team_log_odds %>%
  top_n(20, log_odds_mean) %>%
  arrange(log_odds_mean)

# Plot
ggplot(top_teams_log_odds, aes(x = reorder(TeamYearID, log_odds_mean), y = log_odds_mean)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 Teams by Log-Odds",
       x = "Team & Year",
       y = "Estimated Log-Odds") +
  theme_minimal()