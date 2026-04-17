#Mo Seider

Seider202526 <- Moritz_Seider_woodmoney %>%
  filter(Season==20252026, WMTier != "All")

ggplot(data = Seider202526, aes(x=WMTier, y = DFF.)) +
  geom_bar(stat="identity", aes(fill = as.factor(Name)),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#CE1126")) +
  labs(title = "Moritz Seider Dangerous Fenwick For % By Comp Tier",
       subtitle = "2025-26 Regular Season",
       x = "Competition Tier",
       y = "Dangerous Fenwick For %") + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  theme(legend.position = "none")

ggplot(data = Seider202526, aes(x=WMTier, y = CTOI.)) +
  geom_bar(stat="identity", aes(fill = as.factor(Name)),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("#CE1126")) +
  labs(title = "Moritz Seider TOI % By Comp Tier",
       subtitle = "2025-26 Regular Season",
       x = "Competition Tier",
       y = "Cumulative TOI %") + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  theme(legend.position = "none")
