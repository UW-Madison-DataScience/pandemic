# This idea came from Jerry Zhu <jerryzhu@cs.wisc.edu>
tmp <- cases_state %>%
  group_by(Region, State) %>%
  summarize(maxCount = max(Count),
            time_10 = as.numeric(max(Date) - min(Date[Count >= maxCount / 10])),
            time_100 = as.numeric(max(Date) - min(Date[Count >= maxCount / 100])),
            time_100 = time_100 - time_10) %>%
  ungroup %>%
  filter(maxCount >= 100,
         time_10 > 1, time_100 > 1)
p <- ggplot(tmp) +
  aes(time_10, time_100, label = State) +
  geom_abline(intercept = 0, slope = 1, col = "grey", size = 2) +
  geom_text() + 
  facet_wrap(~Region, scales = "free")
p
pdf("ten_100.pdf")
print(p)
dev.off()
