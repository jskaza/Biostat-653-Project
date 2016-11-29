library(ggplot2)

eeg1 = read.csv("eeg-data.csv")
metadata = read.csv("subject-metadata.csv")
metadata$id =metadata$ID
eeg = merge(eeg1, metadata, by="id")
eeg$indra_time = as.POSIXlt(eeg$indra_time)
eeg$indra_time_delta = as.numeric(difftime(eeg$indra_time, 
                                           min(eeg$indra_time), unit='secs'))

eeg_clean = subset(eeg, signal_quality < 128)

ggplot(eeg_clean, aes(indra_time)) + 
  stat_smooth(aes(y=attention_esense)) + 
  stat_smooth(aes(y=meditation_esense), colour="black") +
  facet_wrap(~ id) + xlab("Time") + ylab("Standardized Score") + ylim(c(0,100))

ggsave("trajectories.pdf", width=10, height=10)

# ggplot(eeg_clean, aes(indra_time)) + 
#   geom_line(aes(y=attention_esense)) + 
#   geom_line(aes(y=meditation_esense), colour="black", alpha=0.2) +
#   facet_wrap(~ id)
# 
# ggplot(eeg, aes(indra_time)) + stat_smooth(aes(y=meditation_esense), 
#                                                               colour="black") + 
#   stat_smooth(aes(y=attention_esense), colour="red")
# 
# ggplot(eeg, aes(indra_time, meditation_esense, colour=factor(Chosen.color))) +
#   stat_smooth()

# check nobs
mean(table(eeg$id))
min(table(eeg$id))
max(table(eeg$id))


