library(ggplot2)
library(mlmmm)
library(MCMCglmm)
library(lme4)

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

eeg_clean = eeg_clean[which(eeg_clean$label != 'unlabeled'),]
eeg_clean$time = ifelse(eeg_clean$Session == 1, eeg_clean$indra_time_delta -
                                        min(eeg_clean$indra_time_delta),
                                      eeg_clean$indra_time_delta - 
                                        min(subset(eeg_clean, eeg_clean$Session == 2)$indra_time_delta))



eeg_clean$int = 1
y = cbind(eeg_clean$attention_esense, eeg_clean$meditation_esense)
subj = eeg_clean$id
pred = cbind(eeg_clean$int, eeg_clean$Session, eeg_clean$time, eeg_clean$Gender)
xcol = 1:4
zcol = 1

# 
# # mlmmm.em(y, subj, pred, xcol, zcol,maxits=2)
fit = MCMCglmm(cbind(attention_esense, meditation_esense) ~ trait - 1 + Gender + 
                 Seen.video.before. + Saw.icons. + Chosen.color +
                 time*as.factor(Session),
         random = ~us(time):id, data = eeg_clean,
         family = c("gaussian", "gaussian"), rcov = ~us(trait):units)


