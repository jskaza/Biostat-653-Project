library(ggplot2)
library(mlmmm)
library(MCMCglmm)
library(lme4)

############## data cleaning ############## 
eeg1 = read.csv("eeg-data.csv") # read-in dataset
metadata = read.csv("subject-metadata.csv") # read-in metadata/other covariates
metadata$id = metadata$ID # create identical identifier
eeg = merge(eeg1, metadata, by="id") # merge dataset and metadata
eeg$indra_time = as.POSIXlt(eeg$indra_time) # create time variable
eeg$indra_time_delta = as.numeric(difftime(eeg$indra_time, 
                                           min(eeg$indra_time), unit='secs')) # time in secs
eeg_clean = subset(eeg, signal_quality < 128) # delete observations with poor signal quality
eeg_clean = eeg_clean[which(eeg_clean$label != 'unlabeled'),] # delete observations no related to stimuli
# standardize time between sessions
eeg_clean$time = ifelse(eeg_clean$Session == 1, eeg_clean$indra_time_delta - 
                          min(eeg_clean$indra_time_delta),
                        eeg_clean$indra_time_delta - 
                          min(subset(eeg_clean, eeg_clean$Session == 2)$indra_time_delta)) 

############## exploratory analysis ##############
traj = ggplot(eeg_clean, aes(indra_time)) + 
  stat_smooth(aes(y=attention_esense)) + 
  stat_smooth(aes(y=meditation_esense), colour="black") +
  facet_wrap(~ id) + xlab("Time") + ylab("Standardized Score") + 
  ylim(c(0,100))# panel plot of attention and meditation trajectories

# plot distributions of attention and meditation scores
dist_a = qplot(eeg_clean$attention_esense, geom = 'histogram', 
               xlab = "Standardized Attention Score")
dist_m = qplot(eeg_clean$meditation_esense, geom = 'histogram', 
               xlab = "Standardized Meditation Score")

# check nobs per subject
mean(table(eeg$id))
min(table(eeg$id))
max(table(eeg$id))

mean(table(eeg_clean$id))
min(table(eeg_clean$id))
max(table(eeg_clean$id))



############## models ##############
# univariate mixed models
fit_a = lmer(attention_esense ~  1 + Gender + Seen.video.before. + Saw.icons. + Chosen.color + 
               time*as.factor(Session) + (1|id), data = eeg_clean)
fit_m = lmer(meditation_esense ~  1 + Gender + Seen.video.before. + Saw.icons. + Chosen.color + 
               time*as.factor(Session) + (1|id), data = eeg_clean)

# multivariate mixed models
prior = list(R=list(V=diag(2), nu=1.002),G=list(G1=list(V=diag(2), nu=1.002)))

fit_am = MCMCglmm(cbind(attention_esense, meditation_esense) ~ trait-1+
                 at.level(trait,1):Gender + at.level(trait,2):Gender+ 
                 at.level(trait,1):Seen.video.before. +  at.level(trait,2):Seen.video.before.
               + at.level(trait,1):Saw.icons. +  at.level(trait,2):Saw.icons.
               + at.level(trait,1):Chosen.color + at.level(trait,2):Chosen.color
               + at.level(trait,1):time*as.factor(Session)+ at.level(trait,2):time*as.factor(Session),
               random = ~us(trait):id, data = eeg_clean, prior=prior, burnin=20000, nitt = 35000,
               singular.ok = T, family = c("gaussian", "gaussian"), rcov = ~us(trait):units)