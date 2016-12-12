library(ggplot2)
library(mlmmm)
library(MCMCglmm)
library(lme4)
library(xtable)

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

############## rename/create variables ##############
names(eeg_clean)[names(eeg_clean) == 'attention_esense'] = 'attention'
names(eeg_clean)[names(eeg_clean) == 'meditation_esense'] = 'meditation'
names(eeg_clean)[names(eeg_clean) == 'Gender'] = 'gender'
names(eeg_clean)[names(eeg_clean) == 'Seen.video.before.'] = 'seen.video.before'
names(eeg_clean)[names(eeg_clean) == 'Chosen.color'] = 'color'
names(eeg_clean)[names(eeg_clean) == 'Session'] = 'session'

# spline terms
time = eeg_clean$time
eeg_clean$time.calculations = (time>48)*(time-48)
eeg_clean$time.music = (time>83)*(time-83)
eeg_clean$time.ad = (time>118)*(time-118)
eeg_clean$time.categories = (time>154)*(time-154)
eeg_clean$time.colors = (time>202)*(time-202)

# make binary
eeg_clean$saw.icons = ifelse(eeg_clean$Saw.icons. == "n", 
                        c("n"), c("y")) 
eeg_clean$session = as.factor(eeg_clean$session)

############## exploratory analysis ##############
# panel plot of attention and meditation trajectories
traj = ggplot(eeg_clean, aes(indra_time)) + 
  stat_smooth(aes(y=attention, colour="Attention")) + 
  stat_smooth(aes(y=meditation, colour="Meditation")) +
  facet_wrap(~ id) + xlab("Time") + ylab("Standardized Score") + ylim(c(0,100))+
  scale_colour_manual(name="", values=c(Attention="blue", Meditation="black")) +
  theme(legend.position="top")

# plot distributions of attention and meditation scores
dist_a = qplot(eeg_clean$attention, geom = 'histogram', 
               xlab = "Standardized Attention Score")
dist_m = qplot(eeg_clean$meditation, geom = 'histogram', 
               xlab = "Standardized Meditation Score")

# check nobs per subject
mean(table(eeg$id))
min(table(eeg$id))
max(table(eeg$id))

mean(table(eeg_clean$id))
min(table(eeg_clean$id))
max(table(eeg_clean$id))

# pre-cleaning
pre = summary(as.numeric(as.matrix(table(eeg$id))), 
        digits=10)

# post-cleaning
post = summary(as.numeric(as.matrix(table(eeg_clean$id))), 
        digits=10)

meta = data.frame(pre[1:6], post[1:6])
colnames(meta) = c('Raw', 'Clean')


############## models ##############
# univariate mixed models
fit_a = lmer(attention ~  1 + gender + seen.video.before + saw.icons + color + 
               time + time.calculations + time.music + time.ad + time.categories + 
               time.colors + time:session +  time.calculations:session +
               time.music:session + time.ad:session + 
               time.categories:session + time.colors:session +
               (1|id), data = eeg_clean)
fit_m = lmer(meditation ~  1 + gender + seen.video.before + saw.icons + color + 
               time + time.calculations + time.music + time.ad + time.categories + 
               time.colors + time:session +  time.calculations:session +
               time.music:session + time.ad:session + 
               time.categories:session + time.colors:session +
               (1|id), data = eeg_clean)

# multivariate mixed model
fit_am = MCMCglmm(cbind(attention, meditation) ~ trait-1
                       + at.level(trait,1):gender + at.level(trait,2):gender 
                       + at.level(trait,1):seen.video.before +  at.level(trait,2):seen.video.before
                       + at.level(trait,1):time + at.level(trait,2):time
                       + at.level(trait,1):time.calculations + at.level(trait,2):time.calculations
                       + at.level(trait,1):time.music + at.level(trait,2):time.music
                       + at.level(trait,1):time.ad + at.level(trait,2):time.ad
                       + at.level(trait,1):time.categories + at.level(trait,2):time.categories
                       + at.level(trait,1):time.colors + at.level(trait,2):time.colors
                       + at.level(trait,1):time:session + at.level(trait,2):time:session
                       + at.level(trait,1):time.calculations:session + at.level(trait,2):time.calculations:session
                       + at.level(trait,1):time.music:session + at.level(trait,2):time.music:session
                       + at.level(trait,1):time.ad:session + at.level(trait,2):time.ad:session
                       + at.level(trait,1):time.categories:session + at.level(trait,2):time.categories:session
                       + at.level(trait,1):time.colors:session + at.level(trait,2):time.colors:session,
                       random = ~us(trait):id, data = eeg_clean, 
                       family = c("gaussian", "gaussian"), rcov = ~us(trait):units)


# fit_am = MCMCglmm(cbind(attention, meditation) ~ trait-1+
#                  at.level(trait,1):gender + at.level(trait,2):gender+ 
#                  at.level(trait,1):seen.video.before +  at.level(trait,2):seen.video.before
#                + at.level(trait,1):time+ at.level(trait,2):time
#                + at.level(trait,1):time:as.factor(session)+ at.level(trait,2):time:as.factor(session),
#                random = ~us(trait):id, data = eeg_clean, 
#                family = c("gaussian", "gaussian"), rcov = ~us(trait):units)

## Trace plots of the parameters
par(mai=rep(0.5,4))
plot(fit$VCV)
par(mfrow=c(1,1))

par(mai=rep(0.5,4))
plot(fit$Sol)
par(mfrow=c(1,1))


############## figures ##############
# metadata
metadata = xtable(meta, caption="Summary statistics for number of repeated measures, $n_i$, before
                  and after data cleaning.", label="tab:meta")
print.xtable(metadata, type="latex", file="figures/meta.tex")

# attention results
att = xtable(coef(summary(fit_a)), label="tab:att", caption="Results from the linear mixed effects model
             with attention as the response.")
print.xtable(att, type="latex", file="figures/attention.tex")

# meditation results
med = xtable(coef(summary(fit_m)), label="tab:med", caption="Results from the linear mixed effects model
             with meditation as the response.")
print.xtable(med, type="latex", file="figures/meditation.tex")

# multivariate results
multi = xtable(summary(fit_am)$solutions, label="tab:bivariate", caption="Results from the bivariate
               linear mixed effects model. at.level(trait,1) refers to attention while 
               at.level(trait,2) corresponds to meditaion.")
print.xtable(multi, type="latex", file="figures/bivariate.tex")

# repsonse distributions
ggsave("figures/dista.pdf", dist_a, device="pdf")
ggsave("figures/distm.pdf", dist_m, device="pdf")

# trajectories
ggsave("figures/traj.pdf", traj, device="pdf", height = 10, width = 10)
