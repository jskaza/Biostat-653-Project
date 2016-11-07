library(ggplot2)

# Tom
eeg1 = read.csv("eeg-data.csv")
metadata = read.csv("subject-metadata.csv")
metadata$id =metadata$ID
eeg = merge(eeg1, metadata, by="id")
eeg$indra_time = as.POSIXlt(eeg$indra_time)
eeg$indra_time_delta = as.numeric(difftime(eeg$indra_time, 
                                           min(eeg$indra_time), unit='secs'))

ggplot(eeg, aes(indra_time, attention_esense, colour=factor(id))) + 
  geom_line(alpha=0.2)

ggplot(eeg, aes(indra_time, attention_esense, colour=factor(Chosen.color))) +
  stat_smooth()

ggplot(eeg, aes(indra_time, meditation_esense, colour=factor(Chosen.color))) +
  stat_smooth()



