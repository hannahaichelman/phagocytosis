# This script takes data obtained during Hannah's visit to UMiami from the flow cytometer
# and compares phagocytosis across symbiotic state and temperature in
# Oculina arbuscula

library(tidyverse)
library(dplyr)
library(Rmisc)

# read in raw data

oc = read.csv("/Users/hannahaichelman/Documents/BU/RCN_Exchange_UMiami/oculina_raw_final.csv")

head(oc)
str(oc)

# clean up names of columns and make treatments of interest factors
oc_clean = oc %>%
  mutate(Temperature = as.factor(Temperature), Symbiotic.State = as.factor(Symbiotic.State),
         Genotype = as.factor(Genotype), Replicate = as.factor(Replicate), Treatment = as.factor(Treatment)) %>%
  dplyr::rename(Num_Cells = X.Cells) %>%
  dplyr::rename(Avg_Control = Average.Control..for.that.given.genotype_treatment_.temperature.) %>%
  dplyr::rename(Adj_Percent = Adjusted.value..Percent....Average.Control.)

str(oc_clean)

# subset data to only look at treatments, since data has been corrected to controls
oc_treatments = oc_clean %>%
  subset(Treatment == "Beads" | Treatment == "Ecoli")

# calculate mean percent of cells across the three replicate measurements
oc_summary = oc_treatments %>%
  #group_by(Genotype, Temperature, Treatment) %>%
  #dplyr::summarize(mean_percent = mean(Adj_Percent)) %>%
  unite(temp_treat, c(Temperature,Treatment), sep = "_", remove = FALSE) %>%
  mutate(temp_treat = as.factor(temp_treat)) %>%
  droplevels()


# PLOT

# xlab = temperature, ylab = percent, color = Treatment, facet by genotype


cell_means <- summarySE(oc_summary, measurevar="Adj_Percent", groupvars=c("Temperature","Treatment","Genotype"))

# plot, treatment x axis colored by lineage data figure
plot <- ggplot(oc_summary, aes(x = Temperature, y = Adj_Percent))+
  theme_bw()+
  geom_jitter(aes(color = Treatment, fill = Treatment),
              position=position_dodge(width=0.3),
              alpha=0.2, pch = 21,
              color = "black") +
  geom_errorbar(data = cell_means, aes(x = Temperature, ymax = Adj_Percent+se, ymin = Adj_Percent-se, color = Treatment), width = .2, position = position_dodge(width=0.4)) +
  geom_point(data = cell_means, mapping = aes(x=Temperature, y=Adj_Percent, color = lineage, fill = Treatment), size = 3.5, pch = 21, color = "black", position = position_dodge(width=0.4))+
  #scale_fill_manual(name = "Treatment",
  #                  breaks = c("L1","L2"),
  #                  values = cols_lineage)+
  #scale_color_manual(name = "Lineage",
  #                   breaks = c("L1","L2"),
  #                   values = cols_lineage)+
  xlab("Temperature")+
  ylab("Adjusted Percent of Phagocytes")+
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  facet_wrap(~Genotype, nrow = 2, ncol = 4)
plot

ggsave(plot, filename = "Phagocyte_Number.pdf", width=6, height=4, units=c("in"), useDingbats=FALSE)




#
oc_syms = oc_clean %>%
  subset(Treatment == "Control_symbionts") %>%
  droplevels()

# PLOT

# xlab = temperature, ylab = percent, color = Treatment, facet by genotype


cell_means <- summarySE(oc_syms, measurevar="Statistic", groupvars=c("Temperature","Treatment","Genotype"))

# plot ratio of symbiont cells to coral cells - not absolute number of symbionts
plot <- ggplot(oc_syms, aes(x = Temperature, y = Statistic))+
  theme_bw()+
  geom_jitter(aes(color = Treatment, fill = Treatment),
              position=position_dodge(width=0.3),
              alpha=0.2, pch = 21,
              color = "black") +
  geom_errorbar(data = cell_means, aes(x = Temperature, ymax = Statistic+se, ymin = Statistic-se, color = Treatment), width = .2, position = position_dodge(width=0.4)) +
  geom_point(data = cell_means, mapping = aes(x=Temperature, y=Statistic, color = lineage, fill = Treatment), size = 3.5, pch = 21, color = "black", position = position_dodge(width=0.4))+
  xlab("Temperature")+
  ylab("Adjusted Percent of Symbiont Cells")+
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  facet_wrap(~Genotype, nrow = 2, ncol = 4)
plot

ggsave(plot, filename = "Symbiont_Number.pdf", width=6, height=4, units=c("in"), useDingbats=FALSE)

