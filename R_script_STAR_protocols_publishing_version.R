########################--------- We load necessary packages ###################

library(gplots)
library(ggplot2)
library(ggpubr)
library(ggridges)
require(graphics)
library(tidyverse)
library(tidyr)
library(magrittr)
library(dplyr)

########################--------- Preparation ##################################

# In this script, we simply load the R object containing the final data frame,
# so we can progress towards analysing the data immediately.

########################--------- We look at MAAP data #########################

# Without additional information

ggscatter(MAAP_ELISA_RBDs, x = "KA", y = "Conc",                          
          fullrange = TRUE,
          rug = TRUE) +
  ggtitle('KA versus IgG concentration - all data') +
  labs(y="IgG concentration (M)", x='KA (1/M)') +
  geom_errorbar(data=MAAP_ELISA_RBDs, aes(ymin = Conc_l, 
                                          ymax = Conc_u), size=0.05, width = 0.05, alpha=0.1, color = 'red') +
  geom_errorbarh(data=MAAP_ELISA_RBDs, aes(xmin = KA_l, 
                                           xmax = KA_u), size=0.05, height = 0.05, alpha=0.1, color = 'red') +
  yscale("log10", .format = TRUE) +
  xscale("log10", .format = TRUE)

# With additional information

ggscatter(MAAP_ELISA_RBDs, x = "KA", y = "Conc",                          
          color = "Type_sort",
          shape = "REGN",
          palette = "jco",
          fullrange = TRUE,
          rug = TRUE) +
  stat_density_2d(data=MAAP_ELISA_RBDs, geom = "polygon",
                  aes(alpha = ..level.., fill = Type_sort),
                  bins = 4) +
  scale_alpha_continuous(range=c(0.1,0.3)) +
  ggtitle('KA versus IgG concentration - annotated') +
  labs(y="IgG concentration (M)", x='KA (1/M)') +
  geom_errorbar(data=MAAP_ELISA_RBDs, aes(ymin = Conc_l, 
                                          ymax = Conc_u), size=0.05, width = 0.05, alpha=0.1, color = 'red') +
  geom_errorbarh(data=MAAP_ELISA_RBDs, aes(xmin = KA_l, 
                                           xmax = KA_u), size=0.05, height = 0.05, alpha=0.1, color = 'red') +
  yscale("log10", .format = TRUE) +
  xscale("log10", .format = TRUE)

########################--------- We look at MAAP data statistically ###########

ggscatter(MAAP_ELISA_RBDs, x = "KA", y = "Type_sort_numbered",
          conf.int = TRUE,                          
          color = "REGN",
          palette = c("#0073C2FF", "#A73030FF"),
          fullrange = TRUE,
          rug = FALSE,
) +
  ggtitle('KA versus  Wildtype, Delta, Omicron') +
  labs(y="RBD variant", x='KA (1/M)') +
  geom_boxplot(alpha=0) +
  xscale("log10", .format = TRUE)

compare_means(KA ~ Type_sort_numbered, data = MAAP_ELISA_RBDs, method = "kruskal.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

compare_means(KA ~ Type_sort_numbered, data = MAAP_ELISA_RBDs, method = "wilcox.test", paired = FALSE,
              group.by = NULL, ref.group = NULL)

########################--------- We correlate ELISA and MAAP ##################

# KA versus ELISA

ggscatter(MAAP_ELISA_RBDs, x = "KA", y = "ELISA",
          add = "reg.line",                       
          conf.int = TRUE,                          
          fullrange = TRUE,
          rug = FALSE,
) +
  stat_cor() +
  ggtitle('KA versus ELISA titre') +
  labs(y="p(EC50) values", x='KA (1/M)') +
  xscale("log10", .format = TRUE)

# IgG concentration versus ELISA

ggscatter(MAAP_ELISA_RBDs, x = "Conc", y = "ELISA",
          add = "reg.line",                       
          conf.int = TRUE,                          
          fullrange = TRUE,
          rug = FALSE,
) +
  stat_cor() +
  ggtitle('IgG concentration versus ELISA titre') +
  labs(y="p(EC50) values", x='IgG concentration (M)') +
  xscale("log10", .format = TRUE)






