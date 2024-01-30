######################################################################
#                                                                    #
#   The effect of 2 different antimicrobial substances on the        #
#   prevention of E.Coli microbial growth.                           #
#                                                                    #
######################################################################

######################################################################
#                             Overview                               #
######################################################################

# The data included in ecoli.xlsx is from an investigation on the efficacy of  
# two different antimicrobial substances on preventing the growth of E.coli 
# microbe.The data measures diameter of inhibition zones around the antimicrobe,
# which gives an indication of bacterial growth prevention.

# The data is organised into two columns, one for type of antimicrobe
# and one for innoculation zone diameter. In each sheet there are two columns:
#    Only one type of antimicrobe is used: E.Coli - using same growth media.
#    Four antimicrobial agents: Waitrose Antibacterial hand wash, Carex handwash,
#           Kanamtcin antibiotic (positve control) and water (negative control)


######################################################################
#                            Set up                                  #
######################################################################

# Packages to Run
library(tidyverse) 
# for summarising and plotting
# Hadley Wickham (2017). tidyverse: Easily Install and Load the
# 'Tidyverse'. R package version 1.2.1.
# https://CRAN.R-project.org/package=tidyverse

library(readxl)
# for importing excel worksheets
# Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel
# Files. R package version 1.3.1.
# https://CRAN.R-project.org/package=readxl

library(writexl)
# For exporting summary of variables into an excel table format
# Ooms J (2023). writexl: Export Data Frames to Excel 'xlsx' Format. 
# R package version 1.4.2
# https://CRAN.R-project.org/package=writexl

######################################################################
#                    Import and describe data                        #
######################################################################

# Read in the excel data - Excell data is already in Tidy format
# so no need to reformat. There is only one dataset.
ecoli <- read_excel("raw_data/ecoli.xlsx")

# view data and check structure
View(ecoli)
str(ecoli)
# tibble [80 × 3] (S3: tbl_df/tbl/data.frame)
#   $ Bacteria : chr [1:80] "E.Coli" "E.Coli" "E.Coli" "E.Coli" ...
#   $ Treatment: chr [1:80] "Antimicrobial - Waitrose" "Antimicrobial - Waitrose" ...
#   $ Diameter : num [1:80] 22 15.5 17 18 20 17 19 17 18 16 ...

# Explore variables
ecoli %>% group_by(Treatment) %>% count()
# Total of 60 disks used for antimicrobial agent.
# 10 disks for kanamycin were used and randomly assigned to 10 plates
# 10 disks for water were used and randomly asigned to the remaining 10 plates
# Groups:   Treatment [4]
# Treatment     n
# <chr>     <int>
# 1 Carex        30
# 2 Kanamycin    10
# 3 Waitrose     30
# 4 Water        10

summary(ecoli$Diameter)
# No values in the dataset are extreme outliers / differ greatly from the majority
# All values for water are 0, lowest value from antimicrobials is 15
# The diameter variable is a measure that we would expect to be normally distributed.
#       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       0.00   17.00   18.00   15.91   19.00   22.00 

######################################################################
#                     Exploratory Analysis                           #
######################################################################

# quick plot of the data
ggplot(data = ecoli,
       aes(x = Treatment, y = Diameter)) + 
  geom_boxplot()
# overall - Both antimicrobials appear to show similarities in efficacy, with
# values mainly falling between 17mm and 20mm. Both antimicrobes appear as
# effective as Kanamycin antibiotic used as positive control.

# Summarise the data
# Include mean, standard deviation(sd) and standard error(se)
ecolisum <- ecoli %>% 
  group_by(Treatment) %>% 
  summarise(mean = mean(Diameter),
            std = sd(Diameter),
            n = length(Diameter),
            se = std/sqrt(n))

# Export data as table to be used within lab report.
# Data exported in .xlsx format.
write_xlsx(ecolisum, "figures/Table2.xlsx")

# conclusion: n = 30 for each antimicrobial is relatively
# small but the design is balanced, the data includes little decimal places,
# meaning repeated values are common. Extreme values do not effect the data however.

# We will carry out a one-way ANOVA test.We have one explanatory variable, "Diameter" 
# comprising 4 levels. The data appears normally distributed therefore one-way 
# ANOVA is the desired test - we will check the assumptions after building the model

######################################################################
#                     Statistical Analysis                           #
######################################################################

# The one-way ANOVA test assumes the “residuals” are normally distributed and 
# have homogeneity of variance (variances around means of two or more samples 
# are considered equal).

# Run the one-way ANOVA to examine the effect of antimicrobial and whether their 
# effects are independant.
mod <- aov(data = ecoli, Diameter ~ Treatment)
# Summarise the data
summary(mod)

#             Df Sum Sq Mean Sq F value Pr(>F)    
# Treatment    3   2916   972.1   439.7 <2e-16 ***
# Residuals   76    168     2.2                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# There is a highly significant effect of Antimicrobial on the diameter (mm)
# of bacterial colonies (F = 439.7; d.f = 3, 76; p = <0.001).

# Post-Hoc test. Only done after significant ANOVA. The ANOVA tells you at least 
# two of means differ, the post-hoc test tells you where the differences are.
# Carry out a Tukey Honest Signifcant differences test to establish where 
# signifcant differences bewteen Antimicrobial treatment combinations are.
TukeyHSD(mod)
# $Treatment
#                                                   diff         lwr        upr     p adj
# Antimicrobe - Waitrose-Antimicrobe - Carex  -0.7333333  -1.7418237   0.275157 0.2324827
# Kanamycin-Antimicrobe - Carex                0.9333333  -0.4928874   2.359554 0.3210715
# Water-Antimicrobe - Carex                  -18.3666667 -19.7928874 -16.940446 0.0000000
# Kanamycin-Antimicrobe - Waitrose             1.6666667   0.2404459   3.092887 0.0154117
# Water-Antimicrobe - Waitrose               -17.6333333 -19.0595541 -16.207113 0.0000000
# Water-Kanamycin                            -19.3000000 -21.0467566 -17.553243 0.0000000

# Plot the Post-Hoc
plot(TukeyHSD(mod))
# examine residuals. plot model residuals
# against examined residuals

plot(mod, which = 1)
# The group means are the fitted (or predicted) values; 
# each residual is the difference between the mean and 
# the actual value.

# examine normality of the model residuals we use a histogram
hist(mod$residuals)
# use shapiro test
shapiro.test(mod$residuals)
#   Shapiro-Wilk normality test
#   data:  mod$residuals
#   W = 0.94593, p-value = 0.00203

# CONCLUSION: the residuals are normally distributed. The 
# histogram is roughly normal - it's symmetrical - and 
# the shapiro test is NS.


######################################################################
#                                 Figure                             #
######################################################################

# Illustrating
# Add annotations for significance "***"

ggplot() +
  geom_point(data = ecoli, aes(x = Treatment , y = Diameter),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = ecolisum, 
                aes(x = Treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = ecolisum, 
                aes(x = Treatment, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = expression(Diameter~of~resistance~zone~(mm)),
                     limits = c(0, 25), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = c("Carex", "Waitrose", "Kanamycin", "dH20"), 
                   name = "Treatment") +
  annotate("segment", x = 1, xend = 2,   
           y = 23, yend = 23, 
           colour = "black") +
  # short horizontal, x and xend are the same at harbour (xend = 2)
  # y and yend are slightly apart
  annotate("segment", x = 2, xend = 2, 
           y = 23, yend = 22.5,
           colour = "black") +
  # short horizontal, x and xend are the same at bladdernose (x = 1)
  # y and yend are slightly apart
  annotate("segment", x = 1, xend = 1,
           y = 23, yend = 22.5,
           colour = "black") +
  # The text
  annotate("text", x = 1.5,  y = 23.3, size=10,
           label = expression("***")) +
  theme_classic()

# Assign to fig1.
fig1 <- ggplot() +
  geom_point(data = ecoli, aes(x = Treatment , y = Diameter),
             position = position_jitter(width = 0.1, height = 0),
             colour = "gray50") +
  geom_errorbar(data = ecolisum, 
                aes(x = Treatment, ymin = mean - se, ymax = mean + se),
                width = 0.3) +
  geom_errorbar(data = ecolisum, 
                aes(x = Treatment, ymin = mean, ymax = mean),
                width = 0.2) +
  scale_y_continuous(name = expression(Diameter~of~resistance~zone~(mm)),
                     limits = c(0, 25), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = c("Carex", "Waitrose", "Kanamycin", "dH20"), 
                   name = "Treatment") +
  annotate("segment", x = 1, xend = 2,   
           y = 23, yend = 23, 
           colour = "black") +
  # short horizontal, x and xend are the same at harbour (xend = 2)
  # y and yend are slightly apart
  annotate("segment", x = 2, xend = 2, 
           y = 23, yend = 22.5,
           colour = "black") +
  # short horizontal, x and xend are the same at bladdernose (x = 1)
  # y and yend are slightly apart
  annotate("segment", x = 1, xend = 1,
           y = 23, yend = 22.5,
           colour = "black") +
  # The text
  annotate("text", x = 1.5,  y = 23.3, size=10,
           label = expression("***")) +
  theme_classic()

# Figure 1. The effect of Different antimicrobials on preventing  
# the growth of E.coli. Error bars are +/- 1 S.E. Comparisons are
# significant under post-hoc testing with Tukey's Honest 
# Significant Difference test.

# figure saving settings
units = "in"
fig_w <- 5
fig_h <- 4
dpi <- 300
device <- "png" 

# Saving figure 1 to figures folder
ggsave("figures/figure1.png", 
       plot = fig1, 
       device = device,
       width = fig_w, 
       height = fig_h,
       units = units,
       dpi = dpi)


######################################################################
#                         END OF ANALYSIS                            #
######################################################################





