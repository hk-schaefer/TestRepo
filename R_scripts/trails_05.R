################################################################################
# Project 2
################################################################################

#########################################################################
# Analysis of variances (ANOVA Test) (Ref. 7.5)
#
# Numerical variable: number of children (childs)
# Categorical variable: social class (class)

# Claim: There is a variability in the average number of children, 
# at least between two groups
# 
# CI: for difference in mean
# H0: mue_1 = mue_2 = mue_3 ... (there is no difference)
# Ha: mue_i != mue_j       (there is a difference, at least between two groups)
#########################################################################

# Load packages
library(tidyverse)
library(statsr)
library(data.table)
library(RColorBrewer)
library(patchwork)

# Load data
load("~/R/TestRepoStatR/Project_02/Data/gss.Rdata")

############################################################# Select variables

gss_select <- gss %>% 
  select(year, childs, class) %>% 
  filter(!is.na(childs), !is.na(class))

######################################################## 2012 filter

gss_select <- gss_select %>% 
  filter(year == c(2012))

############################################################### Cleaning

gss_select %>% 
  group_by(year, class) %>% 
  count()

gss_select <- gss_select %>% 
   filter(class != "No Class")

gss_select$class <- droplevels(gss_select$class)

gss_select %>% 
   group_by(year, class) %>% 
   summarise(n = n())


########################################################## Check conditions


# 1. Independence
# within each group: random sample, n < 10% of population, cleared
# between groups: groups are independent of each other and non-paired, cleared


plot1 <- gss_select %>% 
  ggplot(aes(x=childs)) +
  geom_histogram(fill="lightblue", color="black", binwidth = 1)+
  facet_wrap(~class, nrow=1)+
  labs(title = "Distribution of Children per Class")+
  ylab("Count")+
  theme(plot.title.position = "plot",
        axis.title.y = element_text(hjust=1),
        axis.title.x = element_text(hjust=0))
  
  
plot2 <- gss_select %>% 
  ggplot(aes(y=childs))+
  geom_boxplot()+
  facet_wrap(~class, nrow = 1)+
  coord_flip()+
  labs(caption = "General Social Survey (GSS), 1972-2012") +
  ylab("Number of children per family")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  theme(plot.title.position = "plot",
        axis.title.y = element_text(hjust=1),
        axis.title.x = element_text(hjust=0))

plot1 / plot2


sum1 <- gss_select %>% 
  group_by(class) %>% 
  summarise(n_class = n(),
            mean_class = mean(childs),
            sd_class = sd(childs))
sum1

    

# 3. Constant variability
# The variability of the standard deviations for each group is very small
# From summary and from plot we can assume that the variability is consistent
# across groups (homoscedastic groups)



gss_select %>% 
  ggplot(aes(sample = childs))+
  geom_qq()+
  geom_qq_line()+
  facet_wrap(~class, nrow = 1)+
  labs(title = "QQ Plot for Normal Distribution: Children per Class")+
  xlab("Normal theoretical quantile (z-score)")+
  ylab("Data quantile (z-score)")+
  theme(plot.title.position = "plot",
        axis.title.y = element_text(hjust=1),
        axis.title.x = element_text(hjust=0))


# 2. Normal distribution:
# Distributions are all right skewed.
# Distribution in three classes show nearly normal distribution
# The distribution in "Lower Class" is deviating from a normal distribution
# but its sample size is relatively large (n = 200)
# We, therefore, can assume that all samples are nearly normal distributed



######################################################## ANOVA Test

# H0: average number of children per respondent is the same across all classes
# Ha: average number of children per respondent is not the same across all classes,
# at least not between two groups

alpha <- 0.05

gss_select %>% 
  aov(childs ~ class, data = .) %>% 
  summary()



# Result:
# The p_value is 0.004 < alpha 0.05. We can therefore reject H0
# We are 95% confident that there are at least two groups with 
# a different population mean

# Post-hoc test (TukeyHSD Test):


model <- gss_select %>% 
  aov(childs ~ class, data = .) %>% 
  TukeyHSD()
model


plot(model)

########################################################## ANOVA Test (manually)

# Calculate SSE  and MSE
SSE <- sum((sum1$n_class - 1) * sum1$sd_class^2)
N <- length(gss_select$childs)
K <- length(sum1$class)
dfe <- N - K
MSE <- SSE/dfe

# Calculate SST
mean_total <- sum(gss_select$childs)/N
SST <- sum((gss_select$childs - mean_total)^2)


# Calculate SSG, MSG
SSG <- sum(sum1$n_class * (sum1$mean_class - mean_total)^2)
dfg <- K - 1
MSG <- SSG/dfg

# Check sum
# SST2 <- SSG + SSE
# N2 <- dfe + dfg + 1

# F.Test
F <- MSG/MSE
p_value <- pf(F, dfg, dfe, lower.tail = FALSE)

##################### Pairwise comparison by t-test of all possible combinations

# Bonferroni correction
alpha_star <- alpha/(K*(K-1)/2)

### Create summary table with all possible combinations
# 
# Below a long process to create a combination table.
# There might be a more elegant solution to this.

# ----------------------------------------------------------------------------
# change factor to character
sum1
sum1$class <- as.character(sum1$class)

# create combinatoric array
dfc <- combn(sum1$class,2)
dfc <- as.data.frame(dfc)

# Transpose dataframe
dfc_t <- t(dfc)
dfc_t <- as.data.frame(dfc_t)

# Rename name of column
dfc_t_left <- dfc_t %>% 
  rename(c("class" = "V1"))

dfc_t_right <- dfc_t %>% 
  rename(c("class" = "V2"))

# Join with dataframe sum1 (for values)
df_joint_left <- dfc_t_left %>% 
  left_join(sum1, by = "class")
df_joint_left

df_joint_right <- dfc_t_right %>% 
  left_join(sum1, by = "class")
df_joint_right

# Add row ID and rename columns (left)
df_joint_left <- df_joint_left %>% 
  rowid_to_column(var="id")

df_joint_left <- df_joint_left %>% 
  rename(c("V1" = "class"))
df_joint_left

# Add row ID and rename columns (right)
df_joint_right <- df_joint_right %>% 
  rowid_to_column(var="id")

df_joint_right <- df_joint_right %>% 
  rename(c("V2" = "class"))
df_joint_right

# Join left and right tables by ID
df_joint_total <- df_joint_left %>% 
  inner_join(df_joint_right, by ="id")
df_joint_total

# Clean up
sum1_combi <- df_joint_total %>% 
  select(id, 
         class.x = "V1.x", 
         n_class.x,
         mean_class.x,
         sd_class.x,
         class.y = "V2.y",
         n_class.y,
         mean_class.y,
         sd_class.y)

rm(df_joint_left, df_joint_right, dfc, dfc_t, dfc_t_left, dfc_t_right)

# ----------------------------------------------------------------------------

# Summary table for pairwise testing of all possible combinations
sum1_combi

# Calculate for each combination, SE, t and p_value
sum1_combi2 <- sum1_combi %>% 
  mutate(mean_diff = mean_class.x - mean_class.y,
         SE = sqrt(MSE/n_class.x + MSE/n_class.y),
         T = mean_diff / SE,
         p_value = 2 * pt(abs(T),dfe, lower.tail = FALSE),
         result = ifelse(p_value < alpha_star, 
                         "Reject H0", 
                         "Fail to reject H0"))

sum1_combi2_short <- sum1_combi2 %>% 
  select(id,
         class.x,
         class.y,
         mean_diff,
         SE,
         T,
         p_value,
         result)
sum1_combi2_short
print(paste0("modified significance level, alpha_star: ", alpha_star))
print(paste0("Mean square error: ", MSE))
print(paste0("Degree of freedom dfE: ", dfe))


##############################################################################
# inference function
##############################################################################

t_test_HT <- inference(y = childs,
                       x = class,
                       data = gss_select,
                       statistic = "mean",
                       type = "ht",
                       null = 0,
                       alternative = "greater",
                       method = "theoretical",
                       sig_level = 0.05)

##############################################################################
EOF
##############################################################################