library(psych)
library(tidyverse)
# 2. If you had to pick 3 beers to recommend using only this data, which would you pick?
# 3. Which of the factors (aroma, taste, appearance, palette) are most important in determining the overall quality of a beer?
# 4. Lastly, if I typically enjoy a beer due to its aroma and appearance, which beer style should I try?


hist(beer_df_core$beer_abv)# for histograms
hist(beer_df_core$beer_abv, breaks = 100, xlab = "ABV %", main = "Beer ABV%")# for histograms
plot(aggdata$Group.1,aggdata$beer_abv)
ggplot(beer_df_core$beer_abv)
ggplot(data = beer_df_core, aes(x = brewery_name, y = beer_abv))
#barplot(beer_df_core$beer_abv)
#boxplot(beer_df_core$beer_abv)
summary(aggdata$beer_abv)
plot(beer_df_core[c(3:5,7:8,10)])# for scatterplots
barplot(beer_df_core[c(3:5,7:8,10)])# for barcharts, but only on the results of table() ▪ boxplot()
##################################################
##########   Frequencies and Crosstabs  ##########
##################################################

# 1. Which brewery produces the strongest beers by ABV%?
abv_table <-
  beer_df %>%
  select(brewery_name, beer_name, beer_abv) %>%
  group_by(brewery_name) %>%
  summarise(brewery_avg = mean(beer_abv, na.rm = T),
            brewery_min = min(beer_abv, na.rm = T),
            brewery_max = max(beer_abv, na.rm = T),
            total = n()) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  arrange(desc(brewery_avg))
write.csv(abv_table, file = "sink/ABV_Summary.csv")
skimr::skim(beer_df)
# 2-Way Frequency Table 
write.csv(table(aggdata$Group.1, aggdata$Group.2), file = "sink/2way_table_abv.csv")

mytable <- table(aggdata$Group.1, aggdata$Group.2) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages


# 3-Way Frequency Table
mytable <- xtabs(~beer_name+brewery_name+beer_abv, data=aggdata)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedence

##################################################
########   End Frequencies and Crosstabs  ########
##################################################

# One Way Anova (Completely Randomized Design)
fit <- aov(beer_abv ~ brewery_name, data=beer_df_core)

# Randomized Block Design (B is the blocking factor) 
fit <- aov(y ~ A + B, data=mydataframe)

# Two Way Factorial Design 
fit <- aov(y ~ A + B + A:B, data=mydataframe)
fit <- aov(y ~ A*B, data=mydataframe) # same thing

# Analysis of Covariance 
fit <- aov(y ~ A + x, data=mydataframe)


# Correlation matrix
# with beer_style, beer_abv, beer_name as rows 
# and review_overall, review_aroma, review_appearance as columns 
x <- beer_df[1:3]
y <- beer_df[4:6]
cor(beer_df$brewery_name, beer_df$beer_abv)

model <- aov(beer_abv ~ brewery_name, data = beer_df_10k)
summary(model)
plot(model)

library(broom)
tidy(model)

library(yarrr)
pirateplot(formula = beer_abv ~ brewery_name, data = beer_df_core, inf.method="ci")
