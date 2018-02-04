##################################################
##########   Frequencies and Crosstabs  ##########
##################################################

# 2-Way Frequency Table 
attach(beer_df)
write.csv(table(beer_df$beer_style, beer_df$review_overall), file = "sink/2way_table.csv")

mytable <- table(beer_df$beer_style, beer_df$review_overall) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

##################################################
########   End Frequencies and Crosstabs  ########
##################################################


# 3-Way Frequency Table
mytable <- xtabs(~A+B+c, data=beer_df)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedence