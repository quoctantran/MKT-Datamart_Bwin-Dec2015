#-----------------------------------------------------
# STEP 6 - VISUALIZATION
#-----------------------------------------------------

# Load packages
library(ggvis)
library(ggplot2)

# 1. Check most used languages by users
lang <- dm[,.N,by="Language"][order(N,decreasing = TRUE)[1:5]]

lang2 <- ggplot(lang, aes(x=factor(Language), y =N, size= N, 
                          colour= factor(Language))) + geom_boxplot()

lang2 + scale_y_log10(name="Language") +
  scale_x_discrete(name="Users") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_discrete(name="Language")

# 2. Top 10 customers with high Spending
One <- ggplot(dm[order(Total_Spend,decreasing = TRUE)[1:10],],
              aes(x=factor(Country),y = Total_Spend,colour=factor(Gender))) + geom_boxplot()

# 3. Top Countries with highest users
pie(slices$percent, labels=paste(slices$Country, "  ",slices$percent,"%", sep=""),
    col=rainbow(length(slices$Country)), main = "Percentage of Customers by \n language spoken")

# 4. Top 10 Users that bring Most Revenues according to Gender
One <- ggplot(dm[order(Total_Spend,decreasing = TRUE)[1:10],],
              aes(x=factor(UserID),y = Total_Spend,colour=factor(Gender))) +
  geom_boxplot()+facet_grid(.~Gender)+ theme(axis.text.x = element_text(angle = 90, hjust = 1) )

#-----------------------------------------------------
# Updated on 31 Dec 2015
#-----------------------------------------------------