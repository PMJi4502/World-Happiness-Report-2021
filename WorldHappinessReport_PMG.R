#World Happiness Report

#World Happiness Report is a publication of the United Nations Sustainable Development Solution Network
#Respondents rate their own lives on various factors
#The report primarily uses data from the Gallup World Poll

#Factors Affecting Happiness Score
# 1. GDP
# 2. Freedom of choice
# 3. Health Life Expectancy
# 4. Social Support
# 5. Generosity
# 6. Corruption

#libraries used

# We have used DPLYR Library for finding out top 10 countries according to each variable
library(dplyr)


# We have used GGPLOT Library for making aesthetic plots
library(ggplot2)


# We have used E1071 Library for calculating advanced statistics like kurtosis and skewness
library(e1071)

# We have used these packages to insert images
library(png)


# We have used this package to convert a ggplot to an image
library(patchwork)

#Importing the data

hr<- read.csv(file='C:\\Users\\premm\\OneDrive\\Desktop\\Happinessreport.csv')


# Exploring the data
head(hr)
View(hr)
dim(hr)
str(hr)

# Finding out descriptive statistics
summary(hr)

# Data Visualization and advanced statistics



# We have used DPLYR Library for finding out top 10 countries according to each variable


#Density Distribution of each factor for all countries

#Density Distribution of Happiness Score for all countries
kurtosis(hr$happiness_score)
skewness(hr$happiness_score)
ggplot(data=hr)+ geom_density(aes(x=happiness_score), fill="grey50") + 
  ggtitle("Happiness Score Density Distribution") + 
  labs (x="Happiness Score",y= "Density")+ theme_bw()

#The distribution is a platykurtic distribution
#The distribution is a bimodal left skewed distribution

#Density Distribution of GDP for all countries
kurtosis(hr$logged_GDP)
skewness(hr$logged_GDP)
ggplot(data=hr)+ geom_density(aes(x=logged_GDP), fill="grey50") + 
  ggtitle("GDP Density Distribution") + 
  labs (x="Logged GDP",y= "Density")+ theme_bw()

#The distribution is a platykurtic distribution
#The distribution is a bimodal moderately left skewed distribution

#Density Distribution of Social Support for all countries
kurtosis(hr$social_support)
skewness(hr$social_support)
ggplot(data=hr)+ geom_density(aes(x=social_support), fill="grey50") + 
  ggtitle("Social Support Density Distribution") + 
  labs (x="Social Support",y= "Density")+ theme_bw()

# The distribution is a leptokurtic distribution
# The distribution is a left skewed distribution

#Density Distribution of Life Expectancy for all countries
kurtosis(hr$life_expectancy)
skewness(hr$life_expectancy)
ggplot(data=hr)+ geom_density(aes(x=life_expectancy), fill="grey50") + 
  ggtitle("Life Expectancy Density Distribution") + 
  labs (x="Life Expectancy",y= "Density")+ theme_bw()
# The distribution is a platykurtic distribution
# The distribution is a left skewed distribution

#Density Distribution of Freedom of choice for all countries
kurtosis(hr$freedom)
skewness(hr$freedom)
ggplot(data=hr)+ geom_density(aes(x=freedom), fill="grey50") + 
  ggtitle("Freefom of choice Density Distribution") + 
  labs (x="Freedom of choice",y= "Density")+ theme_bw()
# The distribution is a leptokurtic distribution
# The distribution is a bimodal left tailed distribution


#Density Distribution of Generosity for all countries
kurtosis(hr$generosity)
skewness(hr$generosity)
ggplot(data=hr)+ geom_density(aes(x=generosity), fill="grey50") + 
  ggtitle("Generosity Density Distribution") + 
  labs (x="Generosity",y= "Density")+ theme_bw()
# The distribution is a leptokurtic distribution
# The distribution is a Right tailed distribution


#Density Distribution of Corruption for all countries
kurtosis(hr$corruption)


skewness(hr$corruption)

ggplot(data=hr)+ geom_density(aes(x=corruption), fill="grey50") + 
  ggtitle("Corruption Density Distribution") + 
  labs (x="Corruption",y= "Density")+ theme_bw()
# The distribution is a leptokurtic distribution
# The distribution is a left tailed distribution


# Top 10 countries According to each factor

# Top 10 Countries based on Happiness Score
top10_hs <- hr %>% top_n(10,hr$happiness_score)


top10_happiness <- data.frame(top10_hs$country,top10_hs$happiness_score)

top10_happiness %>% 
  ggplot(aes(reorder(top10_hs.country, top10_hs.happiness_score ), top10_hs.happiness_score)) + 
  geom_col(aes(fill = top10_hs.happiness_score)) + 
  scale_fill_gradient2(
    high = "blue", midpoint =(min(top10_happiness$top10_hs.happiness_score)-1))+ 
  coord_flip() + 
  labs(x = "Happiness Score",y="Top 10 Countries") + 
  ggtitle("Top 10 Countries with Max Happiness score")+ theme_bw()

# Top 10 countries According to logged_GDP

top10_gdp <- hr %>% top_n(10,hr$logged_GDP)
top10_lgdp <- data.frame(top10_gdp$country,top10_gdp$logged_GDP)
top10_lgdp
top10_lgdp %>% 
  ggplot(aes(reorder(top10_gdp.country, top10_gdp.logged_GDP ), top10_gdp.logged_GDP)) + 
  geom_col(aes(fill = top10_gdp.logged_GDP)) + 
  scale_fill_gradient2(
    high = "green", midpoint =(min(top10_lgdp$top10_gdp.logged_GDP)-0.5))+ 
  coord_flip() + 
  labs(x = "Logged GDP",y="Top 10 Countries") + 
  ggtitle("Top 10 Countries with Max Logged GDP")+ theme_bw()

# Top 10 countries According to Social Support

top10_ss <- hr %>% top_n(10,hr$social_support)
top10_support <- data.frame(top10_ss$country,top10_ss$social_support)
top10_support
top10_support %>% 
  ggplot(aes(reorder(top10_ss.country, top10_ss.social_support ), top10_ss.social_support)) + 
  geom_col(aes(fill = top10_ss.social_support)) + 
  scale_fill_gradient2(
    high = "red", midpoint =(min(top10_support$top10_ss.social_support)-0.05))+ 
  coord_flip() + 
  labs(x = "Social Support",y="Top 10 Countries")+ 
  ggtitle("Top 10 Countries with Max Social Support")+ theme_bw()

#ggplot(aes(top10_ss.country, top10_ss.social_support ), top10_ss.social_support)) + geom_line()


# Top 10 countries According to Life Expectancy

top10_le <- hr %>% top_n(10,hr$life_expectancy)
top10_life <- data.frame(top10_le$country,top10_le$life_expectancy)
top10_life

top10_life %>% 
  ggplot(aes(reorder(top10_le.country, top10_le.life_expectancy ), top10_le.life_expectancy)) + 
  geom_col(aes(fill = top10_le.life_expectancy)) + 
  scale_fill_gradient2(
    high = "orange", midpoint =(min(top10_life$top10_le.life_expectancy)-2))+ 
  coord_flip() + 
  labs(x = "Life Expectancy",y="Top 10 Countries")+ 
  ggtitle("Top 10 Countries with Max Life Expectancy")+ theme_bw()


# Top 10 countries According to Freedom of Choice

top10_fd <- hr %>% top_n(10,hr$freedom)
top10_free <- data.frame(top10_fd$country,top10_fd$freedom)
top10_free
top10_free %>% 
  ggplot(aes(reorder(top10_fd.country, top10_fd.freedom ), top10_fd.freedom)) + 
  geom_col(aes(fill = top10_fd.freedom)) + 
  scale_fill_gradient2(
    high = "pink", midpoint =(min(top10_free$top10_fd.freedom)-0.05))+ 
  coord_flip() + 
  labs(x = "Freedom",y="Top 10 Countries") + theme_bw() +
  ggtitle("Top 10 Countries with Max Freedom of Choice") 


# Top 10 countries According to Generosity

top10_ge <- hr %>% top_n(10,hr$generosity)
top10_generous <- data.frame(top10_ge$country,top10_ge$generosity)
top10_generous
top10_generous %>% 
  ggplot(aes(reorder(top10_ge.country, top10_ge.generosity ), top10_ge.generosity)) + 
  geom_col(aes(fill = top10_ge.generosity)) + 
  scale_fill_gradient2(
    high = "yellow", midpoint =(min(top10_generous$top10_ge.generosity)-0.5))+ 
  coord_flip() + 
  labs(x = "Generosity",y="Top 10 Countries")+ 
  ggtitle("Top 10 Countries with Max Generosity") + theme_bw()

# Top 10 countries with least Corruption

top10_cr <- hr %>% top_n(-10,hr$corruption)
top10_corrupt <- data.frame(top10_cr$country,top10_cr$corruption)
top10_corrupt
top10_corrupt %>%
  ggplot(aes(reorder(top10_cr.country, desc(top10_cr.corruption) ), top10_cr.corruption)) + 
  geom_col(aes(fill = top10_cr.corruption)) + 
  scale_fill_gradient2(
    low = "violet", midpoint =(max(top10_corrupt$top10_cr.corruption)+0.05))+ 
  coord_flip() + 
  labs(x = "Corruption",y="Top 10 Countries")+ 
  ggtitle("Top 10 Countries with least Corruption") + theme_bw()


# Correlation between Happiness Score and All the factors

# Logged GDP vs Happiness Score
cor(hr$logged_GDP,hr$happiness_score)
ggplot(hr, aes(x=logged_GDP,y=happiness_score))+ geom_point() +  
  labs(x = "Logged GDP",y="Happiness Score") + theme_bw() +
  ggtitle("Logged GDP vs Happiness Score")
#Logged GDP and Happiness Score is directly proportional and has a strong positive correlation

# Social Support vs Happiness Score
cor(hr$social_support,hr$happiness_score)
ggplot(hr, aes(x=social_support,y=happiness_score))+ geom_point() +  
  labs(x = "Social Support",y="Happiness Score") + theme_bw() +
  ggtitle("Social Support vs Happiness Score")
#Social Support and Happiness Score is directly proportional and has a strong positive correlation

#  Life Expectancy vs Happiness Score
cor(hr$life_expectancy,hr$happiness_score)
ggplot(hr, aes(x=life_expectancy,y=happiness_score))+ geom_point() +  
  labs(x = "Life Expectancy",y="Happiness Score") + theme_bw()+
  ggtitle(" Life Expectancy vs Happiness Score")
#Life Expectancy and Happiness Score is directly proportional and has a strong positive correlation


#  Freedom of choice vs Happiness Score
cor(hr$freedom,hr$happiness_score)
ggplot(hr, aes(x=freedom,y=happiness_score))+ geom_point() +  labs(x = "Freedom of Choice",y="Happiness Score") +
  ggtitle("Freedom of choice vs Happiness Score") + theme_bw()
#Freedom of Choice and Happiness Score is directly proportional and has a moderate positive correlation

#  Generosity vs Happiness Score
cor(hr$generosity,hr$happiness_score)
ggplot(hr, aes(x=generosity,y=happiness_score))+ geom_point() +  labs(x = "Generosity",y="Happiness Score") +
  ggtitle("Generosity vs Happiness Score") + theme_bw()
# Generosity and Happiness Score has no correlation between them

#  Corruption vs Happiness Score
cor(hr$corruption,hr$happiness_score)
ggplot(hr, aes(x=corruption,y=happiness_score))+ geom_point() +  labs(x = "Corruption",y="Happiness Score") +
  ggtitle("Corruption vs Happiness Score")  + theme_bw()
# Corruption and Happiness Score are inversely proportional and has a moderate negative correlation

#Region Wise Comparison


#grouping regions and arranging them in descending order of happiness
hr %>% filter(happiness_score>5.533) %>% group_by(regions) %>% summarize(avg_happiness=mean(happiness_score)) %>% arrange(desc(avg_happiness)) -> happ_region
happ_region
happ_region


# Violin Plot to demonstrate the distribution of Happiness score region wise

ggplot(hr, aes(x=happiness_score, y=regions, fill=regions)) + 
  geom_violin(scale='width') +geom_boxplot(width=0.1) + 
  ggtitle("Regionwise Volin Plot") + 
  labs (x="Happiness Score",y= "Regions")

#Insights:
#1. It can be easily seen that Western European countries have the Maximum Happiness score followed by North America and ANZ
#2. While Regions like South Asia, Middle East and North Africa and Latin America and Caribbean has the maximum spread
#3. Middle East and North Africa has highly Right Skewed Distribution
#4. South Asia has highly Left skewed Distribution

# Region wise Bi variate analysis of factors affecting Happiness Score

# Region wise relationship between Logged GDP vs Happiness Score
ggplot(hr, aes(x=logged_GDP,y=happiness_score, color=regions))+ geom_point() +  labs(x = "Logged_GDP",y="Happiness Score") +
  ggtitle("Region wise relationship between Logged GDP vs Happiness Score") + facet_wrap(~regions)

# Region wise relationship between Social Support vs Happiness Score
ggplot(hr, aes(x=social_support,y=happiness_score, color=regions))+ geom_point() +  labs(x = "Social Support",y="Happiness Score") +
  ggtitle("Region wise relationship between Social Support vs Happiness Score") + facet_wrap(~regions)

# Region wise relationship between Life Expectancy vs Happiness Score
ggplot(hr, aes(x=life_expectancy,y=happiness_score, color=regions))+ geom_point() +  labs(x = "Life Expectancy",y="Happiness Score") +
  ggtitle("Region wise relationship Life Expectancy vs Happiness Score") + facet_wrap(~regions)

# Region wise relationship between Freedom of Choice vs Happiness Score
ggplot(hr, aes(x=freedom,y=happiness_score, color=regions))+ geom_point() +  labs(x = "Freedom of Choice",y="Happiness Score") +
  ggtitle("Region wise relationship between Freedom of Choice vs Happiness Score") + facet_wrap(~regions)

# Region wise relationship between Corruption vs Happiness Score
ggplot(hr, aes(x=corruption,y=happiness_score, color=regions))+ geom_point() +  labs(x = "Corruption",y="Happiness Score") +
  ggtitle("Region wise relationship between Corruption vs Happiness Score") + facet_wrap(~regions)

# All these above graphs shows that WHY Western Europe and North America and ANZ are the happiest countries and Middle East and North Africa is growing

## India vs Others (Considering Germany, UK, US and Canada for comparison as most of the Indians migrate there for better opportunities)

indvothers<- filter(hr, country=="India" | country=="United States" | country=="United Kingdom" | country=="Canada" | country=="Germany")
indvothers


#IND vs others Happiness Score comparison

ggplot(data=hr)+ geom_density(aes(x=happiness_score), fill="grey") + 
  geom_vline(data = indvothers,aes(xintercept=happiness_score[1],color="Germany"), linetype="dashed", size=1) +
  geom_vline(data = indvothers,aes(xintercept=happiness_score[2],color="Canada"), linetype="dashed", size=1) +
  geom_vline(data = indvothers,aes(xintercept=happiness_score[3],color="UK"), linetype="dashed", size=1) +
  geom_vline(data = indvothers,aes(xintercept=happiness_score[4],color="US"), linetype="dashed", size=1) +
  geom_vline(data = indvothers,aes(xintercept=happiness_score[5],color="India"), linetype="dashed", size=1) +
  scale_color_manual(name = "Country", values = c(Germany="red",Canada="yellow",UK="green",US="blue",India="violet"))+
  labs(x="Happiness Score", y="Density") +
  geom_text(aes(3.75,0.2 , label = "India", angle=90,hjust=-0.5)) +
  geom_text(aes(7.2,0.25 , label = "Germany",angle=90, hjust=-0.5)) +
  geom_text(aes(7.10,0.2 , label = "Canada",angle=90, hjust=-1)) +
  geom_text(aes(7,0.185 , label = "UK",angle=90, hjust=0.5)) +
  geom_text(aes(6.9,0.175 , label = "US",angle=90, hjust=0.5)) + theme_bw() +
  theme(legend.position=c(0.08,0.8),legend.background = element_rect(fill="lightblue",
                                                                     size=0.5, linetype="solid", 
                                                                     colour ="darkblue"))+
  ggtitle("INDIA vs others Happiness Score comparison")


#IND vs others all factors comparison


ggplot(indvothers,aes(x=country,y= happiness_score)) + geom_bar( stat="identity", fill='green') + 
  coord_flip() + labs(x='Country',y="Happiness Score") +
  ggtitle("India Vs Others HappinessScore comparision")

# This graph demonstrates that India is quite lagging in terms of Happiness Score
# Now lets check the factors where India is lagging

ggplot(indvothers,aes(x=country,y= logged_GDP)) + geom_bar( stat="identity", fill='blue') + 
  coord_flip() + labs(x='Country',y="GDP") +
  ggtitle("India Vs Others GDP comparision")

ggplot(indvothers,aes(x=country,y=life_expectancy )) + geom_bar( stat="identity", fill='Red') + 
  coord_flip() + labs(x="Country",y="Life Expectancy") +
  ggtitle("India Vs Others Life Expectancy comparision")

ggplot(indvothers,aes(x=country,y=corruption )) + geom_bar( stat="identity", fill='pink') + 
  coord_flip() + labs(x="Country",y="Corruption") +
  ggtitle("India Vs Others Corruption Comparision")

ggplot(indvothers,aes(x=country,y=freedom )) + geom_bar( stat="identity", fill='yellow') + 
  coord_flip() + labs(x="Country",y="Freedom") +
  ggtitle("India Vs Others Freedom Comparision")

ggplot(indvothers,aes(x=country,y=social_support )) + geom_bar( stat="identity", fill='purple') + 
  coord_flip() + labs(x="Country",y="Freedom") +
  ggtitle("India Vs Others Social Support Comparision")


ggplot(indvothers,aes(x=country,y=life_expectancy )) + geom_bar( stat="identity", fill='green') + 
  coord_flip() + labs(x="Country",y="Life Expectancy") +
  ggtitle("India Vs Others Life Expectancy comparision")



# From the above graphs it is pretty evident that Except Freedom of choice India has to improve in every other factor
# In other words India is lagging behind due to:
# 1. The idea of rising expectations
# 2. Increasing Inequality
# 3. Widening of the Economic gap
# 4. Unending Corruption

# So why do INDIANS migrate to these Countries
# 1. Better Standards of living
# 2. Better Income Opportunities
# 3. Education Proficiency
# 4. Better governance 


