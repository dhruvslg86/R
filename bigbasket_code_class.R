# install.packages("pacman")
library(pacman)
pacman::p_load(xlsx,gdata,openxlsx,readxl, tibble, plyr, dplyr, arules, arulesViz, ggplot2)
#setwd("E:\\IMT Official\\Teaching\\IMT Offerings\\PGDM Part Time\\BA\\Term IX 2017-2020\\Teaching\\Session 5 and 6")
big_data <- read.csv("IMB575-XLS-ENG.csv", header = T, stringsAsFactors = F)
big_data2 <- as.data.frame(big_data)

all(big_data == big_data2)
rm("big_data")
str(big_data2)

unique(is.na(big_data2))

big_data2 <- big_data2[complete.cases(big_data2), ]
big_data2 %>% mutate(Description = as.factor(Description))
big_data2$Date <- as.Date(big_data2$Created.On, format='%d-%m-%Y')
big_data2$Time <- substr(big_data2$Created.On, start = 12, stop = 16)
str(big_data2)
head(big_data2)

big_data2$Time <- as.factor(big_data2$Time)
length(unique(big_data2$Time))

ggplot(data = big_data2, aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

temp <- head(big_data2, 500)
ggplot(data = temp, aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")
rm("temp")

detach("package:plyr", unload=TRUE)

big_data2 %>% 
  group_by(Member) %>% 
  summarize(n_Orders = mean(Order)) %>%
  ggplot(aes(x=n_Orders))+
  geom_histogram(fill="indianred", bins = 100) + 
  geom_rug()

big_data2 %>%
  group_by(SKU, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

big_data2 %>%
  group_by(Member, SKU, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

big_data2 %>%
  group_by(SKU, Description) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

## Association Rule

big_data2_sorted <- big_data2[order(big_data2$Member),]
library(plyr)
itemList <- ddply(big_data2,c("Member","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$Member <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.05, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules[1:10])

rules <- apriori(tr, parameter = list(supp=0.01, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)
plot(rules)

plot(topRules, method="graph")
plot(rules, method = "graph")

plot(topRules, method = "grouped")


