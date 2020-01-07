remove(list = ls())
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)        
library(skimr)
library(caret)
library(plotrix)
library(RColorBrewer)
library(corrplot)
setwd('C:\\Users\\lenovo\\Desktop\\IRD')
data <- read.csv('online_shoppers_intention.csv')
str(data)

data$dochod <- ifelse(data$Revenue == 'TRUE', 'Tak', 'Nie' )
data$week <- ifelse(data$Weekend == 'TRUE', 'Tak', 'Nie')
#ANALIZA DANYCH#

skim(data)
na <- as.data.frame(sapply(data, function(x){sum(is.na(x))}))
na$zmienna1 <- na$`sapply(data, function(x) {     sum(is.na(x)) })`
na$zmienna <- rownames(na)

na <- na %>% select(zmienna, zmienna1) %>% filter(zmienna1 > 0)
data2 <- na.omit(data)
#zmienne binarne


ggplot(data = data2)+
  geom_bar(mapping = aes(x = dochod, fill = dochod))+
  xlab('Czy sesja przynios³a dochód?')+ scale_fill_brewer(palette="Dark2")+
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none")


prop.table(table(data$dochod))

ggplot(data = data2)+
  geom_bar(mapping = aes(x = week, fill = week))+
  xlab('Czy sesja by³a w weekend?')+ scale_fill_brewer(palette="Dark2")+
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none")+
  ggtitle('Rozk³ad sesji ze wzglêdu na dzieñ pracuj¹cy i weekend')


  ggplot(data = data2)+
    geom_bar(mapping = aes(x = week, fill = dochod), position = 'fill')+
    xlab('Czy sesja by³a w weekend?')+ scale_fill_brewer(palette="Set1")+
  theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany rozk³ad sesji przynosz¹cych dochód \n (ze wzglêdu na dzieñ tygodnia)')
  
  prop.table(table(data2$week))
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = SpecialDay))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    scale_x_continuous( breaks=seq(0,1, 0.2))+
    ggtitle('Iloœæ sesji ze wzglêdu na bliskoœæ w czasie dni specjalnych')
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = SpecialDay, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    scale_x_continuous( breaks=seq(0,1, 0.2))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na bliskoœæ do dni specjalnych)')
  
 data2$Month <- as.Date(data2$Month)
  
 data2$Miesiac <- ifelse(data2$Month == 'Aug', 'August', ifelse(
   data2$Month == 'Dec', 'December', ifelse(
     data2$Month == 'Feb', "February", ifelse(
       data2$Month == 'Jul', 'July', ifelse(
         data2$Month == 'June', 'June', ifelse(
           data2$Month == "Mar", "March", ifelse(
             data2$Month == 'May', 'May', ifelse(
               data2$Month == 'Nov', 'November', ifelse(
                 data2$Month == 'Oct', 'October', 'September'
               )
             )
           )
         )
       )
     )
   )
 ))
 
 data2$Miesiac <- factor(data2$Miesiac, levels = month.name)
 

 
 ggplot(data = data2)+
    geom_bar(mapping = aes(x = Miesiac, fill = Miesiac))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    ggtitle('Iloœæ sesji ze wzglêdu na miesi¹c')
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Miesiac, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na miesi¹c)')
  
  
  table(data2$OperatingSystems)
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = OperatingSystems, fill = OperatingSystems))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    scale_x_continuous(name="Systemy operacyjne", breaks=seq(0,8))+
    ggtitle('Iloœæ sesji ze wzglêdu na system operacyjny')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = OperatingSystems, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    scale_x_continuous(name="Systemy operacyjne", breaks=seq(0,8))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na system operacyjny)')
  
  


  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Browser, fill = Browser))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    scale_x_continuous(name="Typ przegl¹darki", breaks=seq(0,13))+
    ggtitle('Iloœæ sesji ze wzglêdu na przegl¹darke')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Browser, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    scale_x_continuous(name="Typ przegl¹darki", breaks=seq(0,13))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na przegl¹darke)')
  
  
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Region, fill = Region))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    scale_x_continuous(name="Region", breaks=seq(0,9))+
    ggtitle('Iloœæ sesji ze wzglêdu na region')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Region, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    scale_x_continuous(name="Region", breaks=seq(0,13))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na region)')
  
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = TrafficType))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    scale_x_continuous(name="Rodzaj ruchu w sieci", breaks=seq(0,20))+
    ggtitle('Iloœæ sesji ze wzglêdu na rodzaj ruchu w sieci')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = TrafficType, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    scale_x_continuous(name="Rodzaj ruchu w sieci", breaks=seq(0,20))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na rodzaj ruchu w sieci)')
table(data2$TrafficType)  




ggplot(data = data2)+
  geom_bar(mapping = aes(x = VisitorType))+
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none")+
  ggtitle('Iloœæ sesji ze wzglêdu na typ u¿ytkownika')


ggplot(data = data2)+
  geom_bar(mapping = aes(x = VisitorType, fill = dochod), position = 'fill')+
  scale_fill_brewer(palette="Set1")+
  theme(panel.background = element_rect(fill = 'white'))+
  ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na typ u¿ytkownika)')
table(data2$TrafficType)  




table(data2$Administrative)

ggplot(data = data2)+
  geom_bar(mapping = aes(x = Administrative))+
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none")+
  ggtitle('Iloœæ sesji ze wzglêdu na iloœæ \n odwiedzonych stron administruj¹cych')


ggplot(data = data2)+
  geom_bar(mapping = aes(x = Administrative, fill = dochod), position = 'fill')+
  scale_fill_brewer(palette="Set1")+
  theme(panel.background = element_rect(fill = 'white'))+
  ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na iloœæ odwiedzonych stron administruj¹cych)')



ggplot(data = data2)+
  geom_histogram(mapping = aes(x = Administrative_Duration), binwidth = 10)+
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none")+ylim(0,150)+
  ggtitle('Iloœæ sesji ze wzglêdu na czas spêdzony \n na stronach administracyjnych')


  histogram(data2$Administrative_Duration,
            xlab = 'Administratie Duration',
            main = 'Procentowy rozk³ad sesji ze wzglêdu na czas spêdzony \n na stronach administracyjnych')
  
  ggplot(data = data2)+
  geom_histogram(mapping = aes(x = Administrative_Duration, fill = dochod),
           position = 'fill', binwidth = 75)+
  scale_fill_brewer(palette="Set1")+xlim(0,1700)+
  theme(panel.background = element_rect(fill = 'white'))+
  ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na czas spêdzony na stronach administracyjnych)')

  
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Informational))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    ggtitle('Iloœæ sesji ze wzglêdu na iloœæ \n odwiedzonych stron informacyjnych')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = Informational, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+xlim(0,15)+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na iloœæ odwiedzonych stron informacyjnych)')
  
  
  
  
  histogram(data2$Informational_Duration,
            xlab = 'Informational Duration',
            main = 'Procentowy rozk³ad sesji ze wzglêdu na czas spêdzony \n na stronach informacyjnych')
  
  ggplot(data = data2)+
    geom_histogram(mapping = aes(x = Informational_Duration, fill = dochod),
                   position = 'fill', binwidth = 75)+
    scale_fill_brewer(palette="Set1")+xlim(0,1500)+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na czas spêdzony na stronach informacyjnych)')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = ProductRelated))+
    theme(panel.background = element_rect(fill = 'white'),
          legend.position = "none")+
    ggtitle('Iloœæ sesji ze wzglêdu na iloœæ \n odwiedzonych stron zwi¹zaych z produktem')
  
  
  ggplot(data = data2)+
    geom_bar(mapping = aes(x = ProductRelated, fill = dochod), position = 'fill')+
    scale_fill_brewer(palette="Set1")+xlim(0,50)+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na iloœæ odwiedzonych stron zwi¹zanych z produktem)')
  
  
  
  histogram(data2$ProductRelated_Duration,
            xlab = 'Product Related Duration',
            main = 'Procentowy rozk³ad sesji ze wzglêdu na czas spêdzony \n na stronach zwi¹zanych z produktem')
  
  ggplot(data = data2)+
    geom_histogram(mapping = aes(x = ProductRelated_Duration, fill = dochod),
                   position = 'fill', binwidth = 75)+
    scale_fill_brewer(palette="Set1")+xlim(0,1500)+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na czas spêdzony na stronach informacyjnych)')
  
  
  
  histogram(data2$BounceRates,
            xlab = 'Bounce Rates',
            main = 'Procentowy rozk³ad sesji ze wzglêdu na bounce rate')
  
  ggplot(data = data2)+
    geom_histogram(mapping = aes(x = BounceRates, fill = dochod),
                   position = 'fill', binwidth = 0.01)+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na bounce rate)')
  
  
  mean(data2$ExitRates)
  
  histogram(data2$ExitRates,
            xlab = 'Bounce Rates',
            main = 'Procentowy rozk³ad sesji ze wzglêdu na exit rates')
  
  ggplot(data = data2)+
    geom_histogram(mapping = aes(x = ExitRates, fill = dochod),
                   position = 'fill', binwidth = 0.01)+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na exit rates)')
  
  histogram(data2$PageValues,
            xlab = 'Bounce Rates',
            main = 'Procentowy rozk³ad sesji ze wzglêdu na page value')
  
  ggplot(data = data2)+
    geom_histogram(mapping = aes(x = PageValues, fill = dochod),
                   position = 'fill')+ xlim(0,100)+
    scale_fill_brewer(palette="Set1")+
    theme(panel.background = element_rect(fill = 'white'))+
    ggtitle('Skumulowany wykres sesji przynosz¹cych dochód\n (ze wzglêdu na page value)')
  
  
  
  data <- read.csv('online_shoppers_intention.csv')
  data2 <- na.omit(data)
  str(data2)
  
  
  data2$week <- ifelse(data2$Weekend == 'TRUE', 1, 0)
  
  cor <- cor(data2[,c('Administrative', 'Administrative_Duration',
                   'Informational', 'Informational_Duration',
                   'ProductRelated', 'ProductRelated_Duration',
                   'BounceRates', 'ExitRates',
                   'PageValues', 'SpecialDay')])
corrplot(cor, method = 'color', type = 'lower', addCoef.col = 'black',
         tl.srt=45)  
cor
