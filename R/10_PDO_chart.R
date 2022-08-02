

library(here)
library(tidyverse)

## PDO clean file name
pdo <- "pdo_data_for_analysis.csv"

## raw file locations
pdo <- here("data/clean", pdo)

pdo <- read_csv(pdo,
                   na = c("", "NA"))


ggplot(data = pdo, aes(x=year, y=pdo))+
  theme_classic()+
  scale_x_continuous(expand=c(0,0),limits = c(1999,2019))+ 
  scale_y_continuous(limits = c(-2,1.2))+ 
  labs(x= "year",y= "PDO",title="")+
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=0.5)+
  geom_area(mapping = aes(y = ifelse(pdo>0 & pdo< 2, yes=pdo, no=pdo)), fill = "#e41a1c")+
  geom_area(mapping = aes(y = ifelse(pdo>-2 & pdo< 0, yes=pdo, no=0)), fill = "#377eb8")

            
ggplot(data = pdo, aes(x=year, y=pdo))+
  theme_classic()+
  scale_x_continuous(expand=c(0,0),limits = c(1999,2019))+ 
  scale_y_continuous(limits = c(-2,1.2))+ 
  labs(x= "year",y= "PDO",title="")+
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=0.5)+
  geom_area(fill="#e41a1c")
          
  

  
