#load relevant libraries
library(tidyverse)
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)
library(writexl)


#load data
WDI <- read_excel("WDI.xlsx")
View(WDI)

#renaming the columns
WDI <- WDI %>% rename(country=`Country Name`) %>% rename(code=`Country Code`) %>% rename(series=`Series Name`) %>% 
  rename(WB.Region =`W.B. Region`)

#converting data from wide to long format
WDI <- melt(WDI, id.vars = c("country", "code", "WB.Region", "series"), variable.name = "Years", value.name = "value")


#spreading the columns
WDI <- spread(WDI, key = "series", value = "value")

#again renaming columns
WDI <- WDI %>% rename(GDP="GDP (constant 2010 US$)") %>% rename(GNI ="GNI per capita, Atlas method (current US$)") %>% 
  rename(POP="Population, total")

#drop column
WDI <- WDI[,-c(2)]

#converting missing values with ".." to "NA"
is.na(WDI) <- WDI==".."

#drop rows in population column with NA
WDI <- WDI %>% drop_na(POP)


#exporting the manipulated data as excel
write_xlsx(WDI, path = "wdiApp.xlsx", col_names = TRUE)
