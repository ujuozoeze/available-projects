library(tidyverse)
library(dplyr)
library(esquisse)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(plotly)

options(scipen = 999)

#merge datasets

first_merge <- merge(agg.wks_TXIC, agg.wks_TXCC, by = c("short.date"))
second_merge <- merge(first_merge, agg.wks_PEUCCTX, by = c("short.date"))
third_merge <- merge(second_merge, agg.wks_PUAICTX, by = c("short.date"))
fourth_merge <- merge(third_merge, agg.wks_PUACCTX, by = c("short.date"))
fifth_merge <- merge(fourth_merge, agg.wks_Insured_UER, by = c("short.date"))
full_merge <- merge(fifth_merge, TLUNEMP, by =c("short.date"))

#rename datasets

str(full_merge)
 full_merge <- full_merge %>%
   rename(reg_IC = `join_allclaims$TXICLAIMS`,
          reg_CC = `join_allclaims$TXCCLAIMS`,
          PEUCC = `join_allclaims$PEUCCCTX`,
          PUAIC = `join_allclaims$PUACCTX`,
          PUACC = `join_allclaims$PUACCTX`)
 
 full_merge <- full_merge %>%
   rename(insured_uer = `join_allclaims$Insured_unemployment_rate`,
          PUAIC = `join_allclaims$PUAICTX`)
 
 full_merge <- full_merge %>%
   rename(week_ending = short.date)
 
 full_merge <- full_merge %>%
   rename(unemployment_rate = `unemployment rate`)
 
 
esquisser()


library(ggplot2)

ggplot(full_merge) +
 aes(x = week_ending, fill = PEUCC, colour = PUACC, weight = employment) +
 geom_bar() +
 scale_fill_viridis_c(option = "viridis") +
 scale_color_viridis_c(option = "viridis") +
 labs(title = "Pandemic Continued Claim vs Unemployment By Numbers", subtitle = "2020 - 2021", caption = "Data Sources: FRED & Bureau of Labor Statistics") +
 theme_minimal()

library(ggplot2)

ggplot(full_merge) +
 aes(x = week_ending, y = unemployment, fill = reg_CC, colour = PUACC, size = PEUCC) +
 geom_boxplot() +
 scale_fill_gradient() +
 scale_color_gradient() +
 theme_minimal()
#plots

ggplot(full_merge) +
  geom_point(mapping = aes(x = week_ending, y= unemployment_rate, color = insured_uer))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
 
ggplot(full_merge)+
  aes(x = week_ending, y= unemployment_rate, fill = insured_uer)+
  geom_line()

 ggplot(join_allclaims)+
   aes(x = week_ending, y = TXCCLAIMS, fill = PEUCCCTX, size = PUAICTX, colour = PUACCTX) +
   geom_line()+
   scale_color_viridis()+ labs(title = "Unemployment Claims by Week")+
   theme_bw()