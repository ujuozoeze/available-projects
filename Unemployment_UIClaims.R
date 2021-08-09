library(tidyverse)
library(dplyr)
library(viridis)
library(plotly)
library(data.table)

options(scipen = 999)

#join datasets

join_puadataframes <- left_join(PUAICTX, PUACCTX, by = c("week_ending"))
join_pandemicdataframes <- left_join(PEUCCCTX, join_puadataframes, by =c("week_ending"))

join_regularstateclaims <- left_join(oui_claims, TXICLAIMS, by = c("week_ending"))

join_regularstateclaims <- join_regularstateclaims %>%
   rename(TXCCLAIMS = `Continued Claims  (CC)`)

join_allclaims <- left_join(join_regularstateclaims, join_pandemicdataframes, by =c("week_ending"))

join_allclaims <- join_allclaims %>% relocate(TXICLAIMS, .before = TXCCLAIMS)

join_allclaims <- join_allclaims %>%
   rename(Covered_Employment = `Covered Employment`)

join_allclaims <- join_allclaims %>%
   rename(Insured_unemployment_rate = `Insured Unemployment Rate`)


#aggregate weeks into month

short.date = strftime(join_allclaims$week_ending, "%Y/%m")
agg.wks_TXIC = aggregate(join_allclaims$TXICLAIMS ~ short.date, FUN =  sum)
agg.wks_TXCC = aggregate(join_allclaims$TXCCLAIMS ~ short.date, FUN =  sum)
agg.wks_Insured_UER = aggregate(join_allclaims$Insured_unemployment_rate ~ short.date, FUN =  sum)
agg.wks_PEUCCTX = aggregate(join_allclaims$PEUCCCTX ~ short.date, FUN =  sum)
agg.wks_PUAICTX = aggregate(join_allclaims$PUAICTX ~ short.date, FUN =  sum)
agg.wks_PUACCTX = aggregate(join_allclaims$PUACCTX ~ short.date, FUN =  sum)

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
          PUACC = `join_allclaims$PUACCTX`,
          insured_uer = `join_allclaims$Insured_unemployment_rate`,
          month = short.date,
          unemployment_rate = `unemployment rate`)
 
 full_merge <- full_merge %>%
    rename(PUAIC = `join_allclaims$PUAICTX`)
 

#plots
 
str(full_merge)

ggplot(full_merge) +
 aes(x = month, fill = PEUCC, colour = PUACC, weight = unemployment) +
 geom_bar() +
 scale_fill_viridis_c(option = "viridis") +
 scale_color_viridis_c(option = "viridis") +
 labs(title = "Pandemic Continued Claims vs Unemployment By Numbers", subtitle = "2020 - 2021", caption = "Data Sources: FRED & Bureau of Labor Statistics") +
 theme_minimal()


p <- full_merge %>%
   ggplot( aes(x= insured_uer, y= unemployment_rate, fill = month)) +
   geom_area(fill="red3", alpha= 0.5) +
   geom_line(color="dark blue") +
   labs(title = "Insured Unemployment Rate versus Unemployment Rate by Months", subtitle = "March 2020 - June 2021",
        caption = "Data Sources: FRED & Bureau of Labor Statistics" )+
   xlab("Insured Unemployment Rate(Continued Claims divided by Covered Employment )")+
   ylab("Unemployment Rate")+
   theme_classic()

p <- ggplotly(p)
p

full_merge %>%
   mutate(PEUCC=as.numeric(PEUCC), PUACC = as.numeric(PUACC), unemployment=as.numeric(unemployment)) %>%
   gather(variable, value, -month) %>%
   ggplot(aes(x = month, y = value, color = variable, group = variable))+
   geom_line()+
   facet_wrap(~variable, scales = "free_y")+
   theme_gray()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))+
   labs(title = "Trend of Unemployment Claims and Variables", subtitle = "March 2020 - June 2021",
   caption = "Data Sources: FRED & Bureau of Labor Statistics" )

ggplot(full_merge) +
   aes(x = reg_IC, y = PUAIC, size=unemployment) +
   geom_point(colour = "#065caf") +
   labs(x = "Regular Initial Claims", y = "PUA Initial Claims", title = "Initial Claims and Unemployment by Numbers", subtitle = "March 2020 - June 2021", caption = "Data Sources: FRED & Bureau of Labor Statistics", size = "Unemployment") +
   theme_minimal() +
   facet_wrap(vars(month))


