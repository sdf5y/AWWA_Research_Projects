setwd("C:/Users/sfranco/Documents/Python/SDWIS") 
#install.packages('ggplot', 'dplyr', 'readxl','readr', 'stringr')
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(vtable)
library(ggridges)
library(data.table)
library(stargazer)
library(xtable)
library(lmtest)
library(MASS)
library(forcats)

states <- c("AK", "AL", "AR", "AZ", "CA", "CO", 
  "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", 
  "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", 
  "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
  "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", 
  "WA", "WI", "WV", "WY")

#Census Regions and Divisions prep -----

census_regions <- list(
  Northeast = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
  Midwest = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  South = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX", "DC"),
  West = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
)

census_divisions <- list(
  NewEngland = c("CT", "ME", "MA", "NH", "RI", "VT"),
  MiddleAtlantic = c("NJ", "NY", "PA"),
  EastNorthCentral = c("IL", "IN", "MI", "OH", "WI"),
  WestNorthCentral = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  SouthAtlantic = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "DC"),
  EastSouthCentral = c("AL", "KY", "MS", "TN"),
  WestSouthCentral = c("AR", "LA", "OK", "TX"),
  Mountain = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
  Pacific = c("AK", "CA", "HI", "OR", "WA")
)

find_region <- function(state_abbr) {
  region <- NA
  for (r in names(census_regions)) {
    if (state_abbr %in% census_regions[[r]]) {
      region <- r
      break
    }
  }
  return(region)
}

find_division <- function(state_abbr) {
  division <- NA
  for (d in names(census_divisions)) {
    if (state_abbr %in% census_divisions[[d]]) {
      division <- d
      break
    }
  }
  return(division)
}

#Load Dataset ----

arsenic <- read_delim("arsenic.txt", delim = "\t", 
                      escape_double = FALSE, trim_ws = TRUE)

#write.table(arsenic, file = "arsenic_df.txt", sep = '\t')

as.factor(arsenic$`State Code`)
arsenic <- subset(arsenic, `State Code` %in% states)

arsenic$Region <- sapply(arsenic$`State Code`, find_region)
arsenic$Division <- sapply(arsenic$`State Code`, find_division)
arsenic$`Sample Collection Date` <- as.Date(arsenic$`Sample Collection Date`)

unique(arsenic$`Detection Limit Unit`)
unique(arsenic$Unit)

arsenic <- subset(arsenic, `Detection Limit Unit` != 'NTU')

arsenic <- arsenic %>%
  mutate( `Detection Limit Value` = case_when(`Detection Limit Unit` %in% c('UG/L', 'ug/l') ~ `Detection Limit Value` * 0.001,
      TRUE ~ `Detection Limit Value`) )%>%
  mutate( `Detection Limit Unit` = case_when(`Detection Limit Unit` %in% c('UG/L', 'ug/l') ~ 'MG/L'))

arsenic$`Detection Limit Value` = 0.01

arsenic$'Amount Over Limit' <- arsenic$Value - arsenic$`Detection Limit Value`

table(arsenic$Division, (arsenic$Detect)) 

table(arsenic$Region, (arsenic$Detect))

detection_ratio <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`)) %>%
  group_by(Year, Region) %>%
  summarize(ratio_dect = 100*sum(Detect == 1) / n(),
            number = sum(arsenic$Detect )  )

#PLOT Quartiles By State at a Given Year----
new_df <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`))%>%
  filter(Year == 2011)%>%
  group_by(`State Code`, Year) %>%
  summarise(Q1 = quantile(Value, probs = 0.25, na.rm = TRUE),
            Q2 = quantile(Value, probs = 0.5, na.rm = TRUE),
            Q3 = quantile(Value, probs = 0.75, na.rm = TRUE),
            IQR = c(Q1, Q2, Q3))%>%
  filter(!is.na(IQR)) 

new_df %>%
  filter(!`State Code` %in% c('AK','OH','SC')) %>%
ggplot( aes(x = reorder(`State Code`, `IQR`),
           y = IQR)) +
  geom_line() +
  geom_line(aes(x = reorder(`State Code`, Q2), 
                           y = Q2),
            color = "blue", size = 1, linetype = "dashed") +
  geom_point()+
  labs(title = "Arsenic Quartile Values In 2011 by State for All Source Water Types",
       x = "State",
       y = "Arsenic Detection Value (MG/L)")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5),    
        plot.title = element_text(hjust = 0.5),    
        text = element_text(size = 16),
        panel.grid = element_line(size = .001,
                                  linetype = 1)) +
  geom_hline(yintercept = 0.01, color = "orange", size = 0.75) +
  geom_hline(yintercept = 0.003, color = 'red', size = 0.75) 

#PLOT Percentile Values By Region in Time ------
quartile_data <- arsenic %>%
  mutate(
    Year = year(`Sample Collection Date`),
    Value = as.numeric(Value) ) %>%
  group_by(Region, Year) %>%
  summarize(
    Q1 = quantile(Value, probs = 0.25, na.rm = TRUE),
    Q2 = quantile(Value, probs = 0.5, na.rm = TRUE),
    Q3 = quantile(Value, probs = 0.75, na.rm = TRUE)  )

ggplot(quartile_data, aes(x = Year, 
                          color = `Region`)) +
  geom_line(aes(y = Q1, linetype = "25%"), size = 1.5, alpha = 0.9) +
  geom_line(aes(y = Q2, linetype = "50%"), size = 1.5) +
  geom_line(aes(y = Q3, linetype = "75%"), size = 1.5, alpha = 0.9) +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Arsenic Percentile Values From 2006 to 2011 by Region for All Source Water Types",
       x = "Year",
       y = "Arsenic Detection Value (MG/L)",
       color = "Region",
       linetype = "Percentile") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5),    
        plot.title = element_text(hjust = 0.5),    
        text = element_text(size = 16)) +
  geom_hline(yintercept = 0.01, color = "orange", size = 0.75) +
  geom_hline(yintercept = 0.003, color = 'red', size = 0.75) +
  scale_linetype_manual(name = "Percentile",
                        values = c("25%" = "dotted", "50%" = "solid", "75%" = "dashed"))

#PLOT Percentile Values By Region and Water Type in Time ------
quartile_data <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`), 
         Value = as.numeric(Value)) %>%
  group_by(Region, Year, `Source Water Type`) %>%
  summarize(Q1 = quantile(Value, probs = 0.25, na.rm = TRUE),
            Q2 = quantile(Value, probs = 0.5, na.rm = TRUE),
            Q3 = quantile(Value, probs = 0.75, na.rm = TRUE)) %>%
  mutate(`Source Water rank` = recode(`Source Water Type`,
                                 "GW" = "GW",
                                 "GU" = "GU",
                                 "SW" = "SW",
                                 .default = as.character(`Source Water Type`)) %>%
      factor(levels = c("GW", "GU", "SW")))

ggplot(quartile_data, aes(x = Year, 
                          y = Q2, 
                          fill = `Source Water rank`)) +
  facet_wrap(~Region, scales = "free_y") +
  geom_ribbon(aes(ymin = Q1, 
                  ymax = Q3), 
              alpha = .8, 
              color = "black") +
  labs(title = "Arsenic Source Water Quartile Values From 2006 to 2011 by Region",
       x = "Year",
       y = "Arsenic Detection Value (MG/L)",
       fill = "Source Water Type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .75),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16))  +
  geom_hline(yintercept = 0.01, color = "orange", size = 0.75) +
  geom_hline(yintercept = 0.003, color = 'red', size = 0.75) 

#PLOT Percentile Values By Water Type in Time ------
quartile_data <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`), 
         Value = as.numeric(Value)) %>%
  group_by(Year, `Source Water Type`) %>%
  summarize(Q1 = quantile(Value, probs = 0.25, na.rm = TRUE),
            Q2 = quantile(Value, probs = 0.5, na.rm = TRUE),
            Q3 = quantile(Value, probs = 0.75, na.rm = TRUE))

ggplot(quartile_data, aes(x = Year, 
                          y = Q2, 
                          fill = `Source Water Type`)) +
  geom_ribbon(aes(ymin = Q1, 
                  ymax = Q3), 
              alpha = 0.4, 
              color = "black") +
  geom_line(aes(y = Q1, linetype = '25%', color = `Source Water Type`), size = 1)+
  geom_line(aes(y = Q2, linetype = '50%', color = `Source Water Type`), size = 1)+
  geom_line(aes(y = Q3, linetype = '75%', color = `Source Water Type`), size = 1)+
  facet_wrap(~`Source Water Type`, scales = 'free_x') +
  labs(title = "National Arsenic Source Water Quartile Values From 2006 to 2011",
       x = "Year",
       y = "Arsenic Detection Value (MG/L)",
       fill = "Source Water Type") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = .75),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16)) +
  geom_hline(yintercept = 0.01, color = "orange", size = 0.75) +
  geom_hline(yintercept = 0.003, color = 'red', size = 0.75) + 
  scale_linetype_manual(name = "Percentile",
                        values = c("25%" = "dotted", "50%" = "solid", "75%" = "dashed")) 
  scale_fill_manual(values = c("GU" = "#EC9211", "GW" = "#0097D6", "SW" = "#787A7C")) 


#PLOT Percentage Detection By Region in Time ------
quartile_data_weights <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`)) %>%
  group_by(Year, Region) %>%
  summarise(total_count = n(),
            detection_1_count = sum(Detect == 1),
            detection_0_count = sum(Detect == 0),
            detection_1_percentage = detection_1_count / total_count * 100) 

ggplot(quartile_data_weights, aes(x = as.factor(Year), 
                          y = detection_1_percentage, 
                          fill = Region)) +
  #geom_bar(stat = "identity") +
  geom_line(aes(group = Region,
                color = Region), size = 1.5)+
  #facet_wrap(~Region, scales = "free_y") +
  labs(title = "Regional Variation in Arsenic Trace Detection Rates for All Water Sources, 2006-2011",
       x = "Year",
       y = "Percent Detected",
       fill = "Region") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16))

#PLOT Percentile Values By Division in Time ------
quartile_data_div <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`),
         Value = as.numeric(Value) ) %>%
  group_by(Division, Year) %>%
  summarize(Q1 = quantile(Value, probs = 0.25, na.rm = TRUE),
            Q2 = quantile(Value, probs = 0.5, na.rm = TRUE),
            Q3 = quantile(Value, probs = 0.75, na.rm = TRUE)  )

ggplot(quartile_data_div, aes(x = Year, 
                              color = Division)) +
  geom_line(aes(y = Q1, linetype = "25%"), size = 1.15, alpha = 0.9) +
  geom_line(aes(y = Q2, linetype = "50%"), size = 1.15) +
  geom_line(aes(y = Q3, linetype = "75%"), size = 1.15, alpha = 0.9) +
  facet_wrap(~Division, scales = "free_y") +
  labs(title = "Arsenic Percentile Values From 2006 to 2011 by Division for All Source Water Types", 
       x = "Year",
       y = "Arsenic Detection Value (MG/L)",
       color = "Division",
       linetype = "Percentile") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5),    
        plot.title = element_text(hjust = 0.5),    
        text = element_text(size = 16)) +
  geom_hline(yintercept = 0.01, color = "orange", size = 0.5) +
  geom_hline(yintercept = 0.003, color = 'red', size = 0.5) +
  scale_linetype_manual(name = "Percentile",
                        values = c("25%" = "dotted", "50%" = "solid", "75%" = "dashed"))

#PLOT Percentage Detection By Division in Time ------
quartile_data <- arsenic %>%
  mutate(Year = year(`Sample Collection Date`)) %>%
  group_by(Division, Year) %>%
  summarize(ded = 100*sum(Detect == 1)/ sum(c(Detect == 1, Detect == 0))) 

ggplot(quartile_data, aes(x = as.factor(Year), 
                          y = ded, 
                          fill = Division)) +
  #geom_bar(stat = "identity") +
  geom_line(aes(group = Division,
                color = Division), size = 1.5)+
  # facet_wrap(~Division, scales = "free_y") +
  labs(title = "Percent of Arsenic Detections from 2006 to 2011 by Division",
       x = "Year",
       y = "Detected Percentage Status",
       fill = "Division") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16))

#PLOT Arsenic Levels by Individual State over time -----
arsenic %>%
  filter(`State Code` == 'VA') %>%
  mutate(Value = as.numeric(Value),
         Year = year(`Sample Collection Date`)) %>%
ggplot(aes(x = as.factor(Year), 
           y = Value )) +
  geom_boxplot() +
  labs(title = "Arsenic Quartile Values From 2006 to 2011 by ___",
       x = "Year",
       y = "Arsenic Detection Value") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = .5),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16))+
  geom_hline(yintercept = 0.01, color = "orange", size = 0.75) +
  geom_hline(yintercept = 0.003, color = 'red', size = 0.75) 
