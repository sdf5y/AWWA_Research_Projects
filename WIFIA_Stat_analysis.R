setwd("C:/Users/sfranco/Documents/Python")
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

Clean_df <- read_excel('Final_Clean.xlsx', range = cell_cols("A:M"))  #Clean_WIFIA - Copy

plot(Clean_df)

#Cleaning the data -----

#removed items.
Clean_df <-  Clean_df[c(-10,-11,-12)] #remove text culled 

Clean_df <- subset(Clean_df, `NUMBER OF JOBS CREATED:` < 5000) #remove NJ 

Clean_df <- subset(Clean_df, `NUMBER OF JOBS CREATED:` > 0) #remove Baltimore 

Clean_df <- subset(Clean_df, Clean_df$Keyword != 'san_diego_wifiaprojectfactsheet_loanclosefinal.pdf') #remove the double entry for San Diego

Clean_df['Loan Percapita'] <- round(Clean_df$`WIFIA LOAN AMOUNT:` / Clean_df$`POPULATION SERVED BY PROJECT :`, 2) 

Clean_df['Loan ratio'] <- round(Clean_df$`WIFIA LOAN AMOUNT:` / Clean_df$`TOTAL WIFIA PROJECT COSTS:`, 3) 

#Descriptive Stats -----

test <-  Clean_df %>%
  arrange(Clean_df$`WIFIA LOAN AMOUNT:`) %>%
  head()

xtable(test)

table(Clean_df$`PROJECT TYPE:`)

sumtable(Clean_df[c(4:8)])

reuse_df <- subset(Clean_df, `PROJECT TYPE:` == 'Reuse')
wastewater_df <- subset(Clean_df, `PROJECT TYPE:` == 'Wastewater')
drinkingwater_df <- subset(Clean_df, `PROJECT TYPE:` == 'Drinking water')
stormwater_df <- subset(Clean_df, `PROJECT TYPE:` == 'Stormwater')

data_frames <- list(reuse_df, wastewater_df, drinkingwater_df, stormwater_df)

categories <- c("Reuse", "Wastewater", "Drinking Water", "Stormwater")

sums <- numeric(length(data_frames))
sums2 <- numeric(length(data_frames))
sums3 <- numeric(length(data_frames))
sums4 <- numeric(length(data_frame))

for (i in seq_along(data_frames)) {
  sums[i] <- sum(data_frames[[i]]$`NUMBER OF JOBS CREATED:`)
  sums2[i] <- sum(data_frames[[i]]$`WIFIA LOAN AMOUNT:`)
  sums3[i] <- sum(data_frames[[i]]$`POPULATION SERVED BY PROJECT :`)
  sums4[i] <- sum(data_frames[[i]]$`TOTAL WIFIA PROJECT COSTS:`)
}

totals_df <- data.frame(Category = categories, 
                        'NUMBER OF JOBS CREATED:' = sums,
                        'WIFIA LOAN AMOUNT' = sums2,
                        'POPULATION SERVED BY PROJECT :' = sums3,
                        'TOTAL WIFIA PROJECT COSTS:' = sums4)

print(totals_df)
xtable(totals_df)
stargazer(totals_df)

cor.test(Clean_df$`WIFIA LOAN AMOUNT:`, Clean_df$`NUMBER OF JOBS CREATED:`) #positive, strong correlation
cor.test(Clean_df$`WIFIA LOAN AMOUNT:`, Clean_df$`POPULATION SERVED BY PROJECT :`) #positive, weak correlation
cor.test(Clean_df$`TOTAL WIFIA PROJECT COSTS:`, Clean_df$`NUMBER OF JOBS CREATED:`) #positive, strong correlation

plot(Clean_df$`WIFIA LOAN AMOUNT:`, Clean_df$`NUMBER OF JOBS CREATED:`)

options(scipen = 999, digits = 10)
options(scipen = 0, digits = 7)  

summary(reuse_df)
summary(wastewater_df)
summary(drinkingwater_df)
summary(stormwater_df)

#Plot Boxplot WIFIA loan and Field----

Clean_df %>%
  arrange(desc(`WIFIA LOAN AMOUNT:`)) %>%
  mutate(`PROJECT TYPE:` = factor(`PROJECT TYPE:`)) %>%
ggplot(aes(y = (`WIFIA LOAN AMOUNT:`),
           x = reorder(`PROJECT TYPE:`, `WIFIA LOAN AMOUNT:`),
           fill = `PROJECT TYPE:`)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Wastewater" = "red", "Drinking water" = "cyan", "Stormwater" = "green", "Reuse" = "purple"))+
  labs(title = "WIFIA Closed Applications",
       x = "Project Type",
       y = "Loan Amount in 2023 $",
       fill='Project Type')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        text = element_text(size = 16) )+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6), 
                     breaks = seq(0, 1750000000, by = 150000000))

#Plot VIolin WIFIA PerCapita loan and Field----

Clean_df %>%
  arrange((`Loan Percapita`)) %>%
  mutate(`PROJECT TYPE:` = factor(`PROJECT TYPE:`)) %>%
  ggplot(aes(y = (`Loan Percapita`),
             x = reorder(`PROJECT TYPE:`, `Loan Percapita`),
             fill = `PROJECT TYPE:`)) + 
  geom_violin() +
  labs(title = "WIFIA Closed Applications",
       x = "Project Type",
       y = "Loan Amount in 2023 $",
       fill='Project Type')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        text = element_text(size = 16) )

#Plot density WIFIA loan, Field and Population----
Clean_df %>%
  arrange(desc(`WIFIA LOAN AMOUNT:`)) %>%
  mutate(`PROJECT TYPE:` = factor(`PROJECT TYPE:`)) %>%
  ggplot(aes(y = `WIFIA LOAN AMOUNT:`,
                       x = reorder(`PROJECT TYPE:`, `WIFIA LOAN AMOUNT:`), 
                       size= `POPULATION SERVED BY PROJECT :`,
                       color = `PROJECT TYPE:`)) +
  geom_point(stroke = 1, alpha=.45,
             position = position_jitterdodge(jitter.width = 2.5)) +
  scale_color_manual(values = c("Wastewater" = "red", "Drinking water" = "cyan", "Stormwater" = "green", "Reuse" = "purple"))+
  scale_size(range = c(1, 20), 
             labels = scales::unit_format(unit = "M", scale = 1e-6), 
             name="Population Size",
             breaks = c(50000,100000,1000000,3000000,5000000))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16) )+
  labs(title = "WIFIA Closed Applications",
       x = "Project Type",
       y = "Loan Amount in 2023 $",
       color='Project Type')+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
                     breaks = seq(0, 1000000000, by = 150000000))+
  scale_x_discrete(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

#Regressions ----

par(mfrow=c(2,2))

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

reg <- lm(data = Clean_df, 
           (`WIFIA LOAN AMOUNT:`) ~ 
            (Clean_df$`POPULATION SERVED BY PROJECT :`))

reg1 <- lm(data = Clean_df, 
          log(`WIFIA LOAN AMOUNT:`) ~ 
            (Clean_df$`POPULATION SERVED BY PROJECT :`) )

reg2 <- lm(data = Clean_df, 
           log(`WIFIA LOAN AMOUNT:`) ~ 
             log(Clean_df$`POPULATION SERVED BY PROJECT :`) )

reg3 <- lm(data = Clean_df, 
           log(`WIFIA LOAN AMOUNT:`) ~ 
             log(Clean_df$`POPULATION SERVED BY PROJECT :`)+ log(Clean_df$`NUMBER OF JOBS CREATED:`)  )

reg4 <- lm(data = Clean_df, 
           log(`WIFIA LOAN AMOUNT:`) ~ 
             log(Clean_df$`POPULATION SERVED BY PROJECT :`) + log(Clean_df$`NUMBER OF JOBS CREATED:`) + (Clean_df$`PROJECT TYPE:`) )

reg5 <- lm(data = Clean_df, 
           (`WIFIA LOAN AMOUNT:`) ~ 
             (Clean_df$`POPULATION SERVED BY PROJECT :`) + (Clean_df$`NUMBER OF JOBS CREATED:`) + (Clean_df$`PROJECT TYPE:` ))

reg4_robust <- rlm(data = Clean_df, log(`WIFIA LOAN AMOUNT:`) ~ 
                     log(Clean_df$`POPULATION SERVED BY PROJECT :`) + log(Clean_df$`NUMBER OF JOBS CREATED:`) + (Clean_df$`PROJECT TYPE:`))

plot(reg)
plot(reg1)
plot(reg2)
plot(reg3)
plot(reg4)
plot(reg5)

par(resetPar())

lmtest::harvtest(reg4) #Null not rejected that there is a better linear reg

lmtest::raintest(reg4) #Failed, but in log-log, Residual vs.Fitted plot is evenly rand, and line around 0

lmtest::bptest(reg4, studentize = FALSE) #H(a) is not rejected: heteroskedasticity, but diagnostic plots and consulting with statistician (see acknowledgments show otherwise) 

lmtest::dwtest(reg4) #Null not rejected: no autocorrelation

shapiro.test((reg4$residuals))

summary(reg4)
summary(reg4_robust)

test <- anova(reg2, reg3, reg4)

xtable(test)
stargazer(reg4, reg4_robust, title="Results",align=TRUE)

#Other tests ----

reg_other <- lm(`Loan Percapita` ~ `PROJECT TYPE:`, data = Clean_df)
anova_result <- aov(`Loan Percapita` ~ `PROJECT TYPE:`, data = Clean_df)

summary(anova_result)

plot(reg_other)
summary(reg_other) #loan percapita is not statistically different among project type

summary(Clean_df$`Loan Percapita`)
hist(Clean_df$`Loan Percapita`)
hist(Clean_df$`NUMBER OF JOBS CREATED:`)

summary(lm(Clean_df$`WIFIA LOAN AMOUNT:` ~ Clean_df$`NUMBER OF JOBS CREATED:` + Clean_df$`PROJECT TYPE:`))

quantile(Clean_df$`Loan Percapita`)
ecdf(Clean_df$`Loan Percapita`)(1000)*100

quantile(Clean_df$`NUMBER OF JOBS CREATED:`)
ecdf(Clean_df$`NUMBER OF JOBS CREATED:`)(2000)*100

predicted_log_wifia_loan <- coef(reg4)["(Intercept)"] + 
  coef(reg4)["Clean_df$`PROJECT TYPE:`Wastewater"]* 1 + 
  (coef(reg4)["log(Clean_df$`NUMBER OF JOBS CREATED:`)"] * 1)

predicted_wifia_loan <- exp(predicted_log_wifia_loan)

wastewater_series <- wastewater_df$`WIFIA LOAN AMOUNT:`/predicted_wifia_loan

test <- data.frame(wastewater_series, wastewater_df$`NUMBER OF JOBS CREATED:`)

test$'residuals' <- abs(test$wastewater_series - test$wastewater_df..NUMBER.OF.JOBS.CREATED..)

plot(test$residuals, sqrt(test$residuals))

reg_j1 <- lm(data = Clean_df, log(`NUMBER OF JOBS CREATED:`) ~  `PROJECT TYPE:` + log(`POPULATION SERVED BY PROJECT :`))
plot(reg_j1)
summary(reg_j1)

#Mapping prep ----

Clean_df['LAT'] <- sapply(strsplit(Clean_df$`LAT LONG`, ","), function(x) x[1])

Clean_df['LONG'] <- sapply(strsplit(Clean_df$`LAT LONG`, ","), function(x) x[2])
