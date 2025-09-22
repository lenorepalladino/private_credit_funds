#############################################
### ProPreqin Data & Graphs ## 07/01/2024 ###
#############################################

##Cleans the Environment
rm(list=ls())

##Packages
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(ggtext)
library(writexl)
library(reshape2)
library(zoo)

##Importing Data
##CHANGE TO NEW PATH
setwd("C:/Users/hjkar/Documents/School/1-UMass/PERI_RA/Data/ProPreqin Data/07_01_24")
preqin_investors <- read_csv("Preqin Investors Full Pull.csv")

##Working with Preqin data for private markets
#Average holdings in USD millions by firm type, converted to billions
table(preqin_investors$COUNTRY)

preqin_investors <- preqin_investors %>%
  group_by(`FIRM TYPE`) %>%
  mutate(`AUM (USD MN)` = `AUM (USD MN)`/1000,
         `PE ALLOCATION (USD MN)` = `PE ALLOCATION (USD MN)`/1000,
         `HF ALLOCATION (USD MN)` = `HF ALLOCATION (USD MN)`/1000,
         `ALLOCATION: ALTERNATIVES (USD MN)` = `ALLOCATION: ALTERNATIVES (USD MN)`/1000,
         `ALLOCATION: EQUITIES (USD MN)` = `ALLOCATION: EQUITIES (USD MN)`/1000,
         `ALLOCATION: FIXED INCOME (USD MN)` = `ALLOCATION: FIXED INCOME (USD MN)`/1000,
         `ALLOCATION: CASH (USD MN)` = `ALLOCATION: CASH (USD MN)`/1000,
         `ALLOCATION: OTHER (USD MN)` = `ALLOCATION: OTHER (USD MN)`/1000)

mean_allocations_aum <- preqin_investors %>%
  select(`FIRM TYPE`,`AUM (USD MN)`, `PE ALLOCATION (USD MN)`, 
         `HF ALLOCATION (USD MN)`, `ALLOCATION: ALTERNATIVES (USD MN)`,
         `ALLOCATION: EQUITIES (USD MN)`, `ALLOCATION: FIXED INCOME (USD MN)`,
         `ALLOCATION: CASH (USD MN)`, `ALLOCATION: OTHER (USD MN)`) %>%
  group_by(`FIRM TYPE`) %>%
  summarize(mean_AUM = mean(`AUM (USD MN)`, na.rm=TRUE),
            mean_PE = mean(`PE ALLOCATION (USD MN)`, na.rm=TRUE),
            mean_HF = mean(`HF ALLOCATION (USD MN)`, na.rm=TRUE),
            mean_Alt = mean(`ALLOCATION: ALTERNATIVES (USD MN)`, na.rm=TRUE),
            mean_Equity = mean(`ALLOCATION: EQUITIES (USD MN)`, na.rm=TRUE),
            mean_FI = mean(`ALLOCATION: FIXED INCOME (USD MN)`, na.rm=TRUE),
            mean_Cash = mean(`ALLOCATION: CASH (USD MN)`, na.rm=TRUE),
            mean_Other = mean(`ALLOCATION: OTHER (USD MN)`, na.rm=TRUE),
            mean_FI_C = mean((`ALLOCATION: FIXED INCOME (USD MN)` + `ALLOCATION: CASH (USD MN)`), na.rm=TRUE),
            mean_Alt_net = mean((`ALLOCATION: ALTERNATIVES (USD MN)` - `PE ALLOCATION (USD MN)` - `HF ALLOCATION (USD MN)`), na.rm=TRUE))

#Average holdings by percentage of AUM by firm type
mean_allocations_per <- preqin_investors %>%
  select(`FIRM TYPE`, `PE ALLOCATION (%)`, 
         `HF ALLOCATION (%)`, `ALLOCATION: ALTERNATIVES (%)`,
         `ALLOCATION: EQUITIES (%)`, `ALLOCATION: FIXED INCOME (%)`,
         `ALLOCATION: CASH (%)`, `ALLOCATION: OTHER (%)`) %>%
  group_by(`FIRM TYPE`) %>%
  summarize(mean_PE_per = mean(`PE ALLOCATION (%)`, na.rm=TRUE),
            mean_HF_per = mean(`HF ALLOCATION (%)`, na.rm=TRUE),
            mean_Equity_per = mean(`ALLOCATION: EQUITIES (%)`, na.rm=TRUE),
            mean_FI_per = mean(`ALLOCATION: FIXED INCOME (%)`, na.rm=TRUE),
            mean_Cash_per = mean(`ALLOCATION: CASH (%)`, na.rm=TRUE),
            mean_Other_per = mean(`ALLOCATION: OTHER (%)`, na.rm=TRUE),
            mean_FI_C_per = mean((`ALLOCATION: FIXED INCOME (%)` + `ALLOCATION: CASH (%)`), na.rm=TRUE),
            mean_Alt_per = mean(`ALLOCATION: ALTERNATIVES (%)`, na.rm=TRUE),
            mean_Alt_net_per = mean((`ALLOCATION: ALTERNATIVES (%)` - `PE ALLOCATION (%)` - `HF ALLOCATION (%)`), na.rm=TRUE))

#file_path <- "mean_allocations_aum.xlsx"
#write_xlsx(mean_allocations_aum, path = file_path)

#file_path <- "mean_allocations_per.xlsx"
#write_xlsx(mean_allocations_per, path = file_path)

# Reshape data for plotting
mean_allocations_aum1 <- mean_allocations_aum %>%
  filter(`FIRM TYPE` == 'Public Pension Fund')
mean_allocations_aum2 <- mean_allocations_aum %>%
  filter(`FIRM TYPE` == 'Private Pension Fund')
mean_allocations_aum3 <- mean_allocations_aum %>%
  filter(`FIRM TYPE` == 'Endowment Plan')
mean_allocations_aum4 <- mean_allocations_aum %>%
  filter(`FIRM TYPE` == 'Foundation')
#mean_allocations_aum5 <- mean_allocations_aum %>%
#  filter(`FIRM TYPE` == 'Sovereign Wealth Fund')

mean_allocations_aum_pension <- rbind(mean_allocations_aum1, mean_allocations_aum2)
mean_allocations_aum_other <- rbind(mean_allocations_aum3, mean_allocations_aum4)


mean_allocations_per1 <- mean_allocations_per %>%
  filter(`FIRM TYPE` == 'Public Pension Fund')
mean_allocations_per2 <- mean_allocations_per %>%
  filter(`FIRM TYPE` == 'Private Pension Fund')
mean_allocations_per3 <- mean_allocations_per %>%
  filter(`FIRM TYPE` == 'Endowment Plan')
mean_allocations_per4 <- mean_allocations_per %>%
  filter(`FIRM TYPE` == 'Foundation')
#mean_allocations_per5 <- mean_allocations_per %>%
#  filter(`FIRM TYPE` == 'Sovereign Wealth Fund')

mean_allocations_per_all <- rbind(mean_allocations_per1, mean_allocations_per2,
                                  mean_allocations_per3, mean_allocations_per4)

mean_allocations_aum_pension1 <- mean_allocations_aum_pension %>%
  select(`FIRM TYPE`, mean_Equity, mean_FI_C, mean_Alt)
mean_allocations_aum_pension_melted2 <- melt(mean_allocations_aum_pension1, id.vars = "FIRM TYPE")
mean_allocations_aum_pension_melted1 <- mean_allocations_aum_pension %>%
  select(-mean_AUM, mean_PE, mean_HF, -mean_Alt, mean_FI, mean_Cash,
         mean_Other, mean_Equity, -mean_FI_C, mean_Alt_net) %>%
  melt(id.vars = "FIRM TYPE")


mean_allocations_aum_other1 <- mean_allocations_aum_other %>%
  select(`FIRM TYPE`, mean_Equity, mean_FI_C, mean_Alt)
mean_allocations_aum_other_melted2 <- melt(mean_allocations_aum_other1, id.vars = "FIRM TYPE")
mean_allocations_aum_other_melted1 <- mean_allocations_aum_other %>%
  select(-mean_AUM, mean_PE, mean_HF, -mean_Alt, mean_FI, mean_Cash,
         mean_Other, mean_Equity, -mean_FI_C, mean_Alt_net) %>%
  melt(id.vars = "FIRM TYPE")


#mean_allocations_aum51 <- mean_allocations_aum5 %>%
#  select(`FIRM TYPE`, mean_Equity, mean_FI_C, mean_Alt)
#mean_allocations_aum_sovereign_melted2 <- melt(mean_allocations_aum51, id.vars = "FIRM TYPE")
#mean_allocations_aum_sovereign_melted1 <- mean_allocations_aum5 %>%
#  select(-mean_AUM, mean_PE, mean_HF, mean_Alt, mean_FI, mean_Cash,
#         mean_Other, mean_Equity, -mean_FI_C) %>%
#  melt(id.vars = "FIRM TYPE")


mean_allocations_per_all1 <- mean_allocations_per_all %>%
  select(`FIRM TYPE`, mean_PE_per, mean_HF_per, mean_Alt_net_per, mean_Equity_per,
         mean_FI_per, mean_Cash_per, mean_Other_per)
mean_allocations_per_all2 <- mean_allocations_per_all %>%
  select(`FIRM TYPE`, mean_Equity_per, mean_FI_C_per, mean_Alt_per)

mean_allocations_per_melted1 <- melt(mean_allocations_per_all1, id.vars = "FIRM TYPE")
mean_allocations_per_melted2 <- melt(mean_allocations_per_all2, id.vars = "FIRM TYPE")


#Graphing the average allocations
#preqin_allocations_aum1 <- ggplot(mean_allocations_aum_pension_melted1, aes(x = `FIRM TYPE`, y = value, fill = variable)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = c("mean_PE" = "darkred",
#                               "mean_HF" = "darkblue",
#                               "mean_Alt_net" = "cadetblue",
#                               "mean_Equity" = "indianred",
#                               "mean_FI" = "cadetblue2",
#                               "mean_Cash" = "lightsalmon",
#                               "mean_Other" = "grey"),
#                    labels = c("Private Equity", "Hedge Funds", "Equity", "Fixed Income", "Cash", "Other", "Alternatives")) +  
#  labs(x = "Institutional Shareholder", y = "$USD Billion", fill = "Asset Class",
#       caption = str_wrap("Source: Preqin Pro. AUM data from private equity (buyouts and VC included) and hedge fund deals for 1,479 private and 565 public US-based pension funds.
#                          The average AUM for private and public pension funds is $2.8 and $10.5 billion, respectively. Accessed 04/15/24.", 90)) +
#  ggtitle("Pension Fund Average AUM Portfolio Allocation") +
#  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
#        plot.caption = element_text(hjust = 0))
#preqin_allocations_aum1

preqin_allocations_aum2 <- ggplot(mean_allocations_aum_pension_melted2, aes(x = `FIRM TYPE`, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_Equity" = "cadetblue",
                               "mean_FI_C" = "darkred",
                               "mean_Alt" = "darkblue"),
                    labels = c(str_wrap("Traditional Equities", 10), str_wrap("Fixed Income & Cash", 15), "Alternatives")) +  
  labs(x = "Institutional Shareholder", y = "$USD Billion", fill = "Asset Class",
       caption = str_wrap("Source: Preqin Pro. AUM data for 1,479 private and 565 public US-based pension funds. The average AUM for private and public pension funds is $2.8 and $10.5 billion, respectively. 
                          Alternatives include private equity (with buyouts and venture capital), hedge funds, real estate, infrastructure, private debt, and natural resources. Accessed 07/01/24.", 90)) +
  ggtitle("Pension Fund Average AUM Portfolio Allocation") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.caption = element_text(hjust = 0))
preqin_allocations_aum2

preqin_pension_private <- preqin_investors %>% 
  filter(`FIRM TYPE` == 'Private Pension Fund')
preqin_pension_public <- preqin_investors %>% 
  filter(`FIRM TYPE` == 'Public Pension Fund')


#preqin_allocations_aum3 <- ggplot(mean_allocations_aum_other_melted1, aes(x = `FIRM TYPE`, y = value, fill = variable)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = c("mean_PE" = "darkred",
#                               "mean_HF" = "darkblue",
#                               "mean_Alt_net" = "cadetblue",
#                               "mean_Equity" = "indianred",
#                               "mean_FI" = "cadetblue2",
#                               "mean_Cash" = "lightsalmon",
#                               "mean_Other" = "grey"),
#                    labels = c("Private Equity", "Hedge Funds", "Equity",
#                               "Fixed Income", "Cash", "Other", "Alternatives")) +  
#  labs(x = "Institutional Shareholder", y = "$USD Billion", fill = "Asset Class",
#       caption = str_wrap("Source: Preqin Pro. AUM data from private equity (buyouts and VC included) and hedge fund deals for 750 endowment plans and 1,389 foundations based in the US.
#                          The average AUM for endowment plans and foundations is $1.3 and $1 billion, respectively. Accessed 04/15/24.", 90)) +
#  ggtitle("Endowment & Foundation Average AUM Portfolio Allocation") +
#  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
#        plot.caption = element_text(hjust = 0))
#preqin_allocations_aum3

preqin_allocations_aum4 <- ggplot(mean_allocations_aum_other_melted2, aes(x = `FIRM TYPE`, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_Equity" = "cadetblue",
                               "mean_FI_C" = "darkred",
                               "mean_Alt" = "darkblue"),
                    labels = c(str_wrap("Traditional Equities", 10), str_wrap("Fixed Income & Cash", 15), "Alternatives")) +  
  labs(x = "Institutional Shareholder", y = "$USD Billion", fill = "Asset Class",
       caption = str_wrap("Source: Preqin Pro. AUM data for 750 endowment plans and 1,389 foundations based in the US. The average AUM for endowment plans and foundations is $1.3 and $1 billion, respectively. 
                          Alternatives include private equity (with buyouts and venture capital), hedge funds, real estate, infrastructure, private debt, and natural resources. Accessed 07/01/24.", 95)) +
  ggtitle("Endowment & Foundation Average AUM Portfolio Allocation") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.caption = element_text(hjust = 0))
preqin_allocations_aum4

preqin_endowment <- preqin_investors %>% 
  filter(`FIRM TYPE` == 'Endowment Plan')
preqin_foundation <- preqin_investors %>% 
  filter(`FIRM TYPE` == 'Foundation')

preqin_allocations_per2 <- ggplot(mean_allocations_per_melted2, aes(x = `FIRM TYPE`, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_Equity_per" = "cadetblue",
                               "mean_FI_C_per" = "darkred",
                               "mean_Alt_per" = "darkblue"),
                    labels = c(str_wrap("Traditional Equities", 10), str_wrap("Fixed Income & Cash", 15), "Alternatives")) +  
  labs(x = "Institutional Shareholder", y = "Percentage Allocation (%)", fill = "Asset Class",
       caption = str_wrap("Source: Preqin Pro. AUM data for public and private pension funds, endowment plans, and foundations.
                          Alternatives includes private equity (with buyouts and venture capital), hedge funds, real estate, infrastructure, private debt, and natural resources. Accessed 07/01/24.", 100)) +
  ggtitle("Institutional Shareholders' Average Percentage Portfolio Allocation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))
preqin_allocations_per2


#Working with Preqin League Tables for Public Pensions
preqin_league <- read_csv("Preqin Investors League Pensions.csv")

preqin_league %>%
  select(`Firm Type`, `AUM (USD)`, `PE Allocation (USD MN)`, `HF Allocation (USD MN)`,
         `PD Allocation (USD MN)`, `RE Allocation (USD MN)`, 
         `INF Allocation (USD MN)`, `NR Allocation (USD MN)`) %>%
  summary()
preqin_league <- preqin_league %>%
  mutate(`AUM (USD)` = `AUM (USD)`/1000)

pensions_preqin <- preqin_investors %>%
  filter(`FIRM TYPE` == 'Public Pension Fund')

pensions_preqin %>%
  group_by(`FIRM ID`) %>%
  select(`AUM (USD MN)`) %>%
  summary()

preqin_league %>%
  group_by(`Rank`) %>%
  select(`AUM (USD)`) %>%
  summary()

preqin_league_15 <- preqin_league %>%
  filter(`Rank`<=15)

preqin_league_15 %>%
  group_by(`Rank`) %>%
  select(`AUM (USD)`) %>%
  summary()

pensions_preqin <- pensions_preqin %>%
  group_by(`FIRM TYPE`) %>%
  mutate(rank = dense_rank(desc(pensions_preqin$`AUM (USD MN)`)))

pensions_preqin_100 <- pensions_preqin %>%
  filter(rank <= 100)
pensions_preqin_100 <- pensions_preqin_100[1:100,]
pensions_preqin_15 <- pensions_preqin %>%
  filter(rank <= 15)


#Grabbing averages allocations for top 100 and 15 from main data set
#Average holdings in USD millions by firm type, converted to billions
mean_allocations_aum100 <- pensions_preqin_100 %>%
  select(`FIRM TYPE`,`AUM (USD MN)`, `PE ALLOCATION (USD MN)`, 
         `HF ALLOCATION (USD MN)`, `ALLOCATION: ALTERNATIVES (USD MN)`,
         `ALLOCATION: EQUITIES (USD MN)`, `ALLOCATION: FIXED INCOME (USD MN)`,
         `ALLOCATION: CASH (USD MN)`, `ALLOCATION: OTHER (USD MN)`) %>%
  group_by(`FIRM TYPE`) %>%
  summarize(mean_AUM = mean(`AUM (USD MN)`, na.rm=TRUE),
            mean_PE = mean(`PE ALLOCATION (USD MN)`/1000, na.rm=TRUE),
            mean_HF = mean(`HF ALLOCATION (USD MN)`/1000, na.rm=TRUE),
            mean_Alt = mean(`ALLOCATION: ALTERNATIVES (USD MN)`/1000, na.rm=TRUE),
            mean_Equity = mean(`ALLOCATION: EQUITIES (USD MN)`/1000, na.rm=TRUE),
            mean_FI = mean(`ALLOCATION: FIXED INCOME (USD MN)`/1000, na.rm=TRUE),
            mean_Cash = mean(`ALLOCATION: CASH (USD MN)`/1000, na.rm=TRUE),
            mean_Other = mean(`ALLOCATION: OTHER (USD MN)`/1000, na.rm=TRUE),
            mean_FI_C = mean((`ALLOCATION: FIXED INCOME (USD MN)` + `ALLOCATION: CASH (USD MN)`)/1000, na.rm=TRUE),
            mean_Alt_net = mean((`ALLOCATION: ALTERNATIVES (USD MN)` - `PE ALLOCATION (USD MN)` - `HF ALLOCATION (USD MN)`)/1000, na.rm=TRUE))


#Average holdings by percentage of AUM by firm type
mean_allocations_per100 <- pensions_preqin_100 %>%
  select(`FIRM TYPE`, `PE ALLOCATION (%)`, 
         `HF ALLOCATION (%)`, `ALLOCATION: ALTERNATIVES (%)`,
         `ALLOCATION: EQUITIES (%)`, `ALLOCATION: FIXED INCOME (%)`,
         `ALLOCATION: CASH (%)`, `ALLOCATION: OTHER (%)`) %>%
  group_by(`FIRM TYPE`) %>%
  summarize(mean_PE_per = mean(`PE ALLOCATION (%)`, na.rm=TRUE),
            mean_HF_per = mean(`HF ALLOCATION (%)`, na.rm=TRUE),
            mean_Equity_per = mean(`ALLOCATION: EQUITIES (%)`, na.rm=TRUE),
            mean_FI_per = mean(`ALLOCATION: FIXED INCOME (%)`, na.rm=TRUE),
            mean_Cash_per = mean(`ALLOCATION: CASH (%)`, na.rm=TRUE),
            mean_Other_per = mean(`ALLOCATION: OTHER (%)`, na.rm=TRUE),
            mean_FI_C_per = mean((`ALLOCATION: FIXED INCOME (%)` + `ALLOCATION: CASH (%)`), na.rm=TRUE),
            mean_Alt_per = mean(`ALLOCATION: ALTERNATIVES (%)`, na.rm=TRUE),
            mean_Alt_net_per = mean((`ALLOCATION: ALTERNATIVES (%)` - `PE ALLOCATION (%)` - `HF ALLOCATION (%)`), na.rm=TRUE))

#file_path <- "mean_allocations_aum100.xlsx"
#write_xlsx(mean_allocations_aum100, path = file_path)

#file_path <- "mean_allocations_per100.xlsx"
#write_xlsx(mean_allocations_per100, path = file_path)


#Average holdings in USD millions by firm type, converted to billions
mean_allocations_aum15 <- pensions_preqin_15 %>%
  select(`FIRM TYPE`,`AUM (USD MN)`, `PE ALLOCATION (USD MN)`, 
         `HF ALLOCATION (USD MN)`, `ALLOCATION: ALTERNATIVES (USD MN)`,
         `ALLOCATION: EQUITIES (USD MN)`, `ALLOCATION: FIXED INCOME (USD MN)`,
         `ALLOCATION: CASH (USD MN)`, `ALLOCATION: OTHER (USD MN)`) %>%
  group_by(`FIRM TYPE`) %>%
  summarize(mean_AUM = mean(`AUM (USD MN)`, na.rm=TRUE),
            mean_PE = mean(`PE ALLOCATION (USD MN)`/1000, na.rm=TRUE),
            mean_HF = mean(`HF ALLOCATION (USD MN)`/1000, na.rm=TRUE),
            mean_Alt = mean(`ALLOCATION: ALTERNATIVES (USD MN)`/1000, na.rm=TRUE),
            mean_Equity = mean(`ALLOCATION: EQUITIES (USD MN)`/1000, na.rm=TRUE),
            mean_FI = mean(`ALLOCATION: FIXED INCOME (USD MN)`/1000, na.rm=TRUE),
            mean_Cash = mean(`ALLOCATION: CASH (USD MN)`/1000, na.rm=TRUE),
            mean_Other = mean(`ALLOCATION: OTHER (USD MN)`/1000, na.rm=TRUE),
            mean_FI_C = mean((`ALLOCATION: FIXED INCOME (USD MN)` + `ALLOCATION: CASH (USD MN)`)/1000, na.rm=TRUE),
            mean_Alt_net = mean((`ALLOCATION: ALTERNATIVES (USD MN)` - `PE ALLOCATION (USD MN)` - `HF ALLOCATION (USD MN)`)/1000, na.rm=TRUE))

#Average holdings by percentage of AUM by firm type
mean_allocations_per15 <- pensions_preqin_15 %>%
  select(`FIRM TYPE`, `PE ALLOCATION (%)`, 
         `HF ALLOCATION (%)`, `ALLOCATION: ALTERNATIVES (%)`,
         `ALLOCATION: EQUITIES (%)`, `ALLOCATION: FIXED INCOME (%)`,
         `ALLOCATION: CASH (%)`, `ALLOCATION: OTHER (%)`) %>%
  group_by(`FIRM TYPE`) %>%
  summarize(mean_PE_per = mean(`PE ALLOCATION (%)`, na.rm=TRUE),
            mean_HF_per = mean(`HF ALLOCATION (%)`, na.rm=TRUE),
            mean_Equity_per = mean(`ALLOCATION: EQUITIES (%)`, na.rm=TRUE),
            mean_FI_per = mean(`ALLOCATION: FIXED INCOME (%)`, na.rm=TRUE),
            mean_Cash_per = mean(`ALLOCATION: CASH (%)`, na.rm=TRUE),
            mean_Other_per = mean(`ALLOCATION: OTHER (%)`, na.rm=TRUE),
            mean_FI_C_per = mean((`ALLOCATION: FIXED INCOME (%)` + `ALLOCATION: CASH (%)`), na.rm=TRUE),
            mean_Alt_per = mean(`ALLOCATION: ALTERNATIVES (%)`, na.rm=TRUE),
            mean_Alt_net_per = mean((`ALLOCATION: ALTERNATIVES (%)` - `PE ALLOCATION (%)` - `HF ALLOCATION (%)`), na.rm=TRUE))

#file_path <- "mean_allocations_aum15.xlsx"
#write_xlsx(mean_allocations_aum15, path = file_path)

#file_path <- "mean_allocations_per15.xlsx"
#write_xlsx(mean_allocations_per15, path = file_path)

#Average allocations into broad asset classes of top 100 pension funds
preqin_league <- preqin_league %>%
  mutate(PE_percent = (`PE Allocation (USD MN)`/1000)/`AUM (USD)`,
         HF_percent = (`HF Allocation (USD MN)`/1000)/`AUM (USD)`,
         PD_percent = (`PD Allocation (USD MN)`/1000)/`AUM (USD)`,
         RE_percent = (`RE Allocation (USD MN)`/1000)/`AUM (USD)`,
         INF_percent = (`INF Allocation (USD MN)`/1000)/`AUM (USD)`,
         NR_percent = (`NR Allocation (USD MN)`/1000)/`AUM (USD)`)

preqin_league %>%
  select(PE_percent, HF_percent, PD_percent, RE_percent, INF_percent, NR_percent) %>%
  summary()

preqin_league_15 <- preqin_league %>%
  filter(`Rank`<=15)

preqin_league_15 %>%
  select(PE_percent, HF_percent, PD_percent, RE_percent, INF_percent, NR_percent) %>%
  summary()

preqin_league <- preqin_league %>%
  mutate(`Firm Type` = 'Top 100')

preqin_league_15 <- preqin_league_15 %>%
  mutate(`Firm Type` = 'Top 15')

#Average holdings by AUM, top 100
mean_allocations_aum_100 <- preqin_league %>%
  select(`Firm Type`, `AUM (USD)`, `PE Allocation (USD MN)`, `HF Allocation (USD MN)`,
         `PD Allocation (USD MN)`, `RE Allocation (USD MN)`, 
         `INF Allocation (USD MN)`, `NR Allocation (USD MN)`) %>%
  group_by(`Firm Type`) %>%
  summarize(mean_AUM_al = mean(`AUM (USD)`, na.rm=TRUE),
            mean_PE_al = mean(`PE Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_HF_al = mean(`HF Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_PD_al = mean(`PD Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_RE_al = mean(`RE Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_INF_al = mean(`INF Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_NR_al = mean(`NR Allocation (USD MN)`/1000, na.rm=TRUE))

#Average holdings by percentage of AUM, top 100
mean_allocations_per_100 <- preqin_league %>%
  select(`Firm Type`, PE_percent, HF_percent, PD_percent, 
         RE_percent, INF_percent, NR_percent) %>%
  group_by(`Firm Type`) %>%
  summarize(mean_PE_percent = mean(PE_percent, na.rm=TRUE)*100, 
            mean_HF_percent = mean(HF_percent, na.rm=TRUE)*100, 
            mean_PD_percent = mean(PD_percent, na.rm=TRUE)*100, 
            mean_RE_percent = mean(RE_percent, na.rm=TRUE)*100, 
            mean_INF_percent = mean(INF_percent, na.rm=TRUE)*100, 
            mean_NR_percent = mean(NR_percent, na.rm=TRUE)*100)

#Average holdings by AUM, top 15
mean_allocations_aum_15 <- preqin_league_15 %>%
  select(`Firm Type`, `AUM (USD)`, `PE Allocation (USD MN)`, `HF Allocation (USD MN)`,
         `PD Allocation (USD MN)`, `RE Allocation (USD MN)`, 
         `INF Allocation (USD MN)`, `NR Allocation (USD MN)`) %>%
  group_by(`Firm Type`) %>%
  summarize(mean_AUM_al = mean(`AUM (USD)`, na.rm=TRUE),
            mean_PE_al = mean(`PE Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_HF_al = mean(`HF Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_PD_al = mean(`PD Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_RE_al = mean(`RE Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_INF_al = mean(`INF Allocation (USD MN)`/1000, na.rm=TRUE),
            mean_NR_al = mean(`NR Allocation (USD MN)`/1000, na.rm=TRUE))

#Average holdings by percentage of AUM, top 15
mean_allocations_per_15 <- preqin_league_15 %>%
  select(`Firm Type`, PE_percent, HF_percent, PD_percent, 
         RE_percent, INF_percent, NR_percent) %>%
  group_by(`Firm Type`) %>%
  summarize(mean_PE_percent = mean(PE_percent, na.rm=TRUE)*100, 
            mean_HF_percent = mean(HF_percent, na.rm=TRUE)*100, 
            mean_PD_percent = mean(PD_percent, na.rm=TRUE)*100, 
            mean_RE_percent = mean(RE_percent, na.rm=TRUE)*100, 
            mean_INF_percent = mean(INF_percent, na.rm=TRUE)*100, 
            mean_NR_percent = mean(NR_percent, na.rm=TRUE)*100)

mean_allocations_aum_league <- rbind(mean_allocations_aum_100, mean_allocations_aum_15)
mean_allocations_per_league <- rbind(mean_allocations_per_100, mean_allocations_per_15)

mean_allocations_aum_league_melted <- mean_allocations_aum_league %>%
  select(-mean_AUM_al) %>%
  melt(id.vars = "Firm Type")

mean_allocations_per_league_melted <- mean_allocations_per_league %>%
  melt(id.vars = "Firm Type")

preqin_league_aum <- ggplot(mean_allocations_aum_league_melted, aes(x = `Firm Type`, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_PE_al" = "darkred",
                               "mean_HF_al" = "darkblue",
                               "mean_PD_al" = "cadetblue",
                               "mean_RE_al" = "indianred",
                               "mean_INF_al" = "cadetblue2",
                               "mean_NR_al" = "lightsalmon"),
                    labels = c("Private Equity", "Hedge Funds", "Private Debt",
                               "Real Estate", "Infrastructure", "Natural Resources")) +  
  labs(x = "Top Public Pension Funds", y = "$USD Billion", fill = "Asset Class",
       caption = str_wrap("Source: Preqin Pro. Charts & League Tables. Investor League Tables. Data is constructed as averages for the top 100 and top 15
                          public pension funds. This represents 93% and 46% of total AUM for all pension funds, respectively. Accessed 07/01/24.", 90)) +
  ggtitle("Top Public Pension Funds Average AUM Portfolio Allocations") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.caption = element_text(hjust = 0))
preqin_league_aum

preqin_league_per <- ggplot(mean_allocations_per_league_melted, aes(x = `Firm Type`, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_PE_percent" = "darkred",
                               "mean_HF_percent" = "darkblue",
                               "mean_PD_percent" = "cadetblue",
                               "mean_RE_percent" = "indianred",
                               "mean_INF_percent" = "cadetblue2",
                               "mean_NR_percent" = "lightsalmon"),
                    labels = c("Private Equity", "Hedge Funds", "Private Debt",
                               "Real Estate", "Infrastructure", "Natural Resources")) +  
  labs(x = "Top Public Pension Funds", y = "Percentage Allocation (%)", fill = "Asset Class",
       caption = str_wrap("Source: Preqin Pro. Charts & League Tables. Investor League Tables. Data is constructed as averages for the top 100 and top 15
                          public pension funds. This represents 93% and 46% of total AUM for all pension funds, respectively. Accessed 07/01/24.", 90)) +
  ggtitle("Top Public Pension Funds Average Percentage Portfolio Allocations") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.caption = element_text(hjust = 0))
preqin_league_per


#Overall private markets capital breakdown
private_capital <- read_csv("Private Capital Breakdown.csv")

private_capital <- private_capital[rev(seq_len(nrow(private_capital))), ]

private_capital %>%
  group_by(Year) %>%
  summary()

private_capital <- private_capital %>%
  group_by(Year) %>%
  filter(Year <= 2023) %>%
  select(Raised_PE, Raised_RE, Raised_INF, Raised_PD, Raised_NR)

private_capital_long <- private_capital %>%
  gather(key = "Variable", value = "Value", -Year)

private_capital_long$Year <- as.numeric(as.character(private_capital_long$Year))

private_capital_graph <- ggplot(private_capital_long, aes(x = Year, y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color="black") +
  labs(title = "Private Capital Breakdown - 2004-2023",
       x = "Year",
       y = "$USD Billions",
       caption = str_wrap("Source: Preqin Pro. Charts & League Tables. 
                          Private Capital Breakdown. Private capital refers to 
                          private closed-end funds in the indicated categories. 
                          Private equity includes buyouts and VC. Accessed 07/01/24.", 90)) +
  scale_fill_manual(name = "Fund Categories",
                    values = c("Raised_PE" = "darkred", "Raised_PD" = "cadetblue", "Raised_RE" = "lightsalmon",
                               "Raised_INF" = "indianred", "Raised_NR" = "darkblue"),
                    labels = c("Infrastructure", "Natural Resources", "Private Debt", 
                               "Private Equity", "Real Estate"))  +
  #scale_x_continuous(breaks = seq(2004, 2023, by = 5)) +
  theme(plot.caption = element_text(hjust = 0))
private_capital_graph

private_capital <- private_capital %>%
  mutate(sum = (Raised_PE + Raised_RE + Raised_INF + Raised_PD + Raised_NR))

private_capital <- private_capital %>%
  mutate(PE_share = Raised_PE/sum,
         RE_share = Raised_RE/sum,
         INF_share = Raised_INF/sum,
         PD_share = Raised_PD/sum,
         NR_share = Raised_NR/sum)


pe <- ts(private_capital$Raised_PE, start=c(2004,1), freq=1)
pe_r <- diff(pe, lag=1)/stats::lag(pe, k=-1)
pe_r <- pe_r*100

re <- ts(private_capital$Raised_RE, start=c(2004,1), freq=1)
re_r <- diff(re, lag=1)/stats::lag(re, k=-1)
re_r <- re_r*100

inf <- ts(private_capital$Raised_INF, start=c(2004,1), freq=1)
inf_r <- diff(inf, lag=1)/stats::lag(inf, k=-1)
inf_r <- inf_r*100

pd <- ts(private_capital$Raised_PD, start=c(2004,1), freq=1)
pd_r <- diff(pd, lag=1)/stats::lag(pd, k=-1)
pd_r <- pd_r*100

nr <- ts(private_capital$Raised_NR, start=c(2004,1), freq=1)
nr_r <- diff(nr, lag=1)/stats::lag(nr, k=-1)
nr_r <- nr_r*100

private_capital_r <- data.frame(Date = time(pe_r),
                                pe_r,
                                re_r,
                                inf_r,
                                pd_r,
                                nr_r)

private_capital_r %>%
  group_by(Date) %>%
  summary()



##Decomposing Pension Fund dealings with PE funds
top_100_pe <- read_csv("Preqin Pensions.csv")
top_100_pe <- top_100_pe[16:100,]
top_100_pe <- top_100_pe[,1:31]

top_100_pe$total <- rowSums(top_100_pe[,7:30], na.rm=TRUE)

top_100_pe <- top_100_pe %>%
  group_by(`FIRM ID`) %>%
  mutate(diff = total - `PE ALLOCATION (USD MN)`)

sum_by_category_1 <- data.frame(Balanced = colSums(top_100_pe[,7]),
                                Buyout = colSums(top_100_pe[,8]),
                                Coinvestment = colSums(top_100_pe[,9]) + colSums(top_100_pe[,10]),
                                Early_Stage = colSums(top_100_pe[,15]) + colSums(top_100_pe[,16]) + colSums(top_100_pe[,17]),
                                Late_Stage = colSums(top_100_pe[,18]),
                                Fund_of_Funds = colSums(top_100_pe[,19]) + colSums(top_100_pe[,27]),
                                Growth = colSums(top_100_pe[,20]),
                                Secondaries = colSums(top_100_pe[,13]) + colSums(top_100_pe[,28]),
                                Venture = colSums(top_100_pe[,31]),
                                Other = colSums(top_100_pe[,11]) + colSums(top_100_pe[,12]) + colSums(top_100_pe[,14]) + 
                                  colSums(top_100_pe[,21]) + colSums(top_100_pe[,22]) + colSums(top_100_pe[,23]) + colSums(top_100_pe[,24]) + 
                                  colSums(top_100_pe[,25]) + colSums(top_100_pe[,26]) + colSums(top_100_pe[,29]) + colSums(top_100_pe[,30]))

sum_by_category_1 <- t(sum_by_category_1)

sum_by_category_1 <- data.frame(sum_by_category_1)

sum_by_category_1 <- rownames_to_column(sum_by_category_1, var = "Category")

colnames(sum_by_category_1) <- c("Category", "Committed (MN)")

sum_by_category_1 <- sum_by_category_1 %>%
  mutate(`Committed (MN)` = `Committed (MN)`/1000)

sum_by_category_1 <- sum_by_category_1 %>% 
  arrange(desc(`Category`))

sum_by_category_1 <- sum_by_category_1 %>% 
  mutate(prop = `Committed (MN)` / sum(`Committed (MN)`) *100)



top_15_pe <- read_csv("Preqin Pensions.csv")
top_15_pe <- top_15_pe[1:15,]
top_15_pe <- top_15_pe[,1:31]

top_15_pe$total <- rowSums(top_15_pe[,7:30], na.rm=TRUE)

top_15_pe <- top_15_pe %>%
  group_by(`FIRM ID`) %>%
  mutate(diff = total - `PE ALLOCATION (USD MN)`)

sum_by_category_2 <- data.frame(Balanced = colSums(top_15_pe[,7]),
                                Buyout = colSums(top_15_pe[,8]),
                                Coinvestment = colSums(top_15_pe[,9]) + colSums(top_15_pe[,10]),
                                Early_Stage = colSums(top_15_pe[,15]) + colSums(top_15_pe[,16]) + colSums(top_15_pe[,17]),
                                Late_Stage = colSums(top_15_pe[,18]),
                                Fund_of_Funds = colSums(top_15_pe[,19]) + colSums(top_15_pe[,27]),
                                Growth = colSums(top_15_pe[,20]),
                                Secondaries = colSums(top_15_pe[,13]) + colSums(top_15_pe[,28]),
                                Venture = colSums(top_15_pe[,31]),
                                Other = colSums(top_15_pe[,11]) + colSums(top_15_pe[,12]) + colSums(top_15_pe[,14]) + 
                                  colSums(top_15_pe[,21]) + colSums(top_15_pe[,22]) + colSums(top_15_pe[,23]) + colSums(top_15_pe[,24]) + 
                                  colSums(top_15_pe[,25]) + colSums(top_15_pe[,26]) + colSums(top_15_pe[,29]) + colSums(top_15_pe[,30]))

sum_by_category_2 <- t(sum_by_category_2)

sum_by_category_2 <- data.frame(sum_by_category_2)

sum_by_category_2 <- rownames_to_column(sum_by_category_2, var = "Category")

colnames(sum_by_category_2) <- c("Category", "Committed (MN)")

sum_by_category_2 <- sum_by_category_2 %>%
  mutate(`Committed (MN)` = `Committed (MN)`/1000)

sum_by_category_2 <- sum_by_category_2 %>% 
  arrange(desc(`Category`))

sum_by_category_2 <- sum_by_category_2 %>% 
  mutate(prop = `Committed (MN)` / sum(`Committed (MN)`) *100)


# Add a column to each data set to identify the source
sum_by_category_1$Sample <- "Top 100"
sum_by_category_2$Sample <- "Top 15"

# Combine the two data sets
combined_data <- bind_rows(sum_by_category_1, sum_by_category_2)

side_by_side_stacked_bar_graph <- ggplot(combined_data, aes(x = Sample, 
                                                            y = prop, 
                                                            fill = `Category`)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "darkblue", "indianred", 
                               "cornsilk3", "cadetblue", "maroon4", 
                               "cadetblue2", "hotpink2", "darkolivegreen3",
                               "steelblue"),
                    labels = c("Balanced", "Buyout", "Coinvestment", "Early Stage",
                               "Fund of Funds", "Growth", "Late Stage", "Other",
                               "Secondaries", "Venture")) +
  labs(title = "Comparison of Portfolio Distributions", 
       fill = "Fund Type",
       x = NULL,
       y = "Allocation of Committed Funds (%)",
       caption = str_wrap("Source: Preqin Pro. Comparison of top 100 and top 15 pension fund's committed funds to private equity by fund type. Other includes hybrid, private debt, infrastructure, and natural resources. Accessed 07/01/24.", 100)) +
  theme(plot.caption = element_text(hjust = 0))
side_by_side_stacked_bar_graph




#Create stacked bar graphs
a <- sum(top_15_pe$`AUM (USD MN)`)
b <- sum(top_100_pe$`AUM (USD MN)`)
c <- b-a


stacked_bar_graph <- ggplot(sum_by_category_1, aes(x = "Top 15 Pension Funds", 
                                                   y = `Committed (MN)`, 
                                                   fill = `Category`)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "darkblue", "indianred", 
                               "cornsilk3", "cadetblue", "maroon4", 
                               "cadetblue2", "hotpink2", "darkolivegreen3",
                               "steelblue"),
                    labels = c("Balanced", "Buyout", "Coinvestment", "Early Stage",
                               "Fund of Funds", "Growth", "Late Stage", "Other",
                               "Secondaries", "Venture")) +
  labs(title = "Figure A1: Top 15 Pension Funds' Private Equity Portfolio Breakdown", 
       fill = "Fund Type",
       x = NULL,
       y = "Committed (MN)",
       caption = str_wrap("Source: Preqin Pro. Top 15 pension funds' committed funds to private equity by fund type. Other includes hybrid, private debt, infrastructure,
                          and natural resources. Total committed funds are roughly $750 billion. Accessed 04/15/24.", 94)) +
  theme(plot.caption = element_text(hjust = 0))

stacked_bar_graph







