################################################
### Public Plans Data & Graphs ## 07/01/2024 ###
################################################

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
setwd("C:/Users/hjkar/Documents/School/1-UMass/PERI_RA/Data/Public Plans Data")
ppd_data_full <- read_csv("ppd-data-latest2023.csv")

#Cleaning PPD Data
ppd_data_full <- ppd_data_full %>%
  filter(fy > 2000)

ppd_data <- ppd_data_full %>%
  group_by(ppd_id, fy) %>%
  select(ppd_id, fy, PlanType, MktAssets_net,
         RETotal_Actl, PETotal_Actl, OtherTotal_Actl,
         HFTotal_Actl, FITotal_Actl, EQTotal_Actl,
         COMDTotal_Actl, CashTotal_Actl, AltMiscTotal_Actl,
         RETotal_Rtrn, PETotal_Rtrn, OtherTotal_Rtrn,
         HFTotal_Rtrn, FITotal_Rtrn, EQTotal_Rtrn,
         COMDTotal_Rtrn, CashTotal_Rtrn, AltMiscTotal_Rtrn)

#ppd_data <- ppd_data %>%
#  group_by(ppd_id) %>%
#  mutate(MktAssets_net = MktAssets_net/1000000)

ppd_data <- ppd_data %>%
  mutate(MktAssets_net = as.numeric(MktAssets_net),
         RETotal_Actl = as.numeric(RETotal_Actl), 
         PETotal_Actl = as.numeric(PETotal_Actl), 
         OtherTotal_Actl = as.numeric(OtherTotal_Actl),
         HFTotal_Actl = as.numeric(HFTotal_Actl), 
         FITotal_Actl = as.numeric(FITotal_Actl), 
         EQTotal_Actl = as.numeric(EQTotal_Actl),
         COMDTotal_Actl = as.numeric(COMDTotal_Actl), 
         CashTotal_Actl = as.numeric(CashTotal_Actl), 
         AltMiscTotal_Actl = as.numeric(AltMiscTotal_Actl),
         RETotal_Rtrn = as.numeric(RETotal_Rtrn), 
         PETotal_Rtrn = as.numeric(PETotal_Rtrn), 
         OtherTotal_Rtrn = as.numeric(OtherTotal_Rtrn),
         HFTotal_Rtrn = as.numeric(HFTotal_Rtrn), 
         FITotal_Rtrn = as.numeric(FITotal_Rtrn), 
         EQTotal_Rtrn = as.numeric(EQTotal_Rtrn),
         COMDTotal_Rtrn = as.numeric(COMDTotal_Rtrn), 
         CashTotal_Rtrn = as.numeric(CashTotal_Rtrn), 
         AltMiscTotal_Rtrn = as.numeric(AltMiscTotal_Rtrn))

#PPD Data and graphs for alternative investments
ppd_data %>%
  group_by(ppd_id, fy) %>%
  select(MktAssets_net, RETotal_Actl, PETotal_Actl, OtherTotal_Actl,
         HFTotal_Actl, FITotal_Actl, EQTotal_Actl,
         COMDTotal_Actl, CashTotal_Actl, AltMiscTotal_Actl) %>%
  summary()

#ppd_data <- na.omit(ppd_data)

#ppd_data <- ppd_data %>%
#  mutate_all(funs(ifelse(is.na(.), 0, .)))

#Note that PPD reports AUM in thousands, so we convert to billions here
averages_by_year <- ppd_data %>%
  group_by(fy) %>%
  summarise(
    Avg_Mkt = mean(MktAssets_net/1000000, na.rm= TRUE),
    Avg_RE = 100*mean(RETotal_Actl, na.rm = TRUE),
    Avg_PE = 100*mean(PETotal_Actl, na.rm = TRUE),
    Avg_Other = 100*mean(OtherTotal_Actl, na.rm = TRUE),
    Avg_HF = 100*mean(HFTotal_Actl, na.rm = TRUE),
    Avg_FI = 100*mean(FITotal_Actl, na.rm = TRUE),
    Avg_EQ = 100*mean(EQTotal_Actl, na.rm = TRUE),
    Avg_COM = 100*mean(COMDTotal_Actl, na.rm = TRUE),
    Avg_Cash = 100*mean(CashTotal_Actl, na.rm = TRUE),
    Avg_Alt = 100*mean(AltMiscTotal_Actl, na.rm = TRUE),
    Avg_FI_C = 100*mean((FITotal_Actl + CashTotal_Actl), na.rm = TRUE),
    Avg_Alternatives = 100*mean((RETotal_Actl + PETotal_Actl +
                                   HFTotal_Actl + COMDTotal_Actl), na.rm = TRUE)
  )
averages_by_year

#file_path <- "averages_by_year.xlsx"
#write_xlsx(averages_by_year, path = file_path)

#scaling_factor <- max(averages_by_year$Avg_Alt)/max(averages_by_year$Avg_RE)
#portfolio_allocations1 <- ggplot(averages_by_year, aes(x = fy, y = Avg_RE)) +
#  geom_line(aes(color="Real Estate")) + 
#  geom_hline(yintercept = 0, color="black") +
#  labs(title="Pension Fund Average Portfolio Allocations - 2001-2022",
#       x ="Year", y = "Percentage Share of Portfolio",
#       color = "Asset Type",
#       caption = str_wrap("Source: Public Plans Data (PPD) for public pension plans in the U.S. Shown as individual components, excluding commodities,
#                          cash, and miscellaneous other assets (see Aubry 2022).", 90)) + 
#  geom_line(aes(y = (Avg_PE), color = "Private Equity")) +
#  geom_line(aes(y = (Avg_HF), color = "Hedge Funds")) +
#  geom_line(aes(y = (Avg_FI)*scaling_factor, color = "Fixed Income (right)")) +
#  geom_line(aes(y = (Avg_EQ)*scaling_factor, color = "Equity (right)")) +
#  geom_line(aes(y = (Avg_Alt), color = "Alternatives")) +
#  scale_color_manual(values = c("darkblue", "cadetblue", 
#                                "cadetblue2", "darkred",
#                                "indianred", "lightsalmon")) +
#  scale_y_continuous(name = "Percentage Share of Portfolio",
#                     sec.axis = sec_axis(~./scaling_factor, name = "")) + 
#  theme(plot.caption = element_text(hjust = 0))
#portfolio_allocations1

portfolio_allocations2 <- ggplot(averages_by_year, aes(x = fy, y = Avg_EQ)) +
  geom_line(aes(color="Traditional Equities")) + 
  geom_hline(yintercept = 0, color="black") +
  labs(title="Pension Fund Average Portfolio Allocations - 2001-2023",
       x ="Year", y = "Percentage Allocation (%)",
       color = "Asset Class",
       caption = str_wrap("Source: Public Plans Data (PPD) for public pension plans in the U.S. The sum of real estate,
       private equity, hedge fund and commodity positions are combined as alternative assets (see Aubry 2022). Accessed 07/01/24.", 90)) + 
  geom_line(aes(y = (Avg_FI_C), color = "Fixed Income & Cash")) +
  geom_line(aes(y = (Avg_Alternatives), color = "Alternatives")) +
  scale_color_manual(values = c("darkblue", "darkred", 
                                "cadetblue")) + 
  theme(plot.caption = element_text(hjust = 0))
portfolio_allocations2


#Calculating average rates of return
averages_rr_year <- ppd_data %>%
  group_by(fy) %>%
  summarise(
    Avg_Mkt = mean(MktAssets_net/1000000, na.rm= TRUE),
    Avg_RE = 100*mean(RETotal_Rtrn, na.rm = TRUE),
    Avg_PE = 100*mean(PETotal_Rtrn, na.rm = TRUE),
    Avg_Other = 100*mean(OtherTotal_Rtrn, na.rm = TRUE),
    Avg_HF = 100*mean(HFTotal_Rtrn, na.rm = TRUE),
    Avg_FI = 100*mean(FITotal_Rtrn, na.rm = TRUE),
    Avg_EQ = 100*mean(EQTotal_Rtrn, na.rm = TRUE),
    Avg_COM = 100*mean(COMDTotal_Rtrn, na.rm = TRUE),
    Avg_Cash = 100*mean(CashTotal_Rtrn, na.rm = TRUE),
    Avg_Alt = 100*mean(AltMiscTotal_Rtrn, na.rm = TRUE),
    Avg_FI_C = 100*mean(((FITotal_Actl/(FITotal_Actl + CashTotal_Actl))*FITotal_Rtrn 
                         + (CashTotal_Actl/(FITotal_Actl + CashTotal_Actl))*CashTotal_Rtrn), na.rm = TRUE),
    Avg_Alternatives = 100*mean(((RETotal_Actl/(RETotal_Actl + PETotal_Actl + HFTotal_Actl + COMDTotal_Actl))*RETotal_Rtrn 
                                 + (PETotal_Actl/(RETotal_Actl + PETotal_Actl + HFTotal_Actl + COMDTotal_Actl))*PETotal_Rtrn 
                                 + (HFTotal_Actl/(RETotal_Actl + PETotal_Actl + HFTotal_Actl + COMDTotal_Actl))*HFTotal_Rtrn 
                                 + (COMDTotal_Actl/(RETotal_Actl + PETotal_Actl + HFTotal_Actl + COMDTotal_Actl))*COMDTotal_Rtrn), na.rm = TRUE)
    )
averages_rr_year

rates_of_return <- ggplot(averages_rr_year, aes(x = fy, y = Avg_EQ)) +
  geom_line(aes(color="Traditional Equities")) + 
  geom_hline(yintercept = 0, color="black") +
  labs(title="Pension Fund Average Rates of Return - 2001-2023",
       x ="Year", y = "Rate of Return (%)",
       color = "Asset Class",
       caption = str_wrap("Source: Public Plans Data (PPD) for public pension plans in the U.S. The sum of real estate,
       private equity, hedge fund and commodity positions are combined as alternative assets (see Aubry 2022). Accessed 07/01/24.", 90)) + 
  geom_line(aes(y = (Avg_FI_C), color = "Fixed Income & Cash")) +
  geom_line(aes(y = (Avg_Alternatives), color = "Alternatives")) +
  scale_color_manual(values = c("darkblue", "darkred", 
                                "cadetblue")) + 
  theme(plot.caption = element_text(hjust = 0))
rates_of_return

averages_rr_long <- averages_rr_year %>%
  pivot_longer(cols = c(Avg_EQ, Avg_FI_C, Avg_Alternatives),
               names_to = "Asset_Class",
               values_to = "Rate_of_Return") %>%
  mutate(Asset_Class = recode(Asset_Class,
                              "Avg_EQ" = "Traditional Equities",
                              "Avg_FI_C" = "Fixed Income & Cash",
                              "Avg_Alternatives" = "Alternatives"))

rates_of_return2 <- ggplot(averages_rr_long, aes(x = fy, y = Rate_of_Return, fill = Asset_Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, color="black") +
  labs(title="Pension Fund Average Rates of Return - 2001-2023",
       x ="Year", y = "Rate of Return (%)",
       fill = "Asset Class",
       caption = str_wrap("Source: Public Plans Data (PPD) for public pension plans in the U.S. The sum of real estate,
       private equity, hedge fund and commodity positions are combined as alternative assets (see Aubry 2022). Accessed 07/01/24.", 90)) + 
  scale_fill_manual(values = c("Traditional Equities" = "cadetblue",
                               "Fixed Income & Cash" = "darkred",
                               "Alternatives" = "darkblue")) + 
  theme(plot.caption = element_text(hjust = 0))
rates_of_return2


ppd_data <- ppd_data %>%
  rowwise() %>%
  mutate(
    total_return = sum(
      if_else(is.na(FITotal_Actl) | is.na(FITotal_Rtrn), 0, FITotal_Actl * FITotal_Rtrn),
      if_else(is.na(CashTotal_Actl) | is.na(CashTotal_Rtrn), 0, CashTotal_Actl * CashTotal_Rtrn),
      if_else(is.na(EQTotal_Actl) | is.na(EQTotal_Rtrn), 0, EQTotal_Actl * EQTotal_Rtrn),
      if_else(is.na(RETotal_Actl) | is.na(RETotal_Rtrn), 0, RETotal_Actl * RETotal_Rtrn),
      if_else(is.na(PETotal_Actl) | is.na(PETotal_Rtrn), 0, PETotal_Actl * PETotal_Rtrn),
      if_else(is.na(HFTotal_Actl) | is.na(HFTotal_Rtrn), 0, HFTotal_Actl * HFTotal_Rtrn),
      if_else(is.na(COMDTotal_Actl) | is.na(COMDTotal_Rtrn), 0, COMDTotal_Actl * COMDTotal_Rtrn),
      if_else(is.na(OtherTotal_Actl) | is.na(OtherTotal_Rtrn), 0, OtherTotal_Actl * OtherTotal_Rtrn),
      if_else(is.na(AltMiscTotal_Actl) | is.na(AltMiscTotal_Rtrn), 0, AltMiscTotal_Actl * AltMiscTotal_Rtrn),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

averages_total_rr <- ppd_data %>%
  group_by(fy) %>%
  summarise(Avg_Total_rr = 100*mean(total_return, na.rm = TRUE))
averages_total_rr

total_rr <- ggplot(averages_total_rr, aes(x = fy, y = Avg_Total_rr)) +
  geom_line(color="darkblue") + 
  geom_hline(yintercept = 0, color="black") +
  labs(title="Pension Fund Average Rates of Return - 2001-2023",
       x ="Year", y = "Rate of Return (%)",
       caption = str_wrap("Source: Public Plans Data (PPD) for public pension plans in the U.S. Accessed 07/01/24.", 90)) +
  theme(plot.caption = element_text(hjust = 0))
total_rr 

averages_rr_long <- averages_rr_long %>%
  left_join(averages_total_rr, by = "fy")

rates_of_return3 <- ggplot(averages_rr_long, aes(x = fy, y = Rate_of_Return, fill = Asset_Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, color="black") +
  geom_line(aes(y = Avg_Total_rr, group = 1, color = "Average Total Return"), size = 1) + # Added line for average total return
  labs(title="Pension Fund Average Rates of Return - 2001-2023",
       x ="Year", y = "Rate of Return (%)",
       fill = "Asset Class",
       caption = str_wrap("Source: Public Plans Data (PPD) for public pension plans in the U.S. The sum of real estate,
       private equity, hedge fund and commodity positions are combined as alternative assets (see Aubry 2022). Accessed 07/01/24.", 90),
       color = "Total Portfolio") +  # Added legend for the line
  scale_fill_manual(values = c("Traditional Equities" = "cadetblue",
                               "Fixed Income & Cash" = "darkred",
                               "Alternatives" = "darkblue")) + 
  scale_color_manual(values = c("Average Total Return" = "lightsalmon")) +  # Color for the line
  theme(plot.caption = element_text(hjust = 0))
rates_of_return3


#Pulling in data for 60/40 equity bond split
russell_3000 <- read.csv("Russell3000.csv")
russell_3000 <- russell_3000 %>%
  filter(fy > 2000) %>%
  select(-Volume, -Adj.Close)

bonds <- read.csv("Bonds.csv")
bonds <- bonds %>%
  filter(fy > 2000)

russell_3000 <- russell_3000 %>%
  left_join(bonds, by = "fy")

russell_3000 <- russell_3000 %>%
  mutate(split_rr = (0.6)*(Return) + (0.4)*(bond_rr))

averages_true_split <- averages_rr_year %>%
  select(fy, Avg_EQ, Avg_FI_C)

averages_true_split <- averages_true_split %>%
  mutate(true_split_rr = (0.6)*Avg_EQ + (0.4)*Avg_FI_C)

split_comparison <- russell_3000 %>%
  select(fy, split_rr)

split_comparison <- split_comparison %>%
  left_join(averages_total_rr, by="fy")

split_comparison <- split_comparison %>%
  left_join(averages_true_split, by="fy")

split_comparison <- split_comparison %>%
  select(-Avg_EQ, -Avg_FI_C)

split_comparison_long <- split_comparison %>%
  pivot_longer(cols = c(Avg_Total_rr, split_rr, true_split_rr),
               names_to = "Portfolios",
               values_to = "Rate_of_Return") %>%
  mutate(Portfolios = recode(Portfolios,
                              "Avg_Total_rr" = "PPD Portfolio",
                              "split_rr" = "60/40 Simulation",
                              "true_split_rr" = "60/40 PPD"))

rates_of_return4 <- ggplot(split_comparison_long, aes(x = fy, y = Rate_of_Return, fill = Portfolios)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, color="black") +
  labs(title="Comparing Real to Simulated Portfolios - 2001-2023",
       x ="Year", y = "Rate of Return (%)",
       fill = "Portfolio",
       caption = str_wrap("Source: Public Plans Data (PPD) is used to calcualte the true average rate of return for pension fund portfolios. Simulated portfolios with a 60/40 split for equiities and bonds are calculated using the Russell 3000 index and
                          the ICE BoFA US Corporate Index or the rates of return for equities and fixed income and cash stated from PPD. Accessed 07/01/24.", 90)) + 
  scale_fill_manual(values = c("PPD Portfolio" = "cadetblue",
                               "60/40 Simulation" = "darkred",
                               "60/40 PPD" = "darkblue")) + 
  theme(plot.caption = element_text(hjust = 0))
rates_of_return4


#Calculating rolling averages
rolling_averages <- split_comparison %>%
  select(fy, Avg_Total_rr, split_rr) %>%
  mutate(rollmean_5_true = rollmean(Avg_Total_rr, k = 5, fill = NA, align = "right"),
         rollmean_10_true = rollmean(Avg_Total_rr, k = 10, fill = NA, align = "right"),
         rollmean_5_sim = rollmean(split_rr, k = 5, fill = NA, align = "right"),
         rollmean_10_sim = rollmean(split_rr, k = 10, fill = NA, align = "right")) %>%
  mutate(rollmean_5_diff = rollmean_5_true - rollmean_5_sim,
         rollmean_10_diff = rollmean_10_true - rollmean_10_sim) %>%
  select(fy, rollmean_5_diff, rollmean_10_diff)

rolling_averages$year_range_5 <- NA
for (i in 5:nrow(rolling_averages)) {
  rolling_averages$year_range_5[i] <- paste(rolling_averages$fy[i-4], rolling_averages$fy[i], sep = "-")
}

rolling_averages$year_range_10 <- NA
for (i in 10:nrow(rolling_averages)) {
  rolling_averages$year_range_10[i] <- paste(rolling_averages$fy[i-9], rolling_averages$fy[i], sep = "-")
}

rolling_averages_5 <- rolling_averages %>% 
  filter(!is.na(rollmean_5_diff)) %>%
  select(year_range_5, rollmean_5_diff)

rolling_averages_10 <- rolling_averages %>% 
  filter(!is.na(rollmean_10_diff)) %>%
  select(year_range_10, rollmean_10_diff)

ggplot(rolling_averages_5, aes(x = year_range_5, y = rollmean_5_diff)) +
  geom_bar(stat = "identity", fill="darkred") +
  labs(title = "5-Year Rolling Average of Returns Relative to Indexed Portfolio",
       x = "Year Range",
       y = "Percentage Difference (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(rolling_averages_10, aes(x = year_range_10, y = rollmean_10_diff)) +
  geom_bar(stat = "identity", fill="darkred") +
  labs(title = "10-Year Rolling Average of Returns Relative to Indexed Portfolio",
       x = "Year Range",
       y = "Percentage Difference (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












