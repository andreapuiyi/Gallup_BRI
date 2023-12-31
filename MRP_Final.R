
#load library 
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(ggrepel)
setwd("~/Desktop/MA 2023 Fall 2/MRP/Gallup data")

#load data
BRI_list <- read_excel("23_12_BRI-countries-public.xlsx")

Data <- read_csv("Analysis.csv")
aiddata <- read_csv("~/Desktop/MA 2023 Fall 2/MRP/Gallup data/bri.csv")

bridata <- read_csv("bri.csv")
GDP <- read_excel("GDP.xlsx")
Import <- read_excel("Import.xlsx")
Export <- read_excel("Export.xlsx")
Final <- read_csv("Final1.csv") #to explore the data of gallup dataset 

#MERGE DATA
#merge final with Data in order to get the approval rate before 2011
#Final_1 <- Final %>%
#  filter(YEAR_CALENDAR <= 2011)


#recode BRI_list
BRI_list <- BRI_list %>%
  mutate(COUNTRY_ISO3 = `Country Code`)

BRI_list <- BRI_list %>%
  mutate(year_column = year(`Likely date of joining`))


aiddata_new <- aiddata %>%
  mutate(COUNTRY_ISO3 = `Recipient ISO-3`) %>%
  select("Recipient", "Recipient Region", "Implementation Start Year", "Flow Type", "COUNTRY_ISO3", "Commitment Year", "Flow Class")


BRI <- left_join(aiddata_new, BRI_list, by = "COUNTRY_ISO3")

BRI$"Commitment Year" <- as.numeric(BRI$"Commitment Year")
BRI$year_column <- as.numeric(BRI$year_column)

BRI <- BRI %>%  rename(commitment_year = "Commitment Year") 


BRI <- BRI%>% 
  mutate(BRI_before = ifelse(year_column >commitment_year, 1, 0)) #project before BRI

BRI <- BRI%>% 
  mutate(BRI_after = ifelse(commitment_year>= year_column , 1, 0)) #project after BRI

#merge gdp data with other dataset
GDP_1 <- GDP %>%  rename(COUNTRY_ISO3 = "Country Code") %>%
  select(-c("Country Name", "Indicator Name", "Indicator Code"))

long_data <- pivot_longer(GDP_1, cols = -COUNTRY_ISO3, names_to = "YEAR_CALENDAR", values_to = "GDP") 
long_data$YEAR_CALENDAR <- as.numeric(long_data$YEAR_CALENDAR)
Data <- left_join(Data, long_data, by = c("COUNTRY_ISO3", "YEAR_CALENDAR"))

#merge Data with BRI
#BRI_a <- BRI %>% select(COUNTRY_ISO3, `Likely date of joining`)

#  BRI_unique <- BRI_a %>%
 # distinct(COUNTRY_ISO3, .keep_all = TRUE)

#Data_1 <- left_join(Data, BRI_unique, by = c("COUNTRY_ISO3"))
  
#merge import with other dataset

Import_1 <- Import %>%  rename(COUNTRY_ISO3 = "Country Code") %>%
  select(-c("Country Name", "Indicator Name", "Indicator Code"))

long_data_1 <- pivot_longer(Import_1, cols = -COUNTRY_ISO3, names_to = "YEAR_CALENDAR", values_to = "China_import") 
long_data_1$YEAR_CALENDAR <- as.numeric(long_data_1$YEAR_CALENDAR)

Data <- left_join(Data, long_data_1, by = c("COUNTRY_ISO3", "YEAR_CALENDAR"))

#merge export with other dataset

Export_1 <- Export %>%  rename(COUNTRY_ISO3 = "Country Code") %>%
  select(-c("Country Name", "Indicator Name", "Indicator Code"))

long_data_2 <- pivot_longer(Export_1, cols = -COUNTRY_ISO3, names_to = "YEAR_CALENDAR", values_to = "China_export") 
long_data_2$YEAR_CALENDAR <- as.numeric(long_data_2$YEAR_CALENDAR)

Data <- left_join(Data, long_data_2, by = c("COUNTRY_ISO3", "YEAR_CALENDAR"))


  
#recode====
df_no_na <- Data[!is.na(Data$WP156), ] #remove the NA from WP156
#df_no_na <- df_no_na[df_no_na$WP156 != 3, ]
#df_no_na <- df_no_na[df_no_na$WP156 != 4, ]

#rename column names
df_no_na <- df_no_na %>%
  rename(Approval_rate = WP156, Employment = EMP_2010, Income = INCOME_4, Gender = WP1219, Num_per_year = n,
         Age = WP1220, Education = WP3117)


#recode WP156 from 1 (approve) and 2(disapprove) to -1(disapprove) and 1(approve), 3 and 4 to be 0 
df_no_na <- df_no_na %>%
  mutate(Approval_rate = ifelse(Approval_rate == 1, 1, 
                                ifelse(Approval_rate == 2, -1, 
                                       ifelse(Approval_rate == 3, 0,
                                              ifelse(Approval_rate == 4, 0, Approval_rate)))))

#recode education because 4 and 5 means dk or refused:https://www.unicef.org/globalinsight/media/2236/file
df_no_na$Education[df_no_na$Education == 4] <- NA
df_no_na$Education[df_no_na$Education == 5] <- NA
summary(df_no_na$Education)
#solve the prob of income variable
df_no_na[df_no_na == Inf] <- NA
answer <- log(df_no_na$Income)
summary(answer)

df_no_na$log <- log(df_no_na$Income)
df_no_na$logincome <- log(df_no_na$Income+ 1e-6)  # Add a small constant (e.g., 1e-6)
summary(df_no_na$logincome)
df_no_na$Income <- df_no_na$Income+ 1e-6
summary(df_no_na$nonlogincome)
summary(df_no_na$Income)

#trial1:a different way of recode - approve(1), disapprove(0)
df_no_na_trial1 <- df_no_na %>%
  mutate(Approval_rate = ifelse(Approval_rate == 1, 1, ifelse(Approval_rate == -1, 0, Approval_rate)))

#troubleshoot: code non-BRI country before and after for BRI too to do DiD
df_no_na <- df_no_na%>% 
  rename(joining_date = "Likely date of joining") 


#troubleshoot: code countries with no proj to 0 for Num_per_year
df_no_na <- df_no_na%>%
  mutate(Num_per_year = replace(Num_per_year, is.na(Num_per_year), 0))

summary(df_no_na$BRI_status)
summary(df_no_na$BRI_after)
summary(df_no_na$Num_per_year)

num_na <- sum(is.na(df_no_na$BRI_after))


df_no_na <- df_no_na%>% 
  mutate(BRI_after = ifelse(BRI_status == 1 & (joining_date <= YEAR_CALENDAR), 1, 0)) %>% #public opinion after BRI 
mutate(BRI_after = ifelse(BRI_status == 0 & YEAR_CALENDAR >= 2013, 1,0)) %>%
  mutate(BRI_after = ifelse(BRI_status == 1 & BRI_after == 0 & BRI_before == 0, 1, BRI_after)) 

df_no_na <- df_no_na %>% #code those BRI countries with no commtiment years 
  mutate(BRI_after = ifelse(is.na(joining_date) & YEAR_CALENDAR >= 2013, 1, 0))

subset_with_na <- df_no_na %>%
  filter(is.na(BRI_after)) #checkL no more NA 

summary(df_no_na$BRI_after)

#troubleshoot:231229: there is still prob with bri after 
library(lubridate)

# Assuming your dataframe is named 'your_data' and the column is 'joining_year'
df_no_na <- df_no_na %>%
  mutate(joining_year_only = year(as.Date(joining_date)))

#mutate
df_no_na <- df_no_na %>%
  mutate(BRI_after = ifelse(joining_year_only < YEAR_CALENDAR, 1, BRI_after)) %>%
  mutate(BRI_after = ifelse(BRI_status == 1 & (joining_date <= YEAR_CALENDAR), 1, 0)) %>% #public opinion after BRI 
  mutate(BRI_after = ifelse(BRI_status == 0 & YEAR_CALENDAR >= 2013, 1,0)) %>%
  mutate(BRI_after = ifelse(BRI_status == 1 & BRI_after == 0 & BRI_before == 0, 1, BRI_after)) %>%
  mutate(BRI_after = ifelse(is.na(BRI_after) & YEAR_CALENDAR > 2013, 1, BRI_after)) %>%
  mutate(BRI_after = ifelse(is.na(BRI_after) & YEAR_CALENDAR <= 2013, 0, BRI_after)) 

df_no_na_na <- df_no_na %>%
  filter(is.na(BRI_after))

#check 
Vietnam_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "VNM") #some NA in 2022(need to cross check)

Ethopia_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "ETH") #PROB for joining to leaving too

Myanmar_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "MMR") 

Iran_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "IRN") 

#graph====
#graph for oda and oof
oda_oof <- aiddata %>%
  filter(`Flow Class` == "ODA-like" | `Flow Class` == "OOF-like" ) %>%
  group_by(`Commitment Year`, `Flow Class`) %>%
  count()

ggplot(oda_oof, aes(x = `Commitment Year`, y = n, color = `Flow Class`, group = `Flow Class`)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2001, 2023, by = 1)) +
  labs(
    title = "Chinese Development Finance (ODA and OOF)",
    x = "Year",
    y = "Number of Projects"
  ) +
  theme_bw()

#graph for gallup: general trend of approval rate towards China 

#recode WP156 from 1 (approve) and 2(disapprove) to -1(disapprove) and 1(approve), 3 and 4 to be 0 
Final <- Final %>%
  mutate(Approval_rate = ifelse(WP156 == 1, 1, 
                                ifelse(WP156 == 2, -1, 
                                       ifelse(WP156 == 3, 0,
                                              ifelse(WP156 == 4, 0, WP156)))))
Final <- Final %>%
  mutate(Approval_rate_1 = ifelse(WP156 == 1, 1, 
                                ifelse(WP156 == 2, -1, 
                                       ifelse(WP156 == 3, NA,
                                              ifelse(WP156 == 4, NA, WP156)))))

gallup_average_opinion <- 
  Final %>%
  filter(!COUNTRY_ISO3 == "CHN") %>%
  filter(!WP3117 == "4") %>%
  filter(!WP3117 == "5") %>%
  group_by(YEAR_CALENDAR, WP3117) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE))  

gallup_average_opinion_1 <- 
  Final %>%
  filter(!COUNTRY_ISO3 == "CHN") %>%
  filter(!WP3117 == "4") %>%
  filter(!WP3117 == "5") %>%
  group_by(YEAR_CALENDAR, WP3117) %>%
  summarise(mean = mean(Approval_rate_1, na.rm = TRUE))  

gallup_average_opinion <-  gallup_average_opinion %>% mutate(Education = ifelse(WP3117 == 1, "Primary", 
                                                         ifelse(WP3117 == 2, "Seconary", 
                                                                ifelse(WP3117 == 3, "Tertiary", WP3117))))


gallup_average_opinion_1 <-  gallup_average_opinion_1 %>% mutate(Education = ifelse(WP3117 == 1, "Primary", 
                                                                                ifelse(WP3117 == 2, "Seconary", 
                                                                                       ifelse(WP3117 == 3, "Tertiary", WP3117))))




#education level and gallup public opinion 
ggplot(gallup_average_opinion, aes(x = YEAR_CALENDAR, y = mean, color = Education, group = Education)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2001, 2023, by = 1)) +
  labs(
    title = "Gallup Public Opinion towards China (2001-2023) by Education",
    x = "Year",
    y = "Average Approval Points"
  ) +
  theme_bw()

ggplot(gallup_average_opinion_1, aes(x = YEAR_CALENDAR, y = mean, color = Education, group = Education)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2001, 2023, by = 1)) +
  labs(
    title = "Gallup Public Opinion towards China (2001-2023) by Education",
    x = "Year",
    y = "Average Approval Points"
  ) +
  theme_bw()


#graph for top 15 recipient countries 
top_countries <- aiddata %>% 
  group_by(Recipient) %>%
  count() %>%
  arrange(desc(n)) %>%
  top_n(n>254) %>%
  ungroup()

ggplot(top_countries, aes(x = reorder(Recipient, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 15 Recipients(2000-2021) ",
       x = "Recipient",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +coord_flip()

#graph for projects number over years
df_year <- bridata %>% 
  group_by(`Commitment Year`) %>%
  count() 

bri_year <- BRI%>% 
  filter(BRI_after == 1) %>% 
  group_by(commitment_year) %>%
  count() 

table(bri_year$commitment_year)

ggplot(df_year, aes(x = `Commitment Year`, y = n)) +
  geom_line() +
  labs(title = "China's Overseas Developent Finance (2000-2021)",
       x = "Year",
       y = "Number of Projects") +
  theme_bw() +
  geom_vline(xintercept = 2013, 
             color = "blue", size=0.5) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
  annotate(geom="text", x=2010, y=1700, label="Announcement of BRI",
           color="blue") 

#graph for development finance and BRI 
ggplot() +
  geom_line(data = df_year, aes(x = `Commitment Year`, y = n), color = "red") +
  geom_line(data = bri_year, aes(x = commitment_year, y = n), color = "black") +
  theme_bw() +
  geom_vline(xintercept = 2013, 
             color = "blue", size=0.5) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
  annotate(geom="text", x=2010, y=1700, label="Announcement of BRI",
           color="blue") +
  labs(title = "China's Overseas Developent Finance (2000-2021)",
       x = "Commtiment Year",
       y = "Number of Projects") +
  annotate(geom="text", x=2010, y=1200, label="All projects",
           color="red")+
  annotate(geom="text", x=2020, y=1000, label="BRI projects",
           color="black")

#public opinion graph====

#graph: 
global_average_opinion <- 
  df_no_na %>%
  filter(!COUNTRY_ISO3 == "CHN") %>%
  filter(!YEAR_CALENDAR == 2023) %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE)) 

bri_countries <- 
  df_no_na %>%
  filter(BRI_status == 1)%>%
  filter(!COUNTRY_ISO3 == "CHN") %>%
  filter(!YEAR_CALENDAR == 2023) %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE)) 

non_bri_countries<- 
  df_no_na %>%
  filter(BRI_status == 0)%>%
  group_by(YEAR_CALENDAR) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE)) 

ggplot() +
  geom_line(data = global_average_opinion, aes(x = YEAR_CALENDAR , y = mean), color = "black") +
  geom_line(data = bri_countries, aes(x = YEAR_CALENDAR , y = mean), color = "red") +
  geom_line(data = non_bri_countries, aes(x = YEAR_CALENDAR , y = mean), color = "blue") +
  scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
  labs(title = "Global Public Opinion towards China (2011-2022) ",
       x = "Year",
       y = "Average Approval Points") +
  geom_vline(xintercept = 2013, 
             color = "darkgreen", size=0.5) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 1)) +
  annotate(geom="text", x=2014.8, y=0.5, label="Announcement of BRI",
           color="darkgreen") +
  annotate(geom="text", x=2017, y=0.1, label="Average",
           color="black") +
  annotate(geom="text", x=2017, y=0.3, label="BRI Countries",
           color="red") +
  annotate(geom="text", x=2017, y=-0.25, label="non-BRI Countries",
           color="blue") +
  theme_bw() 


#scatterplot===
scatterplot_approval <-  df_no_na %>%
  group_by(COUNTRY_ISO3) 

scatterplot_approval <- scatterplot_approval %>%
  mutate(approval_2013 = ifelse(YEAR_CALENDAR == 2013, Approval_rate, NA),
         approval_2014 = ifelse(YEAR_CALENDAR == 2014, Approval_rate, NA),
         approval_2022 = ifelse(YEAR_CALENDAR == 2022, Approval_rate, NA),
         approval_2021 = ifelse(YEAR_CALENDAR == 2021, Approval_rate, NA))%>%
  group_by(COUNTRY_ISO3, BRI_status)  %>%
  summarise(mean_approval_2013 = mean(approval_2013, na.rm = TRUE),
            mean_approval_2014 = mean(approval_2014, na.rm = TRUE),
            mean_approval_2021 = mean(approval_2021, na.rm = TRUE),
            mean_approval_2022 = mean(approval_2022, na.rm = TRUE) ) 


#with text (cluster i)
scatterplot_approval %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_manual(values = c("green", "red")) +  # Adjust colors as needed
  theme_bw() +
  geom_text(aes(label = COUNTRY_ISO3), hjust = 0, vjust = 0, size = 3, color = "black", check_overlap = TRUE) +
  geom_vline(xintercept = 0, 
             color = "black", size=0.5) +
  geom_hline(yintercept = 0, 
             color = "black", size=0.5) +
  annotate("text", x = -0.5, y = 0.6, 
           label = "I", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.6, 
           label = "II", size = 5, fontface = "bold") +
  annotate("text", x = -0.5, y = -0.2, 
           label = "IV", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, 
           label = "III", size = 5, fontface = "bold") +
  coord_cartesian(xlim =  c(-0.75, 0), ylim = c(0, 0.3)) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  )

#with text (cluster ii)
scatterplot_approval %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_manual(values = c("green", "red")) +  # Adjust colors as needed
  theme_bw() +
  geom_text(aes(label = COUNTRY_ISO3), hjust = 0, vjust = 0, size = 3, color = "black", check_overlap = TRUE) +
  geom_vline(xintercept = 0, 
             color = "black", size=0.5) +
  geom_hline(yintercept = 0, 
             color = "black", size=0.5) +
  annotate("text", x = -0.5, y = 0.6, 
           label = "I", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.6, 
           label = "II", size = 5, fontface = "bold") +
  annotate("text", x = -0.5, y = -0.2, 
           label = "IV", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, 
           label = "III", size = 5, fontface = "bold") +
  coord_cartesian(xlim =  c(0, 1), ylim = c(0, 1)) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  )

#with text (cluster iii)
scatterplot_approval %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_manual(values = c("green", "red")) +  # Adjust colors as needed
  theme_bw() +
  geom_text(aes(label = COUNTRY_ISO3), hjust = 0, vjust = 0, size = 3, color = "black", check_overlap = TRUE) +
  geom_vline(xintercept = 0, 
             color = "black", size=0.5) +
  geom_hline(yintercept = 0, 
             color = "black", size=0.5) +
  annotate("text", x = -0.5, y = 0.6, 
           label = "I", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.6, 
           label = "II", size = 5, fontface = "bold") +
  annotate("text", x = -0.5, y = -0.2, 
           label = "IV", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, 
           label = "III", size = 5, fontface = "bold") +
  coord_cartesian(xlim = c(0, 0.5), ylim = c(-0.75, 0)) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  ) 

#with text (cluster iv)
scatterplot_approval %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_manual(values = c("green", "red")) +  # Adjust colors as needed
  theme_bw() +
  geom_text(aes(label = COUNTRY_ISO3), hjust = 0, vjust = 0, size = 3, color = "black", check_overlap = TRUE) +
  geom_vline(xintercept = 0, 
             color = "black", size=0.5) +
  geom_hline(yintercept = 0, 
             color = "black", size=0.5) +
  annotate("text", x = -0.5, y = 0.6, 
           label = "I", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.6, 
           label = "II", size = 5, fontface = "bold") +
  annotate("text", x = -0.5, y = -0.2, 
           label = "IV", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, 
           label = "III", size = 5, fontface = "bold") +
  coord_cartesian(xlim = c(-0.75, 0), ylim = c(-1, 0)) +  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  ) 

#without text 
scatterplot_approval %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 2, alpha = 0.3) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  ) +
  theme_bw() 

#without text + categorisation
scatterplot_approval %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 2, alpha = 0.3) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  ) +
  theme_bw() +
  geom_vline(xintercept = 0, 
             color = "black", size=0.5) +
  geom_hline(yintercept = 0, 
             color = "black", size=0.5) +
  annotate("text", x = -0.5, y = 0.6, 
           label = "I", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.6, 
           label = "II", size = 5, fontface = "bold") +
  annotate("text", x = -0.5, y = -0.2, 
           label = "IV", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, 
           label = "III", size = 5, fontface = "bold") 


#look at 3 BRI countries in cluster 1 ====

cluster_one <- df_no_na %>%
  filter(COUNTRY_ISO3 %in% c("IRN", "MAR", "SLV")) %>%
  group_by(COUNTRY_ISO3, YEAR_CALENDAR) %>%
  summarise(mean_approval_rate = mean(Approval_rate, na.rm = TRUE))

cluster_one %>%
  ggplot() +
  geom_line(aes(x = YEAR_CALENDAR , y = mean_approval_rate, 
                color = COUNTRY_ISO3, group = COUNTRY_ISO3)) +
  scale_x_continuous(breaks = seq(2006, 2022, by = 1)) +
  theme_bw() +
  labs(title = "Cluster I (BRI Countries) ",
       x = "Year",
       y = "Average Approval Points", ,
       color = "Country") +
  scale_color_manual(values = c("IRN" = "red", "MAR" = "blue", "SLV" = "green"),
                     breaks = c("IRN", "MAR", "SLV"),
                     labels = c("Iran", "Morocco", "El Salvador")) +
  geom_vline(xintercept = 2013, 
             color = "black", size=0.5) +
  annotate(geom="text", x=2015.5, y=0.5, label="Announcement of BRI",
           color="black") 



#zoom into cluster ii for an abline
regression_clusterii <- scatterplot_approval %>%
  filter((mean_approval_2013 >= 0 & mean_approval_2013 <= 1) & (mean_approval_2022 >= 0 & mean_approval_2022 <= 1))

regression_clusterii %>%
  ggplot(aes(x = mean_approval_2013, y = mean_approval_2022, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 3, alpha = 0.3) +
  scale_color_manual(values = c("green", "red")) +  # Adjust colors as needed
  theme_bw() +
  geom_text(aes(label = COUNTRY_ISO3), hjust = 0, vjust = 0, size = 3, color = "black", check_overlap = TRUE) +
  geom_vline(xintercept = 0, 
             color = "black", size=0.5) +
  geom_hline(yintercept = 0, 
             color = "black", size=0.5) +
  annotate("text", x = -0.5, y = 0.6, 
           label = "I", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.6, 
           label = "II", size = 5, fontface = "bold") +
  annotate("text", x = -0.5, y = -0.2, 
           label = "IV", size = 5, fontface = "bold") +
  annotate("text", x = 0.5, y = -0.2, 
           label = "III", size = 5, fontface = "bold") +
  coord_cartesian(xlim =  c(0, 1), ylim = c(0, 1)) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  ) +geom_abline(intercept = 0, slope = 1, color = "blue") 




  

#the bri projects across continents
bri_continent <- BRI%>% 
  filter(BRI_after == 1) %>% 
  group_by(commitment_year) %>%
  count() 

bri_continent <- BRI %>%
  filter(BRI_after == 1) %>% 
  group_by(Region, Country) %>%
  summarise(BRI_after = n()) %>%
  arrange(desc(BRI_after)) %>%
  group_by(Region) %>%
  top_n(10)

ggplot(bri_continent, aes(x = reorder(Country, -BRI_after), y = BRI_after, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Top 10 Countries with Most Projects in Each Continent",
       x = "Country",
       y = "Number of Projects") +
  theme_minimal()

ggplot(bri_continent, aes(x = reorder(Country, BRI_after), y = BRI_after, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Region, scales = "free_y") +
  geom_text(aes(label = BRI_after), position = position_stack(vjust = 0.5), size = 2, hjust = -0.1) +
  labs(title = "Top 10 BRI Countries in Each Region",
       x = "Country",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") + # Hide the legend
  coord_flip()

#china export scatterplot (2021)
BRI_list <- BRI_list%>%
  mutate(BRI_status = 1)

export_scat <- 
 left_join(Export, BRI_list, by = c("Country Code" = "Country Code")) %>%
  select("2021", "2013", "Country Code", "BRI_status") %>%
  rename(Country = 'Country Code')

export_scat <-export_scat %>% 
  mutate(BRI_status = replace(BRI_status, is.na(BRI_status), 0))

  export_scat %>%
  ggplot(aes(x = 2013, y = 2021, color = factor(BRI_status))) +
  ggtitle("Scatterplot of Approval Rates (2013 vs. 2022)") +
  xlab("Approval Rate in 2013") +
  ylab("Approval Rate in 2022") +
  geom_point(size = 2, alpha = 0.3) +
  scale_color_manual(
    values = c("green", "red"),
    name = "BRI Status",
    labels = c("Non-BRI Countries", "BRI Countries")
  ) +
  theme_bw()   

  ggplot(export_scat, aes(x = log(`2021`), y = log(`2013`), color = as.factor(BRI_status))) +
    geom_point() +  # Add labels with repulsion to avoid overlap
    labs(x = "2021 Data", y = "2013 Data", title = "Scatterplot with BRI Status") +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +  # Assign colors to BRI status
    theme_minimal()
  
  
  ggplot(export_scat, aes(x = `2021`, y = `2013`, color = as.factor(BRI_status))) +
    geom_point() +  # Add labels with repulsion to avoid overlap
    labs(x = "2021 Data", y = "2013 Data", title = "Scatterplot with BRI Status") +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +  # Assign colors to BRI status
    theme_minimal()
  
#graoh for vietnam approval rate change over years
  
  viet_graph <- 
  Vietnam_Approval %>%
  group_by(YEAR_CALENDAR ) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE))  

  viet_graph <-  viet_graph %>% mutate(Education = ifelse(Education == 1, "Primary", 
                                                         ifelse(Education == 2, "Seconary", 
                                                                ifelse(Education == 3, "Tertiary", Education))))


#education level and viet graph
ggplot(viet_graph, aes(x = YEAR_CALENDAR, y = mean, color = Education, group = Education)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(2001, 2023, by = 1)) +
  labs(
    title = "Gallup Public Opinion towards China (2001-2023) by Education",
    x = "Year",
    y = "Average Approval Points"
  ) +
  theme_bw() 

ggplot(viet_graph, aes(x = YEAR_CALENDAR, y = mean)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(2001, 2023)) +
  labs(
    title = "Vietnam's Public Opinion towards China (2006-2022)",
    x = "Year",
    y = "Average Approval Points"
  ) +
  theme_bw() +
  geom_vline(xintercept = 2013, 
             color = "black", size=0.5) +
  annotate(geom="text", x=2014.8, y=0.5, label="Announcement of BRI",
           color="black") +
  geom_vline(xintercept = 2017, 
             color = "red", size=0.5) +
  annotate(geom="text", x=2019, y=0.5, label="Vietnam Joins the BRI",
           color="red")

#graph for myanmar approval rate change over years ====

myan_graph <- 
  Myanmar_Approval %>%
  group_by(YEAR_CALENDAR ) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE)) 

ggplot(myan_graph, aes(x = YEAR_CALENDAR, y = mean)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(2001, 2023)) +
  labs(
    title = "Myanmar's Public Opinion towards China (2012-2022)",
    x = "Year",
    y = "Average Approval Points"
  ) +
  theme_bw() +
  geom_vline(xintercept = 2013, 
             color = "black", size=0.5) +
  annotate(geom="text", x=2014, y=0.5, label="Announcement of BRI",
           color="black") +
  geom_vline(xintercept = 2016.8, 
             color = "red", size=0.5) +
  annotate(geom="text", x=2018, y=0.5, label="Myanmar Joins the BRI",
           color="red")


#graph for Ethiopia approval rate change over years ====

eth_graph <- 
  Ethopia_Approval %>%
  group_by(YEAR_CALENDAR ) %>%
  summarise(mean = mean(Approval_rate, na.rm = TRUE)) 

ggplot(eth_graph, aes(x = YEAR_CALENDAR, y = mean)) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(2001, 2023)) +
  labs(
    title = "Ethiopia's Public Opinion towards China (2012-2022)",
    x = "Year",
    y = "Average Approval Points"
  ) +
  theme_bw() +
  geom_vline(xintercept = 2013, 
             color = "black", size=0.5) +
  annotate(geom="text", x=2014, y=0.8, label="Announcement of BRI",
           color="black") +
  geom_vline(xintercept = 2016.8, 
             color = "red", size=0.5) +
  annotate(geom="text", x=2018, y=0.8, label="Ethiopia Joins the BRI",
           color="red")

#summary stat 
#install.packages("descr")
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable     # converting tables to pretty images
)
#install.packages("pacman")

df_no_na %>% tabyl(YEAR_CALENDAR)

?tabyl
library(psych)
#install.packages("psych")
describe(df_no_na)
?describe
#regression
library(stargazer)
DiD <- df_no_na$BRI_after*df_no_na$BRI_status

model1 <- lm(data = df_no_na, Approval_rate ~ BRI_status:BRI_after  + BRI_status + as.factor(Employment) + Gender + Education + Age + log(GDP) + log(China_import) + log(China_export) +log(Income),
             na.action = na.omit)

model2 <- lm(data = df_no_na, Approval_rate ~ BRI_status:Num_per_year   + BRI_status + as.factor(Employment) + Gender + Education + Age + log(GDP) + log(China_import) + log(China_export) +log(Income),
             na.action = na.omit)

model3 <- lm(data = df_no_na_trial1, Approval_rate ~ BRI_status:BRI_after  +BRI_status + as.factor(Employment) + Gender + Education + Age + log(GDP),
             na.action = na.omit)
model4 <- lm(data = df_no_na_trial1, Approval_rate ~ BRI_status:Num_per_year   + BRI_status + as.factor(Employment) + Gender + Education + Age + log(GDP),
             na.action = na.omit)

#without employment variable 
model5 <- lm(data = df_no_na, Approval_rate ~ BRI_status:BRI_after  + BRI_status + BRI_after+ Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
             na.action = na.omit)

model6 <- lm(data = df_no_na, Approval_rate ~ BRI_status:Num_per_year   + Num_per_year + BRI_status + Gender + Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
             na.action = na.omit)

#robustness check ====
df_no_na_robust_country_level <- df_no_na %>%
  group_by(COUNTRY_ISO3, YEAR_CALENDAR, BRI_after, Num_per_year, BRI_status, GDP, China_import, China_export) %>%
  summarise(Net_approval = mean(Approval_rate, na.rm = TRUE), Gender = mean(Gender, na.rm = TRUE),
            Age = mean(Age, na.rm = TRUE), Income = mean(Income, na.rm = TRUE ),
            Education = mean(Education, na.rm = TRUE ),
            Approve = mean(Approval_rate == 1, na.rm = TRUE ),
            Disapprove = mean(Approval_rate == -1, na.rm = TRUE ))

model7 <-  lm(data = df_no_na_robust_country_level, Net_approval ~ BRI_status:BRI_after  + BRI_status + BRI_after +Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model8 <-  lm(data = df_no_na_robust_country_level, Net_approval ~ BRI_status:Num_per_year   + BRI_status + Num_per_year + Gender + Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model9 <-  lm(data = df_no_na_robust_country_level, Approve ~ BRI_status:BRI_after  + BRI_status + BRI_after +Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model10 <-  lm(data = df_no_na_robust_country_level, Approve ~ BRI_status:Num_per_year   + BRI_status + Gender +Num_per_year+ Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model11 <-  lm(data = df_no_na_robust_country_level, Disapprove ~ BRI_status:BRI_after  + BRI_status + BRI_after  +Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model12 <-  lm(data = df_no_na_robust_country_level, Disapprove ~ BRI_status:Num_per_year   + BRI_status + Num_per_year +Gender + Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
               na.action = na.omit)

#analysis (scatterplot clusters) ===
Vietnam_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "VNM") #some NA in 2022(need to cross check)

Vietnam_country_level <- Vietnam_Approval %>%
  group_by(COUNTRY_ISO3, YEAR_CALENDAR, BRI_after, Num_per_year, BRI_status, GDP, China_import, China_export) %>%
  summarise(Approval_rate = mean(Approval_rate), Gender = mean(Gender, na.rm = TRUE),
            Age = mean(Age, na.rm = TRUE), Income = mean(Income, na.rm = TRUE ),
            Education = mean(Education, na.rm = TRUE ))
  
  group_by(YEAR_CALENDAR)

Ethopia_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "ETH") #PROB for joining to leaving too

Myanmar_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "MMR") 

NA_BRI <- (is.na(df_no_na_robust_country_level$BRI_after))
NA_BRI <- df_no_na_robust_country_level %>%
  filter(is.na(BRI_after))

Myanmar_Approval <- Myanmar_Approval[complete.cases(Myanmar_Approval$Age, Myanmar_Approval$Education), ]

Iran_Approval <- df_no_na %>%
  filter(COUNTRY_ISO3 == "IRN") 

model13 <- lm(data = Iran_Approval, Approval_rate ~ BRI_status:BRI_after  + BRI_status + Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model14 <- lm(data = Ethopia_Approval, Approval_rate ~ BRI_after   + Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model15 <- lm(data = Myanmar_Approval, Approval_rate ~ BRI_status:BRI_after  + BRI_status + Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model15 <- lm(data = Myanmar_Approval, Approval_rate ~ BRI_status:BRI_after  + BRI_status + Gender + Education + Age + log(Income) + log(GDP) )

model16 <- lm(data = Vietnam_country_level, Approval_rate ~   BRI_after  + Gender + Education + Age + log(Income) + log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)

model17 <- lm(data = Iran_Approval, Approval_rate ~ BRI_status:Num_per_year   + BRI_status + Gender + Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
             na.action = na.omit)
model18 <- lm(data = Ethopia_Approval, Approval_rate ~ Num_per_year   + Gender + Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)
model19 <- lm(data = Myanmar_Approval, Approval_rate ~ BRI_status:Num_per_year   + BRI_status + Gender + Education + Age + log(Income) + log(GDP) )

model20 <- lm(data = Vietnam_country_level, Approval_rate ~  Num_per_year + Gender + Education + Age + log(Income) +log(GDP) + log(China_import) + log(China_export),
              na.action = na.omit)


#bri_Status: whether a country is a BRI country
#bri_after: whether that country is a BRI country at a certain time 
stargazer(model1, model2, type = "text")
stargazer(model1, model2, model3, model4, type = "text")
stargazer(model5, model6, type = "text")
stargazer(model7, model8, model9, model10, model11, model12, type = "text") #robustness check
stargazer(model13, model14, model15, model16, type = "text")
stargazer(model17, model18, model19, model20, type = "text")
stargazer(model16, model20, type = "text") #vietnam
stargazer(model14, model18, type = "text") #ethipoa

summary(df_no_na$BRI_status)
summary(df_no_na$Num_per_year)
summary(df_no_na$Income)

#create map ===
#install.packages("sf")
#install.packages("rnaturalearth")
library(sf)
library(leaflet)
library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf") 

world <- world %>%
  rename(COUNTRY_ISO3 = sov_a3) 

world1 <- world %>%
  select(name, country)

Export_2021 <- Export%>%
  select(`Country Code`, "2021",  "Country Name") %>%
  rename(COUNTRY_ISO3 = `Country Code`) %>%
  rename(China_export = "2021") 
  
merged_data <- merge(world, df_no_na, by.x = "adm0_a3", by.y = "COUNTRY_ISO3", all.x = TRUE)
merged_data1 <- merge(world, Export_2021, by.x = "adm0_a3", by.y = "COUNTRY_ISO3", all.x = TRUE)

# Create a leaflet map
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = merged_data,
    fillColor = ~colorNumeric("Blues", domain = China_export)(China_export),
    fillOpacity = 0.7,
    color = "white",
    stroke = TRUE,
    weight = 1,
    label = ~paste(name, ": ", export, " million USD"),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = colorNumeric("Blues", domain = export),
    values = ~China_export,
    title = "Export (million USD)",
    opacity = 1
  )
library(terra)
library(dplyr)
#install.packages("spDataLarge")
library(spData)
library(spDataLarge)
#install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
                                   "https://cloud.r-project.org"))
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

tm_shape(nz) +
  tm_fill()
?tm_shape

ggplot() +
  geom_polygon(data = merged_data1, aes(x = long, y = lat, group = group, fill = fill_flg)) +
  geom_point(data = df.country_points, aes(x = lon, y = lat), color = "#e60000") +
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'Countries with highest "talent competitiveness"'
       ,subtitle = "source: INSEAD, https://www.insead.edu/news/2017-global-talent-competitiveness-index-davos") +
  theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
        ,panel.background = element_rect(fill = "#444444")
        ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 30)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none"
  )
#new attempt
library(viridis)
map.world <- map_data("world")
map.world_joined <- left_join(map.world, Export_2021, by = c('region' = "Country Name"))

ggplot(data = map.world_joined, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = China_export)) +
  scale_fill_viridis( alpha = 1,option = 'plasma')

#install.packages("viridis")
summary(df_no_na$Education)

US <- Export %>%
  filter(`Country Code` == "USA")
