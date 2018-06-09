##################################################################
# Project: LPPI June 5 Primary
# Title: LA County Initial Turnout Analysis 
# Authors: Bryan Wilcox-Archuleta
# Begin: June 9, 2018
# Update: June 9, 2018
##################################################################

# read in data from LA County
la <- read_csv("la_county_gov.csv")

# this line was used in version 1 - had to remove a * 
# since the data were gathered from PDFs
la$precinct <- gsub("\\*","",la$PRECINCT)

# We only care about the the total row and Ballots Cast 
la <- la %>% filter(TYPE == "TOTAL") %>% 
  group_by(precinct) %>%
  summarise(totvote18 = sum(`BALLOTS CAST`, na.rm = T), 
            reg_18 = sum(REGISTRATION, na.rm = T)) 

# Read in 2014 Information
prec <- read_csv("allprecincts_june8.csv")

# clean data up and subset to LA County
prec <- prec %>% select(srprec_key, totvote14, county_n, percent_lat) %>% 
  filter(county_n == 37) %>% 
  mutate(precinct = substring(srprec_key, 6,15))

# Join two dataframes
res <- left_join(prec, la, by = "precinct")

# create ballot change measure
res <- res %>% mutate(ballots_change = ((totvote18 - totvote14) / totvote14)*100)

# save for later analysis 
# write_csv(res, "la_county_ballots_cast_14_18.csv")

# plot 
ggplot(res, aes(x = percent_lat, y = ballots_change)) + 
  geom_point(alpha = .25) + 
  geom_smooth(method = "lm", se = F) + 
  ylim(c(-50,200)) + 
  labs(y = "% Change in Ballots Cast", x = "% Latino in Precinct") + 
  theme_bw() +
  #geom_hline(yintercept = 25, lty = 2, color = "red", size = 1) + 
  ggsave("figures/pct_change_la_county_4.png")


# Highest Latino precincts
highest_latino <- res %>% filter(percent_lat >= 75)
highest_latino <- highest_latino %>% 
  mutate(growth = case_when(ballots_change < 0 ~ "Decrease in Ballots",
                            ballots_change > 0 & ballots_change < 50 ~ "0%-50% Increase", 
                            ballots_change > 50 ~ "> 50% Increase"))

# calculate # and %
table(highest_latino$growth)
round(prop.table(table(highest_latino$growth)),3)*100


# lowest latino preicnts
lowest_latino <- res %>% filter(percent_lat <= 25)
lowest_latino <- lowest_latino %>% 
  mutate(growth = case_when(ballots_change < 0 ~ "Decrease in Ballots",
                            ballots_change > 0 & ballots_change < 50 ~ "0%-50% Increase", 
                            ballots_change > 50 ~ "> 50% Increase"))

# calculate # and %
table(lowest_latino$growth)
round(prop.table(table(lowest_latino$growth)),3)*100

