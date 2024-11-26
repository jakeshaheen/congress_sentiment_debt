rm(list=ls())

library(tidyverse)
library(stm)
library(tm)
library(lubridate)

##### Load in the Data and Have just Democrats and Republicans
# Using "national debt" as the keyword phrase for dcinbox
ntldebt <- read_csv("dcinbox_export_national_debt.csv")

ntldebt <- ntldebt %>% select(Subject, Body, `Unix Timestamp`, Congress, `First Name`, `Last Name`, State, Party, Chamber)

# Removing the one independent member to include just Democrats and Republicans
ntldebt <- ntldebt[ntldebt$Party!="Independent",]

##### Majority Info #####
# Majority Party binary indicator using Democrat as 1
Congress <- c(111, 112, 113, 114, 115, 116, 117)

Dem_maj_sen <- c(1, 1, 1, 0, 0, 0, 1)
Dem_maj_house <- c(1, 0, 0, 0, 0, 1, 1)


# Create the Data frames of both houses with majority info
Maj_info_sen <- data.frame(Congress, Dem_maj_sen)
Maj_info_house <- data.frame(Congress, Dem_maj_house)

# Convert Party to numeric where Democrat == 1
ntldebt$Party <- ntldebt$Party == "Democrat"
ntldebt$Party <- as.numeric(ntldebt$Party)

# Subset into two data frames by Chamber
Senate <- ntldebt %>% filter(Chamber=="Senate")
House <- ntldebt %>% filter(Chamber=="House")

# Merge Majority Info fo each Chamber data frame
Senate <- merge(Senate, Maj_info_sen)
House <- merge(House, Maj_info_house)

rm(Maj_info_house, Maj_info_sen)

# For House: If party != maj_party, output 1 where 1 == minority
House$Minority <- House$Party != House$Dem_maj_house
House$Minority <- as.numeric(House$Minority)

# For Senate: If party != maj_party, output 1 where 1 == minority
Senate$Minority <- Senate$Party != Senate$Dem_maj_sen
Senate$Minority <- as.numeric(Senate$Minority)

# Remove Dem_maj_sen/house info in order to combine the two chamber data frames
House <- House %>% select(-Dem_maj_house)
Senate <- Senate %>% select(-Dem_maj_sen)

ntldebt <- rbind(House, Senate)

# Converting these to characters
ntldebt$Minority <- ifelse(ntldebt$Minority==1, "Minority", "Majority")

# Changing Party Back
ntldebt$Party <- ifelse(ntldebt$Party==1, "Democrat", "Republican")

# Tables of Minority and Party
table(ntldebt$Minority)
table(ntldebt$Party)

rm(Congress, Dem_maj_sen, Dem_maj_house, House, Senate)

#####

##### Time Info #####
# Adding date data from Unix Timestamp
ntldebt$`Unix Timestamp` <- ntldebt$`Unix Timestamp`/1000
date <- ymd("1970 1 1")
ntldebt$date <- ntldebt$`Unix Timestamp` %>% as_datetime(date)

ntldebt$year <- year(ntldebt$date)

rm(date)

#####

##### Pre-process #####
# Removing these 35 newsletters by two members as they contribute largely to a topic that is messed up with email newsletter formatting jargon if they were to be included.
ntldebt <- ntldebt %>% 
  filter(Congress != 111 & Congress != 112 | `Last Name` != "Latham") %>% 
  filter(Congress != 111 & Congress != 112 | `Last Name` != "Ross" | 
           `First Name` != "Mike")

# Pre-Process
temp <- textProcessor(documents = ntldebt$Body, metadata = ntldebt)

plotRemoved(temp$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 15)

rm(temp)


#####

##### ----- STM using Prevalence for Party and Minority and Congress ----- #####
# Create STM
nd.stm.PrevParMinCon <- stm(out$documents, out$vocab, K = 10, 
                           prevalence = ~Party + Minority + s(Congress), data = out$meta)
# Create Topic Labels
labelTopics(nd.stm.PrevParMinCon)

# Plot Top Topics by Proportion
plot(nd.stm.PrevParMinCon, n = 5)

# Same Plot with Labels
plot(nd.stm.PrevParMinCon, custom.labels = c("Email Administration", "Sen. Moran KS - Communicating Locally", "Tax Reform and ACA Pushback", "Issue Communication", "Sen. Scott FL - COVID Restrictions and Intl. Rel.", "Limited Spending", "Rep. Peterson MN - Veterans and Agriculture", "Rep. Simpson ID - Environment and Congressional Spending", "Rep. Pearce NM - Energy Independence", "Control Debt and Balance Budget"))

# Explore Topics
findThoughts(nd.stm.PrevParMinCon, texts = out$meta$Body, topics=4, n=5)


#####

##### --- Estimate Effects for PrevParMinCon --- #####
# Estimate relationship between Party over Congress
ee.stm.parcng <- estimateEffect(1:10 ~ Party + s(Congress), nd.stm.PrevParMinCon, meta = out$meta)

# Plot the Comparative use of a Topic by Party with some Topics
plot(ee.stm.parcng, covariate = "Party", topics = c(3, 4, 6, 10), 
     method = "difference", cov.value1 = "Democrat", cov.value2 = "Republican",
     xlab = "Republican Party           Democratic Party",
     main = "Effect of Republican vs. Democrat",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Tax and ACA Pushback', 'Issue Communication','Limited Spending', 'Control Debt and Budget'))

## 
# Estimate relationship between Minority over Congress
ee.stm.mincng <- estimateEffect(1:10 ~ Minority + s(Congress), nd.stm.PrevParMinCon, meta = out$meta)

# Plot the Comparative use of a Topic by Party with some Topics
plot(ee.stm.mincng, covariate = "Minority", topics = c(3, 4, 6, 10), 
     method = "difference", cov.value1 = "Majority", cov.value2 = "Minority",
     xlab = "Minority Party           Majority Party",
     main = "Effect of Minority vs. Majority",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Tax Reform and ACA Pushback', 'Issue Communication','Limited Spending', 'Control Debt and Budget'))
##

# Estimate relationship of Party and Topic by Congress
plot(ee.stm.parcng, "Congress", method="continuous", topics=1:10, ci.level = F, printlegend = F)

# Estimate relationship between Party over Congress
ee.stm.parmincng <- estimateEffect(1:10 ~ Party + Minority + s(Congress), nd.stm.PrevParMinCon, meta = out$meta)

# Estimate relationship of Party and Minority for Topic by Congress
plot(ee.stm.parmincng, "Congress", method="continuous", topics=c(3, 4, 6, 10), 
     ci.level = F, printlegend = F, 
     xlab = "Congress", main = "Effect of Party and Majority over Congress")
legend(114, -.05, c("Topic 3", "Topic 4", "Topic 6", "Topic 10"), 
       lwd = 2, col = c("red", "green", "aquamarine", "purple"), y.intersp = c(.5, .5, .5, .5))

###### -- These were not used in the Memo:
##### --- Topic Proportion by Party over Congress
prep <- estimateEffect(c(10) ~Party + Minority + s(Congress), nd.stm.PrevParMinCon, 
                       metadata = out$meta, uncertainty = "None")

plot(prep, covariate = "Congress", model = nd.stm.PrevParMinCon, 
     method = "continuous", xlab = "Congress", moderator = "Party", 
     moderator.value = "Democrat", linecol = "blue", ylim = c(-.35, .35), 
     printlegend = F, ci.level = F, main = "Topic 10")
plot(prep, covariate = "Congress", model = nd.stm.PrevParMinCon, 
     method = "continuous", xlab = "Congress", moderator = "Party", 
     moderator.value = "Republican", linecol = "red", ylim = c(-.35, .35), 
     add = T, printlegend = F, ci.level = F)
legend(112, -.05, c("Democrat", "Republican"), 
       lwd = 2, col = c("blue", "red"))

##### --- Topic Proportion by Minority/Majority over Congress
plot(prep, covariate = "Congress", model = nd.stm.PrevParMinCon, 
     method = "continuous", xlab = "Congress", moderator = "Minority", 
     moderator.value = "Minority", linecol = "blue", ylim = c(-.35, .35), 
     printlegend = F, ci.level = F, main = "Topic 10")
plot(prep, covariate = "Congress", model = nd.stm.PrevParMinCon, 
     method = "continuous", xlab = "Congress", moderator = "Minority", 
     moderator.value = "Majority", linecol = "red", ylim = c(-.35, .35), 
     add = T, printlegend = F, ci.level = F)
legend(112, -.05, c("Minority", "Majority"), 
       lwd = 2, col = c("blue", "red"))


#####

##### ----- STM using Content for Minority Party ----- #####
# Create STM
nd.stm.ConMin <- stm(out$documents, out$vocab, K = 10, 
                     content = ~Minority, data = out$meta)
# Create Topic Labels
labelTopics(nd.stm.ConMin)

# Explore Topics
findThoughts(nd.stm.ConMin, texts = out$meta$year, topics=1, n=50)


#####

##### --- Estimate Effects for ConMin --- #####
# For Plotting words by Minority
plot(nd.stm.ConMin, type="perspectives", topics=10, plabels = c("Majority", "Minority"), main = "Topic 10")


#####
