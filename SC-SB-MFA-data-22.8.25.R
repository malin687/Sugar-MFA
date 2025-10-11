
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggthemes")
install.packages("writexl")
install.packages("tidylog")
install.packages("tidyverse")
install.packages("GGally")
install.packages("performance")
install.packages("latex2exp")
install.packages("gtsummary")
install.packages("tinytex")
install.packages("tableone")
install.packages("kableExtra")
install.packages("splitstackshape")
install.packages("stargazer")
install.packages("olsrr")
install.packages("tableone")
install.packages("latex2exp")
install.packages("patchwork")
install.packages("lmtest")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("epiR")
install.packages("broom")
install.packages("corrplot")
install.packages("carData")
install.packages("car")
install.packages("gridExtra")
install.packages("stringr")
install.packages("arulesViz")
install.packages("knitr")
install.packages("coefplot")
install.packages("cowplot")
install.packages("egg")
install.packages("ggpubr")
install.packages("ggtext")
install.packages("ggplot2")  
install.packages("stringi")
install.packages("stringdist")
install.packages("plotly")
install.packages("orca")
install.packages("orca")
install.packages("orca")
install.packages("kaleido")
install.packages("networkD3")
install.packages("dplyr")
install.packages("openxlsx")
update.packages("plotly")
install.packages("circlize")
install.packages("rworldmap")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("grid")
install.packages("treemap")
install.packages("treemapify")
install.packages("webr")
install.packages("forcats")
install.packages("dplyr")     
install.packages("readxl")    
install.packages("tidyr") 




if (!require("d3r")) install.packages("d3r", dependencies = TRUE)
if (!require("jsonlite")) install.packages("jsonlite", dependencies = TRUE)
if (!require("sunburstR")) install.packages("sunburstR", dependencies = TRUE)
if (!require("plotly")) install.packages("plotly", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("sunburstR")) install.packages("sunburstR")
if (!require("ggplotify")) install.packages("ggplotify")
if (!require("readxl")) install.packages("readxl")



library(ggplot2); library(dplyr); library(readxl); library(ggthemes);
library(writexl); library(tidylog); library(tidyverse); library(GGally);
library(performance); library(tinytex); library(tableone); library(kableExtra) ;
library(splitstackshape); library(stargazer) ; library(stargazer) ; library(olsrr); library(tableone); library(latex2exp); library(gtsummary); library(patchwork);
library(lmtest); library(gridExtra); library(reshape2); library(epiR); library(broom); library(corrplot);
library(car); library(carData); library(gridExtra); library(stringr); library(arulesViz); library(knitr); library(coefplot);
library(cowplot); library(egg); library(ggpubr); library(ggtext); library(ggplot2) ;
library(stringi); library(stringdist) ; library(plotly); library(orca) ;library(networkD3);library(dplyr) ;
library(openxlsx) ; library(circlize) ; library(rworldmap) ; library(sf) ; library(rnaturalearth) ; library(rnaturalearthdata); library(grid) ; library(treemap) ; library(treemapify) ;
library(d3r) ; library(jsonlite) ; library(sunburstR) ; library(sunburstR); library(ggplotify); library(webr); library(forcats)


R.version
# to get detailed information about my R session 
#including the version of R, the operating system, and the installed packages.
sessionInfo()

#clean the working environment
rm(list = ls())
?rm



###############################################################
# Export df to CSV
# write.csv(df_name, "df_name.csv", row.names = FALSE)

#library(openxlsx)
# Export DataFrame to Excel
# write.xlsx(df_name, "df_name.xlsx")


######################################################################################

# Set the working directory to the folder containing the Excel files
setwd("C:/Users/nisni/OneDrive/Documents/R")

library(readxl)

# Read data from CSV file - crops production - 180 countries - FAO data
CROPS_WORLD <- read.csv("crops_production_FAO_2000-2020 - 180_countries.csv")
colnames(CROPS_WORLD)
str(CROPS_WORLD)

# Filter the 2018 crops
library(dplyr)
CROPS_2018 <- filter(CROPS_WORLD, year == 2018)
colnames(CROPS_2018)
str(CROPS_2018)

# Filter the sugarcane crop
SC_2018 <- filter(CROPS_2018, Parent_name == "Sugar cane")
colnames(SC_2018)
str(SC_2018)

# Count the number of unique country names
unique_country_count <- SC_2018 %>% 
  distinct(Country_name) %>% 
  count()

# View the count
print(unique_country_count)


####################################################################################################################
# adding data from the FAO production (SC_2018) into SC_MFA

# Read data from excel file - Base file for MFA
SC_MFA <- read_excel("Sugarcane_2018_MFA.xlsx")
colnames(SC_MFA)
str(SC_MFA)

# Convert logical columns to numeric
#logical_columns <- sapply(SC_MFA, is.logical) # Identify logical columns
#SC_MFA[logical_columns] <- lapply(SC_MFA[logical_columns], as.numeric) # Convert logical to numeric

######
########################## Field - Harvest ###############################
# 1. Sugar cane Stalk - before processing 
# SC_2018$availability = SC_MFA$Stalk base on match country name

# Filter out NA values and then extract the first "availability" value for each country name
availability_by_country <- SC_2018 %>%
  filter(!is.na(availability)) %>%
  group_by(Country_name) %>%
  summarize(first_availability = first(availability))

str(availability_by_country)

#Remove rows with production equal to 0
availability_by_country <- availability_by_country %>%
  filter(first_availability != 0)


# Left join SC_MFA with availability_by_country on FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(availability_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Rename first_availability to Stalk
  rename(Stalk = first_availability)

# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
print(SC_MFA$Stalk)

# 2. calculation of Total harvest and the straw left in the field
# mutate() function is used to create new columns or modify existing columns in a dataframe. 
SC_MFA <- SC_MFA %>%
  mutate(Stalk_moister = Stalk * 0.70,
         Stalk_dry = Stalk * 0.30,
         Stalk_sucrose = Stalk_dry * 0.50,
         SC_harvest = Stalk * 1.24,
         Straw = SC_harvest - Stalk,
         Straw_dry = Straw * 0.85,
         Straw_moister = Straw * 0.15,
         Dry_leaves = Straw * 0.61,
         Green_leaves = Straw * 0.32,
         Top = Straw * 0.07)

# Rearranging the columns
# Move SC_harvest after FAO_countries_production
SC_MFA <- SC_MFA %>%
  relocate(SC_harvest, .after = FAO_countries_production)

# Move Stalk_sucrose and Stalk_moister after Stalk
#SC_MFA <- SC_MFA %>%
 # relocate(Stalk, .after = Top) %>%
 # relocate(Stalk_sucrose, .after = Stalk) %>%
#  relocate(Stalk_moister, .after = Stalk_sucrose)


######
########################## Sugar mill - Processing ###############################
# 1. Stalk for sugar production = Processed_parent according to FAO data = The sugarcane stalk that enters the mill for sugar production
# Filter out NA values and then extract the first "Processed_parent" value for each country name
Processed_parent_by_country <- SC_2018 %>%
  filter(!is.na(Processed_parent)) %>%
  group_by(Country_name) %>%
  summarize(first_Processed_parent = first(Processed_parent))

str(Processed_parent_by_country)

#Remove rows with production equal to 0
Processed_parent_by_country <- Processed_parent_by_country %>%
  filter(first_Processed_parent != 0)

write.xlsx(Processed_parent_by_country, "Processed_parent_by_country.xlsx")

# Left join SC_MFA with Processed_parent_by_country on FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Processed_parent_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Rename first_Processed_parent to SCP (Sugarcane Processed - according to the FAO data - this is without the Baggasse or Molasses)
  rename(SCP = first_Processed_parent)

# View the updated SC_MFA dataframe
colnames(SC_MFA)
print(SC_MFA$SCP)

# 2. SCP (SugarCane Processed) VS Stalk - to find how much from the field goes to the mill for processing
SC_MFA <- SC_MFA %>%
  mutate(SCP_VS_Stalk_gap = Stalk - SCP,
         SCP_VS_Stalk_gap_perc = (SCP_VS_Stalk_gap/Stalk)*100,# The percentage of the harvest stalk that does not undergo processing to become sugar
         SCP_sucrose = SCP*0.50,
         SCP_moister = SCP*0.70) 

print(SC_MFA$SCP_VS_Stalk_gap_perc)

# 3.Bagasse - from the FAO data
# Creating Bagasse df by filtering from th df =SC_2018
Bagasse_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Bagasse") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)


#Remove rows with production equal to 0
Bagasse_by_country <- Bagasse_by_country %>%
  filter(production != 0)

write.xlsx(Bagasse_by_country, "Bagasse_by_country.xlsx")

# Left join SC_MFA with Bagasse_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Bagasse_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name,-stockChange) %>%
  # Rename columns
  rename(Bagasse_pro = production,
         Bagasse_import = imports,
         Bagasse_export = exports,
         Bagasse_ER = extractionRate)


# View the updated SC_MFA dataframe
colnames(SC_MFA)

# 4. calculating the Bagasse moister / dry and Bagasse pith
SC_MFA <- SC_MFA %>%
  mutate(Bagasse_moister = Bagasse_pro * 0.48,
         Bagasse_dry = Bagasse_pro - Bagasse_moister)


# View the updated SC_MFA dataframe
colnames(SC_MFA)

# 5. calculating filter cake and press mud (filter mud)
SC_MFA <- SC_MFA %>%
  mutate(Filter_cake = SCP * 0.04,
         Filter_cake_moister = Filter_cake * 0.69 ,
         Filter_cake_dry = Filter_cake - Filter_cake_moister,
         Filter_mud = SCP * 0.03,
         Filter_mud_moister =  Filter_mud * 0.58,
         Filter_mud_dry = Filter_mud - Filter_mud_moister)
 

# 6.NCS = Non-centrifugal sugar(Natural brown sugar) - from the FAO data
# Creating Non_centrifugal_sugar df by filtering from the df =SC_2018
Non_centrifugal_sugar_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Cane sugar, non-centrifugal") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

#Remove rows with production equal to 0
Non_centrifugal_sugar_by_country <- Non_centrifugal_sugar_by_country %>%
  filter(production != 0)

write.xlsx(Non_centrifugal_sugar_by_country, "Non_centrifugal_sugar_by_country.xlsx")

# Left join SC_MFA with Non_centrifugal_sugar_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Non_centrifugal_sugar_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name,-imports,-exports) %>% # There is o value in imports and exports
  # Rename columns
  rename(NCS_pro = production,
         NCS_stock = stockChange,
         NCS_ER = extractionRate)

#  calculating the NCS moister
SC_MFA <- SC_MFA %>%
  mutate(NCS_moister = NCS_pro * 0.03)

# View the updated SC_MFA dataframe
colnames(SC_MFA)

# 7.Raw sugar (centrifugal) = like Demerara Sugar - from the FAO data
# Creating Raw sugar df by filtering from the df =SC_2018
Raw_sugar_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Raw cane or beet sugar (centrifugal only)") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

#Remove rows with production equal to 0
Raw_sugar_by_country <- Raw_sugar_by_country %>%
  filter(production != 0)

# Left join SC_MFA with Raw_sugar_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Raw_sugar_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Raw_sugar_pro = production,
         Raw_sugar_import = imports,
         Raw_sugar_export = exports,
         Raw_sugar_stock = stockChange,
         Raw_sugar_ER = extractionRate)

#  calculating the Raw_sugar moister
SC_MFA <- SC_MFA %>%
  mutate(Raw_sugar_moister = Raw_sugar_pro * 0.007)

# View the updated SC_MFA dataframe
colnames(SC_MFA)
SC_MFA$Raw_sugar_pro

# 8.Sugar&Syrups nes - from the FAO data
# Creating Sugar&Syrups_nes df by filtering from the df =SC_2018
SSnes_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Sugar and Syrups nes") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

#Remove rows with production equal to 0
SSnes_by_country <- SSnes_by_country %>%
  filter(production != 0)

write.xlsx(SSnes_by_country, "SSnes_by_country.xlsx")

# Left join SC_MFA with SSnes_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(SSnes_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(SSnes_pro = production,
         SSnes_import = imports,
         SSnes_export = exports,
         SSnes_stock = stockChange,
         SSnes_ER = extractionRate)

# View the updated SC_MFA dataframe
colnames(SC_MFA)
SC_MFA$SSnes_pro

# 9.Molasses - from the FAO data
# Creating Molasses df by filtering from the df =SC_2018
Molasses_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Molasses (from beet, cane and maize)") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)


#Remove rows with production equal to 0
Molasses_by_country <- Molasses_by_country %>%
  filter(production != 0)


# Left join SC_MFA with Molasses_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Molasses_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Molasses_pro = production,
         Molasses_import = imports,
         Molasses_export = exports,
         Molasses_stock = stockChange,
         Molasses_ER = extractionRate)

#  calculating the Molasses moister / sucrose
SC_MFA <- SC_MFA %>%
  mutate(Molasses_moister = Molasses_pro * 0.18)


# View the updated SC_MFA dataframe
colnames(SC_MFA)
SC_MFA$Molasses_pro

# 10. calculating Ethanol according to Molasses mass: 5.11 ton molasses = 1 ton ethanol
# SC_MFA <- SC_MFA %>%
#  mutate(Ethanol_pro = Molasses_pro * 5.07)

# View the updated SC_MFA dataframe
#View(SC_MFA)
#colnames(SC_MFA)
#SC_MFA$Ethanol_pro

# 11.Bev_alco = Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages - from the FAO data
# Creating BevNA df by filtering from the df =SC_2018
Bev_alco_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

#Remove rows with production equal to 0
Bev_alco_by_country <- Bev_alco_by_country %>%
  filter(production != 0)

write.xlsx(Bev_alco_by_country, "Bev_alco_by_country.xlsx")

# Left join SC_MFA with Bev_alco_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Bev_alco_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Bev_alco_pro = production,
         Bev_alco_import = imports,
         Bev_alco_export = exports,
         Bev_alco_stock = stockChange,
         Bev_alco_ER = extractionRate)

# View the updated SC_MFA dataframe
colnames(SC_MFA)
SC_MFA$Bev_alco_pro

# 12.Non-food_alco = Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher - from the FAO data
# Creating Non-food_alco df by filtering from the df =SC_2018
NF_alco_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

#Remove rows with production equal to 0
NF_alco_by_country <- NF_alco_by_country %>%
  filter(production != 0)

write.xlsx(NF_alco_by_country, "NF_alco_by_country.xlsx")

# Left join SC_MFA with NF_alco_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(NF_alco_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(NF_alco_pro = production,
         NF_alco_import = imports,
         NF_alco_export = exports,
         NF_alco_stock = stockChange,
         NF_alco_ER = extractionRate)

# View the updated SC_MFA dataframe
colnames(SC_MFA)
SC_MFA$NF_alco_pro

#############################################

# Filter the Refined sugar and his products (Sugar Confectionery &  non-alcoholic caloric beverages) - FAO

RefS_products_2018 <- filter(CROPS_2018, Parent_name %in% c("Raw cane or beet sugar (centrifugal only)", "Refined sugar")) # %in% = specify a multiple values. c() = function to combine values into a vector
colnames(RefS_products_2018)
str(RefS_products_2018)

##############################################
# Filter Sugar beet - FAO

SB_2018 <- filter(CROPS_2018, Parent_name == "Sugar beet")
#View(SB_2018)
colnames(SB_2018)
str(SB_2018)

# Creating Raw sugar beet df by filtering from the df = SB_2018
RawS_beet_by_country <- SB_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Raw cane or beet sugar (centrifugal only)") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         availability = ifelse(is.na(availability), 0, availability),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),
         source = "sugar beet") %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange,availability, Processed_parent, source)


# Creating Raw sugarcane df by filtering from the df = SC_2018
RawS_cane_by_country <- SC_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Raw cane or beet sugar (centrifugal only)") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         availability = ifelse(is.na(availability), 0, availability),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),
         source = "sugar cane") %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, availability, Processed_parent, source)

# Combine them vertically
raw_sugar_by_source <- rbind(RawS_beet_by_country, RawS_cane_by_country)

# Calculate the frequency of each country name
country_counts <- table(raw_sugar_by_source$Country_name)

# Filter out the countries that appear more than once
repeated_countries <- country_counts[country_counts > 1]

# Print the countries that are repeating
print(names(repeated_countries))

# Convert names of repeated countries into a data frame
repeated_countries_raw_sugar <- data.frame(Country_name = names(repeated_countries))

# Print the data frame of repeating countries
print(repeated_countries_raw_sugar)

colnames(raw_sugar_by_source)
colnames(repeated_countries_raw_sugar)

# filter the raw_sugar_by_source df so that it includes only the rows where Country_name matches those in the repeated_countries_raw_sugar df
filtered_raw_sugar_by_source <- raw_sugar_by_source %>%
  filter(Country_name %in% repeated_countries_raw_sugar$Country_name)
colnames(filtered_raw_sugar_by_source)


# Calculating how much raw sugar is obtained from sugar beet and how much from sugar cane - by the ratio between the Processed_parent (= Represents what is the initial raw material amount that reached the sugar mill)
# a.Create a summary df that sums Processed_parent by Country_name for the specified sources and sum: Processed_parent sugar beet + Processed_parent sugar beet +
processed_parent_summary <- filtered_raw_sugar_by_source %>%
  filter(source %in% c("sugar beet", "sugar cane")) %>%
  group_by(Country_name) %>%
  summarise(Processed_parent_beet_and_cane = sum(Processed_parent, na.rm = TRUE))


# Left join filtered_raw_sugar_by_source with processed_parent_summary by Country_name
filtered_raw_sugar_by_source <- filtered_raw_sugar_by_source %>%
  left_join(processed_parent_summary, by = c("Country_name" = "Country_name"))

colnames(filtered_raw_sugar_by_source)

# b. calculating the ratio to find how much raw sugar is obtained from sugar beet and how much from sugar cane
filtered_raw_sugar_by_source <- filtered_raw_sugar_by_source %>%
  mutate(production_correction = (production * Processed_parent) / Processed_parent_beet_and_cane,
             exports_correction = (exports * Processed_parent) / Processed_parent_beet_and_cane)

colnames(filtered_raw_sugar_by_source)
View(filtered_raw_sugar_by_source)

# Left join raw_sugar_by_source with filtered_raw_sugar_by_source by Country_name and source
raw_sugar_by_source <- raw_sugar_by_source %>%
  left_join(filtered_raw_sugar_by_source %>% select(Country_name, source, production_correction, exports_correction),
            by = c("Country_name", "source"))

# Move production_correction after production
raw_sugar_by_source <- raw_sugar_by_source %>%
  relocate(production_correction, .after = production)%>%
  relocate(exports_correction, .after = exports)

SC_MFA$Raw_sugar_pro

# filtering from the df raw_sugar_by_source the sugar cane - to correct the SC_MFA$Raw_sugar_pro in the repeated countries raw sugar
raw_sugar_cane_correction <- raw_sugar_by_source %>%
  filter(!is.na(production_correction)) %>%
  filter(!is.na(exports_correction)) %>%
  filter(source == "sugar cane") %>%
  filter(!Country_name %in% c("France", "Portugal", "Spain", "Yemen", "Iraq")) %>% # these ountries has no harvest data of sugar cane acording to the FAO
  select(Country_name, production_correction, exports_correction, source)



# replacing the values in the Raw_sugar_pro column of the SC_MFA dataframe with values from the production_correction column of the raw_sugar_cane_correction dataframe based on matching Country_name
# and to generate a report of which values were replaced

library(dplyr)

# Create a report df that captures the old values before replacement
report_before <- SC_MFA %>%
  select(FAO_countries_production, Raw_sugar_pro, Raw_sugar_export)
 
colnames(SC_MFA)

# Perform the replacement by joining the correction dataframe
# Using the 'by' argument of left_join to specify custom join conditions
SC_MFA <- SC_MFA %>%
  left_join(
    raw_sugar_cane_correction %>%
      rename(
        FAO_countries_production = Country_name) %>%
      select(FAO_countries_production, production_correction, exports_correction),
    by = c("FAO_countries_production")) %>%
  mutate(
    Raw_sugar_pro = ifelse(!is.na(production_correction), production_correction, Raw_sugar_pro),
    Raw_sugar_export = ifelse(!is.na(exports_correction), exports_correction, Raw_sugar_export)
  ) %>%
  select(-production_correction, -exports_correction)


# Create a report dataframe that captures the new values after replacement
report_after <- SC_MFA %>%
  select(FAO_countries_production, Raw_sugar_pro, Raw_sugar_export)

# Combine the before and after reports to see which values were replaced
report <- report_before %>%
  rename(Old_Raw_sugar_pro = Raw_sugar_pro) %>%
  rename(Old_Raw_sugar_export = Raw_sugar_export) %>%
  inner_join(report_after, by = "FAO_countries_production") %>%
  filter(Old_Raw_sugar_pro != Raw_sugar_pro) %>%
  filter(Old_Raw_sugar_export != Raw_sugar_export) %>%
  mutate(Replaced = "Yes")

# Print the final report to see the changes
print(report)

# Print the row where 'Country_name' is 'Iran (Islamic Republic of)'
print(SC_MFA[SC_MFA$FAO_countries_production == "Iran (Islamic Republic of)", ])

# Checking manually the Iran Raw_sugar_pro value
# Step 1: Find the row index
row_index <- which(SC_MFA$FAO_countries_production == "Iran (Islamic Republic of)")

# Step 2: Record the original value
original_value <- SC_MFA[row_index, "Raw_sugar_pro"]

# view the updated SC_MFA to confirm the changes
colnames(SC_MFA)
SC_MFA$Raw_sugar_pro

# Correcting the calculation of the raw sugar moister and sucrose according to the new values in the raw sugar production
SC_MFA <- SC_MFA %>%
  mutate(Raw_sugar_moister = Raw_sugar_pro * 0.007)

# Updating the df raw_sugar_by_source
# Initialize the 'note' column with NA or ""
raw_sugar_by_source$note <- NA

# Define the countries of interest
countries_of_interest <- c("Portugal", "Spain", "France", "Iraq", "Yemen")

# Identify the rows meeting the conditions
condition <- raw_sugar_by_source$source == "sugar cane" & 
  raw_sugar_by_source$Country_name %in% countries_of_interest

# Update the 'note' column for these rows
raw_sugar_by_source$note[condition] <- "no harvest data"

condition_beet_Yemen <- raw_sugar_by_source$source == "sugar beet" &
  raw_sugar_by_source$Country_name == "Yemen"
# Update 'note' for "Yemen" with "sugar beet"
raw_sugar_by_source$note[condition_beet_Yemen] <- "no harvest data"

#  print the rows where notes were added to verify
print(raw_sugar_by_source[condition, ])
View(raw_sugar_by_source)

write.xlsx(raw_sugar_by_source, "raw_sugar_by_source.xlsx")

##############################################
# 14.Refined sugar - from the FAO data - from sugar cane and beet
# Creating Refined sugar df by filtering from the df RefS_products_2018
refined_sugar_by_country <- RefS_products_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Refined sugar") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

colnames(refined_sugar_by_country)
colnames(raw_sugar_by_source)

#Remove rows with production equal to 0
refined_sugar_by_country <- refined_sugar_by_country %>%
  filter(production != 0)

# Perform a left join with df raw_sugar_by_country - by the source
refined_sugar_by_country <- refined_sugar_by_country %>%
  left_join(raw_sugar_by_source %>% select(Country_name, source), by = "Country_name")

# Filter the country names with NA in 'source' (which we don't know what is the source)
filtered_Ref_sugar_NA <- refined_sugar_by_country %>%
  filter(is.na(source))


########### Replace the NA values:
# Read data from excel file - Harvest data from FAO - BY SUGAR CANE AND SUGAR BEET
Harvest_2018 <- read_excel("HARVEST - SUGAR CANE AND BEET by country - 2018 - FAO.xlsx")
colnames(Harvest_2018)
str(Harvest_2018)
#View(Harvest_2018)

# Filter Harvest_2018 to only include rows where Country_name matches those in filtered_Ref_sugar_NA
# and drop rows where the "Value" column is 0 or less
filtered_Harvest <- Harvest_2018 %>%
  filter(Area %in% filtered_Ref_sugar_NA$Country_name) %>%
  filter(Value > 0)


# To make sure that the countries in the df are having a sugar production
# Filter Harvest_2018 to only include rows where Country_name matches those in filtered_Ref_sugar_NA
# and drop rows where the "Value" column is 0 or less
filtered_Harvest <- Harvest_2018 %>%
  filter(Area %in% filtered_Ref_sugar_NA$Country_name) %>%
  filter(Value > 0)

# Identify rows in filtered_Harvest that do not exist in SC_2018
non_existent_areas <- anti_join(filtered_Harvest, SC_2018, by = c("Area" = "Country_name"))

# Print out the areas that will be dropped
print(non_existent_areas$Area)

# Remove areas from filtered_Harvest that do not exist in SC_2018
filtered_Harvest <- semi_join(filtered_Harvest, SC_2018, by = c("Area" = "Country_name"))

#View(filtered_Harvest)


# Perform the join to get the relevant 'Item' information
update_source <- filtered_Harvest %>%
  select(Area, Item) %>%
  distinct() %>%  # Ensure unique pairs to prevent duplicating rows
  filter(Item %in% c("Sugar beet", "Sugar cane"))  %>%  # Filter to only include relevant rows
  rename(Country_name = Area)  # Rename Area to Country_name

# Update the source column in filtered_Ref_sugar_NA based on the Item values from filtered_Harvest
filtered_Ref_sugar_NA <- filtered_Ref_sugar_NA %>%
  left_join(update_source, by = "Country_name") %>%
  mutate(source = case_when(
    Item == "Sugar beet" ~ "sugar beet",
    Item == "Sugar cane" ~ "sugar cane",
    TRUE ~ source
  )) %>%
  select(-Item)  # Remove the temporary Item column after updating source

# Replace NA values in the source column with "no harvest data"
filtered_Ref_sugar_NA <- filtered_Ref_sugar_NA %>%
  mutate(source = ifelse(is.na(source), "no harvest data", source))

#View(filtered_Ref_sugar_NA)
#View(refined_sugar_by_country)

# Join the source column from filtered_Ref_sugar_NA into refined_sugar_by_country
# Ensuring to only update where source was originally NA
refined_sugar_by_country <- refined_sugar_by_country %>%
  left_join(filtered_Ref_sugar_NA %>% select(Country_name, source) %>% 
              rename(new_source = source),
            by = "Country_name") %>%
  mutate(source = ifelse(is.na(source) & !is.na(new_source), new_source, source)) %>%
  select(-new_source)  # Remove the temporary new_source column

# Drop rows with the specified country names from filtered_Ref_sugar_NA  = the countries that don't have sugar production
refined_sugar_by_country <- refined_sugar_by_country %>%
  filter(!Country_name %in% c("French Polynesia", "Ghana", "Samoa"))


#View(refined_sugar_by_country)
colnames(refined_sugar_by_country)


# find and print the names of countries that appear twice and have two different sources
Refs_countries_with_two_sources <- refined_sugar_by_country %>%
  group_by(Country_name) %>%
  filter(n_distinct(source) == 2) %>%
  summarise(n_sources = n_distinct(source)) %>%
  filter(n_sources == 2) %>%
  pull(Country_name)

print(Refs_countries_with_two_sources)

############################
# Finding the mass of the refined sugar from sugar cane in the countries that having 2 sources:

# Filter countries that appear exactly twice in the Country_name column in the df raw_sugar_by_source
Raw_s_countries_appearing_twice <- raw_sugar_by_source %>%
  group_by(Country_name) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  # Remove countries with "no harvest data" note 
  filter(!grepl("no harvest data", .$note, ignore.case = TRUE)) %>%
# Remove rows where Country_name is "Portugal" or "Spain" or "Iraq" or "France" which have no sugar cane production according to the FAO
filter(Country_name != "Portugal" & Country_name != "Spain"  & Country_name != "Iraq"  & Country_name !=  "France")

# Calculate the ratio of Processed_parent (sugar beet VS sugar cane) for each country that appears twice - so i will have the ratio for refined sugar and his products, and molasses
# For each pair of rows with the same Country_name
Raw_s_countries_appearing_twice_FIN <- Raw_s_countries_appearing_twice %>%
  group_by(Country_name) %>%
  mutate(
    # Calculate the ratio for the row with the low value in Processed_parent
    ratio = ifelse(Processed_parent == min(Processed_parent), min(Processed_parent) / max(Processed_parent), NA_real_),
    # Fill in the ratio value for the other row
    ratio = ifelse(is.na(ratio), 1 - ratio[!is.na(ratio)], ratio)
  )


colnames(refined_sugar_by_country)
#view(refined_sugar_by_country)
#view(Raw_s_countries_appearing_twice_FIN)

# Merge the ratio column from df Raw_s_countries_appearing_twice_FIN into df refined_sugar_by_country
refined_sugar_by_country <- merge(refined_sugar_by_country, 
                                  Raw_s_countries_appearing_twice_FIN[, c("Country_name", "source", "ratio")], 
                                  by = c("Country_name", "source"),
                                  all.x = TRUE)


# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
refined_sugar_by_country <- refined_sugar_by_country %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)

### Filter rows with the source "sugar cane" 
refined_sugar_cane <- refined_sugar_by_country %>%
  filter(source == "sugar cane")

# Find rows in df raw_sugar_by_source with source "sugar cane" and "no harvest data" in note
deleted_countries_by_raw <- raw_sugar_by_source %>%
  filter(source == "sugar cane" & grepl("no harvest data", note, ignore.case = TRUE)) %>%
  pull(Country_name)

# Remove rows from df refined_sugar_cane for countries with "no harvest data" - which they have no sugar cane production so the refined sugar is not obtain from sugar cane
refined_sugar_cane <- refined_sugar_cane %>%
  filter(!Country_name %in% deleted_countries_by_raw)

# Print the names of the deleted countries
print(deleted_countries_by_raw)

#view(refined_sugar_cane)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
refined_sugar_cane <- refined_sugar_cane %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
refined_sugar_cane_FIN <- refined_sugar_cane %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(refined_sugar_cane)

# Remove the specified columns
refined_sugar_cane_by_country <- select(refined_sugar_cane_FIN, -c("Processed_parent", "ratio", "production_correction", "exports_correction", "production_original", "exports_original", "source"))

#view(refined_sugar_cane_by_country)

  
# join the df refined_sugar_cane_by_country to the main df: SC_MFA
# Left join SC_MFA with refined_sugar_cane by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(refined_sugar_cane_by_country, by = c("FAO_countries_production" = "Country_name")) %>%
  select(-Child_name) %>%  # Drop unnecessary columns
  rename(RefS_pro = production,
         RefS_import = imports,
         RefS_export = exports,
         RefS_stock = stockChange,
         RefS_ER = extractionRate)

#  calculating the refined_sugar moister / sucrose
SC_MFA <- SC_MFA %>%
  mutate(RefS_moister = RefS_pro * 0.0004)

# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
SC_MFA$RefS_pro

#############################
######################################### Molasses correction - the value of the countries that have 2 sources - according the raw sugar production ration we found before

# Remove the Molasses columns for replacing them after the ratio correction:
SC_MFA <- select(SC_MFA, -c("Molasses_pro", "Molasses_import", "Molasses_export", "Molasses_stock","Molasses_ER", "Molasses_moister"))

colnames(SC_MFA)

#view(Molasses_by_country)
#view(Raw_s_countries_appearing_twice_FIN)

# Extract ratio values for source "sugar cane"
sugar_cane_ratio <- Raw_s_countries_appearing_twice_FIN %>%
  filter(source == "sugar cane") %>%
  select(Country_name, ratio)

# Extract ratio values for source "sugar beet"
sugar_beet_ratio <- Raw_s_countries_appearing_twice_FIN %>%
  filter(source == "sugar beet") %>%
  select(Country_name, ratio)

# Merge ratio values into Molasses_by_country based on Country_name
Molasses_by_country <- merge(Molasses_by_country, sugar_cane_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Molasses_by_country <- Molasses_by_country %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Molasses_by_country <- Molasses_by_country %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Molasses_by_country <- Molasses_by_country %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

#view(Molasses_by_country)

# Remove the specified columns
Molasses_by_country_sugar_cane <- select(Molasses_by_country, -c( "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))

#view(RawS_cane_by_country)
#Remove rows with production equal to 0 in the df RawS_cane_by_country
RawS_cane_by_country <- RawS_cane_by_country %>%
  filter(production != 0)

# Remove rows with specified Country_names that have no harvest of sugar cane according to the FAO data
RawS_cane_by_country <- RawS_cane_by_country %>%
  filter(!Country_name %in% c("Yemen", "France", "Iraq", "Portugal" ,"Spain", "Bulgaria"))

#Finding the Molasses obtain from Sugar cane
# remove rows from the dataframe Molasses_by_country_sugar_cane where the "Country_name" is not present in the "Country_name" column of the dataframe RawS_cane_by_country
# Identify countries to be removed
removed_countries <- Molasses_by_country_sugar_cane %>%
  anti_join(RawS_cane_by_country, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
Molasses_by_country_sugar_cane <- Molasses_by_country_sugar_cane %>%
  semi_join(RawS_cane_by_country, by = "Country_name")

# Print the names of removed countries
print(removed_countries)

write.xlsx(Molasses_by_country_sugar_cane, "Molasses_by_country_sugar_cane.xlsx")

# Left join SC_MFA with Molasses_by_country by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Molasses_by_country_sugar_cane, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Molasses_pro = production,
         Molasses_import = imports,
         Molasses_export = exports,
         Molasses_stock = stockChange,
         Molasses_ER = extractionRate)

#  calculating the Molasses moister / sucrose
SC_MFA <- SC_MFA %>%
  mutate(Molasses_moister = Molasses_pro * 0.18)

colnames(SC_MFA)

##############################################################################################
# Remove the column Bev_alco and Non-food_alco - for correcting them according to ratio of raw sugar (the countries that have 2 sources)
SC_MFA <- select(SC_MFA, -c("Bev_alco_pro", "Bev_alco_ER", "Bev_alco_import", "Bev_alco_export", "Bev_alco_stock", "NF_alco_pro", "NF_alco_ER", "NF_alco_import", "NF_alco_export", "NF_alco_stock" ))

colnames(SC_MFA)

# Correction - Bev_alco = Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages - from the FAO data
#view(Bev_alco_by_country)

#Remove rows with production equal to 0 in the df Bev_alco_by_country
Bev_alco_by_country <- Bev_alco_by_country %>%
  filter(production != 0)

# Remove rows with specified Country_names that have no harvest of sugar cane according to the FAO data
#RawS_cane_by_country <- RawS_cane_by_country %>%
#  filter(!Country_name %in% c("Yemen", "France", "Iraq", "Portugal" ,"Spain", "Bulgaria"))

#Finding the Bev_alco obtain from Sugar cane
# remove rows from the df Bev_alco_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_cane_by_country
# Identify countries to be removed
removed_countries_Bev_alco <- Bev_alco_by_country %>%
  anti_join(RawS_cane_by_country, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
Bev_alco_by_country_sugar_cane <- Bev_alco_by_country %>%
  semi_join(RawS_cane_by_country, by = "Country_name")

# Print the names of removed countries
print(removed_countries_Bev_alco)

# Merge ratio values into Bev_alco_by_country_sugar_cane based on Country_name
Bev_alco_by_country_sugar_cane <- merge(Bev_alco_by_country_sugar_cane, sugar_cane_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Bev_alco_by_country_sugar_cane <- Bev_alco_by_country_sugar_cane %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Bev_alco_by_country_sugar_cane <- Bev_alco_by_country_sugar_cane %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Bev_alco_by_country_sugar_cane <- Bev_alco_by_country_sugar_cane %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

# Remove the specified columns
Bev_alco_by_country_sugar_cane_FIN <- select(Bev_alco_by_country_sugar_cane, -c( "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))

write.xlsx(Bev_alco_by_country_sugar_cane_FIN, "Bev_alco_by_country_sugar_cane_FIN.xlsx")

# Left join SC_MFA with Bev_alco_by_country_sugar_cane_FIN by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Bev_alco_by_country_sugar_cane_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Bev_alco_pro = production,
         Bev_alco_import = imports,
         Bev_alco_export = exports,
         Bev_alco_stock = stockChange,
         Bev_alco_ER = extractionRate)

colnames(SC_MFA)
SC_MFA$Bev_alco_pro

###################################
# cORRECTION -  Non-food_alco = Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher - from the FAO data

#view(NF_alco_by_country)

#Remove rows with production equal to 0 in the df Bev_alco_by_country
NF_alco_by_country <- NF_alco_by_country %>%
  filter(production != 0)

#Finding the Bev_alco obtain from Sugar cane
# remove rows from the df Bev_alco_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_cane_by_country
# Identify countries to be removed
removed_countries_NF_alco <- NF_alco_by_country %>%
  anti_join(RawS_cane_by_country, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
NF_alco_by_country_sugar_cane <- NF_alco_by_country %>%
  semi_join(RawS_cane_by_country, by = "Country_name")

# Print the names of removed countries
print(removed_countries_NF_alco)

# Merge ratio values into Bev_alco_by_country_sugar_cane based on Country_name
NF_alco_by_country_sugar_cane <- merge(NF_alco_by_country_sugar_cane, sugar_cane_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
NF_alco_by_country_sugar_cane <- NF_alco_by_country_sugar_cane %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
NF_alco_by_country_sugar_cane <- NF_alco_by_country_sugar_cane %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
NF_alco_by_country_sugar_cane <- NF_alco_by_country_sugar_cane %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

# Remove the specified columns
NF_alco_by_country_sugar_cane_FIN <- select(NF_alco_by_country_sugar_cane, -c( "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))

write.xlsx(NF_alco_by_country_sugar_cane_FIN, "NF_alco_by_country_sugar_cane_FIN.xlsx")

# Left join SC_MFA with NF_alco_by_country_sugar_cane_FIN by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(NF_alco_by_country_sugar_cane_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(NF_alco_pro = production,
         NF_alco_import = imports,
         NF_alco_export = exports,
         NF_alco_stock = stockChange,
         NF_alco_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
SC_MFA$NF_alco_pro

###########################################################################################################

# 16. Sugar Confectionery (Refiend sugar) - from the FAO data - obtained from sugar cane and beet
# Creating  SConfect df by filtering from the df RefS_products_2018
SConfect_by_country <- RefS_products_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Sugar Confectionery") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

colnames(SConfect_by_country)

#Remove rows with production equal to 0 in the df SConfect_by_country
SConfect_by_country <- SConfect_by_country %>%
  filter(production != 0)

#Finding the Sugar Confectionery obtained from Sugar cane
# remove rows from the df SConfect_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_cane_by_country
# Identify countries to be removed
removed_countries_SConfect <- SConfect_by_country %>%
  anti_join(RawS_cane_by_country, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
SConfect_by_country_sugar_cane <- SConfect_by_country %>%
  semi_join(RawS_cane_by_country, by = "Country_name")

# Print the names of removed countries
print(removed_countries_SConfect)

# Merge ratio values into SConfect_by_country_sugar_cane based on Country_name
SConfect_by_country_sugar_cane <- merge(SConfect_by_country_sugar_cane, sugar_cane_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
SConfect_by_country_sugar_cane <- SConfect_by_country_sugar_cane %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
SConfect_by_country_sugar_cane <- SConfect_by_country_sugar_cane %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
SConfect_by_country_sugar_cane <- SConfect_by_country_sugar_cane %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(SConfect_by_country_sugar_cane)

# Remove the specified columns
SConfect_by_country_sugar_cane_FIN <- select(SConfect_by_country_sugar_cane, -c( "Processed_parent", "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(SConfect_by_country_sugar_cane_FIN)

write.xlsx(SConfect_by_country_sugar_cane_FIN, "SConfect_by_country_sugar_cane_FIN.xlsx")

# Left join SC_MFA with SConfect_by_country_sugar_cane_FIN by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(SConfect_by_country_sugar_cane_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(SConfect_pro = production,
         SConfect_import = imports,
         SConfect_export = exports,
         SConfect_stock = stockChange,
         SConfect_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
SC_MFA$SConfect_pro


###########################################################################################################

# 17. Refined Sugar flavoured / coloured - from Chatham House - obtained from sugar cane and beet

# Read data from excel file -  Refined Sugar flavoured or coloured - trade data
S_flavoured_all <- read_excel("resourcetradeearth-all-all-948-2020_Refined sugar_flavoured or coloured - trade.xlsx")
colnames(S_flavoured_all)
str(S_flavoured_all)

# Filter the 2018 trade
library(dplyr)
S_flavoured_2018 <- filter(S_flavoured_all, Year == 2018)
#view(S_flavoured_2018)
colnames(S_flavoured_2018)
colnames(SC_MFA)

# Calculate total exports for each country based on weight column
S_flavoured_2018_exports <- S_flavoured_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
S_flavoured_2018_imports <- S_flavoured_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
S_flavoured_Trade_2018 <- full_join(S_flavoured_2018_exports, S_flavoured_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
S_flavoured_Trade_2018$total_export[is.na(S_flavoured_Trade_2018$total_export)] <- 0
S_flavoured_Trade_2018$total_import[is.na(S_flavoured_Trade_2018$total_import)] <- 0

#View(S_flavoured_Trade_2018)
#view(RawS_cane_by_country)
#view(SC_MFA)
colnames(RawS_cane_by_country)

# adding the Countries_code (ISO3) name to the df RawS_cane_by_country
RawS_cane_by_country <- RawS_cane_by_country %>%
  left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
            by = c("Country_name" = "FAO_countries_production"))

# adding the Countries_code (ISO3) name to the df sugar_cane_ratio
sugar_cane_ratio <- sugar_cane_ratio %>%
  left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
            by = c("Country_name" = "FAO_countries_production"))

#Finding the Refined Sugar flavoured / coloured obtained from Sugar cane
# remove rows from the df S_flavoured_Trade_2018 where the "Country_name" is not present in the "Country_name" column of the df RawS_cane_by_country
# Identify countries to be removed
removed_countries_S_flavoured <- S_flavoured_Trade_2018 %>%
  anti_join(RawS_cane_by_country, by = "Countries_code") %>%
  pull(Countries_code)

# Remove rows with Country_names not present in RawS_cane_by_country
S_flavoured_sugar_cane_2018 <- S_flavoured_Trade_2018 %>%
  semi_join(RawS_cane_by_country, by = "Countries_code")

# Print the names of removed countries
print(removed_countries_S_flavoured)


# Merge ratio values into S_flavoured_sugar_cane_2018 based on Countries_code
S_flavoured_sugar_cane_2018 <- merge(S_flavoured_sugar_cane_2018, sugar_cane_ratio, by = "Countries_code", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
S_flavoured_sugar_cane_2018 <- S_flavoured_sugar_cane_2018 %>%
  mutate(total_export_correction = total_export * ratio)


# creating a new column called total_export_original which holds the original values of the total_export column
# Then updating the total_export column with values from total_export_correction
S_flavoured_sugar_cane_2018 <- S_flavoured_sugar_cane_2018 %>%
  mutate(
    total_export_original = ifelse(!is.na(total_export_correction), total_export, NA),
    total_export = ifelse(!is.na(total_export_correction), total_export_correction, total_export)
  )


colnames(S_flavoured_sugar_cane_2018)

# Remove the specified columns
S_flavoured_sugar_cane_2018_FIN <- select(S_flavoured_sugar_cane_2018, -c( "Country_name.x", "Country_name.y", "ratio", "total_export_correction", "total_export_original"))
colnames(S_flavoured_sugar_cane_2018_FIN)

 write.xlsx(S_flavoured_sugar_cane_2018,"S_flavoured_sugar_cane_2018.xlsx")
 
# Left join SC_MFA with S_flavoured_sugar_cane_2018_FIN by Countries_code
SC_MFA <- SC_MFA %>%
  left_join(S_flavoured_sugar_cane_2018_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Sflav_import = total_import,
         Sflav_export = total_export)


# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
SC_MFA$Sflav_import

##############################################################################################################
 
# SSnes_by_country - Sugar and Syrups nes CORRECTION - including the ratio of countries that have 2 sources
#view(SSnes_by_country)

# Remove the SSnes columns for replacing them after the ratio correction:
SC_MFA <- select(SC_MFA, -c("SSnes_pro", "SSnes_import", "SSnes_export", "SSnes_stock","SSnes_ER"))


#Finding the Sugar and Syrups nes obtained from Sugar cane
# remove rows from the df SConfect_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_cane_by_country
# Identify countries to be removed
removed_countries_SSnes <- SSnes_by_country %>%
  anti_join(RawS_cane_by_country, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
SSnes_by_country_sugar_cane <- SSnes_by_country %>%
  semi_join(RawS_cane_by_country, by = "Country_name")

# Print the names of removed countries
print(removed_countries_SSnes)

# Merge ratio values into SConfect_by_country_sugar_cane based on Country_name
SSnes_by_country_sugar_cane <- merge(SSnes_by_country_sugar_cane, sugar_cane_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
SSnes_by_country_sugar_cane <- SSnes_by_country_sugar_cane %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
SSnes_by_country_sugar_cane <- SSnes_by_country_sugar_cane %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
SSnes_by_country_sugar_cane <- SSnes_by_country_sugar_cane %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(SSnes_by_country_sugar_cane)

#Remove rows with production equal to 0 in the df Beverage_Nalco_by_country
SSnes_by_country_sugar_cane <- SSnes_by_country_sugar_cane %>%
  filter(production != 0)

# Remove the specified columns
SSnes_by_country_sugar_cane_FIN <- select(SSnes_by_country_sugar_cane, -c("ratio","Countries_code", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(SSnes_by_country_sugar_cane_FIN)

write.xlsx(SSnes_by_country_sugar_cane_FIN,"SSnes_by_country_sugar_cane_FIN.xlsx")

# Left join SC_MFA with SSnes_by_country_sugar_cane_FIN by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(SSnes_by_country_sugar_cane_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(SSnes_pro = production,
         SSnes_import = imports,
         SSnes_export = exports,
         SSnes_stock = stockChange,
         SSnes_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
SC_MFA$SSnes_pro

########################################################################################################

# 18. Beverage non-alcohol - from the FAO data - obtained from sugar cane and beet
# Creating  Bev_non_alco df by filtering from the df RefS_products_2018
Bev_non_alco_by_country <- RefS_products_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Other non-alcoholic caloric beverages n.e.c") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

colnames(Bev_non_alco_by_country)

#Remove rows with production equal to 0 in the df Beverage_Nalco_by_country
Bev_non_alco_by_country <- Bev_non_alco_by_country %>%
  filter(production != 0)

#Finding the Beverage non-alcohol obtained from Sugar cane
# remove rows from the df Bev_non_alco_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_cane_by_country
# Identify countries to be removed
removed_countries_Bev_non_alco <- Bev_non_alco_by_country %>%
  anti_join(RawS_cane_by_country, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
Bev_non_alco_by_country_sugar_cane <- Bev_non_alco_by_country %>%
  semi_join(RawS_cane_by_country, by = "Country_name")

# Print the names of removed countries
print(removed_countries_Bev_non_alco)

# Merge ratio values into Bev_non_alco_by_country_sugar_cane based on Country_name
Bev_non_alco_by_country_sugar_cane <- merge(Bev_non_alco_by_country_sugar_cane, sugar_cane_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Bev_non_alco_by_country_sugar_cane <- Bev_non_alco_by_country_sugar_cane %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Bev_non_alco_by_country_sugar_cane <- Bev_non_alco_by_country_sugar_cane %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Bev_non_alco_by_country_sugar_cane <- Bev_non_alco_by_country_sugar_cane %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(Bev_non_alco_by_country_sugar_cane)

# Remove the specified columns
Bev_non_alco_by_country_sugar_cane_FIN <- select(Bev_non_alco_by_country_sugar_cane, -c( "Countries_code", "Processed_parent", "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(Bev_non_alco_by_country_sugar_cane_FIN)

write.xlsx(Bev_non_alco_by_country_sugar_cane_FIN, "Bev_non_alco_by_country_sugar_cane_FIN.xlsx")

# Left join SC_MFA with SConfect_by_country_sugar_cane_FIN by FAO_countries_production and Country_name
SC_MFA <- SC_MFA %>%
  left_join(Bev_non_alco_by_country_sugar_cane_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Bev_non_alco_pro = production,
         Bev_non_alco_import = imports,
         Bev_non_alco_export = exports,
         Bev_non_alco_stock = stockChange,
         Bev_non_alco_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_MFA)
colnames(SC_MFA)
SC_MFA$Bev_non_alco_pro


########################################################################################################################
# Export df SC_MFA to Excel
 write.xlsx(SC_MFA, "SC_MFA_31.10.24.xlsx")
 
#######################################################################################################################



 ##################################################################################################################################
 # SUGAR BEET production
   #########################################################################################################################################
 
 #view(SB_2018)
 #view(sugar_beet_ratio)
 #view(RawS_beet_by_country)
 
 # 1. Raw sugar from sugar beet - by the FAO data and the ratio of the raw sugar obtained from sugar beet (relevant to countries which have 2 sources for sugar)
 
 # adding the Countries_code (ISO3) name to the df sugar_beet_ratio
 sugar_beet_ratio <- sugar_beet_ratio %>%
   left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
             by = c("Country_name" = "FAO_countries_production"))
 
 # adding the Countries_code (ISO3) name to the df RawS_beet_by_country
 RawS_beet_by_country <- RawS_beet_by_country %>%
   left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
             by = c("Country_name" = "FAO_countries_production"))
 
 #Remove rows with production equal to 0 in the df RawS_beet_by_country
 RawS_beet_by_country <-  RawS_beet_by_country %>%
   filter(production != 0)
 
 # Merge ratio values into RawS_beet_by_country based on Country_name and Countries_code
RawS_beet_by_country <- merge(RawS_beet_by_country, sugar_beet_ratio, by = c("Country_name", "Countries_code"), all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
RawS_beet_by_country <- RawS_beet_by_country %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
RawS_beet_by_country <- RawS_beet_by_country %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
RawS_beet_by_country <- RawS_beet_by_country %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

#Remove rows with production equal to 0 in the df RawS_beet_by_country
RawS_beet_by_country <-  RawS_beet_by_country %>%
  filter(production != 0)

colnames(RawS_beet_by_country)

# Remove the specified columns
RawS_beet_by_country_FIN <- select(RawS_beet_by_country, -c( "availability", "Processed_parent", "source", "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(RawS_beet_by_country_FIN)
colnames(SC_MFA)

write.xlsx(RawS_beet_by_country_FIN, "RawS_beet_by_country_FIN.xlsx")

# Left join SC_MFA with RawS_beet_by_country_FIN by FAO_countries_production, Country_name, and Countries_code
SC_SB_MFA <- SC_MFA %>%
  left_join(RawS_beet_by_country_FIN, by = c("FAO_countries_production" = "Country_name", "Countries_code" = "Countries_code")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Raw_sugar_beet_pro = production,
         Raw_sugar_beet_import = imports,
         Raw_sugar_beet_export = exports,
         Raw_sugar_beet_stock = stockChange,
         Raw_sugar_beet_ER = extractionRate)

colnames(SC_SB_MFA)
SC_SB_MFA$Raw_sugar_beet_pro

#########################################################
# 2.Molasses from Sugar beet - by the FAO data
# Creating Molasses df by filtering from the df = SB_2018
Molasses_by_country_sugar_beet <- SB_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Molasses (from beet, cane and maize)") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

#Remove rows with production equal to 0 in the Molasses_by_country_sugar_beet
Molasses_by_country_sugar_beet <-  Molasses_by_country_sugar_beet %>%
  filter(production != 0)

# adding the Countries_code (ISO3) name to the df Molasses_by_country_sugar_beet
Molasses_by_country_sugar_beet <- Molasses_by_country_sugar_beet %>%
  left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
            by = c("Country_name" = "FAO_countries_production"))

# Merge ratio values into Molasses_by_country_sugar_beet based on Country_name and Countries_code
Molasses_by_country_sugar_beet <- merge(Molasses_by_country_sugar_beet, sugar_beet_ratio, by = c("Country_name", "Countries_code"), all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Molasses_by_country_sugar_beet <- Molasses_by_country_sugar_beet %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Molasses_by_country_sugar_beet <- Molasses_by_country_sugar_beet %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Molasses_by_country_sugar_beet <- Molasses_by_country_sugar_beet %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(Molasses_by_country_sugar_beet)

# Remove the specified columns
Molasses_by_country_sugar_beet_FIN <- select(Molasses_by_country_sugar_beet, -c("ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(Molasses_by_country_sugar_beet_FIN)

#Remove rows with production equal to 0 in the Molasses_by_country_sugar_beet_FIN
Molasses_by_country_sugar_beet_FIN <-  Molasses_by_country_sugar_beet_FIN %>%
  filter(production != 0)

write.xlsx(Molasses_by_country_sugar_beet_FIN, "Molasses_by_country_sugar_beet_FIN.xlsx")

# Left join SC_MFA with Molasses_by_country_sugar_beet_FIN by FAO_countries_production, Country_name, and Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Molasses_by_country_sugar_beet_FIN, by = c("FAO_countries_production" = "Country_name", "Countries_code" = "Countries_code")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Molasses_beet_pro = production,
         Molasses_beet_import = imports,
         Molasses_beet_export = exports,
         Molasses_beet_stock = stockChange,
         Molassesr_beet_ER = extractionRate)

#  calculating the Molasses moister / sucrose - the same % like we did with the molasses obtained from sugar cane
SC_SB_MFA <- SC_SB_MFA %>%
  mutate(Molasses_beet_moister = Molasses_beet_pro * 0.18,
         Molasses_beet_sucrose = Molasses_beet_pro *0.33)

colnames(SC_SB_MFA)
SC_SB_MFA$Molasses_beet_pro

#############################################

# 3. calculating Ethanol-beet according to Molasses mass: 5.11 ton molasses = 1 ton ethanol
# SC_SB_MFA <- SC_SB_MFA %>%
#  mutate(Ethanol_beet_pro = Molasses_beet_pro * 5.07)

####################################################

# 5. Bev_alco - beet = Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages - from the FAO data
# Creating BevNA_beet df by filtering from the df = SB_2018
Bev_alco_by_country_sugar_beet <- SB_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)

# adding the Countries_code (ISO3) name to the df Bev_alco_by_country_sugar_beet
Bev_alco_by_country_sugar_beet <- Bev_alco_by_country_sugar_beet %>%
  left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
            by = c("Country_name" = "FAO_countries_production"))

# Merge ratio values into Bev_alco_by_country_sugar_beet based on Country_name and Countries_code
Bev_alco_by_country_sugar_beet <- merge(Bev_alco_by_country_sugar_beet, sugar_beet_ratio, by = c("Country_name", "Countries_code"), all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Bev_alco_by_country_sugar_beet <- Bev_alco_by_country_sugar_beet %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Bev_alco_by_country_sugar_beet <- Bev_alco_by_country_sugar_beet %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Bev_alco_by_country_sugar_beet <- Bev_alco_by_country_sugar_beet %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(Bev_alco_by_country_sugar_beet)

#Remove rows with production equal to 0 in the Bev_alco_by_country_sugar_beet
Bev_alco_by_country_sugar_beet <-  Bev_alco_by_country_sugar_beet %>%
  filter(production != 0)

# Remove the specified columns
Bev_alco_by_country_sugar_beet_FIN <- select(Bev_alco_by_country_sugar_beet, -c("ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(Bev_alco_by_country_sugar_beet_FIN)

write.xlsx(Bev_alco_by_country_sugar_beet_FIN, "Bev_alco_by_country_sugar_beet_FIN.xlsx")

# Left join SC_SB_MFA with Bev_alco_by_country_sugar_beet_FIN by FAO_countries_production, Country_name, and Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Bev_alco_by_country_sugar_beet_FIN, by = c("FAO_countries_production" = "Country_name", "Countries_code" = "Countries_code")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(Bev_alco_beet_pro = production,
         Bev_alco_beet_import = imports,
         Bev_alco_beet_export = exports,
         Bev_alco_beet_stock = stockChange,
         Bev_alco_beet_ER = extractionRate)

colnames(SC_SB_MFA)

#######################################################################################

# 6.Non-food_alco - BEET = Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher - from the FAO data
# Creating Non-food_alco df by filtering from the df =SB_2018
NF_alco_by_country_sugar_beet <- SB_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange)


# adding the Countries_code (ISO3) name to the df NF_alco_by_country_sugar_beet
NF_alco_by_country_sugar_beet <- NF_alco_by_country_sugar_beet %>%
  left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
            by = c("Country_name" = "FAO_countries_production"))

# Merge ratio values into NF_alco_by_country_sugar_beet based on Country_name and Countries_code
NF_alco_by_country_sugar_beet <- merge(NF_alco_by_country_sugar_beet, sugar_beet_ratio, by = c("Country_name", "Countries_code"), all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
NF_alco_by_country_sugar_beet <- NF_alco_by_country_sugar_beet %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
NF_alco_by_country_sugar_beet <- NF_alco_by_country_sugar_beet %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
NF_alco_by_country_sugar_beet <- NF_alco_by_country_sugar_beet %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(NF_alco_by_country_sugar_beet)

#Remove rows with production equal to 0 in the NF_alco_by_country_sugar_beet
NF_alco_by_country_sugar_beet <-  NF_alco_by_country_sugar_beet %>%
  filter(production != 0)

# Remove the specified columns
NF_alco_by_country_sugar_beet_FIN <- select(NF_alco_by_country_sugar_beet, -c("ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(NF_alco_by_country_sugar_beet_FIN)

write.xlsx(NF_alco_by_country_sugar_beet_FIN, "NF_alco_by_country_sugar_beet_FIN.xlsx")

# Left join SC_SB_MFA with NF_alco_by_country_sugar_beet_FIN by FAO_countries_production, Country_name, and Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(NF_alco_by_country_sugar_beet_FIN, by = c("FAO_countries_production" = "Country_name", "Countries_code" = "Countries_code")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(NF_alco_beet_pro = production,
         NF_alco_beet_import = imports,
         NF_alco_beet_export = exports,
         NF_alco_beet_stock = stockChange,
         NF_alco_beet_ER = extractionRate)

colnames(SC_SB_MFA)
SC_SB_MFA$NF_alco_beet_pro

#######################################################################################

# 7.Refined sugar - BEET =  by the FAO data according to ratio of countries that have 2 sources
# Creating Refined sugar df by filtering from the df RefS_products_2018
Refined_sugar_cane_and_beet_by_country <- RefS_products_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Refined sugar") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

# adding the Countries_code (ISO3) name to the df Refined_sugar_cane_and_beet_by_country
Refined_sugar_cane_and_beet_by_country <- Refined_sugar_cane_and_beet_by_country %>%
  left_join(SC_MFA %>% select(FAO_countries_production, Countries_code),
            by = c("Country_name" = "FAO_countries_production"))


# Define a list of countries and their corresponding codes - that they have NA as code name ()
country_codes_NA <- c("Palestine(1996-)" = "PSE", "Kiribati" = "KIR", "Democratic People's Republic of Korea" = "PRK", "Luxembourg" = "LUX", "Maldives" = "MDV",
                      "Malta" = "MLT","Mauritania" = "MRT", "Montenegro" = "MNE", "New Caledonia" = "NCLE","Seychelles" = "SYC")

# Update DataFrame using dynamic case_when with the defined vector
Refined_sugar_cane_and_beet_by_country <- Refined_sugar_cane_and_beet_by_country %>%
  mutate(Countries_code = case_when(
    is.na(Countries_code) & Country_name %in% names(country_codes_NA) ~ country_codes_NA[Country_name],
    TRUE ~ Countries_code
  ))


# Merge ratio values into Refined_sugar_beet_by_country based on Country_name and Countries_code
Refined_sugar_beet_by_country <- merge(Refined_sugar_cane_and_beet_by_country, sugar_beet_ratio, by = c("Country_name", "Countries_code"), all.x = TRUE)
view(Refined_sugar_beet_by_country)
# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Refined_sugar_beet_by_country <- Refined_sugar_beet_by_country %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Refined_sugar_beet_by_country <- Refined_sugar_beet_by_country %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Refined_sugar_beet_by_country <- Refined_sugar_beet_by_country %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(Refined_sugar_beet_by_country)

#Remove rows with production equal to 0 in the Refined_sugar_beet_by_country
Refined_sugar_beet_by_country <-  Refined_sugar_beet_by_country %>%
  filter(production != 0)


colnames(Refined_sugar_beet_by_country)
colnames(RawS_beet_by_country_FIN)

# Filter the rows in Refined_sugar_beet_by_country where Countries_code is in RawS_beet_by_country_FIN - to find the countries that produce refined sugar from sugar beet
Refined_sugar_beet_by_country <- Refined_sugar_beet_by_country %>%
  filter(Countries_code %in% RawS_beet_by_country_FIN$Countries_code)

# Remove the specified columns
Refined_sugar_beet_by_country_FIN <- select(Refined_sugar_beet_by_country, -c("Processed_parent", "ratio", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(Refined_sugar_beet_by_country_FIN)

write.xlsx(Refined_sugar_beet_by_country_FIN,"Refined_sugar_beet_by_country_FIN.xlsx")

# Left join SC_SB_MFA with Refined_sugar_beet_by_country_FIN by FAO_countries_production, Country_name, and Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Refined_sugar_beet_by_country_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name, -Country_name) %>%
  # Rename columns
  rename(RefS_beet_pro = production,
         RefS_beet_import = imports,
         RefS_beet_export = exports,
         RefS_beet_stock = stockChange,
         RefS_beet_ER = extractionRate)

colnames(SC_SB_MFA)
SC_SB_MFA$RefS_beet_pro

##########################################################

# 8. SSnes_by_country - Sugar and Syrups nes - beet - including the ratio of countries that have 2 sources
#view(SSnes_by_country)
#view(RawS_beet_by_country_FIN)

#Finding the Sugar and Syrups nes obtained from Sugar beet
# remove rows from the df SSnes_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_beet_by_country_FIN
# Identify countries to be removed
removed_countries_SSnes_beet <- SSnes_by_country %>%
  anti_join(RawS_beet_by_country_FIN, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
SSnes_by_country_sugar_beet <- SSnes_by_country %>%
  semi_join(RawS_beet_by_country_FIN, by = "Country_name")

# Print the names of removed countries
print(removed_countries_SSnes_beet)
#view(SSnes_by_country_sugar_beet)

# Merge ratio values into SSnes_by_country_sugar_beet based on Country_name
SSnes_by_country_sugar_beet <- merge(SSnes_by_country_sugar_beet, sugar_beet_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
SSnes_by_country_sugar_beet <- SSnes_by_country_sugar_beet %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
SSnes_by_country_sugar_beet <- SSnes_by_country_sugar_beet %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
SSnes_by_country_sugar_beet <- SSnes_by_country_sugar_beet %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(SSnes_by_country_sugar_beet)

#Remove rows with production equal to 0 in the df SSnes_by_country_sugar_beet
SSnes_by_country_sugar_beet <- SSnes_by_country_sugar_beet %>%
  filter(production != 0)

# Remove the specified columns
SSnes_by_country_sugar_beet_FIN <- select(SSnes_by_country_sugar_beet, -c("ratio","Countries_code", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(SSnes_by_country_sugar_beet_FIN)

write.xlsx(SSnes_by_country_sugar_beet_FIN,"SSnes_by_country_sugar_beet_FIN.xlsx")

# Left join SC_MFA with SSnes_by_country_sugar_beet_FIN by FAO_countries_production and Country_name
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(SSnes_by_country_sugar_beet_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(SSnes_beet_pro = production,
         SSnes_beet_import = imports,
         SSnes_beet_export = exports,
         SSnes_beet_stock = stockChange,
         SSnes_beet_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_SB_MFA)
colnames(SC_SB_MFA)
SC_SB_MFA$SSnes_beet_pro

#######################################################################################################################

# 9. SConfect_by_country -  Sugar Confectionery (Refiend sugar) - beet - including the ratio of countries that have 2 sources
#view(SConfect_by_country)
#view(RawS_beet_by_country_FIN)

#Finding the Sugar Confectionery obtained from Sugar beet
# remove rows from the df SConfect_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_beet_by_country_FIN
# Identify countries to be removed
removed_countries_SConfect_beet <- SConfect_by_country %>%
  anti_join(RawS_beet_by_country_FIN, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_cane_by_country
SConfect_by_country_sugar_beet <- SConfect_by_country %>%
  semi_join(RawS_beet_by_country_FIN, by = "Country_name")

# Print the names of removed countries
print(removed_countries_SConfect_beet)
#view(SConfect_by_country_sugar_beet)

# Merge ratio values into SConfect_by_country_sugar_beet based on Country_name
SConfect_by_country_sugar_beet <- merge(SConfect_by_country_sugar_beet, sugar_beet_ratio, by = "Country_name", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
SConfect_by_country_sugar_beet <- SConfect_by_country_sugar_beet %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
SConfect_by_country_sugar_beet <- SConfect_by_country_sugar_beet %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
SConfect_by_country_sugar_beet <- SConfect_by_country_sugar_beet %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(SConfect_by_country_sugar_beet)

#Remove rows with production equal to 0 in the df SConfect_by_country_sugar_beet
SConfect_by_country_sugar_beet <- SConfect_by_country_sugar_beet %>%
  filter(production != 0)

# Remove the specified columns
SConfect_by_country_sugar_beet_FIN <- select(SConfect_by_country_sugar_beet, -c("Processed_parent", "ratio","Countries_code", "production_correction", "exports_correction", "production_original", "exports_original"))
colnames(SConfect_by_country_sugar_beet_FIN)

write.xlsx(SConfect_by_country_sugar_beet_FIN,"SConfect_by_country_sugar_beet_FIN.xlsx")

# Left join SC_MFA with SConfect_by_country_sugar_beet_FIN by FAO_countries_production and Country_name
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(SConfect_by_country_sugar_beet_FIN, by = c("FAO_countries_production" = "Country_name")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name) %>%
  # Rename columns
  rename(SConfect_beet_pro = production,
         SConfect_beet_import = imports,
         SConfect_beet_export = exports,
         SConfect_beet_stock = stockChange,
         SConfect_beet_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_SB_MFA)
colnames(SC_SB_MFA)
SC_SB_MFA$SConfect_beet_pro

##################################################################################
# 10. SConfect_by_country -  Sugar Confectionery (Refiend sugar) - beet - including the ratio of countries that have 2 sources
#view(Bev_non_alco_by_country)
#view(RawS_beet_by_country_FIN)

#Finding the Beverage_non_alco obtained from Sugar beet
# remove rows from the df Bev_non_alco_by_country where the "Country_name" is not present in the "Country_name" column of the df RawS_beet_by_country_FIN
# Identify countries to be removed
removed_countries_Bev_non_alco_beet <- Bev_non_alco_by_country %>%
  anti_join(RawS_beet_by_country_FIN, by = "Country_name") %>%
  pull(Country_name)

# Remove rows with Country_names not present in RawS_beet_by_country
Bev_non_alco_by_country_sugar_beet <- Bev_non_alco_by_country %>%
  semi_join(RawS_beet_by_country_FIN, by = "Country_name")

# Print the names of removed countries
print(removed_countries_Bev_non_alco_beet)
#view(Bev_non_alco_by_country_sugar_beet)

# Merge ratio values into Bev_non_alco_by_country_sugar_beet_by_country_sugar_beet based on Country_name
Bev_non_alco_by_country_sugar_beet <- merge(Bev_non_alco_by_country_sugar_beet, sugar_beet_ratio, by = "Country_name", all.x = TRUE)

# Perform a left join and update the country_code conditionally
Bev_non_alco_by_country_sugar_beet <- Bev_non_alco_by_country_sugar_beet %>%
  left_join(RawS_beet_by_country_FIN %>% select(Country_name, Countries_code), by = "Country_name") %>%
  mutate(Countries_code = ifelse(is.na(Countries_code.x), Countries_code.y, Countries_code.x)) %>%
  select(-Countries_code.y, -Countries_code.x) %>%
  rename(Countries_code = Countries_code)



# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
Bev_non_alco_by_country_sugar_beet <- Bev_non_alco_by_country_sugar_beet %>%
  mutate(production_correction = production * ratio,
         exports_correction = exports * ratio)


# creating a new column called production_original which holds the original values of the production column
# Then updating the production column with values from production_correction
Bev_non_alco_by_country_sugar_beet <- Bev_non_alco_by_country_sugar_beet %>%
  mutate(
    production_original = ifelse(!is.na(production_correction), production, NA),
    production = ifelse(!is.na(production_correction), production_correction, production)
  )

# creating a new column called export_original which holds the original values of the exports column
# Then updating the production column with values from exports_correction
Bev_non_alco_by_country_sugar_beet <- Bev_non_alco_by_country_sugar_beet %>%
  mutate(
    exports_original = ifelse(!is.na(exports_correction), exports, NA),
    exports = ifelse(!is.na(exports_correction), exports_correction, exports)
  )

colnames(Bev_non_alco_by_country_sugar_beet)

#Remove rows with production equal to 0 in the df Bev_non_alco_by_country_sugar_beet
Bev_non_alco_by_country_sugar_beet <- Bev_non_alco_by_country_sugar_beet %>%
  filter(production != 0)

# Remove the specified columns
Bev_non_alco_by_country_sugar_beet_FIN <- select(Bev_non_alco_by_country_sugar_beet, -c("Processed_parent", "ratio","production_correction", "exports_correction", "production_original", "exports_original"))
colnames(Bev_non_alco_by_country_sugar_beet_FIN)

write.xlsx(Bev_non_alco_by_country_sugar_beet_FIN,"Bev_non_alco_by_country_sugar_beet_FIN.xlsx")

# Left join SC_MFA with SConfect_by_country_sugar_beet_FIN by FAO_countries_production and Country_name
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Bev_non_alco_by_country_sugar_beet_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Select necessary columns and drop Child_name
  select(-Child_name, Country_name) %>%
  # Rename columns
  rename(Bev_non_alco_beet_pro = production,
         Bev_non_alco_beet_import = imports,
         Bev_non_alco_beet_export = exports,
         Bev_non_alco_beet_stock = stockChange,
         Bev_non_alco_beet_ER = extractionRate)

# View the updated SC_MFA dataframe
#View(SC_SB_MFA)
colnames(SC_SB_MFA)
SC_SB_MFA$Bev_non_alco_beet_pro

########################################################################


# 11. Refined Sugar flavoured / coloured - from Chatham House - obtained from sugar beet

#view(S_flavoured_Trade_2018)
#view(RawS_beet_by_country)

#Finding the Refined Sugar flavoured / coloured obtained from Sugar beet
# remove rows from the df S_flavoured_Trade_2018 where the "Country_name" is not present in the "Country_name" column of the df RawS_beet_by_country
# Identify countries to be removed
removed_countries_S_flavoured_beet <- S_flavoured_Trade_2018 %>%
  anti_join(RawS_beet_by_country, by = "Countries_code") %>%
  pull(Countries_code)

# Remove rows with Country_names not present in RawS_beet_by_country
S_flavoured_sugar_beet_2018 <- S_flavoured_Trade_2018 %>%
  semi_join(RawS_beet_by_country, by = "Countries_code")

# Print the names of removed countries
print(removed_countries_S_flavoured_beet)


# Merge ratio values into S_flavoured_sugar_cane_2018 based on Countries_code
S_flavoured_sugar_beet_2018 <- merge(S_flavoured_sugar_beet_2018, sugar_beet_ratio, by = "Countries_code", all.x = TRUE)

# Multiply the production column by the ratio column and store the result in production_correction
# Multiply the exports column by the ratio column and store the result in exports_correction
S_flavoured_sugar_beet_2018 <- S_flavoured_sugar_beet_2018 %>%
  mutate(total_export_correction = total_export * ratio)


# creating a new column called total_export_original which holds the original values of the total_export column
# Then updating the total_export column with values from total_export_correction
S_flavoured_sugar_beet_2018 <- S_flavoured_sugar_beet_2018 %>%
  mutate(
    total_export_original = ifelse(!is.na(total_export_correction), total_export, NA),
    total_export = ifelse(!is.na(total_export_correction), total_export_correction, total_export)
  )


colnames(S_flavoured_sugar_beet_2018)

# Remove the specified columns
S_flavoured_sugar_beet_2018_FIN <- select(S_flavoured_sugar_beet_2018, -c( "Country_name.x", "Country_name.y", "ratio", "total_export_correction", "total_export_original"))
colnames(S_flavoured_sugar_beet_2018_FIN)

write.xlsx(S_flavoured_sugar_beet_2018_FIN,"S_flavoured_sugar_beet_2018_FIN.xlsx")

# Left join SC_SB_MFA with S_flavoured_sugar_beet_2018_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(S_flavoured_sugar_beet_2018_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Sflav_beet_import = total_import,
         Sflav_beet_export = total_export)


# View the updated SC_SB_MFA dataframe
colnames(SC_SB_MFA)
SC_SB_MFA$Sflav_beet_import

############################################################################################
# 12. Beet availability = mass in the field = harvest
# Filter out NA values and then extract the first "availability" value for each country name
availability_beet_by_country <- SB_2018 %>%
  filter(!is.na(availability)) %>%
  group_by(Country_name) %>%
  summarize(first_availability = first(availability))

str(availability_beet_by_country)

#Remove rows with production equal to 0
availability_beet_by_country <- availability_beet_by_country %>%
  filter(first_availability != 0)


# adding the Countries_code (ISO3) name to the df Refined_sugar_cane_and_beet_by_country
availability_beet_by_country <- availability_beet_by_country %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("Country_name" = "UN_countries"))

# Adding manually the "China,main" country code
availability_beet_by_country <- availability_beet_by_country %>%
  mutate(Countries_code = ifelse(Country_name == "China, Main", "CHN", Countries_code))

# find and keep only the unique rows in the df according to Country_name, using distinct() 
availability_beet_by_country <- availability_beet_by_country %>%
  distinct(Country_name, .keep_all = TRUE)


colnames(availability_beet_by_country)

write.xlsx(availability_beet_by_country,"availability_beet_by_country.xlsx")

# Left join SC_SB_MFA with availability_beet_by_country on Countries_code
# Select a specific columns from availability_beet_by_country
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(availability_beet_by_country %>% select(Countries_code, first_availability), 
            by = "Countries_code") %>%
  # rename column
  rename(S_Beet_harvest = first_availability)

# View the updated SC_MFA dataframe
colnames(SC_SB_MFA)
print(SC_SB_MFA$S_Beet_harvest)

#########################################################################################################
# 13. Beet Processed_parent = the harvest mass that arrived to the mill 
# Filter out NA values and then extract the first "Processed_parent" value for each country name
Processed_parent_beet_by_country <- SB_2018 %>%
  filter(!is.na(Processed_parent)) %>%
  group_by(Country_name) %>%
  summarize(first_Processed_parent = first(Processed_parent))

#Remove rows with production equal to 0
Processed_parent_beet_by_country <- Processed_parent_beet_by_country %>%
  filter(first_Processed_parent != 0)


# adding the Countries_code (ISO3) name to the df Refined_sugar_cane_and_beet_by_country
Processed_parent_beet_by_country <- Processed_parent_beet_by_country %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("Country_name" = "UN_countries"))

# Adding manually the "China,main" country code
Processed_parent_beet_by_country <- Processed_parent_beet_by_country %>%
  mutate(Countries_code = ifelse(Country_name == "China, Main", "CHN", Countries_code))

colnames(Processed_parent_beet_by_country)

# find and keep only the unique rows in the df according to Country_name, using distinct() 
Processed_parent_beet_by_country <- Processed_parent_beet_by_country %>%
  distinct(Country_name, .keep_all = TRUE)


# Left join SC_SB_MFA with Processed_parent_beet_by_country on Countries_code 
# Select a specific columns from Processed_parent_beet_by_country
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Processed_parent_beet_by_country %>% select(Countries_code, first_Processed_parent), 
            by = "Countries_code") %>%
  # rename column
  rename(SBP = first_Processed_parent)

# View the updated SC_MFA dataframe
colnames(SC_SB_MFA)
print(SC_SB_MFA$SBP)

# Find duplicates that appear more than once in the UN_countries column
duplicates_countries <- SC_SB_MFA %>%
  group_by(UN_countries) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

# Only the first row of "Belarus" is kept, and all other duplicates of "Belarus" are removed.
SC_SB_MFA <- SC_SB_MFA %>%
  group_by(UN_countries) %>%
  mutate(row_number = row_number()) %>%
  filter(!(UN_countries == "Belarus" & row_number > 1)) %>%
  select(-row_number) %>%
  ungroup()

############################################################################################################

SC_SB_MFA <- SC_SB_MFA %>%
  # rename column
  rename(Taproot = S_Beet_harvest)

# 14. SBP (Sugar Beet Processed) VS S_Beet_harvest - to find how much from the field goes to the mill for processing
SC_SB_MFA <- SC_SB_MFA %>%
  mutate(SBP_VS_Taproot_gap = Taproot - SBP,
         SBP_VS_Taproot_gap_perc =(SBP_VS_Taproot_gap/Taproot)*100, # The percentage of the harvest beet (taproot) that does not undergo processing to become sugar
                                   SCP_moister = SCP*0.75,
                                   SCP_dry = SCP - SCP_moister,
                                   SCP_sucrose = SCP_dry*0.98)

# 15. sugar beet leaves - 20-30% of the plant's mass (avg. 25%)
SC_SB_MFA <- SC_SB_MFA %>%
  mutate(sugar_beet = Taproot*1.25,
         SB_Leaves = sugar_beet-Taproot, # The percentage of the harvest beet (taproot) that does not undergo processing to become sugar
         SB_Leaves_moister = SB_Leaves*0.8242)
         
colnames(SC_SB_MFA)
         
##############################################################################################################
# 16. calculating the Beet_pulp - wet and dry - 99.5 kg per ton of clean beet with 80% moisture content (Avg.)
SC_SB_MFA <- SC_SB_MFA %>%
  mutate(Beet_pulp_wet = SBP * 0.0995,
         Beet_pulp_dry = SBP *0.077)

#######################################################################################################################

write.xlsx(SC_2018,"SC_2018.xlsx")
write.xlsx(SB_2018,"SB_2018.xlsx")

####################################################################################################################


#Adding all the import-export of the countries that have no sugar production - countries No Manufacturers (NM)

colnames(RawS_cane_by_country)
colnames(RawS_beet_by_country)

#1. merge 2 df: RawS_cane_by_country and RawS_beet_by_country by Country_name, Countries_code, source
# Select the columns from both df
RawS_cane_selected <- RawS_cane_by_country %>%
  select(Country_name, Countries_code, source)

RawS_beet_selected <- RawS_beet_by_country %>%
  select(Country_name, Countries_code, source)

# Merge the two df by combining rows
RawS_cane_beet_manufacturers_countries <- bind_rows(RawS_cane_selected, RawS_beet_selected)

########################################################################################
# 2. Raw sugar cane - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  raw sugar cane - trade data
raw_sugar_cane_TRADE <- read_excel("2018 - raw sugar cane - trade.xlsx")
colnames(raw_sugar_cane_TRADE)
str(raw_sugar_cane_TRADE)

# Filter the 2018 trade
library(dplyr)
raw_sugar_cane_TRADE_2018 <- filter(raw_sugar_cane_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
raw_sugar_cane_TRADE_2018_exports <- raw_sugar_cane_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column and convert it to ton
raw_sugar_cane_TRADE_2018_imports <- raw_sugar_cane_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)


# Join export and import data
raw_sugar_cane_TRADE_2018_all <- full_join(raw_sugar_cane_TRADE_2018_exports, raw_sugar_cane_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
raw_sugar_cane_TRADE_2018_all$total_export[is.na(raw_sugar_cane_TRADE_2018_all$total_export)] <- 0
raw_sugar_cane_TRADE_2018_all$total_import[is.na(raw_sugar_cane_TRADE_2018_all$total_import)] <- 0

colnames(raw_sugar_cane_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in raw_sugar_cane_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_raw_sugar_cane_TRADE <- setdiff(raw_sugar_cane_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new dataframe with only the rows for these unique countries
RawS_cane_trade_no_manufacturers <- raw_sugar_cane_TRADE_2018_all[raw_sugar_cane_TRADE_2018_all$Countries_code %in% unique_countries_raw_sugar_cane_TRADE, ]

# Select specific columns
RawS_cane_trade_no_manufacturers <- RawS_cane_trade_no_manufacturers[, c("Country_name", "Countries_code", "total_export", "total_import")]

colnames(RawS_cane_trade_no_manufacturers)

# Remove the specified columns
RawS_cane_trade_no_manufacturers_FIN <- select(RawS_cane_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
RawS_cane_trade_no_manufacturers_FIN <- RawS_cane_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(RawS_cane_trade_no_manufacturers_FIN,"RawS_cane_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(RawS_cane_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Raw_sugar_NM_import = total_import,
         Raw_sugar_NM_export = total_export)

SC_SB_MFA$Raw_sugar_NM_import
colnames(SC_SB_MFA)

########################################################################################################

# 3. Raw sugar beet - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  raw sugar beet - trade data
raw_sugar_beet_TRADE <- read_excel("2018  - raw sugar beet - trade.xlsx")
colnames(raw_sugar_beet_TRADE)
str(raw_sugar_beet_TRADE)

# Filter the 2018 trade
library(dplyr)
raw_sugar_beet_TRADE_2018 <- filter(raw_sugar_beet_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
raw_sugar_beet_TRADE_2018_exports <- raw_sugar_beet_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
raw_sugar_beet_TRADE_2018_imports <- raw_sugar_beet_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
raw_sugar_beet_TRADE_2018_all <- full_join(raw_sugar_beet_TRADE_2018_exports, raw_sugar_beet_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
raw_sugar_beet_TRADE_2018_all$total_export[is.na(raw_sugar_beet_TRADE_2018_all$total_export)] <- 0
raw_sugar_beet_TRADE_2018_all$total_import[is.na(raw_sugar_beet_TRADE_2018_all$total_import)] <- 0

colnames(raw_sugar_beet_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in raw_sugar_cane_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_raw_sugar_beet_TRADE <- setdiff(raw_sugar_beet_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new df with only the rows for these unique countries
RawS_beet_trade_no_manufacturers <- raw_sugar_beet_TRADE_2018_all[raw_sugar_beet_TRADE_2018_all$Countries_code %in% unique_countries_raw_sugar_beet_TRADE, ]

# Select specific columns
RawS_beet_trade_no_manufacturers <- RawS_beet_trade_no_manufacturers[, c("Country_name", "Countries_code", "total_export", "total_import")]

colnames(RawS_beet_trade_no_manufacturers)

# Remove the specified columns
RawS_beet_trade_no_manufacturers_FIN <- select(RawS_beet_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
RawS_beet_trade_no_manufacturers_FIN <- RawS_beet_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(RawS_beet_trade_no_manufacturers_FIN,"RawS_beet_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(RawS_beet_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Raw_sugar_beet_NM_import = total_import,
         Raw_sugar_beet_NM_export = total_export)

SC_SB_MFA$Raw_sugar_beet_NM_import
colnames(SC_SB_MFA)

###########################################################################################################################

# 4. Refined sugar beet or cane - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  Refined sugar - trade data
refined_sugar_TRADE <- read_excel("2018 - refined sugar - trade.xlsx")
colnames(refined_sugar_TRADE)
str(refined_sugar_TRADE)

# Filter the 2018 trade
library(dplyr)
refined_sugar_TRADE_2018 <- filter(refined_sugar_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
refined_sugar_TRADE_2018_exports <- refined_sugar_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
refined_sugar_TRADE_2018_imports <- refined_sugar_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
refined_sugar_TRADE_2018_all <- full_join(refined_sugar_TRADE_2018_exports, refined_sugar_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
refined_sugar_TRADE_2018_all$total_export[is.na(refined_sugar_TRADE_2018_all$total_export)] <- 0
refined_sugar_TRADE_2018_all$total_import[is.na(refined_sugar_TRADE_2018_all$total_import)] <- 0

colnames(refined_sugar_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in raw_sugar_cane_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_refined_sugar_TRADE <- setdiff(refined_sugar_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new df with only the rows for these unique countries
refined_sugar_trade_no_manufacturers <- refined_sugar_TRADE_2018_all[refined_sugar_TRADE_2018_all$Countries_code %in% unique_countries_refined_sugar_TRADE, ]

# Select specific columns
#refined_sugar_trade_no_manufacturers <- refined_sugar_trade_no_manufacturers[, c("Country_name", "Countries_code", "total_export", "total_import")]

colnames(refined_sugar_trade_no_manufacturers)

# Remove a specified columns
refined_sugar_trade_no_manufacturers_FIN <- select(refined_sugar_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
refined_sugar_trade_no_manufacturers_FIN <- refined_sugar_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(refined_sugar_trade_no_manufacturers_FIN,"refined_sugar_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(refined_sugar_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(RefS_NM_import = total_import,
         RefS_NM_export = total_export)

SC_SB_MFA$RefS_NM_import
colnames(SC_SB_MFA)


###################################################################################################

# 5. Molasses from cane - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  Molasses only from cane - trade data
Molasses_cane_TRADE <- read_excel("2018 - molasses only from cane - trade.xlsx")
colnames(Molasses_cane_TRADE)
str(Molasses_cane_TRADE)

# Filter the 2018 trade
library(dplyr)
Molasses_cane_TRADE_2018 <- filter(Molasses_cane_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
Molasses_cane_TRADE_2018_exports <- Molasses_cane_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
Molasses_cane_TRADE_2018_imports <- Molasses_cane_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
Molasses_cane_TRADE_2018_all <- full_join(Molasses_cane_TRADE_2018_exports, Molasses_cane_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
Molasses_cane_TRADE_2018_all$total_export[is.na(Molasses_cane_TRADE_2018_all$total_export)] <- 0
Molasses_cane_TRADE_2018_all$total_import[is.na(Molasses_cane_TRADE_2018_all$total_import)] <- 0

colnames(Molasses_cane_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in raw_sugar_cane_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_Molasses_cane_TRADE <- setdiff(Molasses_cane_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new df with only the rows for these unique countries
Molasses_cane_trade_no_manufacturers <- Molasses_cane_TRADE_2018_all[Molasses_cane_TRADE_2018_all$Countries_code %in% unique_countries_Molasses_cane_TRADE, ]

colnames(Molasses_cane_trade_no_manufacturers)

# Remove a specified columns
Molasses_cane_trade_no_manufacturers_FIN <- select(Molasses_cane_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
Molasses_cane_trade_no_manufacturers_FIN <- Molasses_cane_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(Molasses_cane_trade_no_manufacturers_FIN,"Molasses_cane_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Molasses_cane_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Molasses_cane_NM_import = total_import,
         Molasses_cane_export = total_export)

SC_SB_MFA$Molasses_cane_NM_import
colnames(SC_SB_MFA)

##########################################################################################################

# 6. Molasses from beet/ other sources that is not cane - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  Molasses not from cane - trade data
Molasses_not_cane_TRADE <- read_excel("2018 - molasses not from cane - trade.xlsx")
colnames(Molasses_not_cane_TRADE)
str(Molasses_not_cane_TRADE)

# Filter the 2018 trade
library(dplyr)
Molasses_not_cane_TRADE_2018 <- filter(Molasses_not_cane_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
Molasses_not_cane_TRADE_2018_exports <- Molasses_not_cane_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
Molasses_not_cane_TRADE_2018_imports <- Molasses_not_cane_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
Molasses_not_cane_TRADE_2018_all <- full_join(Molasses_not_cane_TRADE_2018_exports, Molasses_not_cane_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
Molasses_not_cane_TRADE_2018_all$total_export[is.na(Molasses_not_cane_TRADE_2018_all$total_export)] <- 0
Molasses_not_cane_TRADE_2018_all$total_import[is.na(Molasses_not_cane_TRADE_2018_all$total_import)] <- 0

colnames(Molasses_not_cane_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in raw_sugar_cane_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_Molasses_not_cane_TRADE <- setdiff(Molasses_not_cane_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new df with only the rows for these unique countries
Molasses_not_cane_trade_no_manufacturers <- Molasses_not_cane_TRADE_2018_all[Molasses_not_cane_TRADE_2018_all$Countries_code %in% unique_countries_Molasses_not_cane_TRADE, ]

colnames(Molasses_not_cane_trade_no_manufacturers)

# Remove a specified columns
Molasses_not_cane_trade_no_manufacturers_FIN <- select(Molasses_not_cane_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
Molasses_not_cane_trade_no_manufacturers_FIN <- Molasses_not_cane_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(Molasses_not_cane_trade_no_manufacturers_FIN,"Molasses_not_cane_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Molasses_not_cane_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Molasses_not_cane_NM_import = total_import,
         Molasses_not_cane_export = total_export)

SC_SB_MFA$Molasses_not_cane_NM_import
colnames(SC_SB_MFA)

############################################################################################################

# 7. refined sugar_flavoured or coloured from cane or beet - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  refined sugar_flavoured or coloured - trade data
RS_flavoured_TRADE <- read_excel("2018 - refined sugar_flavoured or coloured - trade.xlsx")
colnames(RS_flavoured_TRADE)
str(RS_flavoured_TRADE)

# Filter the 2018 trade
library(dplyr)
RS_flavoured_TRADE_2018 <- filter(RS_flavoured_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
RS_flavoured_TRADE_2018_exports <- RS_flavoured_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
RS_flavoured_TRADE_2018_imports <- RS_flavoured_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
RS_flavoured_TRADE_2018_all <- full_join(RS_flavoured_TRADE_2018_exports, RS_flavoured_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
RS_flavoured_TRADE_2018_all$total_export[is.na(RS_flavoured_TRADE_2018_all$total_export)] <- 0
RS_flavoured_TRADE_2018_all$total_import[is.na(RS_flavoured_TRADE_2018_all$total_import)] <- 0

colnames(RS_flavoured_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in raw_sugar_cane_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_RS_flavoured_TRADE <- setdiff(RS_flavoured_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new df with only the rows for these unique countries
RS_flavoured_trade_no_manufacturers <-RS_flavoured_TRADE_2018_all[RS_flavoured_TRADE_2018_all$Countries_code %in% unique_countries_RS_flavoured_TRADE, ]

colnames(RS_flavoured_trade_no_manufacturers)

# Remove a specified columns
RS_flavoured_trade_no_manufacturers_FIN <- select(RS_flavoured_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
RS_flavoured_trade_no_manufacturers_FIN <- RS_flavoured_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(RS_flavoured_trade_no_manufacturers_FIN,"RS_flavoured_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(RS_flavoured_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Sflav_NM_import = total_import,
         Sflav_NM_export = total_export)

SC_SB_MFA$Sflav_NM_import
colnames(SC_SB_MFA)


############################################################################################################

# 8. sugar nes_invert sugar_caramel_artificial honey (=Sugar and Syrups nes) from cane or beet - from Chatham House - countries No Manufacturers (NM)

# Read data from excel file -  sugar nes_invert sugar_caramel_artificial honey - trade data
SSnes_TRADE <- read_excel("2018 - sugar nes_invert sugar_caramel_artificial honey - trade.xlsx")
colnames(SSnes_TRADE)
str(SSnes_TRADE)

# Filter the 2018 trade
library(dplyr)
SSnes_TRADE_2018 <- filter(SSnes_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
SSnes_TRADE_2018_exports <- SSnes_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
SSnes_TRADE_2018_imports <- SSnes_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
SSnes_TRADE_2018_all <- full_join(SSnes_TRADE_2018_exports, SSnes_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
SSnes_TRADE_2018_all$total_export[is.na(SSnes_TRADE_2018_all$total_export)] <- 0
SSnes_TRADE_2018_all$total_import[is.na(SSnes_TRADE_2018_all$total_import)] <- 0

colnames(SSnes_TRADE_2018_all)
colnames(RawS_cane_beet_manufacturers_countries)

# Find the unique countries in SSnes_TRADE_2018_all not in RawS_cane_beet_manufacturers_countries
unique_countries_SSnes_TRADE <- setdiff(SSnes_TRADE_2018_all$Countries_code, RawS_cane_beet_manufacturers_countries$Countries_code)

# Create the new df with only the rows for these unique countries
SSnes_trade_no_manufacturers <-SSnes_TRADE_2018_all[SSnes_TRADE_2018_all$Countries_code %in% unique_countries_SSnes_TRADE, ]

colnames(SSnes_trade_no_manufacturers)

# Remove a specified columns
SSnes_trade_no_manufacturers_FIN <- select(SSnes_trade_no_manufacturers, -c( "Country_name"))

# Remove rows where Countries_code is NA
SSnes_trade_no_manufacturers_FIN <- SSnes_trade_no_manufacturers_FIN %>%
  filter(!is.na(Countries_code))

write.xlsx(SSnes_trade_no_manufacturers_FIN,"SSnes_trade_no_manufacturers_FIN.xlsx")

# Left join SC_SB_MFA with RawS_cane_trade_no_manufacturers_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(SSnes_trade_no_manufacturers_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(SSnes_NM_import = total_import,
         SSnes_NM_export = total_export)

SC_SB_MFA$SSnes_NM_import
colnames(SC_SB_MFA)
###########################################################################################

# 9. Sugar Confectionery (Refiend sugar) - from the FAO data - finding countries that are Not Manufacturers (NM)
# Creating  SConfect df by filtering from the df RefS_products_2018
SConfect_Trade <- RefS_products_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Sugar Confectionery") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

colnames(SConfect_Trade)

# Remove rows with production more than 0
SConfect_Trade <- SConfect_Trade %>%
  filter(production <= 0)

# Remove the specified columns
SConfect_Trade <- select(SConfect_Trade, -c( "production", "extractionRate", "Child_name", "stockChange", "Processed_parent"))
colnames(SConfect_Trade)

# adding the Countries_code (ISO3) name to the df SConfect_Trade
SConfect_Trade <- SConfect_Trade %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("Country_name" = "UN_countries"))

# Remove the specified columns
SConfect_Trade_FIN <- select(SConfect_Trade, -c( "Country_name"))
colnames(SConfect_Trade_FIN)

write.xlsx(SConfect_Trade_FIN,"SConfect_Trade_FIN.xlsx")

# Left join SC_SB_MFA with SConfect_Trade_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(SConfect_Trade_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(SConfect_NM_import = imports,
         SConfect_NM_export = exports)

SC_SB_MFA$SConfect_NM_import
colnames(SC_SB_MFA)

####################################################################################################

# 10. Beverage non-alcohol - from the FAO data - finding countries that are Not Manufacturers (NM)
# Creating  Bev_non_alco df - from the FAO data by filtering from the df RefS_products_2018
Bev_non_alco_TRADE <- RefS_products_2018 %>%
  filter(!is.na(production) & !is.na(extractionRate) & !is.na(Child_name)) %>%
  filter(Child_name == "Other non-alcoholic caloric beverages n.e.c") %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent),) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

colnames(Bev_non_alco_TRADE)

# Remove rows with production more than 0
Bev_non_alco_TRADE <- Bev_non_alco_TRADE %>%
  filter(production <= 0)

# Remove the specified columns
Bev_non_alco_TRADE <- select(Bev_non_alco_TRADE, -c( "production", "extractionRate", "Child_name", "stockChange", "Processed_parent"))
colnames(Bev_non_alco_TRADE)

# adding the Countries_code (ISO3) name to the df Bev_non_alco_TRADE
Bev_non_alco_TRADE <- Bev_non_alco_TRADE %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("Country_name" = "UN_countries"))

#view(Bev_non_alco_TRADE)

# Adding manually the "CÃ´te d'Ivoire"  and "Democratic People's Republic of Korea" country code
Bev_non_alco_TRADE <- Bev_non_alco_TRADE %>%
  mutate(Countries_code = ifelse(Country_name == "CÃ´te d'Ivoire", "CIV",
                                 ifelse(Country_name == "Democratic People's Republic of Korea", "PRK",
                                        Countries_code)))

# Remove the specified columns
Bev_non_alco_TRADE_FIN <- select(Bev_non_alco_TRADE, -c( "Country_name"))
colnames(Bev_non_alco_TRADE_FIN)

write.xlsx(Bev_non_alco_TRADE_FIN,"Bev_non_alco_TRADE_FIN.xlsx")

# Left join SC_SB_MFA with Bev_non_alco_TRADE_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Bev_non_alco_TRADE_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Bev_non_alco_NM_import = imports,
         Bev_non_alco_NM_export = exports)

SC_SB_MFA$Bev_non_alco_NM_import
colnames(SC_SB_MFA)

########################################################################################################################

# 11. Bev_alco NM = Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages - from the FAO data
#finding countries that are Not Manufacturers (NM)

#Filtering the "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages" from the crops_2018 df
Bev_alco_TRADE <- CROPS_2018 %>%
  filter(Child_name == "Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages",
         Parent_name %in% c("Sugar cane", "Sugar beet")) %>%
  mutate(imports = ifelse(is.na(imports), 0, imports),
         exports = ifelse(is.na(exports), 0, exports),
         stockChange = ifelse(is.na(stockChange), 0, stockChange),
         Processed_parent = ifelse(is.na(Processed_parent), 0, Processed_parent)) %>%
  select(Country_name, production, extractionRate, Child_name, imports, exports, stockChange, Processed_parent)

colnames(Bev_alco_TRADE)

# Remove rows with production more than 0
Bev_alco_TRADE <- Bev_alco_TRADE %>%
  filter(production <= 0)

# Remove duplicate rows based on "Country_name"
Bev_alco_TRADE <- Bev_alco_TRADE %>%
  distinct(Country_name, .keep_all = TRUE)

# remove rows where both "imports" and "exports" are equal to 0 
Bev_alco_TRADE <-Bev_alco_TRADE %>%
  filter(!(imports == 0 & exports == 0))

# Remove the specified columns
Bev_alco_TRADE <- select(Bev_alco_TRADE, -c( "production", "extractionRate", "Child_name", "stockChange", "Processed_parent"))
colnames(Bev_alco_TRADE)

# adding the Countries_code (ISO3) name to the df Bev_non_alco_TRADE
Bev_alco_TRADE <- Bev_alco_TRADE %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("Country_name" = "UN_countries"))


# Adding manually the "CÃ´te d'Ivoire" country code
Bev_alco_TRADE <- Bev_alco_TRADE %>%
  mutate(Countries_code = ifelse(Country_name == "CÃ´te d'Ivoire", "CIV",
                                        Countries_code))

#view(Bev_alco_TRADE)

# Remove the specified columns
Bev_alco_TRADE_FIN <- select(Bev_alco_TRADE, -c( "Country_name"))
colnames(Bev_alco_TRADE_FIN)

write.xlsx(Bev_alco_TRADE_FIN,"Bev_alco_TRADE_FIN.xlsx")

# Left join SC_SB_MFA with Bev_alco_TRADE_FIN by Countries_code
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(Bev_alco_TRADE_FIN, by = c("Countries_code" = "Countries_code")) %>%
  # Rename columns
  rename(Bev_alco_NM_import = imports,
         Bev_alco_NM_export = exports)

SC_SB_MFA$Bev_alco_NM_import
colnames(SC_SB_MFA)

###############################################################################################
# Export df SC_SB_MFA to Excel
write.xlsx(SC_SB_MFA, "SC_SB_MFA_1.11.24.xlsx")

################################################################################################

# TRADE - Other sources for sugar consumption - that are note from sugar cane or beet
# only for compering to the consumption - not include in the SC_SB_MFA df
# 1. sugar nes_invert sugar_caramel_artificial honey  - from Chatham House

# Read data from excel file -  2018 - sugar nes_invert sugar_caramel_artificial honey - trade data
sugar_nes_invert__caramel_artificial_honey_TRADE <- read_excel("2018 - sugar nes_invert sugar_caramel_artificial honey - trade.xlsx")
colnames(sugar_nes_invert__caramel_artificial_honey_TRADE)
str(sugar_nes_invert__caramel_artificial_honey_TRADE)

# Filter the 2018 trade
library(dplyr)
sugar_nes_invert__caramel_artificial_honey_TRADE_2018 <- filter(sugar_nes_invert__caramel_artificial_honey_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
sugar_nes_invert__caramel_artificial_honey_TRADE_2018_exports <- sugar_nes_invert__caramel_artificial_honey_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
sugar_nes_invert__caramel_artificial_honey_TRADE_2018_imports <- sugar_nes_invert__caramel_artificial_honey_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all <- full_join(sugar_nes_invert__caramel_artificial_honey_TRADE_2018_exports, sugar_nes_invert__caramel_artificial_honey_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all$total_export[is.na(sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all$total_export)] <- 0
sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all$total_import[is.na(sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all$total_import)] <- 0

colnames(sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all)
View(sugar_nes_invert_caramel_artificial_honey_TRADE_2018_all)


# 2. maple syrup - trade - from Chatham House

# Read data from excel file -  2018 - maple_syrup_TRADE - trade data
maple_syrup_TRADE <- read_excel("2018 - maple sugar_maple syrup - trade.xlsx")
colnames(maple_syrup_TRADE)
str(maple_syrup_TRADE)

# Filter the 2018 trade
library(dplyr)
maple_syrup_TRADE_2018 <- filter(maple_syrup_TRADE, Year == 2018)

# Calculate total exports for each country based on weight column
maple_syrup_TRADE_2018_exports <- maple_syrup_TRADE_2018 %>%
  group_by(`Exporter`, `Exporter ISO3`) %>%
  summarise(total_export = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Exporter, Countries_code = `Exporter ISO3`)

# Calculate total imports for each country based on weight column
maple_syrup_TRADE_2018_imports <- maple_syrup_TRADE_2018 %>%
  group_by(`Importer`, `Importer ISO3`) %>%
  summarise(total_import = sum(`Weight (1000kg)`, na.rm = TRUE), .groups = "drop") %>%
  rename(Country_name = Importer, Countries_code = `Importer ISO3`)

# Join export and import data
maple_syrup_TRADE_2018_all <- full_join(maple_syrup_TRADE_2018_exports, maple_syrup_TRADE_2018_imports, by = c("Country_name", "Countries_code"))

# Replace NAs with 0 if any country is only in exports or imports
maple_syrup_TRADE_2018_all$total_export[is.na(maple_syrup_TRADE_2018_all$total_export)] <- 0
maple_syrup_TRADE_2018_all$total_import[is.na(maple_syrup_TRADE_2018_all$total_import)] <- 0

colnames(maple_syrup_TRADE_2018_all)

###############################################################################################################
# SUGAR CONSUMPTION
################################################################################################################

# 1. Population data by the UN
# Single age - male
# Read data from excel file - POP_SINGLE_AGE_MALE
POP_male <- read_excel("POP_SINGLE_AGE_MALE.xlsx")
colnames(POP_male)
str(POP_male)


# Filter the df to include only rows where Year is 2018
POP_male_2018 <- POP_male %>%
  filter(Year == 2018)

colnames(POP_male_2018)
str(POP_male_2018)

# print specific column from df
print(POP_male$`Region, subregion, country or area *`)

# Filter the name of region / not a country name
Not_country <- c(
  "WORLD", "Sub-Saharan Africa", "Northern Africa and Western Asia",
  "Central and Southern Asia", "Eastern and South-Eastern Asia",
  "Latin America and the Caribbean", "Oceania (excluding Australia and New Zealand)",
  "Australia/New Zealand", "Europe and Northern America", "More developed regions",
  "Less developed regions", "Least developed countries",
  "Less developed regions, excluding least developed countries",
  "Less developed regions, excluding China", "Land-locked Developing Countries (LLDC)",
  "Small Island Developing States (SIDS)", "High-income countries", "Middle-income countries",
  "Upper-middle-income countries", "Lower-middle-income countries", "Low-income countries",
  "No income group available", "AFRICA", "Eastern Africa", "Middle Africa", "Northern Africa",
  "Southern Africa", "Western Africa", "ASIA", "Central Asia", "Eastern Asia", "Southern Asia",
  "South-Eastern Asia", "Western Asia", "EUROPE", "Eastern Europe", "Northern Europe",
  "Southern Europe", "Western Europe", "LATIN AMERICA AND THE CARIBBEAN", "Caribbean",
  "Central America", "South America", "NORTHERN AMERICA", "OCEANIA", "Australia/New Zealand",
  "Melanesia", "Micronesia", "Polynesia"
)

# Filter the POP_male_2018 df to filter out the Not_country value from the `Region, subregion, country or area *` column
POP_male_2018 <- POP_male_2018 %>%
  filter(!`Region, subregion, country or area *` %in% Not_country)

print(POP_male_2018$`Region, subregion, country or area *`)
str(POP_male_2018)

# Identify the columns I want to convert to numeric (from column 11 to the end)
cols_to_convert_num <- 12:ncol(POP_male_2018)

# Use lapply function to convert the selected columns to numeric
POP_male_2018[cols_to_convert_num] <- lapply(POP_male_2018[cols_to_convert_num], as.numeric)

str(POP_male_2018)

colnames(SC_SB_MFA)
colnames(POP_male_2018)

# Change multiple country names in the UN_countries column using dplyr::recode
SC_SB_MFA <- SC_SB_MFA %>%
  mutate(UN_countries = dplyr::recode(UN_countries,
                                      "CÃƒÂ´te d'Ivoire" = "Côte d'Ivoire",
                                      "Dem. People's Republic of Korea  (N.Korea)" = "Dem. People's Republic of Korea",
                                      "Turkey" = "Türkiye",
                                      "Micronesia_Fed. States of" = "Micronesia (Fed. States of)"))


# finding the country names that are not exist in the main df: SC_SB_2018, IN THE COLUMN: un_COUNTRIES
# Extract country names from the "Region, subregion, country or area *" column in POP_male_2018
pop_countries <- unique(POP_male_2018$`Region, subregion, country or area *`)

# Extract country names from the UN_countries column in SC_SB_MFA
sc_countries <- unique(SC_SB_MFA$UN_countries)

# Find the country names that are present in the SC_SB_MFA dataframe
existent_countries_SC_SB_MFA <- intersect(pop_countries, sc_countries)

# Filter POP_male_2018 to include only the countries that are in SC_SB_MFA
POP_male_2018 <- POP_male_2018[POP_male_2018$`Region, subregion, country or area *` %in% existent_countries_SC_SB_MFA, ]

# Find the country names that are not present in the SC_SB_MFA dataframe
non_existent_countries_SC_SB_MFA <- setdiff(pop_countries, sc_countries)

# Print the country names that are filtered out
print(non_existent_countries_SC_SB_MFA)

# Rename the column '100+' to '100'
names(POP_male_2018)[names(POP_male_2018) == "100+"] <- "100"

POP_male_2018$"100"

# Select only the necessary columns from the POP_male_2018
columns_to_keep <- c('Region, subregion, country or area *', as.character(0:100))
POP_male_2018 <- POP_male_2018[, columns_to_keep]

# Ensure all columns except the first one are numeric
POP_male_2018[, -1] <- lapply(POP_male_2018[, -1], function(column) {
  numeric_column <- as.numeric(as.character(column))
  numeric_column[is.na(numeric_column)] <- 0
  return(numeric_column)
})

# Define group age - according to the GDD group with adjusted indices for columns "0" to "100"
age_groups_GDD <- list(
  '0-11 mo' = 2,     # Column "0" (2nd column in df)
  '12-23 mo' = 3,    # Column "1" (3rd column in ddf)
  '2-5 years' = 4:7,
  '6-10 years' = 8:12,
  '11-14 years' = 13:16,
  '15-19 years' = 17:21,
  '20-24 years' = 22:26,
  '25-29 years' = 27:31,
  '30-34 years' = 32:36,
  '35-39 years' = 37:41,
  '40-44 years' = 42:46,
  '45-49 years' = 47:51,
  '50-54 years' = 52:56,
  '55-59 years' = 57:61,
  '60-64 years' = 62:66,
  '65-69 years' = 67:71,
  '70-74 years' = 72:76,
  '75-79 years' = 77:81,
  '80-84 years' = 82:86,
  '85-89 years' = 87:91,
  '90-94 years' = 92:96,
  '95+ years' = 97:102  # Column "100" (102nd column in df)
)

# Create a new df to store the grouped data
grouped_data_POP_MALE_2018 <- data.frame(matrix(ncol = length(age_groups_GDD), nrow = nrow(POP_male_2018)))
colnames(grouped_data_POP_MALE_2018) <- names(age_groups_GDD)

# Calculate the sums for each age group using the correct columns
for (group in names(age_groups_GDD)) {
  indices <- age_groups_GDD[[group]]  # Use indices directly since they are already adjusted
  grouped_data_POP_MALE_2018[[group]] <- rowSums(POP_male_2018[, indices, drop = FALSE], na.rm = TRUE)
}

# Combine the 'Region, subregion, country or area *' column with the grouped data
POP_male_2018_FIN <- cbind(POP_male_2018[, 'Region, subregion, country or area *', drop = FALSE], grouped_data_POP_MALE_2018)

# Rename the column Region, subregion, country or area * to UN_countries
names(POP_male_2018_FIN)[names(POP_male_2018_FIN) == "Region, subregion, country or area *"] <- "UN_countries"

# adding the Countries_code (ISO3) name to the df POP_male_2018_FIN
POP_male_2018_FIN <- POP_male_2018_FIN %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("UN_countries" = "UN_countries"))

# Change / move column place
POP_male_2018_FIN <- POP_male_2018_FIN %>%
  relocate(Countries_code, .after = UN_countries)

# adding "ma_" (= male) to the columns names
# Get the current column names
GDD_current_names <- colnames(POP_male_2018_FIN)

# Add "ma_" prefix to all columns except the first two
GDD_male_new_names <- c(GDD_current_names[1:2], paste0("ma_", GDD_current_names[3:length(GDD_current_names)]))

# Assign the new names to the dataframe
colnames(POP_male_2018_FIN) <- GDD_male_new_names

# Print the new column names to verify
print(colnames(POP_male_2018_FIN))

colnames(POP_male_2018_FIN)


###############################################################################################
# Single age - female
# Read data from excel file - POP_SINGLE_AGE_MALE
POP_female <- read_excel("POP_SINGLE_AGE_FEMALE.xlsx")
colnames(POP_female)
str(POP_female)


# Filter the df to include only rows where Year is 2018
POP_female_2018 <- POP_female %>%
  filter(Year == 2018)

colnames(POP_female_2018)
str(POP_female_2018)

# print specific column from df
print(POP_female$`Region, subregion, country or area *`)

# Filter the name of region / not a country name
Not_country <- c(
  "WORLD", "Sub-Saharan Africa", "Northern Africa and Western Asia",
  "Central and Southern Asia", "Eastern and South-Eastern Asia",
  "Latin America and the Caribbean", "Oceania (excluding Australia and New Zealand)",
  "Australia/New Zealand", "Europe and Northern America", "More developed regions",
  "Less developed regions", "Least developed countries",
  "Less developed regions, excluding least developed countries",
  "Less developed regions, excluding China", "Land-locked Developing Countries (LLDC)",
  "Small Island Developing States (SIDS)", "High-income countries", "Middle-income countries",
  "Upper-middle-income countries", "Lower-middle-income countries", "Low-income countries",
  "No income group available", "AFRICA", "Eastern Africa", "Middle Africa", "Northern Africa",
  "Southern Africa", "Western Africa", "ASIA", "Central Asia", "Eastern Asia", "Southern Asia",
  "South-Eastern Asia", "Western Asia", "EUROPE", "Eastern Europe", "Northern Europe",
  "Southern Europe", "Western Europe", "LATIN AMERICA AND THE CARIBBEAN", "Caribbean",
  "Central America", "South America", "NORTHERN AMERICA", "OCEANIA", "Australia/New Zealand",
  "Melanesia", "Micronesia", "Polynesia"
)

# Filter the POP_female_2018 df to filter out the Not_country value from the `Region, subregion, country or area *` column
POP_female_2018 <- POP_female_2018 %>%
  filter(!`Region, subregion, country or area *` %in% Not_country)

print(POP_female_2018$`Region, subregion, country or area *`)
str(POP_female_2018)

# Identify the columns I want to convert to numeric (from column 11 to the end)
cols_to_convert_num <- 12:ncol(POP_female_2018)

# Use lapply function to convert the selected columns to numeric
POP_female_2018[cols_to_convert_num] <- lapply(POP_female_2018[cols_to_convert_num], as.numeric)

str(POP_female_2018)

colnames(SC_SB_MFA)
colnames(POP_female_2018)


# finding the country names that are not exist in the main df: SC_SB_2018, IN THE COLUMN: un_COUNTRIES
# Extract country names from the "Region, subregion, country or area *" column in POP_female_2018
pop_countries_fe <- unique(POP_female_2018$`Region, subregion, country or area *`)

# Extract country names from the UN_countries column in SC_SB_MFA
sc_countries_fe <- unique(SC_SB_MFA$UN_countries)

# Find the country names that are present in the SC_SB_MFA dataframe
existent_countries_SC_SB_MFA_fe <- intersect(pop_countries_fe, sc_countries_fe)

# Filter POP_male_2018 to include only the countries that are in SC_SB_MFA
POP_female_2018 <- POP_female_2018[POP_female_2018$`Region, subregion, country or area *` %in% existent_countries_SC_SB_MFA_fe, ]

# Find the country names that are not present in the SC_SB_MFA dataframe
non_existent_countries_SC_SB_MFA_fe <- setdiff(pop_countries_fe, sc_countries_fe)

# Print the country names that are filtered out
print(non_existent_countries_SC_SB_MFA_fe)

# Select only the necessary columns from the POP_female_2018
columns_to_keep_fe <- c('Region, subregion, country or area *', as.character(0:100))
POP_female_2018 <- POP_female_2018[, columns_to_keep_fe]

# Ensure all columns except the first one are numeric
POP_female_2018[, -1] <- lapply(POP_female_2018[, -1], function(column) {
  numeric_column <- as.numeric(as.character(column))
  numeric_column[is.na(numeric_column)] <- 0
  return(numeric_column)
})


# Create a new df to store the grouped data
grouped_data_POP_FEMALE_2018 <- data.frame(matrix(ncol = length(age_groups_GDD), nrow = nrow(POP_female_2018)))
colnames(grouped_data_POP_FEMALE_2018) <- names(age_groups_GDD)

# Calculate the sums for each age group using the correct columns
for (group in names(age_groups_GDD)) {
  indices <- age_groups_GDD[[group]]
  grouped_data_POP_FEMALE_2018[[group]] <- rowSums(POP_female_2018[, indices, drop = FALSE], na.rm = TRUE)
}

# Combine the 'Region, subregion, country or area *' column with the grouped data
POP_female_2018_FIN <- cbind(POP_female_2018[, 'Region, subregion, country or area *', drop = FALSE], grouped_data_POP_FEMALE_2018)

# Rename the column Region, subregion, country or area * to UN_countries
names(POP_female_2018_FIN)[names(POP_female_2018_FIN) == "Region, subregion, country or area *"] <- "UN_countries"

# adding the Countries_code (ISO3) name to the df POP_female_2018_FIN
POP_female_2018_FIN <- POP_female_2018_FIN %>%
  left_join(SC_SB_MFA %>% select(UN_countries, Countries_code),
            by = c("UN_countries" = "UN_countries"))

# Change / move column place
POP_female_2018_FIN <- POP_female_2018_FIN %>%
  relocate(Countries_code, .after = UN_countries)

# adding "fe_" (= female) to the columns names
# Get the current column names
GDD_current_names_fe <- colnames(POP_female_2018_FIN)

# Add "fe_" prefix to all columns except the first two
GDD_female_new_names <- c(GDD_current_names_fe[1:2], paste0("fe_", GDD_current_names_fe[3:length(GDD_current_names_fe)]))

# Assign the new names to the df
colnames(POP_female_2018_FIN) <- GDD_female_new_names

# Print the new column names to verify
print(colnames(POP_female_2018_FIN))

############################################################################################################################################
# 2. Added sugars data - according to the GDD - % of the daily calorie intake - person per day
# Both sexes
# Read data from excel file - v35_cnty - added_sugar  - JAN22
GDD_both_sexes <- read_excel("v35_cnty - added_sugar  - JAN22.xlsx")
colnames(GDD_both_sexes)
str(GDD_both_sexes)


# Filter the df to include only rows where Year is 2018
# Filter the df to include only rows where education = All education levels, urban = All urban levels,
# and female is not equal to 999, and and age is not equal to 999
GDD_both_sexes_2018 <- GDD_both_sexes %>%
  filter(year== 2018 & edu == 999 & urban == 999 & female != 999 & age != 999)

colnames(GDD_both_sexes_2018)

# Select only the necessary columns from the GDD_both_sexes_2018
GDD_columns_to_keep <- c("iso3", "age", "female", "median")
GDD_both_sexes_2018 <- GDD_both_sexes_2018[, GDD_columns_to_keep]

# Rename the column "median" to "sugar_cons_perc" (= added sugar consumption percentage) and "female" to "gender"
GDD_both_sexes_2018_FIN <- GDD_both_sexes_2018 %>%
  rename(
    sugar_cons_perc = median,
    gender = female,
    Countries_code = iso3
  )


#############################################################################################
# GDD data - MALE = 0  - % of the daily calorie intake - person per day

# Filter the df to include only rows where sex is male
GDD_male_2018 <- GDD_both_sexes_2018_FIN %>%
  filter(gender== 0)

# Replace the value '0' with 'male' in the 'gender' column
GDD_male_2018$gender[GDD_male_2018$gender == 0] <- "male"


# Print all unique values in the age column of GDD_male_2018 in sorted order
GDD_unique_sorted_ages <- sort(unique(GDD_male_2018$age))
print(GDD_unique_sorted_ages)

# Adding the group age according to the GDD data and the POP_male_2018_FIN
GDD_age_to_column <- c(`0` = "ma_0-11 mo", `2` = "ma_12-23 mo", `4` = "ma_2-5 years", `8` = "ma_6-10 years",
                   `12` = "ma_11-14 years", `18` = "ma_15-19 years", `22` = "ma_20-24 years", `28` = "ma_25-29 years",
                   `32` = "ma_30-34 years", `38` = "ma_35-39 years", `42` = "ma_40-44 years", `48` = "ma_45-49 years",
                   `52` = "ma_50-54 years", `58` = "ma_55-59 years", `62` = "ma_60-64 years", `68` = "ma_65-69 years",
                   `72` = "ma_70-74 years", `78` = "ma_75-79 years", `82` = "ma_80-84 years", `88` = "ma_85-89 years",
                   `92` = "ma_90-94 years", `98` = "ma_95+ years")

# Map the ages to the group names
GDD_male_2018$group_age <- GDD_age_to_column[as.character(GDD_male_2018$age)]

# checking if there are any unmatched age values
GDD_male_2018$group_age[is.na(GDD_male_2018$group_age)] <- "Unknown"

head(GDD_male_2018)

# Change / move column place
GDD_male_2018 <- GDD_male_2018 %>%
  relocate(group_age , .after = age)

# Back to the POPULATION DATA
colnames(POP_male_2018_FIN)

# Modify the columns from "ma_0-11 mo" to "ma_95+ years" into rows for each Countries_code and create a new column named group_age
POP_male_2018_FIN_long <- POP_male_2018_FIN %>%
  pivot_longer(
    cols = starts_with("ma_"),
    names_to = "group_age",
    values_to = "POP"
  )


# marge the POP values to the GDD_male_2018 data
GDD_male_2018_with_POP <- GDD_male_2018 %>%
  left_join(POP_male_2018_FIN_long, by = c("Countries_code", "group_age"))

colnames(GDD_male_2018_with_POP)

# Change / move column place
GDD_male_2018_with_POP <- GDD_male_2018_with_POP %>%
  relocate("UN_countries" , .after = Countries_code)


################################################################################################################################

# GDD data - FEMALE = 1 - % of the daily calorie intake - person per day

# Filter the df to include only rows where sex is female
GDD_female_2018 <- GDD_both_sexes_2018_FIN %>%
  filter(gender== 1)

# Replace the value '1' with 'female' in the 'gender' column
GDD_female_2018$gender[GDD_female_2018$gender == 1] <- "female"

# Adding the group age according to the GDD data and the POP_female_2018_FIN
GDD_age_to_column_fe <- c(`0` = "fe_0-11 mo", `2` = "fe_12-23 mo", `4` = "fe_2-5 years", `8` = "fe_6-10 years",
                       `12` = "fe_11-14 years", `18` = "fe_15-19 years", `22` = "fe_20-24 years", `28` = "fe_25-29 years",
                       `32` = "fe_30-34 years", `38` = "fe_35-39 years", `42` = "fe_40-44 years", `48` = "fe_45-49 years",
                       `52` = "fe_50-54 years", `58` = "fe_55-59 years", `62` = "fe_60-64 years", `68` = "fe_65-69 years",
                       `72` = "fe_70-74 years", `78` = "fe_75-79 years", `82` = "fe_80-84 years", `88` = "fe_85-89 years",
                       `92` = "fe_90-94 years", `98` = "fe_95+ years")

# Map the ages to the group names
GDD_female_2018$group_age <- GDD_age_to_column_fe[as.character(GDD_female_2018$age)]

# checking if there are any unmatched age values
GDD_female_2018$group_age[is.na(GDD_female_2018$group_age)] <- "Unknown"

head(GDD_female_2018)

# Change / move column place
GDD_female_2018 <- GDD_female_2018 %>%
  relocate(group_age , .after = age)

# Back to the POPULATION DATA
colnames(POP_female_2018_FIN)

# Modify the columns from "ma_0-11 mo" to "ma_95+ years" into rows for each Countries_code and create a new column named group_age
POP_female_2018_FIN_long <- POP_female_2018_FIN %>%
  pivot_longer(
    cols = starts_with("fe_"),
    names_to = "group_age",
    values_to = "POP"
  )

# marge the POP values to the GDD_female_2018 data
GDD_female_2018_with_POP <- GDD_female_2018 %>%
  left_join(POP_female_2018_FIN_long, by = c("Countries_code", "group_age"))

colnames(GDD_female_2018_with_POP)

# Change / move column place
GDD_female_2018_with_POP <- GDD_female_2018_with_POP %>%
  relocate("UN_countries" , .after = Countries_code)


##########################################################################################################

# 3. Calorie intake - according to the GENus_model data - for the year 2011
# A. age 0-4_bothsexes - Calorie intake
# Read data from CSV file
CAL_0_4_bothsexes <- read.csv("NutrientTotal_withFortification_age0-4_bothsexes.csv")
colnames(CAL_0_4_bothsexes)
str(CAL_0_4_bothsexes)

# Select only the necessary columns from the CAL_0_4_bothsexes
CAL_0_4_columns_to_keep <- c("X", "X.1", "calories")
CAL_0_4_bothsexes <- CAL_0_4_bothsexes[, CAL_0_4_columns_to_keep]

# Rename the column 
CAL_0_4_bothsexes <- CAL_0_4_bothsexes %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_0_4_bothsexes)
str(CAL_0_4_bothsexes)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_0_4_bothsexes <- CAL_0_4_bothsexes %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_0_4_bothsexes)

# Delete rows 1-3
CAL_0_4_bothsexes <- CAL_0_4_bothsexes[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_0_4_bothsexes <- CAL_0_4_bothsexes %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_0_4_bothsexes)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
data <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake = c(1411.936, 1336.264, 1477.725, 1525.192, 2144.353, 2153.187, 1721.397, 
                     1346.752, 1377.756, 1568.381, 1466.465, 1494.371, 1469.865, 1392.733, 
                     1710.435, 1821.277, 1473.640, 1733.976, 1310.119, 1469.865, 1314.785, 
                     1720.585, 1191.786, 1337.814, 1611.466, 1112.784, 1528.033, 1547.456, 
                     1780.263, 1995.436, 2150.935, 0, 1711.545, 1527.544, 1446.366, 1173.974, 
                     1737.650)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories <- calorie_data %>%
  group_by(Area) %>%
  summarise(
    calorie_intake = mean(calorie_intake, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_0_4_bothsexes_NA df
CAL_0_4_bothsexes_NA <- data %>%
  left_join(mean_calories, by = "Area")

# View the result
print(CAL_0_4_bothsexes_NA)
colnames(CAL_0_4_bothsexes_NA)


# Merge the df to include calorie_intake from CAL_0_4_bothsexes_NA
CAL_0_4_bothsexes_FIN <- CAL_0_4_bothsexes %>%
  left_join(CAL_0_4_bothsexes_NA %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_0_4_bothsexes_FIN <- CAL_0_4_bothsexes_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_NA)) %>%
  select(-calorie_intake_NA)

print(CAL_0_4_bothsexes_FIN)

# Merge the calorie_intake column from CAL_0_4_bothsexes_FIN into GDD_male_2018_with_POP
GDD_male_2018_FIN <- GDD_male_2018_with_POP %>%
  left_join(CAL_0_4_bothsexes_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code")

# Conditionally update the calorie_intake column based on the ag column
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  mutate(calorie_intake = if_else(age %in% c(0, 2, 4), calorie_intake, NA_real_))

print(GDD_male_2018_FIN)

######################## female 

# Merge the calorie_intake column from CAL_0_4_bothsexes_FIN into GDD_female_2018_with_POP
GDD_female_2018_FIN <- GDD_female_2018_with_POP %>%
  left_join(CAL_0_4_bothsexes_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code")

# Conditionally update the calorie_intake column based on the ag column
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  mutate(calorie_intake = if_else(age %in% c(0, 2, 4), calorie_intake, NA_real_))

print(GDD_female_2018_FIN)
print(n=7040,GDD_female_2018_FIN)

##################################################################################################################

# B. age 5-9_bothsexes - Calorie intake
# Read data from CSV file
CAL_5_9_bothsexes <- read.csv("NutrientTotal_withFortification_age5-9_bothsexes.csv")
colnames(CAL_5_9_bothsexes)
str(CAL_5_9_bothsexes)

# Select only the necessary columns from the CAL_0_4_bothsexes
CAL_5_9_columns_to_keep <- c("X", "X.1", "calories")
CAL_5_9_bothsexes <- CAL_5_9_bothsexes[, CAL_5_9_columns_to_keep]

# Rename the column 
CAL_5_9_bothsexes <- CAL_5_9_bothsexes %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_5_9_bothsexes)
str(CAL_5_9_bothsexes)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_5_9_bothsexes <- CAL_5_9_bothsexes %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_5_9_bothsexes)

# Delete rows 1-3
CAL_5_9_bothsexes <- CAL_5_9_bothsexes[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_5_9_bothsexes <- CAL_5_9_bothsexes %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_5_9_bothsexes)

####### check
# compare the Countries_code columns from two dataframes (na_countries_5_9_bothsexes and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_5_9 <- na_countries_5_9_bothsexes$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_5_9_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_5_9, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_5_9_bothsexes
codes_not_in_5_9 <- setdiff(codes_0_4, codes_5_9)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_5_9_bothsexes but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_5_9_bothsexes are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_5_9) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_5_9_bothsexes:\n")
  print(codes_not_in_5_9)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_5_9_bothsexes.\n")
}

############

print(CAL_5_9_bothsexes)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
data <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_5_9 <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_5_9 = c(2101.487, 1988.859, 2199.405, 2270.053, 3191.595, 3204.743, 2562.080, 
                     2004.468, 2050.614, 2334.335, 2182.646, 2224.180, NA, 2072.905, 
                     2545.764, 2710.738, 2193.324, 2580.801, 1949.944, 2187.706, 1956.890, 
                     2560.871, 1773.820, 1991.165, 2398.462, 1656.237, 2274.282, 2303.191, 
                     2649.693, 2969.951, 3201.392, 0, 2547.416, 2273.554, 2152.731, 1747.309, 
                     2586.270)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_5_9 <- calorie_data_5_9 %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_5_9 = mean(calorie_intake_5_9, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_5_9_bothsexes_NA df
CAL_5_9_bothsexes_NA <- data %>%
  left_join(mean_calories_5_9, by = "Area")

# View the result
print(CAL_5_9_bothsexes_NA)

# Rename the column 
# names(CAL_5_9_bothsexes_NA)[names(CAL_5_9_bothsexes_NA) == "calorie_intake_5_9"] <- "calorie_intake"

# Merge the df to include calorie_intake from CAL_5_9_bothsexes_NA
CAL_5_9_bothsexes_FIN <- CAL_5_9_bothsexes %>%
  left_join(CAL_5_9_bothsexes_NA %>% select(Countries_code, calorie_intake_5_9), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_5_9_bothsexes_FIN <- CAL_5_9_bothsexes_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_5_9)) %>%
  select(-calorie_intake_5_9)

print(CAL_5_9_bothsexes_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 8
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_5_9_bothsexes_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 8 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_male_2018_FIN)

######################## female 5-9

# Merge and update calorie_intake in GDD_female_2018_FIN where age is 8
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_5_9_bothsexes_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 8 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

# print (n=,) - n= no. of rows
print(n=7040,GDD_female_2018_FIN)


##################################################################################################################

# C. age 10-14_male - Calorie intake
# Read data from CSV file
CAL_10_14_male <- read.csv("NutrientTotal_withFortification_age10-14_male.csv")
colnames(CAL_10_14_male)
str(CAL_10_14_male)

# Select only the necessary columns from the CAL_10_14_male
CAL_10_14_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_10_14_male <- CAL_10_14_male[, CAL_10_14_male_columns_to_keep]

# Rename the column 
CAL_10_14_male <- CAL_10_14_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_10_14_male)
str(CAL_10_14_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_10_14_male <- CAL_10_14_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_10_14_male)

# Delete rows 1-3
CAL_10_14_male <- CAL_10_14_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_10_14_male <- CAL_10_14_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_10_14_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_10_14_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_10_14_male <- na_countries_10_14_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_10_14_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_10_14_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_10_14_male
codes_not_in_10_14_male <- setdiff(codes_0_4, codes_10_14_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_10_14_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_10_14_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_10_14_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_10_14_male:\n")
  print(codes_not_in_10_14_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_10_14_male.\n")
}

############

print(CAL_10_14_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_10_14_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_10_14_male = c(2626.858, 2486.073, 2749.256, 2837.567, 3989.494, 4005.929, 3202.600, 
                                2505.585, 2563.268, 2917.918, 2728.307, 2780.225, NA, 2591.131, 
                                3182.205, 3388.422, 2741.655, 3226.001, 2437.430, 2734.633, 2446.113, 
                                3201.089, 2217.275, 2488.956, 2998.077, 2070.296, 2842.852, 2878.988, 
                                3312.117, 3712.439, 4001.740, 0, 3184.269, 2841.943, 2690.913, 2184.137, 
                                3232.838)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_10_14_male <- calorie_data_10_14_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_10_14_male = mean(calorie_intake_10_14_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_10_14_male_NA df
CAL_10_14_male_NA <- data %>%
  left_join(mean_calories_10_14_male, by = "Area")

# View the result
print(CAL_10_14_male_NA)

# Merge the df to include calorie_intake from CAL_10_14_male_NA
CAL_10_14_male_FIN <- CAL_10_14_male %>%
  left_join(CAL_10_14_male_NA %>% select(Countries_code, calorie_intake_10_14_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_10_14_male_FIN <- CAL_10_14_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_10_14_male)) %>%
  select(-calorie_intake_10_14_male)

print(CAL_10_14_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 12
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_10_14_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 12 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_male_2018_FIN)

######################## female 10-14

# C. age 10-14_female - Calorie intake
# Read data from CSV file
CAL_10_14_female <- read.csv("NutrientTotal_withFortification_age10-14_female.csv")
colnames(CAL_10_14_female)
str(CAL_10_14_female)

# Select only the necessary columns from the CAL_10_14_female
CAL_10_14_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_10_14_female <- CAL_10_14_female[, CAL_10_14_female_columns_to_keep]

# Rename the column 
CAL_10_14_female <- CAL_10_14_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_10_14_female)
str(CAL_10_14_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_10_14_female <- CAL_10_14_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_10_14_female)

# Delete rows 1-3
CAL_10_14_female <- CAL_10_14_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_10_14_female <- CAL_10_14_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_10_14_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_10_14_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_10_14_female <- na_countries_10_14_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_10_14_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_10_14_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_10_14_male
codes_not_in_10_14_female <- setdiff(codes_0_4, codes_10_14_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_10_14_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_10_14_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_10_14_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_10_14_female:\n")
  print(codes_not_in_10_14_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_10_14_female.\n")
}

############

print(CAL_10_14_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_10_14_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_10_14_female = c(2364.173, 2237.466, 2474.331, 2553.810, 3590.545, 3605.336, 2882.340, 
                                  2255.026, 2306.941, 2626.126, 2455.477, 2502.202, NA, 2332.018, 
                                  2863.985, 3049.580, 2467.490, 2903.401, 2193.688, 2461.169, 2201.501, 
                                  2880.980, 1995.548, 2240.061, 2698.270, 1863.266, 2558.567, 2591.090, 
                                  2980.905, 3341.195, 3601.566, 0, 2865.843, 2557.748, 2421.822, 1965.723, 
                                  2909.554)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_10_14_female <- calorie_data_10_14_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_10_14_female = mean(calorie_intake_10_14_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_10_14_female_NA df
CAL_10_14_female_NA <- data %>%
  left_join(mean_calories_10_14_female, by = "Area")

# View the result
print(CAL_10_14_female_NA)

# Merge the df to include calorie_intake from CAL_10_14_male_NA
CAL_10_14_female_FIN <- CAL_10_14_female %>%
  left_join(CAL_10_14_female_NA %>% select(Countries_code, calorie_intake_10_14_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_10_14_female_FIN <- CAL_10_14_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_10_14_female)) %>%
  select(-calorie_intake_10_14_female)

print(CAL_10_14_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 12
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_10_14_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 12 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_female_2018_FIN)


##################################################################################################################

# D. age 15-19_male - Calorie intake
# Read data from CSV file
CAL_15_19_male <- read.csv("NutrientTotal_withFortification_age15-19_male.csv")
colnames(CAL_15_19_male)
str(CAL_15_19_male)

# Select only the necessary columns from the CAL_15_19_male
CAL_15_19_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_15_19_male <- CAL_15_19_male[, CAL_15_19_male_columns_to_keep]

# Rename the column 
CAL_15_19_male <- CAL_15_19_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_15_19_male)
str(CAL_15_19_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_15_19_male <- CAL_15_19_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_15_19_male)

# Delete rows 1-3
CAL_15_19_male <- CAL_15_19_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_15_19_male <- CAL_15_19_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_15_19_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_15_19_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_15_19_male <- na_countries_15_19_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_10_14_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_15_19_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_10_14_male
codes_not_in_15_19_male <- setdiff(codes_0_4, codes_15_19_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_15_19_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_15_19_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_15_19_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_15_19_male:\n")
  print(codes_not_in_15_19_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_15_19_male.\n")
}

############

print(CAL_15_19_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_15_19_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_15_19_male = c(3261.683, 3086.874, 3413.660, 3523.312, 4953.622, 4974.028, 3976.561, 
                                3111.101, 3182.724, 3623.082, 3387.648, 3452.113, NA, 3217.321, 
                                3951.238, 4207.291, 3404.222, 4005.618, 3026.476, 3395.502, 3037.256, 
                                3974.686, 2753.117, 3090.454, 3722.613, 2570.617, 3529.874, 3574.744, 
                                4112.545, 4609.612, 4968.827, 0, 3953.801, 3528.745, 3341.217, 2711.970, 
                                4014.107)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_15_19_male <- calorie_data_15_19_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_15_19_male = mean(calorie_intake_15_19_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_15_19_male_NA df
CAL_15_19_male_NA <- data %>%
  left_join(mean_calories_15_19_male, by = "Area")

# View the result
print(CAL_15_19_male_NA)

# Merge the df to include calorie_intake from CAL_15_19_male_NA
CAL_15_19_male_FIN <- CAL_15_19_male %>%
  left_join(CAL_15_19_male_NA %>% select(Countries_code, calorie_intake_15_19_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_15_19_male_FIN <- CAL_15_19_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_15_19_male)) %>%
  select(-calorie_intake_15_19_male)

print(CAL_15_19_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 18
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_15_19_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 18 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_male_2018_FIN)

######################## female 15_19

# D. age 15_19_female - Calorie intake
# Read data from CSV file
CAL_15_19_female <- read.csv("NutrientTotal_withFortification_age15-19_female.csv")
colnames(CAL_15_19_female)
str(CAL_15_19_female)

# Select only the necessary columns from the CAL_15_19_female
CAL_15_19_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_15_19_female <- CAL_15_19_female[, CAL_15_19_female_columns_to_keep]

# Rename the column 
CAL_15_19_female <- CAL_15_19_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_15_19_female)
str(CAL_15_19_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_15_19_female <- CAL_15_19_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_15_19_female)

# Delete rows 1-3
CAL_15_19_female <- CAL_15_19_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_15_19_female <- CAL_15_19_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_15_19_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_15_19_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_15_19_female <- na_countries_15_19_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_15_19_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_15_19_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_15_19_male
codes_not_in_15_19_female <- setdiff(codes_0_4, codes_15_19_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_15_19_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_15_19_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_15_19_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_15_19_female:\n")
  print(codes_not_in_15_19_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_15_19_female.\n")
}

############

print(CAL_15_19_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_15_19_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_15_19_female = c(2407.954, 2278.901, 2520.152, 2601.103, 3657.037, 3672.101, 2935.716, 
                                  2296.786, 2349.662, 2674.758, 2500.949, 2548.540, NA, 2375.204, 
                                  2917.022, 3106.054, 2513.184, 2957.168, 2234.311, 2506.747, 2242.270, 
                                  2934.332, 2032.503, 2281.543, 2748.238, 1897.771, 2605.948, 2639.073, 
                                  3036.107, 3403.069, 3668.261, 0, 2918.914, 2605.114, 2466.670, 2002.125, 
                                  2963.435)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_15_19_female <- calorie_data_15_19_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_15_19_female = mean(calorie_intake_15_19_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_15_19_female_NA df
CAL_15_19_female_NA <- data %>%
  left_join(mean_calories_15_19_female, by = "Area")

# View the result
print(CAL_15_19_female_NA)

# Merge the df to include calorie_intake from CAL_15_19_male_NA
CAL_15_19_female_FIN <- CAL_15_19_female %>%
  left_join(CAL_15_19_female_NA %>% select(Countries_code, calorie_intake_15_19_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_15_19_female_FIN <- CAL_15_19_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_15_19_female)) %>%
  select(-calorie_intake_15_19_female)

print(CAL_15_19_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 18
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_15_19_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 18 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_female_2018_FIN)



##################################################################################################################

# E. age 20-24_male - Calorie intake
# Read data from CSV file
CAL_20_24_male <- read.csv("NutrientTotal_withFortification_age20-24_male.csv")
colnames(CAL_20_24_male)
str(CAL_20_24_male)

# Select only the necessary columns from the CAL_20_24_male
CAL_20_24_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_20_24_male <- CAL_20_24_male[, CAL_20_24_male_columns_to_keep]

# Rename the column 
CAL_20_24_male <- CAL_20_24_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_20_24_male)
str(CAL_20_24_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_20_24_male <- CAL_20_24_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_20_24_male)

# Delete rows 1-3
CAL_20_24_male <- CAL_20_24_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_20_24_male <- CAL_20_24_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_20_24_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_20_24_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_20_24_male <- na_countries_20_24_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_20_24_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_20_24_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_20_24_male
codes_not_in_20_24_male <- setdiff(codes_0_4, codes_20_24_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_20_24_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_20_24_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_20_24_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_20_24_male:\n")
  print(codes_not_in_20_24_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_20_24_male.\n")
}

############

print(CAL_20_24_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_20_24_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_20_24_male = c(2798.8428, 2583.5709, 2990.8660, 2963.4564, 4492.4388, 4562.1362, 3483.1048, 
                                2854.1846, 2793.8205, 3133.3224, 2949.0138, 3034.4600, NA, 2760.6019, 
                                3489.3970, 3863.3625, 3083.8036, 3409.8479, 2687.3969, 2955.6981, 2606.9436, 
                                3448.2697, 2481.6455, 2887.2154, 3370.6842, 2332.4145, 3253.7038, 3196.8380, 
                                3675.5822, 4216.5029, 4530.3350, 0, 3575.8449, 3098.7582, 2897.8686, 2353.7937, 
                                3384.9876)
)



# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_20_24_male <- calorie_data_20_24_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_20_24_male = mean(calorie_intake_20_24_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_20_24_male_NA df
CAL_20_24_male_NA <- data %>%
  left_join(mean_calories_20_24_male, by = "Area")

# View the result
print(CAL_20_24_male_NA)

# Merge the df to include calorie_intake from CAL_20_24_male_NA
CAL_20_24_male_FIN <- CAL_20_24_male %>%
  left_join(CAL_20_24_male_NA %>% select(Countries_code, calorie_intake_20_24_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_20_24_male_FIN <- CAL_20_24_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_20_24_male)) %>%
  select(-calorie_intake_20_24_male)

print(CAL_20_24_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 22
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_20_24_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 22 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_male_2018_FIN)

######################## female 20_24

# E. age 20_24_female - Calorie intake
# Read data from CSV file
CAL_20_24_female <- read.csv("NutrientTotal_withFortification_age20-24_female.csv")
colnames(CAL_20_24_female)
str(CAL_20_24_female)

# Select only the necessary columns from the CAL_20_24_female
CAL_20_24_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_20_24_female <- CAL_20_24_female[, CAL_20_24_female_columns_to_keep]

# Rename the column 
CAL_20_24_female <- CAL_20_24_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_20_24_female)
str(CAL_20_24_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_20_24_female <- CAL_20_24_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_20_24_female)

# Delete rows 1-3
CAL_20_24_female <- CAL_20_24_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_20_24_female <- CAL_20_24_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_20_24_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_20_24_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_20_24_female <- na_countries_20_24_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_20_24_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_20_24_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_20_24_male
codes_not_in_20_24_female <- setdiff(codes_0_4, codes_20_24_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_20_24_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_20_24_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_20_24_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_20_24_female:\n")
  print(codes_not_in_20_24_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_20_24_female.\n")
}

############

print(CAL_20_24_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_20_24_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_20_24_female = c(2249.9649, 2073.1350, 2364.6961, 2358.8944, 3452.1140, 3472.5896, 2754.5315, 
                                  2229.7722, 2181.4725, 2417.8832, 2305.8563, 2354.0223, NA, 2186.5405, 
                                  2733.6871, 2966.4883, 2369.9155, 2688.0653, 2129.2313, 2271.5692, 2002.2882, 
                                  2678.9315, 1954.8505, 2160.3506, 2557.4341, 1788.9592, 2494.3204, 2465.3535, 
                                  2846.2552, 3282.6071, 3560.2647, 0, 2786.0389, 2477.0392, 2311.9017, 1876.7658, 
                                  2677.7594)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_20_24_female <- calorie_data_20_24_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_20_24_female = mean(calorie_intake_20_24_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_20_24_female_NA df
CAL_20_24_female_NA <- data %>%
  left_join(mean_calories_20_24_female, by = "Area")

# View the result
print(CAL_20_24_female_NA)

# Merge the df to include calorie_intake from CAL_15_19_male_NA
CAL_20_24_female_FIN <- CAL_20_24_female %>%
  left_join(CAL_20_24_female_NA %>% select(Countries_code, calorie_intake_20_24_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_20_24_female_FIN <- CAL_20_24_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_20_24_female)) %>%
  select(-calorie_intake_20_24_female)

print(CAL_20_24_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 22
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_20_24_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 22 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_female_2018_FIN)



##################################################################################################################

# F. age 25-29_male - Calorie intake
# Read data from CSV file
CAL_25_29_male <- read.csv("NutrientTotal_withFortification_age25-29_male.csv")
colnames(CAL_25_29_male)
str(CAL_25_29_male)

# Select only the necessary columns from the CAL_25_29_male
CAL_25_29_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_25_29_male <- CAL_25_29_male[, CAL_25_29_male_columns_to_keep]

# Rename the column 
CAL_25_29_male <- CAL_25_29_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_25_29_male)
str(CAL_25_29_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_25_29_male <- CAL_25_29_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_25_29_male)

# Delete rows 1-3
CAL_25_29_male <- CAL_25_29_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_25_29_male <- CAL_25_29_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_25_29_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_25_29_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_25_29_male <- na_countries_25_29_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_25_29_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_25_29_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_25_29_male
codes_not_in_25_29_male <- setdiff(codes_0_4, codes_25_29_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_25_29_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_25_29_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_25_29_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_25_29_male:\n")
  print(codes_not_in_25_29_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_25_29_male.\n")
}

############

print(CAL_25_29_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_25_29_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_25_29_male = c(2847.7593, 2623.9125, 3038.6206, 3022.5255, 4532.2637, 4607.1251, 3555.2699, 
                                2883.1732, 2815.2593, 3211.2676, 2984.8566, 3080.9835, NA, 2858.4749, 
                                3588.2873, 3906.3419, 3121.2014, 3506.0212, 2736.9905, 3013.4457, 2641.6256, 
                                3481.3304, 2534.8165, 2899.1152, 3401.4497, 2344.5006, 3275.0126, 3234.3687, 
                                3752.0790, 4242.2349, 4546.7013, 0, 3586.9640, 3131.0765, 2948.7435, 2389.6838, 
                                3422.7116)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_25_29_male <- calorie_data_25_29_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_25_29_male = mean(calorie_intake_25_29_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_25_29_male_NA df
CAL_25_29_male_NA <- data %>%
  left_join(mean_calories_25_29_male, by = "Area")

# View the result
print(CAL_25_29_male_NA)

# Merge the df to include calorie_intake from CAL_25_29_male_NA
CAL_25_29_male_FIN <- CAL_25_29_male %>%
  left_join(CAL_25_29_male_NA %>% select(Countries_code, calorie_intake_25_29_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_25_29_male_FIN <- CAL_25_29_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_25_29_male)) %>%
  select(-calorie_intake_25_29_male)

print(CAL_25_29_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 28
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_25_29_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 28 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_male_2018_FIN)

######################## female 25_29

# F. age 25_29_female - Calorie intake
# Read data from CSV file
CAL_25_29_female <- read.csv("NutrientTotal_withFortification_age25-29_female.csv")
colnames(CAL_25_29_female)
str(CAL_25_29_female)

# Select only the necessary columns from the CAL_25_29_female
CAL_25_29_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_25_29_female <- CAL_25_29_female[, CAL_25_29_female_columns_to_keep]

# Rename the column 
CAL_25_29_female <- CAL_25_29_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_25_29_female)
str(CAL_25_29_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_25_29_female <- CAL_25_29_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_25_29_female)

# Delete rows 1-3
CAL_25_29_female <- CAL_25_29_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_25_29_female <- CAL_25_29_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_25_29_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_25_29_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_25_29_female <- na_countries_25_29_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_25_29_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_25_29_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_25_29_male
codes_not_in_25_29_female <- setdiff(codes_0_4, codes_25_29_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_25_29_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_25_29_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_25_29_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_25_29_female:\n")
  print(codes_not_in_25_29_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_25_29_female.\n")
}

############

print(CAL_25_29_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_25_29_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_25_29_female = c(2260.9537, 2101.2220, 2430.6632, 2384.2218, 3478.9952, 3501.3558, 2771.6616, 
                                  2249.6818, 2203.2681, 2480.4225, 2350.1241, 2390.6248, NA, 2191.6986, 
                                  2786.7863, 3021.2317, 2401.7032, 2727.4790, 2156.2898, 2333.3645, 2044.4831, 
                                  2731.9994, 1992.8188, 2190.6028, 2588.9698, 1811.0406, 2500.0514, 2484.7356, 
                                  2869.0561, 3281.9736, 3575.8771, 0, 2775.5207, 2489.0863, 2344.3550, 1887.5484, 
                                  2774.6628)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_25_29_female <- calorie_data_25_29_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_25_29_female = mean(calorie_intake_25_29_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_25_29_female_NA df
CAL_25_29_female_NA <- data %>%
  left_join(mean_calories_25_29_female, by = "Area")

# View the result
print(CAL_25_29_female_NA)

# Merge the df to include calorie_intake from CAL_25_29_male_NA
CAL_25_29_female_FIN <- CAL_25_29_female %>%
  left_join(CAL_25_29_female_NA %>% select(Countries_code, calorie_intake_25_29_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_25_29_female_FIN <- CAL_25_29_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_25_29_female)) %>%
  select(-calorie_intake_25_29_female)

print(CAL_25_29_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 28
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_25_29_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 28 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_female_2018_FIN)



##################################################################################################################

# G. age 30-34_male - Calorie intake
# Read data from CSV file
CAL_30_34_male <- read.csv("NutrientTotal_withFortification_age30-34_male.csv")
colnames(CAL_30_34_male)
str(CAL_30_34_male)

# Select only the necessary columns from the CAL_30_34_male
CAL_30_34_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_30_34_male <- CAL_30_34_male[, CAL_30_34_male_columns_to_keep]

# Rename the column 
CAL_30_34_male <- CAL_30_34_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_30_34_male)
str(CAL_30_34_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_30_34_male <- CAL_30_34_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_30_34_male)

# Delete rows 1-3
CAL_30_34_male <- CAL_30_34_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_30_34_male <- CAL_30_34_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_30_34_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_30_34_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_30_34_male <- na_countries_30_34_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_30-34_bothsexes but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_30_34_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_30_34_male
codes_not_in_30_34_male <- setdiff(codes_0_4, codes_30_34_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_30_34_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_30_34_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_30_34_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_30_34_male:\n")
  print(codes_not_in_30_34_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_30_34_male.\n")
}

############

print(CAL_30_34_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_30_34_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_30_34_male = c(2883.7268, 2706.3659, 3078.8785, 3115.6766, 4571.0734, 4648.1368, 3607.2144, 
                                2903.2897, 2870.9435, 3271.8131, 3032.6790, 3171.4830, NA, 2912.1733, 
                                3667.3589, 3951.4703, 3172.8076, 3615.4118, 2746.0170, 3098.5583, 2660.4963, 
                                3550.9077, 2559.5394, 2920.4239, 3445.6927, 2382.7891, 3317.6202, 3266.8589, 
                                3818.4907, 4263.1184, 4536.1977, 0, 3617.8172, 3192.3867, 3002.0398, 2432.2346, 
                                3528.7188)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_30_34_male <- calorie_data_30_34_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_30_34_male = mean(calorie_intake_30_34_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_30_34_male_NA df
CAL_30_34_male_NA <- data %>%
  left_join(mean_calories_30_34_male, by = "Area")

# View the result
print(CAL_30_34_male_NA)

# Merge the df to include calorie_intake from CAL_30-34_male_NA
CAL_30_34_male_FIN <- CAL_30_34_male %>%
  left_join(CAL_30_34_male_NA %>% select(Countries_code, calorie_intake_30_34_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_30_34_male_FIN <- CAL_30_34_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_30_34_male)) %>%
  select(-calorie_intake_30_34_male)

print(CAL_30_34_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 32
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_30_34_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 32 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_male_2018_FIN)

######################## female 30-34

# G. age 30_34_female - Calorie intake
# Read data from CSV file
CAL_30_34_female <- read.csv("NutrientTotal_withFortification_age30-34_female.csv")
colnames(CAL_30_34_female)
str(CAL_30_34_female)

# Select only the necessary columns from the CAL_30-34_female
CAL_30_34_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_30_34_female <- CAL_30_34_female[, CAL_30_34_female_columns_to_keep]

# Rename the column 
CAL_30_34_female <- CAL_30_34_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_30_34_female)
str(CAL_30_34_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_30_34_female <- CAL_30_34_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_30_34_female)

# Delete rows 1-3
CAL_30_34_female <- CAL_30_34_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_30_34_female <- CAL_30_34_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_30_34_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_30_34_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_30_34_female <- na_countries_30_34_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_30_34_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_30_34_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_30_34_female
codes_not_in_30_34_female <- setdiff(codes_0_4, codes_30_34_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_30_34_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_30_34_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_30_34_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_30_34_female:\n")
  print(codes_not_in_30_34_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_30_34_female.\n")
}

############

print(CAL_30_34_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_30_34_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_30_34_female = c(2303.6844, 2149.0566, 2434.4129, 2438.1855, 3510.9202, 3526.7289, 2798.7453, 
                                  2248.8207, 2228.0799, 2516.1371, 2369.4833, 2438.8590, NA, 2237.7526, 
                                  2845.8741, 3061.5286, 2453.8631, 2813.5373, 2180.0884, 2382.1595, 2094.9484, 
                                  2793.7197, 2013.1907, 2224.8339, 2630.3406, 1843.9367, 2540.6446, 2494.6181, 
                                  2922.3022, 3307.8030, 3570.7273, 0, 2830.4066, 2543.3888, 2395.3946, 1964.3515, 
                                  2799.1067)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_30_34_female <- calorie_data_30_34_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_30_34_female = mean(calorie_intake_30_34_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_30_34_female_NA df
CAL_30_34_female_NA <- data %>%
  left_join(mean_calories_30_34_female, by = "Area")

# View the result
print(CAL_30_34_female_NA)

# Merge the df to include calorie_intake from CAL_30_34_female_NA
CAL_30_34_female_FIN <- CAL_30_34_female %>%
  left_join(CAL_30_34_female_NA %>% select(Countries_code, calorie_intake_30_34_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_30_34_female_FIN <- CAL_30_34_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_30_34_female)) %>%
  select(-calorie_intake_30_34_female)

print(CAL_30_34_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 32
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_30_34_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 32 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(GDD_female_2018_FIN)



##################################################################################################################

# H. age 35-39_male - Calorie intake
# Read data from CSV file
CAL_35_39_male <- read.csv("NutrientTotal_withFortification_age35-39_male.csv")
colnames(CAL_35_39_male)
str(CAL_35_39_male)

# Select only the necessary columns from the CAL_35_39_male
CAL_35_39_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_35_39_male <- CAL_35_39_male[, CAL_35_39_male_columns_to_keep]

# Rename the column 
CAL_35_39_male <- CAL_35_39_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_35_39_male)
str(CAL_35_39_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_35_39_male <- CAL_35_39_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_35_39_male)

# Delete rows 1-3
CAL_35_39_male <- CAL_35_39_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_35_39_male <- CAL_35_39_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_35_39_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_35_39_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_35_39_male <- na_countries_35_39_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_35_39_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_35_39_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_35_39_male
codes_not_in_35_39_male <- setdiff(codes_0_4, codes_35_39_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_35_39_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_35_39_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_35_39_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_35_39_male:\n")
  print(codes_not_in_35_39_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_35_39_male.\n")
}

############

print(CAL_35_39_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_35_39_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_35_39_male = c(2931.8179, 2702.7130, 3090.5235, 3119.4919, 4615.3877, 4710.5972, 3655.5275, 
                                2925.4664, 2922.0439, 3372.1583, 3055.2914, 3227.3061, NA, 2965.1923, 
                                3735.0517, 4003.8536, 3238.5560, 3721.3723, 2801.2869, 3157.0222, 2698.2993, 
                                3597.2941, 2586.5484, 2934.4528, 3476.0626, 2411.4088, 3358.0248, 3305.0326, 
                                3836.6225, 4289.5914, 4543.7462, 0, 3653.9993, 3206.7622, 3005.3940, 2472.3965, 
                                3585.1168)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_35_39_male <- calorie_data_35_39_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_35_39_male = mean(calorie_intake_35_39_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_35_39_male_NA df
CAL_35_39_male_NA <- data %>%
  left_join(mean_calories_35_39_male, by = "Area")

# View the result
print(CAL_35_39_male_NA)

# Merge the df to include calorie_intake from CAL_35_39_male_NA
CAL_35_39_male_FIN <- CAL_35_39_male %>%
  left_join(CAL_35_39_male_NA %>% select(Countries_code, calorie_intake_35_39_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_35_39_male_FIN <- CAL_35_39_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_35_39_male)) %>%
  select(-calorie_intake_35_39_male)

print(CAL_35_39_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 38
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_35_39_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 38 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=11, GDD_male_2018_FIN)

######################## female 35-39

# H. age 35_39_female - Calorie intake
# Read data from CSV file
CAL_35_39_female <- read.csv("NutrientTotal_withFortification_age35-39_female.csv")
colnames(CAL_35_39_female)
str(CAL_35_39_female)

# Select only the necessary columns from the CAL_35_39_female
CAL_35_39_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_35_39_female <- CAL_35_39_female[, CAL_35_39_female_columns_to_keep]

# Rename the column 
CAL_35_39_female <- CAL_35_39_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_35_39_female)
str(CAL_35_39_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_35_39_female <- CAL_35_39_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_35_39_female)

# Delete rows 1-3
CAL_35_39_female <- CAL_35_39_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_35_39_female <- CAL_35_39_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_35_39_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_35_39_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_35_39_female <- na_countries_35_39_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_35_39_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_35_39_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_35_39_female
codes_not_in_35_39_female <- setdiff(codes_0_4, codes_35_39_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_35_39_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_35_39_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_35_39_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_35_39_female:\n")
  print(codes_not_in_35_39_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_35_39_female.\n")
}

############

print(CAL_35_39_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_35_39_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_35_39_female = c(2340.9520, 2230.9474, 2490.2107, 2486.1143, 3535.0539, 3563.9844, 2837.0549, 
                                  2264.6483, 2289.4351, 2577.4003, 2410.4650, 2480.4492, NA, 2302.1011, 
                                  2914.4889, 3113.6720, 2488.4227, 2898.0375, 2216.8762, 2434.4600, 2140.2185, 
                                  2870.7067, 2042.5937, 2232.8082, 2666.2254, 1884.0601, 2570.8338, 2530.0469, 
                                  2960.6736, 3323.7520, 3568.1091, 0, 2850.9035, 2560.3792, 2427.3996, 2001.3298, 
                                  2901.1432)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_35_39_female <- calorie_data_35_39_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_35_39_female = mean(calorie_intake_35_39_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_35_39_female_NA df
CAL_35_39_female_NA <- data %>%
  left_join(mean_calories_35_39_female, by = "Area")

# View the result
print(CAL_35_39_female_NA)

# Merge the df to include calorie_intake from CAL_35_39_female_NA
CAL_35_39_female_FIN <- CAL_35_39_female %>%
  left_join(CAL_35_39_female_NA %>% select(Countries_code, calorie_intake_35_39_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_35_39_female_FIN <- CAL_35_39_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_35_39_female)) %>%
  select(-calorie_intake_35_39_female)

print(CAL_35_39_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 38
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_35_39_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 38 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=12, GDD_female_2018_FIN)




##################################################################################################################

# I. age 40-44_male - Calorie intake
# Read data from CSV file
CAL_40_44_male <- read.csv("NutrientTotal_withFortification_age40-44_male.csv")
colnames(CAL_40_44_male)
str(CAL_40_44_male)

# Select only the necessary columns from the CAL_40_44_male
CAL_40_44_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_40_44_male <- CAL_40_44_male[, CAL_40_44_male_columns_to_keep]

# Rename the column 
CAL_40_44_male <- CAL_40_44_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_40_44_male)
str(CAL_40_44_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_40_44_male <- CAL_40_44_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_40_44_male)

# Delete rows 1-3
CAL_40_44_male <- CAL_40_44_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_40_44_male <- CAL_40_44_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_40_44_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_40_44_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_40_44_male <- na_countries_40_44_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_40_44_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_40_44_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_40_44_male
codes_not_in_40_44_male <- setdiff(codes_0_4, codes_40_44_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_40_44_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_40_44_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_40_44_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_40_44_male:\n")
  print(codes_not_in_40_44_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_40_44_male.\n")
}

############

print(CAL_40_44_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_40_44_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_40_44_male = c(2978.6482, 2818.7054, 3116.3845, 3253.3954, 4656.8296, 4751.0068, 3700.9500, 
                                2908.7858, 2954.9609, 3405.6070, 3131.0787, 3272.1060, NA, 3017.7212, 
                                3798.5731, 4079.5050, 3299.0787, 3812.7333, 2821.5146, 3225.0740, 2738.8895, 
                                3646.2947, 2626.0762, 3002.2294, 3522.4475, 2451.8580, 3390.3017, 3360.1587, 
                                3892.4320, 4313.9443, 4542.0813, 0, 3701.5346, 3259.8460, 3090.9088, 2522.2854, 
                                3674.7018)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_40_44_male <- calorie_data_40_44_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_40_44_male = mean(calorie_intake_40_44_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_40_44_male_NA df
CAL_40_44_male_NA <- data %>%
  left_join(mean_calories_40_44_male, by = "Area")

# View the result
print(CAL_40_44_male_NA)

# Merge the df to include calorie_intake from CAL_40_44_male_NA
CAL_40_44_male_FIN <- CAL_40_44_male %>%
  left_join(CAL_40_44_male_NA %>% select(Countries_code, calorie_intake_40_44_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_40_44_male_FIN <- CAL_40_44_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_40_44_male)) %>%
  select(-calorie_intake_40_44_male)

print(CAL_40_44_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 42
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_40_44_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 42 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=13, GDD_male_2018_FIN)

######################## female 40-44

# I. age 40_44_female - Calorie intake
# Read data from CSV file
CAL_40_44_female <- read.csv("NutrientTotal_withFortification_age40-44_female.csv")
colnames(CAL_40_44_female)
str(CAL_40_44_female)

# Select only the necessary columns from the CAL_40_44_female
CAL_40_44_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_40_44_female <- CAL_40_44_female[, CAL_40_44_female_columns_to_keep]

# Rename the column 
CAL_40_44_female <- CAL_40_44_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_40_44_female)
str(CAL_40_44_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_40_44_female <- CAL_40_44_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_40_44_female)

# Delete rows 1-3
CAL_40_44_female <- CAL_40_44_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_40_44_female <- CAL_40_44_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_40_44_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_40_44_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_40_44_female <- na_countries_40_44_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_40_44_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_40_44_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_35_39_female
codes_not_in_40_44_female <- setdiff(codes_0_4, codes_40_44_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_40_44_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_40_44_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_40_44_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_40_44_female:\n")
  print(codes_not_in_40_44_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_40_44_female.\n")
}

############

print(CAL_40_44_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_40_44_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_40_44_female = c(2400.1093, 2253.3528, 2518.7892, 2543.1376, 3564.0240, 3588.6218, 2887.5257, 
                                  2292.0381, 2304.3804, 2606.2644, 2428.8596, 2513.7957, NA, 2355.7638, 
                                  2951.8367, 3137.6007, 2541.5621, 2941.5629, 2237.2658, 2488.1260, 2175.2266, 
                                  2943.3070, 2072.5637, 2253.9448, 2695.3466, 1899.9541, 2587.0031, 2567.7169, 
                                  3021.1431, 3337.7857, 3574.3130, 0, 2898.6222, 2625.8200, 2465.6576, 2022.0618, 
                                  2937.1710)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_40_44_female <- calorie_data_40_44_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_40_44_female = mean(calorie_intake_40_44_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_40_44_female_NA df
CAL_40_44_female_NA <- data %>%
  left_join(mean_calories_40_44_female, by = "Area")

# View the result
print(CAL_40_44_female_NA)

# Merge the df to include calorie_intake from CAL_40_44_female_NA
CAL_40_44_female_FIN <- CAL_40_44_female %>%
  left_join(CAL_40_44_female_NA %>% select(Countries_code, calorie_intake_40_44_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_40_44_female_FIN <- CAL_40_44_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_40_44_female)) %>%
  select(-calorie_intake_40_44_female)

print(CAL_40_44_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 42
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_40_44_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 42 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=12, GDD_female_2018_FIN)





##################################################################################################################

# J. age 45-49_male - Calorie intake
# Read data from CSV file
CAL_45_49_male <- read.csv("NutrientTotal_withFortification_age45-49_male.csv")
colnames(CAL_45_49_male)
str(CAL_45_49_male)

# Select only the necessary columns from the CAL_45_49_male
CAL_45_49_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_45_49_male <- CAL_45_49_male[, CAL_45_49_male_columns_to_keep]

# Rename the column 
CAL_45_49_male <- CAL_45_49_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_45_49_male)
str(CAL_45_49_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_45_49_male <- CAL_45_49_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_45_49_male)

# Delete rows 1-3
CAL_45_49_male <- CAL_45_49_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_45_49_male <- CAL_45_49_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_45_49_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_45_49_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_45_49_male <- na_countries_45_49_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_45_49_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_45_49_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_45_49_male
codes_not_in_45_49_male <- setdiff(codes_0_4, codes_45_49_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_45_49_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_45_49_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_40_44_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_45_49_male:\n")
  print(codes_not_in_45_49_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_45_49_male.\n")
}

############

print(CAL_45_49_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_45_49_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_45_49_male = c(3082.0013, 2882.7645, 3228.5045, 3328.7985, 4719.3288, 4798.0866, 3807.1820, 
                                2964.0658, 3012.1540, 3478.2195, 3195.8093, 3361.1396, NA, 3133.6537, 
                                3870.7614, 4105.9507, 3353.1440, 3939.6729, 2868.3608, 3307.8277, 2797.1572, 
                                3724.6323, 2714.7943, 2977.4680, 3573.3028, 2502.8676, 3426.0800, 3420.8137, 
                                3973.5130, 4360.8683, 4599.2777, 0, 3765.3256, 3360.9667, 3173.4967, 2597.9596, 
                                3738.7646)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_45_49_male <- calorie_data_45_49_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_45_49_male = mean(calorie_intake_45_49_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_45_49_male_NA df
CAL_45_49_male_NA <- data %>%
  left_join(mean_calories_45_49_male, by = "Area")

# View the result
print(CAL_45_49_male_NA)

# Merge the df to include calorie_intake from CAL_45_49_male_NA
CAL_45_49_male_FIN <- CAL_45_49_male %>%
  left_join(CAL_45_49_male_NA %>% select(Countries_code, calorie_intake_45_49_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_45_49_male_FIN <- CAL_45_49_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_45_49_male)) %>%
  select(-calorie_intake_45_49_male)

print(CAL_45_49_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 48
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_45_49_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 48 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=13, GDD_male_2018_FIN)

######################## female 45_49

# I. age 45_49_female - Calorie intake
# Read data from CSV file
CAL_45_49_female <- read.csv("NutrientTotal_withFortification_age45-49_female.csv")
colnames(CAL_45_49_female)
str(CAL_45_49_female)

# Select only the necessary columns from the CAL_45_49_female
CAL_45_49_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_45_49_female <- CAL_45_49_female[, CAL_45_49_female_columns_to_keep]

# Rename the column 
CAL_45_49_female <- CAL_45_49_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_45_49_female)
str(CAL_45_49_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_45_49_female <- CAL_45_49_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_45_49_female)

# Delete rows 1-3
CAL_45_49_female <- CAL_45_49_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_45_49_female <- CAL_45_49_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_45_49_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_45_49_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_45_49_female <- na_countries_45_49_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_45_49_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_45_49_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_45_49_female
codes_not_in_45_49_female <- setdiff(codes_0_4, codes_45_49_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_45_49_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_45_49_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_45_49_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_45_49_female:\n")
  print(codes_not_in_45_49_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_45_49_female.\n")
}

############

print(CAL_45_49_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_45_49_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_45_49_female = c(2475.7224, 2354.3890, 2563.0003, 2648.0708, 3611.3277, 3635.7885, 2940.9636, 
                                  2307.8790, 2367.8793, 2710.1503, 2480.1903, 2611.7814, NA, 2448.1588, 
                                  3004.9450, 3175.0292, 2590.6682, 3074.0093, 2287.3971, 2562.3798, 2231.7822, 
                                  3019.4540, 2125.3515, 2282.8966, 2754.2049, 1951.3193, 2631.8557, 2601.2499, 
                                  3069.7172, 3385.7318, 3624.6418, 0, 2944.1443, 2678.7112, 2549.8235, 2080.6974, 
                                  3036.8701)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_45_49_female <- calorie_data_45_49_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_45_49_female = mean(calorie_intake_45_49_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_45_49_female_NA df
CAL_45_49_female_NA <- data %>%
  left_join(mean_calories_45_49_female, by = "Area")

# View the result
print(CAL_45_49_female_NA)

# Merge the df to include calorie_intake from CAL_45_49_female_NA
CAL_45_49_female_FIN <- CAL_45_49_female %>%
  left_join(CAL_45_49_female_NA %>% select(Countries_code, calorie_intake_45_49_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_45_49_female_FIN <- CAL_45_49_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_45_49_female)) %>%
  select(-calorie_intake_45_49_female)

print(CAL_45_49_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 48
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_45_49_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 48 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=14, GDD_female_2018_FIN)




##################################################################################################################

# K. age 50-54_male - Calorie intake
# Read data from CSV file
CAL_50_54_male <- read.csv("NutrientTotal_withFortification_age50-54_male.csv")
colnames(CAL_50_54_male)
str(CAL_50_54_male)

# Select only the necessary columns from the CAL_50_54_male
CAL_50_54_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_50_54_male <- CAL_50_54_male[, CAL_50_54_male_columns_to_keep]

# Rename the column 
CAL_50_54_male <- CAL_50_54_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_50_54_male)
str(CAL_50_54_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_50_54_male <- CAL_50_54_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_50_54_male)

# Delete rows 1-3
CAL_50_54_male <- CAL_50_54_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_50_54_male <- CAL_50_54_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_50_54_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_50_54_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_50_54_male <- na_countries_50_54_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_50_54_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_50_54_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_50_54_male
codes_not_in_50_54_male <- setdiff(codes_0_4, codes_50_54_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_50_54_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_50_54_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_50_54_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_50_54_male:\n")
  print(codes_not_in_50_54_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_50_54_male.\n")
}

############

print(CAL_50_54_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_50_54_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_50_54_male = c(2676.3841, 2568.6348, 2803.3478, 2953.3023, 4014.6542, 4060.2221, 3270.1787, 
                                2553.6657, 2629.5985, 3049.6535, 2766.7703, 2925.6464, NA, 2737.1398, 
                                3325.5307, 3503.2240, 2878.0187, 3430.0970, 2532.6296, 2852.0233, 2405.3533, 
                                3215.5259, 2334.8256, 2547.4186, 3048.5471, 2147.9064, 2929.5727, 2905.7141, 
                                3415.6327, 3704.2847, 3940.2299, 0, 3237.9608, 2921.6303, 2769.3501, 2277.7054, 
                                3323.4613)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_50_54_male <- calorie_data_50_54_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_50_54_male = mean(calorie_intake_50_54_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_50_54_male_NA df
CAL_50_54_male_NA <- data %>%
  left_join(mean_calories_50_54_male, by = "Area")

# View the result
print(CAL_50_54_male_NA)

# Merge the df to include calorie_intake from CAL_50_54_male_NA
CAL_50_54_male_FIN <- CAL_50_54_male %>%
  left_join(CAL_50_54_male_NA %>% select(Countries_code, calorie_intake_50_54_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_50_54_male_FIN <- CAL_50_54_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_50_54_male)) %>%
  select(-calorie_intake_50_54_male)

print(CAL_50_54_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 52
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_50_54_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 52 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=15, GDD_male_2018_FIN)

######################## female 50-54

# K. age 50_54_female - Calorie intake
# Read data from CSV file
CAL_50_54_female <- read.csv("NutrientTotal_withFortification_age50-54_female.csv")
colnames(CAL_50_54_female)
str(CAL_50_54_female)

# Select only the necessary columns from the CAL_50_54_female
CAL_50_54_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_50_54_female <- CAL_50_54_female[, CAL_50_54_female_columns_to_keep]

# Rename the column 
CAL_50_54_female <- CAL_50_54_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_50_54_female)
str(CAL_50_54_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_50_54_female <- CAL_50_54_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_50_54_female)

# Delete rows 1-3
CAL_50_54_female <- CAL_50_54_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_50_54_female <- CAL_50_54_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_50_54_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_50_54_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_50_54_female <- na_countries_50_54_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_50_54_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_50_54_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_50_54_female
codes_not_in_50_54_female <- setdiff(codes_0_4, codes_50_54_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_50_54_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_50_54_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_50_54_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_50_54_female:\n")
  print(codes_not_in_50_54_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_50_54_female.\n")
}

############

print(CAL_50_54_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_50_54_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_50_54_female = c(2321.7243, 2186.8816, 2391.7127, 2490.3199, 3289.1969, 3292.1668, 2720.5080, 
                                  2138.9702, 2177.1524, 2522.9957, 2315.9452, 2408.5751, NA, 2297.1889, 
                                  2774.7798, 2882.4100, 2380.8466, 2870.1831, 2137.6563, 2367.9014, 2043.2048, 
                                  2777.6926, 1957.9574, 2067.5363, 2494.8225, 1782.4927, 2394.0150, 2371.5119, 
                                  2802.6463, 3070.4066, 3313.6643, 0, 2696.3007, 2470.5596, 2382.3052, 1936.6389, 
                                  2836.3033)
)


# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_50_54_female <- calorie_data_50_54_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_50_54_female = mean(calorie_intake_50_54_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

# Join the mean calorie intake and countries used back to the CAL_50_54_female_NA df
CAL_50_54_female_NA <- data %>%
  left_join(mean_calories_50_54_female, by = "Area")

# View the result
print(CAL_50_54_female_NA)

# Merge the df to include calorie_intake from CAL_50_54_female_NA
CAL_50_54_female_FIN <- CAL_50_54_female %>%
  left_join(CAL_50_54_female_NA %>% select(Countries_code, calorie_intake_50_54_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_50_54_female_FIN <- CAL_50_54_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_50_54_female)) %>%
  select(-calorie_intake_50_54_female)

print(CAL_50_54_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 52
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_50_54_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 52 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=14, GDD_female_2018_FIN)



##################################################################################################################

# L. age 55-59_male - Calorie intake
# Read data from CSV file
CAL_55_59_male <- read.csv("NutrientTotal_withFortification_age55-59_male.csv")
colnames(CAL_55_59_male)
str(CAL_55_59_male)

# Select only the necessary columns from the CAL_55_59_male
CAL_55_59_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_55_59_male <- CAL_55_59_male[, CAL_55_59_male_columns_to_keep]

# Rename the column 
CAL_55_59_male <- CAL_55_59_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_55_59_male)
str(CAL_55_59_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_55_59_male <- CAL_55_59_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_55_59_male)

# Delete rows 1-3
CAL_55_59_male <- CAL_55_59_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_55_59_male <- CAL_55_59_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_55_59_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_55_59_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_55_59_male <- na_countries_55_59_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_55_59_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_55_59_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_55_59_male
codes_not_in_55_59_male <- setdiff(codes_0_4, codes_55_59_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_55_59_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_55_59_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_55_59_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_55_59_male:\n")
  print(codes_not_in_55_59_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_55_59_male.\n")
}

############

print(CAL_55_59_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_55_59_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_55_59_male = c(2649.1165, 2554.5498, 2769.5715, 2888.5620, 3881.4780, 3910.0728, 3214.0874, 
                                2453.8119, 2567.7363, 3026.3788, 2678.8332, 2857.8488, NA, 2759.7214, 
                                3259.8533, 3357.7222, 2795.0700, 3442.7006, 2513.8509, 2800.6561, 2355.8402, 
                                3160.6865, 2295.6533, 2440.9468, 2955.3014, 2104.6836, 2854.4817, 2815.8176, 
                                3351.4446, 3598.6890, 3841.3222, 0, 3133.7602, 2880.4371, 2738.8472, 2252.0938, 
                                3325.1129)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_55_59_male <- calorie_data_55_59_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_55_59_male = mean(calorie_intake_55_59_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_55_59_male)

# Join the mean calorie intake and countries used back to the CAL_55_59_male_NA df
CAL_55_59_male_NA <- data %>%
  left_join(mean_calories_55_59_male, by = "Area")

# View the result
print(CAL_55_59_male_NA)

# Merge the df to include calorie_intake from CAL_55_59_male_NA
CAL_55_59_male_FIN <- CAL_55_59_male %>%
  left_join(CAL_55_59_male_NA %>% select(Countries_code, calorie_intake_55_59_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_55_59_male_FIN <- CAL_55_59_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_55_59_male)) %>%
  select(-calorie_intake_55_59_male)

print(CAL_55_59_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 58
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_55_59_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 58 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=15, GDD_male_2018_FIN)

######################## female 55-59

# L. age 55_59_female - Calorie intake
# Read data from CSV file
CAL_55_59_female <- read.csv("NutrientTotal_withFortification_age55-59_female.csv")
colnames(CAL_55_59_female)
str(CAL_55_59_female)

# Select only the necessary columns from the CAL_55_59_female
CAL_55_59_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_55_59_female <- CAL_55_59_female[, CAL_55_59_female_columns_to_keep]

# Rename the column 
CAL_55_59_female <- CAL_55_59_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_55_59_female)
str(CAL_55_59_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_55_59_female <- CAL_55_59_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_55_59_female)

# Delete rows 1-3
CAL_55_59_female <- CAL_55_59_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_55_59_female <- CAL_55_59_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_55_59_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_55_59_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_55_59_female <- na_countries_55_59_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_55_59_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_55_59_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_55_59_female
codes_not_in_55_59_female <- setdiff(codes_0_4, codes_55_59_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_55_59_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_55_59_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_55_59_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_55_59_female:\n")
  print(codes_not_in_55_59_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_55_59_female.\n")
}

############

print(CAL_55_59_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_55_59_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_55_59_female = c(2326.8637, 2242.3014, 2427.7755, 2498.6521, 3270.1111, 3239.4725, 2743.0065, 
                                  2081.1080, 2210.3298, 2554.6345, 2319.3156, 2435.8752, NA, 2302.3968, 
                                  2762.7079, 2832.2719, 2375.6477, 2919.3524, 2168.5126, 2369.6026, 2046.8875, 
                                  2771.7425, 1963.2159, 2052.7968, 2472.5428, 1797.5671, 2376.2030, 2353.1946, 
                                  2820.2708, 3044.7419, 3305.6189, 0, 2691.6908, 2510.8762, 2426.0898, 1978.2716, 
                                  2883.4489)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_55_59_female <- calorie_data_55_59_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_55_59_female = mean(calorie_intake_55_59_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_55_59_female)

# Join the mean calorie intake and countries used back to the CAL_55_59_female_NA df
CAL_55_59_female_NA <- data %>%
  left_join(mean_calories_55_59_female, by = "Area")

# View the result
print(CAL_55_59_female_NA)

# Merge the df to include calorie_intake from CAL_55_59_female_NA
CAL_55_59_female_FIN <- CAL_55_59_female %>%
  left_join(CAL_55_59_female_NA %>% select(Countries_code, calorie_intake_55_59_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_55_59_female_FIN <- CAL_55_59_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_55_59_female)) %>%
  select(-calorie_intake_55_59_female)

print(CAL_55_59_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 58
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_55_59_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 58 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=14, GDD_female_2018_FIN)



##################################################################################################################

# M. age 60-64_male - Calorie intake
# Read data from CSV file
CAL_60_64_male <- read.csv("NutrientTotal_withFortification_age60-64_male.csv")
colnames(CAL_60_64_male)
str(CAL_60_64_male)

# Select only the necessary columns from the CAL_60_64_male
CAL_60_64_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_60_64_male <- CAL_60_64_male[, CAL_60_64_male_columns_to_keep]

# Rename the column 
CAL_60_64_male <- CAL_60_64_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_60_64_male)
str(CAL_60_64_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_60_64_male <- CAL_60_64_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_60_64_male)

# Delete rows 1-3
CAL_60_64_male <- CAL_60_64_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_60_64_male <- CAL_60_64_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_60_64_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_55_59_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_60_64_male <- na_countries_60_64_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_60_64_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_60_64_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_60_64_male
codes_not_in_60_64_male <- setdiff(codes_0_4, codes_60_64_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_60_64_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_60_64_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_60_64_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_60_64_male:\n")
  print(codes_not_in_60_64_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_60_64_male.\n")
}

############

print(CAL_60_64_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_60_64_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_60_64_male = c(2739.1097, 2712.7653, 2914.4150, 3075.9958, 3959.6235, 3958.7417, 3356.5276, 
                                2512.5916, 2680.7010, 3123.9578, 2776.1575, 2978.1638, NA, 2869.6056, 
                                3335.7303, 3412.0245, 2865.1572, 3576.5149, 2601.5360, 2890.2246, 2428.7944, 
                                3281.6814, 2362.5026, 2509.0506, 3027.8324, 2172.0754, 2919.8555, 2911.1274, 
                                3489.1698, 3664.4215, 3935.3830, 0, 3267.0539, 2964.6681, 2862.0369, 2328.8788, 
                                3420.8885)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_60_64_male <- calorie_data_60_64_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_60_64_male = mean(calorie_intake_60_64_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_60_64_male)

# Join the mean calorie intake and countries used back to the CAL_60_64_male_NA df
CAL_60_64_male_NA <- data %>%
  left_join(mean_calories_60_64_male, by = "Area")

# View the result
print(CAL_60_64_male_NA)

# Merge the df to include calorie_intake from CAL_60_64_male_NA
CAL_60_64_male_FIN <- CAL_60_64_male %>%
  left_join(CAL_60_64_male_NA %>% select(Countries_code, calorie_intake_60_64_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_60_64_male_FIN <- CAL_60_64_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_60_64_male)) %>%
  select(-calorie_intake_60_64_male)

print(CAL_60_64_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 62
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_60_64_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 62 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=23, GDD_male_2018_FIN)

######################## female 60-64

# M. age 60_64_female - Calorie intake
# Read data from CSV file
CAL_60_64_female <- read.csv("NutrientTotal_withFortification_age60-64_female.csv")
colnames(CAL_60_64_female)
str(CAL_60_64_female)

# Select only the necessary columns from the CAL_60_64_female
CAL_60_64_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_60_64_female <- CAL_60_64_female[, CAL_60_64_female_columns_to_keep]

# Rename the column 
CAL_60_64_female <- CAL_60_64_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_60_64_female)
str(CAL_60_64_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_60_64_female <- CAL_60_64_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_60_64_female)

# Delete rows 1-3
CAL_60_64_female <- CAL_60_64_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_60_64_female <- CAL_60_64_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_60_64_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_60_64_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_60_64_female <- na_countries_60_64_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_60_64_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_60_64_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_60_64_female
codes_not_in_60_64_female <- setdiff(codes_0_4, codes_60_64_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_60_64_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_60_64_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_60_64_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_60_64_female:\n")
  print(codes_not_in_60_64_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_60_64_female.\n")
}

############

print(CAL_60_64_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_60_64_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_60_64_female = c(2415.7506, 2388.5707, 2515.5953, 2642.9436, 3332.3396, 3303.5808, 2825.8906, 
                                  2132.8338, 2280.2417, 2655.6089, 2387.8591, 2532.3232, NA, 2449.8079, 
                                  2875.4417, 2882.7438, 2435.1030, 3045.9407, 2240.0816, 2450.4496, 2094.0816, 
                                  2853.9515, 2024.9006, 2084.2786, 2531.2995, 1843.6116, 2433.1243, 2414.0909, 
                                  2914.8886, 3113.9398, 3395.4967, 0, 2771.6220, 2601.6788, 2522.9618, 2066.8041, 
                                  3058.3133)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_60_64_female <- calorie_data_60_64_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_60_64_female = mean(calorie_intake_60_64_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_60_64_female)

# Join the mean calorie intake and countries used back to the CAL_60_64_female_NA df
CAL_60_64_female_NA <- data %>%
  left_join(mean_calories_60_64_female, by = "Area")

# View the result
print(CAL_60_64_female_NA)

# Merge the df to include calorie_intake from CAL_60_64_female_NA
CAL_60_64_female_FIN <- CAL_60_64_female %>%
  left_join(CAL_60_64_female_NA %>% select(Countries_code, calorie_intake_60_64_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_60_64_female_FIN <- CAL_60_64_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_60_64_female)) %>%
  select(-calorie_intake_60_64_female)

print(CAL_60_64_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 62
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_60_64_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 62 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=24, GDD_female_2018_FIN)




##################################################################################################################

# N. age 65-69_male - Calorie intake
# Read data from CSV file
CAL_65_69_male <- read.csv("NutrientTotal_withFortification_age65-69_male.csv")
colnames(CAL_65_69_male)
str(CAL_65_69_male)

# Select only the necessary columns from the CAL_65_69_male
CAL_65_69_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_65_69_male <- CAL_65_69_male[, CAL_65_69_male_columns_to_keep]

# Rename the column 
CAL_65_69_male <- CAL_65_69_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_65_69_male)
str(CAL_65_69_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_65_69_male <- CAL_65_69_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_65_69_male)

# Delete rows 1-3
CAL_65_69_male <- CAL_65_69_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_65_69_male <- CAL_65_69_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_65_69_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_65_69_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_65_69_male <- na_countries_65_69_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_65_69_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_65_69_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_65_69_male
codes_not_in_65_69_male <- setdiff(codes_0_4, codes_65_69_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_65_69_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_65_69_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_65_69_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_65_69_male:\n")
  print(codes_not_in_65_69_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_65_69_male.\n")
}

############

print(CAL_65_69_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_65_69_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_65_69_male = c(2893.6315, 2814.9335, 3011.6274, 3206.9616, 4026.9281, 4003.5535, 3418.6318, 
                                2588.2872, 2733.4752, 3243.8324, 2817.4791, 3064.2688, NA, 2953.1521, 
                                3420.2037, 3437.6444, 2914.3369, 3617.0071, 2683.4776, 2954.5817, 2477.3929, 
                                3340.1546, 2398.4155, 2524.8964, 3074.5885, 2225.0915, 2964.8416, 2919.4526, 
                                3522.7811, 3701.8924, 4016.0036, 0, 3333.2126, 3078.8558, 2973.0738, 2404.0617, 
                                3570.8483)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_65_69_male <- calorie_data_65_69_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_65_69_male = mean(calorie_intake_65_69_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_65_69_male)

# Join the mean calorie intake and countries used back to the CAL_65_69_male_NA df
CAL_65_69_male_NA <- data %>%
  left_join(mean_calories_65_69_male, by = "Area")

# View the result
print(CAL_65_69_male_NA)

# Merge the df to include calorie_intake from CAL_65_69_male_NA
CAL_65_69_male_FIN <- CAL_65_69_male %>%
  left_join(CAL_65_69_male_NA %>% select(Countries_code, calorie_intake_65_69_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_65_69_male_FIN <- CAL_65_69_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_65_69_male)) %>%
  select(-calorie_intake_65_69_male)

print(CAL_65_69_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 68
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_65_69_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 68 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=23, GDD_male_2018_FIN)

######################## female 65-69

# M. age 65_69_female - Calorie intake
# Read data from CSV file
CAL_65_69_female <- read.csv("NutrientTotal_withFortification_age65-69_female.csv")
colnames(CAL_65_69_female)
str(CAL_65_69_female)

# Select only the necessary columns from the CAL_65_69_female
CAL_65_69_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_65_69_female <- CAL_65_69_female[, CAL_65_69_female_columns_to_keep]

# Rename the column 
CAL_65_69_female <- CAL_65_69_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_65_69_female)
str(CAL_65_69_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_65_69_female <- CAL_65_69_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_65_69_female)

# Delete rows 1-3
CAL_65_69_female <- CAL_65_69_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_65_69_female <- CAL_65_69_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_65_69_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_65_69_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_65_69_female <- na_countries_65_69_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_65_69_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_65_69_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_65_69_female
codes_not_in_65_69_female <- setdiff(codes_0_4, codes_65_69_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_65_69_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_65_69_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_65_69_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_65_69_female:\n")
  print(codes_not_in_65_69_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_65_69_female.\n")
}

############

print(CAL_65_69_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_65_69_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_65_69_female = c(2506.0629, 2468.9274, 2585.4382, 2703.0180, 3393.3292, 3343.1888, 2913.8956, 
                                  2186.5967, 2367.4632, 2752.7872, 2446.4600, 2574.6964, NA, 2531.9152, 
                                  2930.9320, 2932.8921, 2476.7567, 3095.1773, 2315.2153, 2517.1847, 2137.4355, 
                                  2912.3577, 2066.4982, 2106.0295, 2558.0170, 1878.1386, 2479.0694, 2432.3573, 
                                  3008.3187, 3154.7408, 3483.2124, 0, 2858.6738, 2678.0194, 2598.1954, 2115.5697, 
                                  3123.0276)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_65_69_female <- calorie_data_65_69_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_65_69_female = mean(calorie_intake_65_69_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_65_69_female)

# Join the mean calorie intake and countries used back to the CAL_65_69_female_NA df
CAL_65_69_female_NA <- data %>%
  left_join(mean_calories_65_69_female, by = "Area")

# View the result
print(CAL_65_69_female_NA)

# Merge the df to include calorie_intake from CAL_65_69_female_NA
CAL_65_69_female_FIN <- CAL_65_69_female %>%
  left_join(CAL_65_69_female_NA %>% select(Countries_code, calorie_intake_65_69_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_65_69_female_FIN <- CAL_65_69_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_65_69_female)) %>%
  select(-calorie_intake_65_69_female)

print(CAL_65_69_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 68
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_65_69_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 68 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=24, GDD_female_2018_FIN)



##################################################################################################################

# O. age 70-74_male - Calorie intake
# Read data from CSV file
CAL_70_74_male <- read.csv("NutrientTotal_withFortification_age70-74_male.csv")
colnames(CAL_70_74_male)
str(CAL_70_74_male)

# Select only the necessary columns from the CAL_70_74_male
CAL_70_74_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_70_74_male <- CAL_70_74_male[, CAL_70_74_male_columns_to_keep]

# Rename the column 
CAL_70_74_male <- CAL_70_74_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_70_74_male)
str(CAL_70_74_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_70_74_male <- CAL_70_74_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_70_74_male)

# Delete rows 1-3
CAL_70_74_male <- CAL_70_74_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_70_74_male <- CAL_70_74_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_70_74_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_70_74_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_70_74_male <- na_countries_70_74_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_65_69_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_65_69_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_70_74_male
codes_not_in_70_74_male <- setdiff(codes_0_4, codes_70_74_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_65_69_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_70_74_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_70_74_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_70_74_male:\n")
  print(codes_not_in_70_74_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_70_74_male.\n")
}

############

print(CAL_70_74_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_70_74_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_70_74_male = c(2904.4840, 2920.0991, 3034.9834, 3186.7592, 4056.5794, 4030.3349, 3450.2335, 
                                2563.7871, 2784.9351, 3265.7898, 2864.6476, 3075.5234, NA, 2920.6449, 
                                3467.5613, 3462.2185, 2943.9435, 3704.6183, 2718.8818, 2975.7405, 2465.0914, 
                                3347.5458, 2446.0852, 2530.7543, 3067.9440, 2233.4138, 2993.5155, 2956.4041, 
                                3539.4607, 3727.0042, 4077.0774, 0, 3343.5144, 3107.9414, 2983.2412, 2466.0508, 
                                3642.2466)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_70_74_male <- calorie_data_70_74_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_70_74_male = mean(calorie_intake_70_74_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_70_74_male)

# Join the mean calorie intake and countries used back to the CAL_70_74_male_NA df
CAL_70_74_male_NA <- data %>%
  left_join(mean_calories_70_74_male, by = "Area")

# View the result
print(CAL_70_74_male_NA)

# Merge the df to include calorie_intake from CAL_70_74_male_NA
CAL_70_74_male_FIN <- CAL_70_74_male %>%
  left_join(CAL_70_74_male_NA %>% select(Countries_code, calorie_intake_70_74_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_70_74_male_FIN <- CAL_70_74_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_70_74_male)) %>%
  select(-calorie_intake_70_74_male)

print(CAL_70_74_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 72
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_70_74_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 72 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=23, GDD_male_2018_FIN)

######################## female 70-74

# O. age 70_74_female - Calorie intake
# Read data from CSV file
CAL_70_74_female <- read.csv("NutrientTotal_withFortification_age70-74_female.csv")
colnames(CAL_70_74_female)
str(CAL_70_74_female)

# Select only the necessary columns from the CAL_70_74_female
CAL_70_74_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_70_74_female <- CAL_70_74_female[, CAL_70_74_female_columns_to_keep]

# Rename the column 
CAL_70_74_female <- CAL_70_74_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_70_74_female)
str(CAL_70_74_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_70_74_female <- CAL_70_74_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_70_74_female)

# Delete rows 1-3
CAL_70_74_female <- CAL_70_74_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_70_74_female <- CAL_70_74_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_70_74_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_70_74_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_70_74_female <- na_countries_70_74_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_70_74_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_70_74_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_70_74_female
codes_not_in_70_74_female <- setdiff(codes_0_4, codes_70_74_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_70_74_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_70_74_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_70_74_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_70_74_female:\n")
  print(codes_not_in_70_74_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_70_74_female.\n")
}

############

print(CAL_70_74_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_70_74_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_70_74_female = c(2535.4157, 2501.7792, 2639.7790, 2736.7915, 3416.5905, 3367.7147, 2955.9041, 
                                  2179.9424, 2407.2965, 2747.2788, 2470.9177, 2615.7032, NA, 2532.2113, 
                                  2954.7757, 2951.5301, 2479.2592, 3160.0010, 2333.2776, 2531.4639, 2126.4959, 
                                  2904.2535, 2090.0364, 2124.7084, 2584.7887, 1888.6381, 2496.9227, 2464.4085, 
                                  3018.5269, 3179.7414, 3508.7729, 0, 2876.8377, 2705.0311, 2626.0285, 2152.2696, 
                                  3207.9935)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_70_74_female <- calorie_data_70_74_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_70_74_female = mean(calorie_intake_70_74_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_70_74_female)

# Join the mean calorie intake and countries used back to the CAL_70_74_female_NA df
CAL_70_74_female_NA <- data %>%
  left_join(mean_calories_70_74_female, by = "Area")

# View the result
print(CAL_70_74_female_NA)

# Merge the df to include calorie_intake from CAL_70_74_female_NA
CAL_70_74_female_FIN <- CAL_70_74_female %>%
  left_join(CAL_70_74_female_NA %>% select(Countries_code, calorie_intake_70_74_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_70_74_female_FIN <- CAL_70_74_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_70_74_female)) %>%
  select(-calorie_intake_70_74_female)

print(CAL_70_74_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 72
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_70_74_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 72 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=24, GDD_female_2018_FIN)




##################################################################################################################

# P. age 75-79_male - Calorie intake
# Read data from CSV file
CAL_75_79_male <- read.csv("NutrientTotal_withFortification_age75-79_male.csv")
colnames(CAL_75_79_male)
str(CAL_75_79_male)

# Select only the necessary columns from the CAL_75_79_male
CAL_75_79_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_75_79_male <- CAL_75_79_male[, CAL_75_79_male_columns_to_keep]

# Rename the column 
CAL_75_79_male <- CAL_75_79_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_75_79_male)
str(CAL_75_79_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_75_79_male <- CAL_75_79_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_75_79_male)

# Delete rows 1-3
CAL_75_79_male <- CAL_75_79_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_75_79_male <- CAL_75_79_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_75_79_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_75_79_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_75_79_male <- na_countries_75_79_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_75_79_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_75_79_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_75_79_male
codes_not_in_75_79_male <- setdiff(codes_0_4, codes_75_79_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_75_79_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_75_79_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_75_79_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_75_79_male:\n")
  print(codes_not_in_75_79_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_75_79_male.\n")
}

############

print(CAL_75_79_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_75_79_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_75_79_male = c(2949.1209, 2914.9974, 3068.5551, 3240.6507, 4087.6390, 4061.8397, 3515.6896, 
                                2614.2198, 2800.7694, 3335.8234, 2872.2389, 3117.6981, NA, 3034.8744, 
                                3459.9541, 3469.2837, 2941.2541, 3711.3465, 2732.1074, 2999.6168, 2482.6780, 
                                3374.2570, 2452.2673, 2533.9433, 3101.8160, 2236.1914, 3003.8297, 2950.8103, 
                                3625.1366, 3742.2379, 4124.5180, 0, 3366.7758, 3153.6552, 3009.0573, 2495.8917, 
                                3733.6465)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_75_79_male <- calorie_data_75_79_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_75_79_male = mean(calorie_intake_75_79_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_75_79_male)

# Join the mean calorie intake and countries used back to the CAL_75_79_male_NA df
CAL_75_79_male_NA <- data %>%
  left_join(mean_calories_75_79_male, by = "Area")

# View the result
print(CAL_75_79_male_NA)

# Merge the df to include calorie_intake from CAL_75_79_male_NA
CAL_75_79_male_FIN <- CAL_75_79_male %>%
  left_join(CAL_75_79_male_NA %>% select(Countries_code, calorie_intake_75_79_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_75_79_male_FIN <- CAL_75_79_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_75_79_male)) %>%
  select(-calorie_intake_75_79_male)

print(CAL_75_79_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 78
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_75_79_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 78 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=23, GDD_male_2018_FIN)

######################## female 75-79

# P. age 75_79_female - Calorie intake
# Read data from CSV file
CAL_75_79_female <- read.csv("NutrientTotal_withFortification_age75-79_female.csv")
colnames(CAL_75_79_female)
str(CAL_75_79_female)

# Select only the necessary columns from the CAL_75_79_female
CAL_75_79_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_75_79_female <- CAL_75_79_female[, CAL_75_79_female_columns_to_keep]

# Rename the column 
CAL_75_79_female <- CAL_75_79_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_75_79_female)
str(CAL_75_79_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_75_79_female <- CAL_75_79_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_75_79_female)

# Delete rows 1-3
CAL_75_79_female <- CAL_75_79_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_75_79_female <- CAL_75_79_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_75_79_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_75_79_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_75_79_female <- na_countries_75_79_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_75_79_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_75_79_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_75_79_female
codes_not_in_75_79_female <- setdiff(codes_0_4, codes_75_79_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_75_79_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_75_79_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_75_79_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_75_79_female:\n")
  print(codes_not_in_75_79_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_75_79_female.\n")
}

############

print(CAL_75_79_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_75_79_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_75_79_female = c(2574.1074, 2519.6987, 2670.6628, 2790.2738, 3443.9440, 3394.2170, 2964.8650, 
                                  2207.4758, 2390.4604, 2785.7898, 2472.2782, 2640.0005, NA, 2583.4446, 
                                  2959.9564, 2958.5120, 2477.0438, 3150.2066, 2372.0628, 2539.6742, 2126.9805, 
                                  2910.7931, 2142.4031, 2136.0597, 2578.5694, 1919.5419, 2508.5623, 2475.0005, 
                                  3018.7645, 3200.8793, 3559.4536, 0, 2902.6396, 2737.9566, 2659.1926, 2167.1982, 
                                  3220.8918)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_75_79_female <- calorie_data_75_79_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_75_79_female = mean(calorie_intake_75_79_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_75_79_female)

# Join the mean calorie intake and countries used back to the CAL_75_79_female_NA df
CAL_75_79_female_NA <- data %>%
  left_join(mean_calories_75_79_female, by = "Area")

# View the result
print(CAL_75_79_female_NA)

# Merge the df to include calorie_intake from CAL_75_79_female_NA
CAL_75_79_female_FIN <- CAL_75_79_female %>%
  left_join(CAL_75_79_female_NA %>% select(Countries_code, calorie_intake_75_79_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_75_79_female_FIN <- CAL_75_79_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_75_79_female)) %>%
  select(-calorie_intake_75_79_female)

print(CAL_75_79_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 78
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_75_79_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse(age == 78 & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=24, GDD_female_2018_FIN)



##################################################################################################################

# Q. age 80+_male - Calorie intake
# Read data from CSV file
CAL_80_male <- read.csv("NutrientTotal_withFortification_age80+_male.csv")
colnames(CAL_80_male)
str(CAL_80_male)

# Select only the necessary columns from the CAL_80_male
CAL_80_male_columns_to_keep <- c("X", "X.1", "calories")
CAL_80_male <- CAL_80_male[, CAL_80_male_columns_to_keep]

# Rename the column 
CAL_80_male <- CAL_80_male %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_80_male)
str(CAL_80_male)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_80_male <- CAL_80_male %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_80_male)

# Delete rows 1-3
CAL_80_male <- CAL_80_male[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_80_male <- CAL_80_male %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_80_male)

####### check
# compare the Countries_code columns from two dataframes (na_countries_80_male and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_80_male <- na_countries_80_male$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_80_male but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_80_male, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_80_male
codes_not_in_80_male <- setdiff(codes_0_4, codes_80_male)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_80_male but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_80_male are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_80_male) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_80_male:\n")
  print(codes_not_in_80_male)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_80_male.\n")
}

############

print(CAL_80_male)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_80_male <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_80_male = c(2963.0517, 2987.4475, 3140.2878, 3266.9623, 4160.7539, 4136.5436, 3542.9479, 
                             2652.5305, 2854.6519, 3363.2456, 2930.5294, 3164.1511, NA, 3067.5889, 
                             3514.9511, 3517.5178, 2949.4394, 3802.0038, 2830.6712, 3036.6319, 2476.3386, 
                             3379.4734, 2504.9281, 2552.6605, 3125.6053, 2262.0206, 3044.4083, 2981.6585, 
                             3633.7126, 3798.9649, 4220.0075, 0, 3460.8663, 3240.3126, 3079.1689, 2527.3215, 
                             3807.6634)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_80_male <- calorie_data_80_male %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_80_male = mean(calorie_intake_80_male, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_80_male)

# Join the mean calorie intake and countries used back to the CAL_80_male_NA df
CAL_80_male_NA <- data %>%
  left_join(mean_calories_80_male, by = "Area")

# View the result
print(CAL_80_male_NA)

# Merge the df to include calorie_intake from CAL_80_male_NA
CAL_80_male_FIN <- CAL_80_male %>%
  left_join(CAL_80_male_NA %>% select(Countries_code, calorie_intake_80_male), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_80_male_FIN <- CAL_80_male_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_80_male)) %>%
  select(-calorie_intake_80_male)

print(CAL_80_male_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 82, 88, 92, 98
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(CAL_80_male_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse((age == 82 | age == 88 | age == 92 | age == 98) & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=23, GDD_male_2018_FIN)

######################## female 80+

# Q. age 80+_female - Calorie intake
# Read data from CSV file
CAL_80_female <- read.csv("NutrientTotal_withFortification_age80+_female.csv")
colnames(CAL_80_female)
str(CAL_80_female)

# Select only the necessary columns from the CAL_80_female
CAL_80_female_columns_to_keep <- c("X", "X.1", "calories")
CAL_80_female <- CAL_80_female[, CAL_80_female_columns_to_keep]

# Rename the column 
CAL_80_female <- CAL_80_female %>%
  rename(
    Countries_code = X,
    UN_countries = X.1,
    calorie_intake = calories
  )

colnames(CAL_80_female)
str(CAL_80_female)

# Convert calorie_intake to numeric and replace "*" with NA
CAL_80_female <- CAL_80_female %>%
  mutate(
    calorie_intake = na_if(calorie_intake, "*"),
    calorie_intake = ifelse(calorie_intake == "", NA, calorie_intake),
    calorie_intake = as.numeric(calorie_intake)
  )

# Display the structure of the df
str(CAL_80_female)

# Delete rows 1-3
CAL_80_female <- CAL_80_female[-c(1:3), ]

# Identify country codes with NA in calorie_intake column
na_countries_80_female <- CAL_80_female %>%
  filter(is.na(calorie_intake)) %>%
  select(Countries_code)

print(na_countries_80_female)

####### check
# compare the Countries_code columns from two dataframes (na_countries_80_female and na_countries_0_4_bothsexes) and print the names that do not match
# Extract Countries_code columns
codes_80_female <- na_countries_80_female$Countries_code
codes_0_4 <- na_countries_0_4_bothsexes$Countries_code

# Find the codes that are in na_countries_80_female but not in na_countries_0_4_bothsexes
codes_not_in_0_4 <- setdiff(codes_80_female, codes_0_4)

# Find the codes that are in na_countries_0_4_bothsexes but not in na_countries_80_female
codes_not_in_80_female <- setdiff(codes_0_4, codes_80_female)

# Print the results
if (length(codes_not_in_0_4) > 0) {
  cat("Countries in na_countries_80_female but not in na_countries_0_4_bothsexes:\n")
  print(codes_not_in_0_4)
} else {
  cat("All countries in na_countries_80_female are present in na_countries_0_4_bothsexes.\n")
}

if (length(codes_not_in_80_female) > 0) {
  cat("Countries in na_countries_0_4_bothsexes but not in na_countries_80_female:\n")
  print(codes_not_in_80_female)
} else {
  cat("All countries in na_countries_0_4_bothsexes are present in na_countries_80_female.\n")
}

############

print(CAL_80_female)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
# Create the data
# Create the data with NA calorie intake
data_na <- data.frame(
  Countries_code = c("AFG", "BMU", "KHM", "TCD", "PRK", "DMA", "GAB", "KIR", "LSO", "LBR", 
                     "MMR", "NDA", "KNA", "WSM", "STP", "SLE", "SLB", "SOM", "TLS", "TGO", 
                     "TKM", "UGA", "VUT", "VNM", "ZMB"),
  Country = c("Afghanistan", "Bermuda", "Cambodia", "Chad", "Democratic People's Republic of Korea", 
              "Dominica", "Gabon", "Kiribati", "Lesotho", "Liberia", "Myanmar", 
              "Netherlands Antilles", "Saint Kitts and Nevis", "Samoa", "Sao Tome and Principe", 
              "Sierra Leone", "Solomon Islands", "Somalia", "Timor-Leste", "Togo", 
              "Turkmenistan", "Uganda", "Vanuatu", "Viet Nam", "Zambia"),
  Area = c("South Asia", "North America", "Southeast Asia", "Africa", "East Asia", "Caribbean", 
           "Africa", "Oceania", "Africa", "Africa", "Southeast Asia", "Caribbean", "Caribbean", 
           "Oceania", "Africa", "Africa", "Oceania", "Africa", "Southeast Asia", "Africa", 
           "Central Asia", "Africa", "Oceania", "Southeast Asia", "Africa"),
  stringsAsFactors = FALSE
)

# Create the dataset with the mean calorie intake values for each area
calorie_data_80_female <- data.frame(
  Country = c("India", "Bangladesh", "Pakistan", "Nepal", "Canada", "USA", "Mexico", 
              "Dominican Republic", "El Salvador", "Indonesia", "Thailand", "Philippines", 
              "Viet Nam", "Lao People's Democratic Republic", "Nigeria", "Ghana", "Cameroon", 
              "Niger", "Sudan", "China", "Japan", "Republic of Korea", "Mongolia", 
              "Bahamas", "Barbados", "Haiti", "Jamaica", "Trinidad and Tobago", "Fiji", 
              "Australia", "New Zealand", "Papua New Guinea", "Kazakhstan", "Kyrgyzstan", 
              "Uzbekistan", "Tajikistan", "Azerbaijan"),
  Area = c(rep("South Asia", 4), rep("North America", 5), rep("Southeast Asia", 5), 
           rep("Africa", 5), rep("East Asia", 4), rep("Caribbean", 5), 
           rep("Oceania", 4), rep("Central Asia", 5)),
  calorie_intake_80_female = c(2591.5206, 2590.7604, 2703.0165, 2845.7033, 3500.7367, 3448.0649, 3016.3330, 
                               2234.8475, 2446.4683, 2876.1999, 2544.0423, 2683.6734, NA, 2639.5206, 
                               3006.9789, 2983.8940, 2514.6905, 3246.1645, 2413.9114, 2566.0640, 2121.7886, 
                               2919.6892, 2177.2273, 2143.8136, 2611.4887, 1922.7200, 2546.3228, 2511.2760, 
                               3074.0114, 3239.3287, 3644.1203, 0, 2979.4734, 2812.2776, 2714.1016, 2208.4762, 
                               3326.2840)
)

# Calculate the mean calorie intake for each area and include the names of the countries used for the calculation
mean_calories_80_female <- calorie_data_80_female %>%
  group_by(Area) %>%
  summarise(
    calorie_intake_80_female = mean(calorie_intake_80_female, na.rm = TRUE),
    Countries_Used = paste(Country, collapse = ", ")
  )

print(mean_calories_80_female)

# Join the mean calorie intake and countries used back to the CAL_80_female_NA df
CAL_80_female_NA <- data %>%
  left_join(mean_calories_80_female, by = "Area")

# View the result
print(CAL_80_female_NA)

# Merge the df to include calorie_intake from CAL_80_female_NA
CAL_80_female_FIN <- CAL_80_female %>%
  left_join(CAL_80_female_NA %>% select(Countries_code, calorie_intake_80_female), by = "Countries_code", suffix = c("", "_NA"))

# Replace NA values in calorie_intake with values from calorie_intake_NA
CAL_80_female_FIN <- CAL_80_female_FIN %>%
  mutate(calorie_intake = coalesce(calorie_intake, calorie_intake_80_female)) %>%
  select(-calorie_intake_80_female)

print(CAL_80_female_FIN)

# Merge and update calorie_intake in GDD_male_2018_FIN where age is 82, 88, 92, 98
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(CAL_80_female_FIN %>% select(Countries_code, calorie_intake), by = "Countries_code", suffix = c("", ".new")) %>% # for columns with the same name in both dataframes, the column from the first dataframe (GDD_male_2018_FIN) will keep its original name, and the column from the second dataframe (CAL_5_9_bothsexes_FIN) will have the suffix .new added to its name.
  mutate(calorie_intake = ifelse((age == 82 | age == 88 | age == 92 | age == 98) & !is.na(calorie_intake.new), calorie_intake.new, calorie_intake)) %>%
  select(-calorie_intake.new)

print(n=24, GDD_female_2018_FIN)

###################################################################

################################################################################################

# Countries that have no calorie intake data but have data in the GDD (and are not listed in the calorie intake original data)

# Identify country codes with NA in calorie_intake column
GDD_female_2018_FIN_NA <- GDD_female_2018_FIN %>%
  filter(is.na(calorie_intake)) %>%
  select(UN_countries) %>%
  distinct() #removes any duplicate country names

print(GDD_female_2018_FIN)

# Dealing with countries that have NA data = average calculation of calorie intake of countries in the same area
######################################################################## male
# Create the mapping data frame (based on the provided table (by the file: Countries_Area_with_Calorie_Intake - with GDD value. csv)
mapping_df <- data.frame(
  Country_NA = c("Burundi", "Bahrain", "Bhutan", "Democratic Republic of the Congo", "Comoros", "Eritrea", "Micronesia (Fed. States of)", "Equatorial Guinea", "Marshall Islands", "Oman", "Papua New Guinea", "Qatar", "Singapore", "South Sudan", "Seychelles", "Tonga", "China, Taiwan Province of China"),
  Country_similar_for_mean_calculation = I(list(
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("United Arab Emirates", "Saudi Arabia"),
    c("India", "Bangladesh", "Pakistan", "Nepal"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("United Arab Emirates", "Saudi Arabia"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("United Arab Emirates", "Saudi Arabia"),
    c("Indonesia", "Thailand", "Philippines", "Viet Nam", "Lao People's Democratic Republic"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("China", "Japan", "Republic of Korea", "Mongolia")
  ))
)

# Function to calculate mean calorie intake for each Country_NA
calculate_mean_calorie_intake <- function(country_na, similar_countries, df) {
  similar_data <- df %>% filter(UN_countries %in% similar_countries)
  mean_calorie_intake <- similar_data %>%
    group_by(group_age) %>%
    summarize(mean_calorie_intake = mean(calorie_intake, na.rm = TRUE))
  mean_calorie_intake <- mean_calorie_intake %>% mutate(Country_NA = country_na)
  return(mean_calorie_intake)
}

# Initialize an empty data frame to store results_NA_calorie_intake
results_NA_calorie_intake_male <- data.frame()

# Loop through each Country_NA and calculate the mean calorie intake_male
for (i in 1:nrow(mapping_df)) {
  country_na <- mapping_df$Country_NA[i]
  similar_countries <- mapping_df$Country_similar_for_mean_calculation[[i]]
  mean_calorie_intake <- calculate_mean_calorie_intake(country_na, similar_countries, GDD_male_2018_FIN)
  results_NA_calorie_intake_male <- rbind(results_NA_calorie_intake_male, mean_calorie_intake)
}

# View the results_NA_calorie_intake
print(results_NA_calorie_intake_male)
colnames(results_NA_calorie_intake_male)
colnames(GDD_male_2018_FIN)

# Rename columns in results_NA_calorie_intake_male to match those in GDD_male_2018_FIN
results_NA_calorie_intake_male <- results_NA_calorie_intake_male %>%
  rename(UN_countries = Country_NA, new_calorie_intake = mean_calorie_intake)


# Merge and update the calorie_intake values
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  left_join(results_NA_calorie_intake_male, by = c("UN_countries", "group_age")) %>%
  mutate(calorie_intake = if_else(!is.na(new_calorie_intake), new_calorie_intake, calorie_intake)) %>%
  select(-new_calorie_intake)

######################################################################## female
# Create the mapping data frame (based on the provided table (by the file: Countries_Area_with_Calorie_Intake - with GDD value. csv)
mapping_df <- data.frame(
  Country_NA = c("Burundi", "Bahrain", "Bhutan", "Democratic Republic of the Congo", "Comoros", "Eritrea", "Micronesia (Fed. States of)", "Equatorial Guinea", "Marshall Islands", "Oman", "Papua New Guinea", "Qatar", "Singapore", "South Sudan", "Seychelles", "Tonga", "China, Taiwan Province of China"),
  Country_similar_for_mean_calculation = I(list(
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("United Arab Emirates", "Saudi Arabia"),
    c("India", "Bangladesh", "Pakistan", "Nepal"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("United Arab Emirates", "Saudi Arabia"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("United Arab Emirates", "Saudi Arabia"),
    c("Indonesia", "Thailand", "Philippines", "Viet Nam", "Lao People's Democratic Republic"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Nigeria", "Ghana", "Cameroon", "Niger", "Sudan"),
    c("Fiji", "Australia", "New Zealand", "Papua New Guinea"),
    c("China", "Japan", "Republic of Korea", "Mongolia")
  ))
)

# Function to calculate mean calorie intake for each Country_NA
calculate_mean_calorie_intake_fe <- function(country_na, similar_countries, df) {
  similar_data <- df %>% filter(UN_countries %in% similar_countries)
  mean_calorie_intake <- similar_data %>%
    group_by(group_age) %>%
    summarize(mean_calorie_intake = mean(calorie_intake, na.rm = TRUE))
  mean_calorie_intake <- mean_calorie_intake %>% mutate(Country_NA = country_na)
  return(mean_calorie_intake)
}

# Initialize an empty data frame to store results_NA_calorie_intake
results_NA_calorie_intake_female <- data.frame()

# Loop through each Country_NA and calculate the mean calorie intake_male
for (i in 1:nrow(mapping_df)) {
  country_na <- mapping_df$Country_NA[i]
  similar_countries <- mapping_df$Country_similar_for_mean_calculation[[i]]
  mean_calorie_intake <- calculate_mean_calorie_intake_fe(country_na, similar_countries, GDD_female_2018_FIN)
  results_NA_calorie_intake_female <- rbind(results_NA_calorie_intake_female, mean_calorie_intake)
}

# View the results_NA_calorie_intake
print(results_NA_calorie_intake_female)
colnames(results_NA_calorie_intake_female)
colnames(GDD_female_2018_FIN)

# Rename columns in results_NA_calorie_intake_male to match those in GDD_female_2018_FIN
results_NA_calorie_intake_female <- results_NA_calorie_intake_female %>%
  rename(UN_countries = Country_NA, new_calorie_intake = mean_calorie_intake)


# Merge and update the calorie_intake values
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  left_join(results_NA_calorie_intake_female, by = c("UN_countries", "group_age")) %>%
  mutate(calorie_intake = if_else(!is.na(new_calorie_intake), new_calorie_intake, calorie_intake)) %>%
  select(-new_calorie_intake)

colnames(GDD_female_2018_FIN)

######################################################################################################################
#################### Sugar consumption calculation and % added sugar correction
# male - df: GDD_male_2018_FIN
colnames(GDD_male_2018_FIN)

# Select rows where 'sugar_cons_perc' is larger than 20.00% of added sugar from the 'GDD_male_2018_FIN' df
GDD_male_larger_20 <- GDD_male_2018_FIN[GDD_male_2018_FIN$sugar_cons_perc > 20.00, ]

# Modify sugar_cons_perc values if it is >20.00% to 20.00
GDD_male_2018_FIN$sugar_cons_perc <- ifelse(
  GDD_male_2018_FIN$sugar_cons_perc > 20.00,
  20.00,
  GDD_male_2018_FIN$sugar_cons_perc
)

# Calculate the calories from sugar - daily per person
GDD_male_2018_FIN$sugar_cons_calorie_person <- (GDD_male_2018_FIN$sugar_cons_perc / 100) * GDD_male_2018_FIN$calorie_intake

# Convert calories from sugar to grams of sugar - 1 gram of sugar provides approximately 4 calories
GDD_male_2018_FIN$sugar_cons_grams_person <- GDD_male_2018_FIN$sugar_cons_calorie_person / 4

########################
#########################################
# CONSUMPTION CORRECTIONS male
# changing the sugar_cons_grams_person of all these countries that have % added sugar 33% for all group og age
#according to the data from: https://www.oecd-ilibrary.org/agriculture-and-food/sugar-projections-consumption-per-capita_d5addf4a-en :
# DEVELOPED COUNTRIES = 31.4 kg/capita/year avg. = 86.03 gram per day/capita & DEVELOPING COUNTRIES = 19.5 kg/capita/year avg. = 53.43 gram per day/capita
# LEAST DEVELOPED COUNTRIES (LDC = countries facing the most severe challenges in terms of socioeconomic development) = 11.8 kg/capita/yaer avg. = 32.33 gram per day/capita
# India: based on https://www.nin.res.in/survey_reports/sugar_study_report_part-2.pdf, p 6-8:
# In general, the average sugar consumption for males is 18.7 g/day. female: 20.2 g/day
# age: 0-4: 15.6 g/day ; age: 5-11: 17.6 g/day ; age: 12-17: 19.9 g/day ; age: 18-35: 19.4 g/day ; age 35-39: 20.5 g/day ; age >60: 20.3 g/day

GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  # Create the new column based on the conditions
  mutate(sugar_cons_grams_person_correction = case_when(
    UN_countries == "India" & age %in% c(0, 2, 4) ~ 15.6,
    UN_countries == "India" & age == 8 ~ 17.6,
    UN_countries == "India" & age == c(12, 18) ~ 19.9,
    UN_countries == "India" & age %in% c(22, 28, 32) ~ 19.4,
    UN_countries == "India" & age == 38 ~ 20.5,
    UN_countries == "India" & age %in% c(42, 48, 52, 58, 62, 68, 72, 78, 82, 88, 92, 98) ~ 20.3,
    UN_countries == "Albania" ~ 53.43, # developing country
    UN_countries == "Rwanda" ~ 53.43, # developing country
    TRUE ~ sugar_cons_grams_person  # Default to the original value if no condition is met
  )) %>%
  # Replace the values in the original column with the new ones
  mutate(sugar_cons_grams_person = sugar_cons_grams_person_correction) %>%
  # Remove the correction column
  select(-sugar_cons_grams_person_correction)
########################

## Calculate the mass from sugar - daily pop by group age (pop are in thousands))
GDD_male_2018_FIN$sugar_cons_grams_pop <- GDD_male_2018_FIN$sugar_cons_grams_person * (GDD_male_2018_FIN$POP*1000)


### convert grams to ton - 1 ton equals 1,000,000 grams
GDD_male_2018_FIN$sugar_cons_ton_pop <- GDD_male_2018_FIN$sugar_cons_grams_pop / 10^6

#### Calculate the mass from sugar - daily pop all group for each country
# Group by Countries_code and calculate the total sugar consumption in tons
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  group_by(Countries_code) %>%
  mutate(Total_Sugar_cons_day_ton = ifelse(row_number() == 1, sum(sugar_cons_ton_pop, na.rm = TRUE), NA))

#### Calculate the total pop for each country
# Group by Countries_code and calculate the total sugar consumption in tons
# Calculate the total population for each country and multiply by 1000
GDD_male_2018_FIN <- GDD_male_2018_FIN %>%
  group_by(Countries_code) %>%
  mutate(Total_pop = ifelse(row_number() == 1, sum(POP * 1000, na.rm = TRUE), NA))

##### Calculate the mass from sugar - year -  pop all group for each country
GDD_male_2018_FIN$Total_Sugar_cons_year_ton <- GDD_male_2018_FIN$Total_Sugar_cons_day_ton * 365

#######################
colnames(GDD_male_2018_FIN)

# Filter to keep only the first row of each Countries_code and delete the specified columns
GDD_male_2018_FIN_TOTAL <- GDD_male_2018_FIN %>%
  group_by(Countries_code) %>%
  slice(1) %>%  # Keep only the first row of each group
  ungroup() %>%
  select(-age, -group_age, -POP, -sugar_cons_calorie_person, 
         -sugar_cons_grams_person, -sugar_cons_grams_pop, -sugar_cons_ton_pop)

print(GDD_male_2018_FIN_TOTAL)


######################################################################################################################
#################### Sugar consumption calculation and % added sugar correction
# female - df: GDD_female_2018_FIN
colnames(GDD_female_2018_FIN)

# Select rows where 'sugar_cons_perc' is larger than 20.00% of added sugar from the 'GDD_male_2018_FIN' df
GDD_female_larger_20 <- GDD_female_2018_FIN[GDD_female_2018_FIN$sugar_cons_perc > 20.00, ]

# Modify sugar_cons_perc values if it is >20.00% to 20.00
GDD_female_2018_FIN$sugar_cons_perc <- ifelse(
  GDD_female_2018_FIN$sugar_cons_perc > 20.00,
  20.00,
  GDD_female_2018_FIN$sugar_cons_perc
)

# Calculate the calories from sugar - daily per person
GDD_female_2018_FIN$sugar_cons_calorie_person <- (GDD_female_2018_FIN$sugar_cons_perc / 100) * GDD_female_2018_FIN$calorie_intake

# Convert calories from sugar to grams of sugar - 1 gram of sugar provides approximately 4 calories
GDD_female_2018_FIN$sugar_cons_grams_person <- GDD_female_2018_FIN$sugar_cons_calorie_person / 4

########################
#########################################
# CONSUMPTION CORRECTIONS female
# changing the sugar_cons_grams_person of all these countries that have % added sugar 33% for all group og age
#according to the data from: https://www.oecd-ilibrary.org/agriculture-and-food/sugar-projections-consumption-per-capita_d5addf4a-en :
# DEVELOPED COUNTRIES = 31.4 kg/capita/yaer avg. = 86.03 gram per day/capita & DEVELOPING COUNTRIES = 19.5 kg/capita/yaer avg. = 53.43 gram per day/capita
# LEAST DEVELOPED COUNTRIES (LDC = countries facing the most severe challenges in terms of socioeconomic development) = 11.8 kg/capita/yaer avg. = 32.33 gram per day/capita
# India: based on https://www.nin.res.in/survey_reports/sugar_study_report_part-2.pdf, p 6-8:
# In general, the average sugar consumption for males is 18.7 g/day. female: 20.2 g/day
# age: 0-4: 15.6 g/day ; age: 5-11: 17.6 g/day ; age: 12-17: 19.9 g/day ; age: 18-35: 19.4 g/day ; age 35-39: 20.5 g/day ; age >60: 20.3 g/day

GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  # Create the new column based on the conditions
  mutate(sugar_cons_grams_person_correction = case_when(
    UN_countries == "India" & age %in% c(0, 2, 4) ~ 15.6,
    UN_countries == "India" & age == 8 ~ 17.6,
    UN_countries == "India" & age == c(12, 18) ~ 19.9,
    UN_countries == "India" & age %in% c(22, 28, 32) ~ 19.4,
    UN_countries == "India" & age == 38 ~ 20.5,
    UN_countries == "India" & age %in% c(42, 48, 52, 58, 62, 68, 72, 78, 82, 88, 92, 98) ~ 20.3,
    UN_countries == "Albania" ~ 53.43, # developing country
    UN_countries == "Rwanda" ~ 53.43, # developing country
    UN_countries == "Timor-Leste" ~ 32.33, # least developed country (LDC)
    TRUE ~ sugar_cons_grams_person  # Default to the original value if no condition is met
  )) %>%
  # Replace the values in the original column with the new ones
  mutate(sugar_cons_grams_person = sugar_cons_grams_person_correction) %>%
  # Remove the correction column
  select(-sugar_cons_grams_person_correction)
########################


## Calculate the mass from sugar - daily pop by group age (pop are in thousands))
GDD_female_2018_FIN$sugar_cons_grams_pop <- GDD_female_2018_FIN$sugar_cons_grams_person * (GDD_female_2018_FIN$POP*1000)

### convert grams to ton - 1 ton equals 1,000,000 grams
GDD_female_2018_FIN$sugar_cons_ton_pop <- GDD_female_2018_FIN$sugar_cons_grams_pop / 10^6

#### Calculate the mass from sugar - daily pop all group for each country
# Group by Countries_code and calculate the total sugar consumption in tons
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  group_by(Countries_code) %>%
  mutate(Total_Sugar_cons_day_ton = ifelse(row_number() == 1, sum(sugar_cons_ton_pop, na.rm = TRUE), NA))

#### Calculate the total pop for each country
# Group by Countries_code and calculate the total sugar consumption in tons
# Calculate the total population for each country and multiply by 1000
GDD_female_2018_FIN <- GDD_female_2018_FIN %>%
  group_by(Countries_code) %>%
  mutate(Total_pop = ifelse(row_number() == 1, sum(POP * 1000, na.rm = TRUE), NA))

##### Calculate the mass from sugar - year -  pop all group for each country
GDD_female_2018_FIN$Total_Sugar_cons_year_ton <- GDD_female_2018_FIN$Total_Sugar_cons_day_ton * 365

######################
colnames(GDD_female_2018_FIN)

# Filter to keep only the first row of each Countries_code and delete the specified columns
GDD_female_2018_FIN_TOTAL <- GDD_female_2018_FIN %>%
  group_by(Countries_code) %>%
  slice(1) %>%  # Keep only the first row of each group
  ungroup() %>%
  select(-age, -group_age, -POP, -sugar_cons_calorie_person, 
         -sugar_cons_grams_person, -sugar_cons_grams_pop, -sugar_cons_ton_pop)

print(GDD_female_2018_FIN_TOTAL)

###############################
###############################################################################################
# Export df GDD_female_2018_FIN to Excel and GDD_male_2018_FIN
write.xlsx(GDD_female_2018_FIN, "GDD_female_2018_FIN_1.11.24.xlsx")
write.xlsx(GDD_male_2018_FIN, "GDD_male_2018_FIN_1.11.24.xlsx")
write.xlsx(GDD_female_2018_FIN_TOTAL, "GDD_female_2018_FIN_TOTAL_1.11.24.xlsx")
write.xlsx(GDD_male_2018_FIN_TOTAL, "GDD_male_2018_FIN_TOTAL_1.11.24.xlsx")

########################################################################################
###############################################################################################

# Merge the consumption data into the primary dataset: SC_SB_MFA - female
# Select only necessary columns from GDD_female_2018_FIN_TOTAL before the join
GDD_female_2018_FIN_TOTAL_selected <- GDD_female_2018_FIN_TOTAL %>%
  select(Countries_code, Total_pop, Total_Sugar_cons_year_ton, sugar_cons_perc, calorie_intake)

# Merge the consumption data into the primary dataset: SC_SB_MFA
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(GDD_female_2018_FIN_TOTAL_selected, by = "Countries_code") %>%
  # Rename columns
  rename(Total_pop_female = Total_pop,
         Total_Sugar_cons_year_ton_female = Total_Sugar_cons_year_ton,
         sugar_cons_perc_female = sugar_cons_perc,
         calorie_intake_female = calorie_intake)

colnames(GDD_female_2018_FIN_TOTAL)
colnames(SC_SB_MFA)

#####
# Merge the consumption data into the primary dataset: SC_SB_MFA - male
# Select only necessary columns from GDD_male_2018_FIN_TOTAL before the join
GDD_male_2018_FIN_TOTAL_selected <- GDD_male_2018_FIN_TOTAL %>%
  select(Countries_code, Total_pop, Total_Sugar_cons_year_ton, sugar_cons_perc, calorie_intake)

# Merge the consumption data into the primary dataset: SC_SB_MFA
SC_SB_MFA <- SC_SB_MFA %>%
  left_join(GDD_male_2018_FIN_TOTAL_selected, by = "Countries_code") %>%
  # Rename columns
  rename(Total_pop_male = Total_pop,
         Total_Sugar_cons_year_ton_male = Total_Sugar_cons_year_ton,
         sugar_cons_perc_male = sugar_cons_perc,
         calorie_intake_male = calorie_intake)

colnames(GDD_male_2018_FIN_TOTAL)
colnames(SC_SB_MFA)


#####################################################

# Export df SC_SB_MFA - with consumption
write.xlsx(SC_SB_MFA, "SC_SB_MFA__with_CONS_1.11.24.xlsx")



##########################################################
# Aggregation calculation for building Sankey diagram:
##########################################################
# Aggregation - WORLD

# Select all columns except "Year" and "Column1"
numeric_columns <- SC_SB_MFA[, !(names(SC_SB_MFA) %in% c("Year", "Column1"))]

# Apply the sum function to each numeric column in the selected subset
column_sums <- sapply(numeric_columns, function(x) if(is.numeric(x)) sum(x, na.rm = TRUE) else NA)

# Create a new data frame from the column sums
sum_columns_world <- data.frame(Category = names(column_sums), Sum = column_sums)

# Remove rows with NA (non-numeric columns)
sum_columns_world <- sum_columns_world[!is.na(sum_columns_world$Sum), ]

# Print the new data frame
print(sum_columns_world)

# Export df sum_columns_world
write.xlsx(sum_columns_world, "sum_columns_world_1.11.24.xlsx")

###########################################################

# Building World aggregation df for MFA
# Extract the values for the relevant categories from the df sum_columns_world
# non centrifugal sugar cane (ncs) 
NCS_cane_pro <- sum_columns_world[sum_columns_world$Category == "NCS_pro", "Sum"]
NCS_cane_import <- sum_columns_world[sum_columns_world$Category == "NCS_import", "Sum"]
NCS_cane_export <- sum_columns_world[sum_columns_world$Category == "NCS_export", "Sum"]
NCS_cane_stock <- sum_columns_world[sum_columns_world$Category == "NCS_stock", "Sum"]
# sum
NCS_cane_total <- sum(NCS_cane_pro, NCS_cane_import, -NCS_cane_export, NCS_cane_stock, na.rm = TRUE)

# Raw sugar cane & beet
Raw_sugar_cane_pro <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_pro", "Sum"]
Raw_sugar_cane_import <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_import", "Sum"]
Raw_sugar_cane_export <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_export", "Sum"]
Raw_sugar_cane_stock <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_stock", "Sum"]
Raw_sugar_beet_pro <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_beet_pro", "Sum"]
Raw_sugar_beet_import <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_beet_import", "Sum"]
Raw_sugar_beet_export <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_beet_export", "Sum"]
Raw_sugar_beet_stock <- sum_columns_world[sum_columns_world$Category == "Raw_sugar_beet_stock", "Sum"]
Raw_sugar_cane_NM_export<- sum_columns_world[sum_columns_world$Category == "Raw_sugar_NM_export", "Sum"]
Raw_sugar_cane_NM_import<- sum_columns_world[sum_columns_world$Category == "Raw_sugar_NM_import", "Sum"]
Raw_sugar_beet_NM_export<- sum_columns_world[sum_columns_world$Category == "Raw_sugar_beet_NM_export", "Sum"]
Raw_sugar_beet_NM_import<- sum_columns_world[sum_columns_world$Category == "Raw_sugar_beet_NM_import", "Sum"]
#sum
Raw_sugar_cane_total <- sum(Raw_sugar_cane_pro, Raw_sugar_cane_import, -Raw_sugar_cane_export, Raw_sugar_cane_stock,-Raw_sugar_cane_NM_export,Raw_sugar_cane_NM_import, na.rm = TRUE)
Raw_sugar_beet_total <- sum(Raw_sugar_beet_pro, Raw_sugar_beet_import, -Raw_sugar_beet_export, Raw_sugar_beet_stock,-Raw_sugar_beet_NM_export,Raw_sugar_beet_NM_import, na.rm = TRUE)


# Molasses cane & beet
Molasses_cane_pro<- sum_columns_world[sum_columns_world$Category == "Molasses_pro", "Sum"]
Molasses_cane_import<- sum_columns_world[sum_columns_world$Category == "Molasses_import", "Sum"]
Molasses_cane_export<- sum_columns_world[sum_columns_world$Category == "Molasses_export", "Sum"]
Molasses_cane_stock<- sum_columns_world[sum_columns_world$Category == "Molasses_stock", "Sum"]
Molasses_beet_pro<- sum_columns_world[sum_columns_world$Category == "Molasses_beet_pro", "Sum"]
Molasses_beet_import<- sum_columns_world[sum_columns_world$Category == "Molasses_beet_import", "Sum"]
Molasses_beet_export<- sum_columns_world[sum_columns_world$Category == "Molasses_beet_export", "Sum"]
Molasses_beet_stock<- sum_columns_world[sum_columns_world$Category == "Molasses_beet_stock", "Sum"]
Molasses_cane_NM_import<- sum_columns_world[sum_columns_world$Category == "Molasses_cane_NM_import", "Sum"]
Molasses_cane_NM_export<- sum_columns_world[sum_columns_world$Category == "Molasses_cane_export", "Sum"]
Molasses_not_cane_NM_import<- sum_columns_world[sum_columns_world$Category == "Molasses_not_cane_NM_import", "Sum"]
Molasses_not_cane_NM_export<- sum_columns_world[sum_columns_world$Category == "Molasses_not_cane_export", "Sum"]
# sum
Molasses_cane_total <- sum(Molasses_cane_pro, Molasses_cane_import, -Molasses_cane_export, Molasses_cane_stock,Molasses_cane_NM_import,-Molasses_cane_NM_export, na.rm = TRUE)
Molasses_beet_total <- sum(Molasses_beet_pro, Molasses_beet_import, -Molasses_beet_export, Molasses_beet_stock,Molasses_not_cane_NM_import,-Molasses_not_cane_NM_export, na.rm = TRUE)

# check if result is a vector:
# is.vector(Molasses_not_cane_NM_export)
# check the vector value:
# c(Molasses_not_cane_NM_export)
# c(Molasses_beet_stock)

# Creating SCB_MFA_world df = World aggregation
SCB_MFA_world <- data.frame(
  NCS_cane_total = NCS_cane_total,
  Raw_sugar_cane_total = Raw_sugar_cane_total,
  Raw_sugar_beet_total = Raw_sugar_beet_total,
  Molasses_cane_total = Molasses_cane_total,
  Molasses_beet_total = Molasses_beet_total
)

print(SCB_MFA_world)

# Refined sugar cane & beet
Refined_sugar_cane_pro <- sum_columns_world[sum_columns_world$Category == "RefS_pro", "Sum"]
Refined_sugar_cane_import <- sum_columns_world[sum_columns_world$Category == "RefS_import", "Sum"]
Refined_sugar_cane_export <- sum_columns_world[sum_columns_world$Category == "RefS_export", "Sum"]
Refined_sugar_cane_stock <- sum_columns_world[sum_columns_world$Category == "RefS_stock", "Sum"]
Refined_sugar_beet_pro <- sum_columns_world[sum_columns_world$Category == "RefS_beet_pro", "Sum"]
Refined_sugar_beet_import <- sum_columns_world[sum_columns_world$Category == "RefS_beet_import", "Sum"]
Refined_sugar_beet_export <- sum_columns_world[sum_columns_world$Category == "RefS_beet_export", "Sum"]
Refined_sugar_beet_stock <- sum_columns_world[sum_columns_world$Category == "RefS_beet_stock", "Sum"]
Refined_sugar_NM_export<- sum_columns_world[sum_columns_world$Category == "RefS_NM_export", "Sum"]
Refined_sugar_NM_import<- sum_columns_world[sum_columns_world$Category == "RefS_NM_import", "Sum"]
#sum
Refined_sugar_cane_total <- sum(Refined_sugar_cane_pro, Refined_sugar_cane_import, -Refined_sugar_cane_export, Refined_sugar_cane_stock, na.rm = TRUE)
Refined_sugar_beet_total <- sum(Refined_sugar_beet_pro, Refined_sugar_beet_import, -Refined_sugar_beet_export, Refined_sugar_beet_stock, na.rm = TRUE)
Refined_sugar_NM <- sum(Refined_sugar_NM_import, -Refined_sugar_NM_export, na.rm = TRUE)

# Add the new sum result to the existing SCB_MFA_world df as a new column
SCB_MFA_world$Refined_sugar_cane_total <- Refined_sugar_cane_total
SCB_MFA_world$Refined_sugar_beet_total <- Refined_sugar_beet_total
SCB_MFA_world$Refined_sugar_NM <- Refined_sugar_NM

print(SCB_MFA_world)

##################################
# Refined sugar products cane&beet: Sugar & Syrups nes & Sugar Confectionery & beverages non-alcoholic & Refined Sugar flavoured / coloured
# Sugar & Syrups nes
Refined_Syrups_nes_cane_pro <- sum_columns_world[sum_columns_world$Category == "SSnes_pro", "Sum"]
Refined_Syrups_nes_cane_import <- sum_columns_world[sum_columns_world$Category == "SSnes_import", "Sum"]
Refined_Syrups_nes_cane_export <- sum_columns_world[sum_columns_world$Category == "SSnes_export", "Sum"]
Refined_Syrups_nes_cane_stock <- sum_columns_world[sum_columns_world$Category == "SSnes_stock", "Sum"]
Refined_Syrups_nes_beet_pro <- sum_columns_world[sum_columns_world$Category == "SSnes_beet_pro", "Sum"]
Refined_Syrups_nes_beet_import <- sum_columns_world[sum_columns_world$Category == "SSnes_beet_import", "Sum"]
Refined_Syrups_nes_beet_export <- sum_columns_world[sum_columns_world$Category == "SSnes_beet_export", "Sum"]
Refined_Syrups_nes_beet_stock <- sum_columns_world[sum_columns_world$Category == "SSnes_beet_stock", "Sum"]
Refined_Syrups_nes_NM_export<- sum_columns_world[sum_columns_world$Category == "SSnes_NM_export", "Sum"]
Refined_Syrups_nes_NM_import<- sum_columns_world[sum_columns_world$Category == "SSnes_NM_import", "Sum"]
#sum
Refined_Syrups_nes_cane_total <- sum(Refined_Syrups_nes_cane_pro, Refined_Syrups_nes_cane_import, -Refined_Syrups_nes_cane_export, Refined_Syrups_nes_cane_stock, na.rm = TRUE)
Refined_Syrups_nes_beet_total <- sum(Refined_Syrups_nes_beet_pro, Refined_Syrups_nes_beet_import, -Refined_Syrups_nes_beet_export, Refined_Syrups_nes_beet_stock, na.rm = TRUE)
Refined_Syrups_nes_NM_total <- sum(Refined_Syrups_nes_NM_import,-Refined_Syrups_nes_NM_export, na.rm = TRUE)

# Add the new sum result to the existing SCB_MFA_world df as a new column
SCB_MFA_world$Refined_Syrups_nes_cane_total <- Refined_Syrups_nes_cane_total
SCB_MFA_world$Refined_Syrups_nes_beet_total <- Refined_Syrups_nes_beet_total
SCB_MFA_world$Refined_Syrups_nes_NM_total <- Refined_Syrups_nes_NM_total


################################
# Sugar Confectionery
Refined_confectionery_cane_pro <- sum_columns_world[sum_columns_world$Category == "SConfect_pro", "Sum"]
Refined_confectionery_cane_import <- sum_columns_world[sum_columns_world$Category == "SConfect_import", "Sum"]
Refined_confectionery_cane_export <- sum_columns_world[sum_columns_world$Category == "SConfect_export", "Sum"]
Refined_confectionery_cane_stock <- sum_columns_world[sum_columns_world$Category == "SConfect_stock", "Sum"]
Refined_confectionery_beet_pro <- sum_columns_world[sum_columns_world$Category == "SConfect_beet_pro", "Sum"]
Refined_confectionery_beet_import <- sum_columns_world[sum_columns_world$Category == "SConfect_beet_import", "Sum"]
Refined_confectionery_beet_export <- sum_columns_world[sum_columns_world$Category == "SConfect_beet_export", "Sum"]
Refined_confectionery_beet_stock <- sum_columns_world[sum_columns_world$Category == "SConfect_beet_stock", "Sum"]
Refined_confectionery_NM_export<- sum_columns_world[sum_columns_world$Category == "SConfect_NM_export", "Sum"]
Refined_confectionery_NM_import<- sum_columns_world[sum_columns_world$Category == "SConfect_NM_import", "Sum"]
#sum
Refined_confectionery_cane_total <- sum(Refined_confectionery_cane_pro, Refined_confectionery_cane_import, -Refined_confectionery_cane_export, Refined_confectionery_cane_stock, na.rm = TRUE)
Refined_confectionery_beet_total <- sum(Refined_confectionery_beet_pro, Refined_confectionery_beet_import, -Refined_confectionery_beet_export, Refined_confectionery_beet_stock, na.rm = TRUE)
Refined_confectionery_NM_total <- sum(Refined_confectionery_NM_import,-Refined_confectionery_NM_export,na.rm = TRUE)

# Add the new sum result to the existing SCB_MFA_world df as a new column
SCB_MFA_world$Refined_confectionery_cane_total <- Refined_confectionery_cane_total
SCB_MFA_world$Refined_confectionery_beet_total <- Refined_confectionery_beet_total
SCB_MFA_world$Refined_confectionery_NM_total <- Refined_confectionery_NM_total

#############################################
# Refined Sugar flavoured / coloured
Refined_sugar_flavoured_cane_import <- sum_columns_world[sum_columns_world$Category == "Sflav_import", "Sum"]
Refined_sugar_flavoured_cane_export <- sum_columns_world[sum_columns_world$Category == "Sflav_export", "Sum"]
Refined_sugar_flavoured_beet_import <- sum_columns_world[sum_columns_world$Category == "Sflav_beet_import", "Sum"]
Refined_sugar_flavoured_beet_export <- sum_columns_world[sum_columns_world$Category == "Sflav_beet_export", "Sum"]
Refined_sugar_flavoured_NM_import <- sum_columns_world[sum_columns_world$Category == "Sflav_NM_import", "Sum"]
Refined_sugar_flavoured_NM_export <- sum_columns_world[sum_columns_world$Category == "Sflav_NM_export", "Sum"]

#sum
Refined_sugar_flavoured_cane_total <- sum(Refined_sugar_flavoured_cane_import, -Refined_sugar_flavoured_cane_export,na.rm = TRUE)
Refined_sugar_flavoured_beet_total <- sum(Refined_sugar_flavoured_beet_import, -Refined_sugar_flavoured_beet_export,na.rm = TRUE)
Refined_sugar_flavoured_NM_total <- sum(Refined_sugar_flavoured_NM_import, -Refined_sugar_flavoured_NM_export,na.rm = TRUE)


# Add the new sum result to the existing SCB_MFA_world df as a new column
SCB_MFA_world$Refined_sugar_flavoured_cane_total <- Refined_sugar_flavoured_cane_total
SCB_MFA_world$Refined_sugar_flavoured_beet_total <- Refined_sugar_flavoured_beet_total
SCB_MFA_world$Refined_sugar_flavoured_NM_total <- Refined_sugar_flavoured_NM_total


######################
#Beverages non-alcoholic
Bev_non_alco_cane_pro <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_pro", "Sum"]
Bev_non_alco_cane_import <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_import", "Sum"]
Bev_non_alco_cane_export <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_export", "Sum"]
Bev_non_alco_cane_stock <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_stock", "Sum"]
Bev_non_alco_beet_pro <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_beet_pro", "Sum"]
Bev_non_alco_beet_import <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_beet_import", "Sum"]
Bev_non_alco_beet_export <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_beet_export", "Sum"]
Bev_non_alco_beet_stock <- sum_columns_world[sum_columns_world$Category == "Bev_non_alco_beet_stock", "Sum"]
Bev_non_alco_NM_import <- sum_columns_world[sum_columns_world$Category == "Bev_alco_NM_import", "Sum"]
Bev_non_alco_NM_export <- sum_columns_world[sum_columns_world$Category == "Bev_alco_NM_export", "Sum"]

#sum
Bev_non_alco_cane_total <- sum(Bev_non_alco_cane_pro,Bev_non_alco_cane_import, -Bev_non_alco_cane_export, Bev_non_alco_cane_stock,na.rm = TRUE)
Bev_non_alco_beet_total <- sum(Bev_non_alco_beet_pro,Bev_non_alco_beet_import, -Bev_non_alco_beet_export,Bev_non_alco_beet_stock, na.rm = TRUE)
Bev_non_alco_NM_total <- sum(Bev_non_alco_NM_import, -Bev_non_alco_NM_export,na.rm = TRUE)


# Add the new sum result to the existing SCB_MFA_world df as a new column
SCB_MFA_world$Bev_non_alco_cane_total <- Bev_non_alco_cane_total
SCB_MFA_world$Bev_non_alco_beet_total <- Bev_non_alco_beet_total
SCB_MFA_world$Bev_non_alco_NM_total <- Bev_non_alco_NM_total


#############################################

# Consumption & pop
Total_pop_male <- sum_columns_world[sum_columns_world$Category == "Total_pop_male", "Sum"]
Total_pop_female <- sum_columns_world[sum_columns_world$Category == "Total_pop_female", "Sum"]
Total_Sugar_cons_year_ton_female <- sum_columns_world[sum_columns_world$Category == "Total_Sugar_cons_year_ton_female", "Sum"]
Total_Sugar_cons_year_ton_male <- sum_columns_world[sum_columns_world$Category == "Total_Sugar_cons_year_ton_male", "Sum"]

#sum
Total_pop <- sum(Total_pop_male, Total_pop_female, na.rm = TRUE)
Total_consumption <- sum(Total_Sugar_cons_year_ton_female, Total_Sugar_cons_year_ton_male, na.rm = TRUE)

# Add the new sum result to the existing SCB_MFA_world df as a new column
SCB_MFA_world$Total_pop <- Total_pop
SCB_MFA_world$Total_consumption <- Total_consumption

print(SCB_MFA_world)
view(SCB_MFA_world)

# Export df SCB_MFA_world
write.xlsx(SCB_MFA_world, "SCB_MFA_world_supply_vs_cons_1.11.24.xlsx")


###################################################################################

# Fixing the refined sugar - 21.8.24
View(CROPS_2018)

# Filter the Refined sugar which defined as "parent"
Refined_sugar_Parent_2018 <- filter(CROPS_2018, Parent_name == "Refined sugar")
view(Refined_sugar_Parent_2018)

# Export df 
write.xlsx(Refined_sugar_Parent_2018, "Refined_sugar_Parent_2018.xlsx")

# Filter the Refined sugar which defined as "Child_name"
Refined_sugar_Child_name_2018 <- filter(CROPS_2018, Child_name == "Refined sugar")
view(Refined_sugar_Child_name_2018)

# Export df 
write.xlsx(Refined_sugar_Child_name_2018, "Refined_sugar_Child_name_2018.xlsx")

view(refined_sugar_cane_FIN)
# Export df 
write.xlsx(refined_sugar_cane_FIN, "refined_sugar_cane_FIN_2018.xlsx")

# Export df 
write.xlsx(Refined_sugar_beet_by_country, "refined_sugar_beet_FIN_2018.xlsx")


# Export df 
write.xlsx(Bev_non_alco_by_country_sugar_cane_FIN, "Bev_non_alco_sugar_cane_2018.xlsx")
write.xlsx(Bev_non_alco_by_country_sugar_beet_FIN, "Bev_non_alco_sugar_beet_2018.xlsx")
write.xlsx(Bev_alco_by_country_sugar_beet_FIN, "Bev_alco_sugar_beet_2018.xlsx")
write.xlsx(Bev_alco_by_country_sugar_cane_FIN, "Bev_alco_sugar_cane_2018.xlsx")



#########################################################################################

#Trade figure - Refined sugar 2018 by continents and region: 
# 1. Data arrange

view(SC_SB_MFA)
colnames(SC_SB_MFA)

# creating df of the countries and region - based on selected columns from SC_SB_MFA 
Countries_and_region <- SC_SB_MFA %>% 
  select(UN_countries, Countries_code, Region, Continent)

colnames(Countries_and_region)
view(Countries_and_region)

# Read data from excel file - Refined sugar - original data for the trade fig
ReS_TRADE <- read_excel("Refined sugar - original data for the trade fig..xlsx")
colnames(ReS_TRADE)
str(ReS_TRADE)
View(ReS_TRADE)

# Filter the 2018 trade
ReS_TRADE_2018 <- filter(ReS_TRADE, Year == 2018)
colnames(ReS_TRADE_2018)
str(ReS_TRADE_2018)
view(ReS_TRADE_2018)


#Rename column
Countries_and_region <- Countries_and_region %>%
  rename(`Exporter ISO3` = Countries_code)


# Merge new region into the trade data: Exporter
ReS_TRADE_2018 <- ReS_TRADE_2018 %>% 
  # Join with exporter data
  left_join(Countries_and_region, by = "Exporter ISO3") %>%
  # Rename columns related to the exporter
  rename(Exporter_region = Region,
         Exporter_continent = Continent,
         Exporter_UN_countries = UN_countries)%>%
  # Reorder columns to place new columns after 'Exporter'
  select(Exporter, Exporter_region, Exporter_continent, Exporter_UN_countries, everything())


colnames(ReS_TRADE_2018)
view(ReS_TRADE_2018)

#Rename column & duplicate
Countries_and_region <- Countries_and_region %>%
  mutate(`Importer ISO3` = `Exporter ISO3`)

Countries_and_region_2 <- Countries_and_region %>%
  select(-`Exporter ISO3`) # delete column

view(Countries_and_region_2)
write.xlsx(Countries_and_region_2, "Countries_and_region_2.xlsx")

# Merge new region into the trade data: Importer
ReS_TRADE_2018 <- ReS_TRADE_2018 %>% 
  # Join with importer data
  left_join(Countries_and_region_2, by = "Importer ISO3") %>%
  # Rename columns related to the exporter
  rename(Importer_region = Region,
         Importer_continent = Continent,
         Importer_UN_countries = UN_countries)

colnames(ReS_TRADE_2018)

  # Reorder columns 
ReS_TRADE_2018 <- ReS_TRADE_2018 %>%
  select(Exporter,Exporter_region,Exporter_continent, Exporter_UN_countries,'Exporter ISO3', Importer, Importer_region, Importer_continent, Importer_UN_countries,'Importer ISO3', Resource,Year, 'Value (1000USD)','Weight (1000kg)')

view(ReS_TRADE_2018)

# Filter the data frame to remove rows with NA values in specific columns
ReS_TRADE_2018_final <- ReS_TRADE_2018 %>%
  # Use filter() to keep only rows where both conditions are true
  filter(
    # Check if 'Exporter_region' is not NA
    !is.na(Exporter_region) & 
      # Check if 'Importer_region' is not NA
      !is.na(Importer_region) &
      # Check if 'Weight (1000kg)' is not NA
      !is.na('Weight (1000kg)')
)

write.xlsx(ReS_TRADE_2018, "Refined_Sugar_TRADE_2018.xlsx")

# Extract rows where 'Importer' contains "Areas, nes"
ReS_TRADE_2018_no_importer <- ReS_TRADE_2018[grepl("Areas, nes", ReS_TRADE_2018$Importer), ]

# Extract rows where 'Exporter' contains "Areas, nes"
ReS_TRADE_2018_no_exporter <- ReS_TRADE_2018[grepl("Areas, nes", ReS_TRADE_2018$Exporter), ]

# Extract rows where 'Importer' contains "Other Asia, nes"
ReS_TRADE_2018_no_importer_Asia <- ReS_TRADE_2018[grepl("Other Asia, nes", ReS_TRADE_2018$Importer), ]

# Extract rows where 'Exporter' contains "Other Asia, nes"
ReS_TRADE_2018_no_exporter_Asia <- ReS_TRADE_2018[grepl("Other Asia, nes", ReS_TRADE_2018$Exporter), ]

colnames(ReS_TRADE_2018_no_importer)

# Aggregate 'Weight (1000kg)' by 'Exporter_continent' - Finding the mass of Refined sugar which we know who is the exporter but not who the importer
ReS_TRADE_2018_no_importer_continent <- aggregate(`Weight (1000kg)` ~ Exporter_continent, data = ReS_TRADE_2018_no_importer, FUN = sum)

# Aggregate 'Weight (1000kg)' by 'Importer_continent' - Finding the mass of Refined sugar which we know who is the importer but not who the exporter
ReS_TRADE_2018_no_exporter_continent <- aggregate(`Weight (1000kg)` ~ Importer_continent, data = ReS_TRADE_2018_no_exporter, FUN = sum)

# ASIA - Aggregate 'Weight (1000kg)' by 'Exporter_continent' - Finding the mass of Refined sugar which we know who is the exporter but not who the importer
ReS_TRADE_2018_no_importer_Asia_continent <- aggregate(`Weight (1000kg)` ~ Exporter_continent, data = ReS_TRADE_2018_no_importer_Asia, FUN = sum)

# ASIA - Aggregate 'Weight (1000kg)' by 'Importer_continent' - Finding the mass of Refined sugar which we know who is the importer but not who the exporter
ReS_TRADE_2018_no_exporter_Asia_continent <- aggregate(`Weight (1000kg)` ~ Importer_continent, data = ReS_TRADE_2018_no_exporter_Asia, FUN = sum)


# Filtering out rows containing "Other Asia, nes" or "Areas, nes"
ReS_TRADE_2018_final <- ReS_TRADE_2018_final %>%
  filter(!grepl("Other Asia, nes", Exporter) & !grepl("Other Asia, nes", Importer) &
           !grepl("Areas, nes", Exporter) & !grepl("Areas, nes", Importer))

view(ReS_TRADE_2018_final)

# Filtering out rows where 'Weight (1000kg)' is NA
ReS_TRADE_2018_final <- ReS_TRADE_2018_final %>%
  filter(!is.na(`Weight (1000kg)`))

# Create df that summarizes the total weight for each exporter-importer region pair
ReS_TRADE_2018_sum_by_region<- ReS_TRADE_2018_final %>%
  group_by(`Exporter_region`, `Importer_region`) %>%  # Grouping data by Exporter region and Importer region
  summarize(Total_Weight_1000kg = sum(`Weight (1000kg)`, na.rm = TRUE))  # Summarizing total weight for each group
view(ReS_TRADE_2018_sum_by_region)

# Filter the data frame to remove rows with NA values in specific columns
ReS_TRADE_2018_sum_by_region <- ReS_TRADE_2018_sum_by_region %>%
  # Use filter() to keep only rows where both conditions are true
  filter(
    # Check if 'Exporter_region' is not NA
    !is.na(Exporter_region) & 
      # Check if 'Importer_region' is not NA
      !is.na(Importer_region)
  )


# Create df that summarizes the total weight for each exporter-importer continent pair
ReS_TRADE_2018_sum_by_continent<- ReS_TRADE_2018_final %>%
  group_by(`Exporter_continent`, `Importer_continent`) %>%  # Grouping data by Exporter continent and Importer continent
  summarize(Total_Weight_1000kg = sum(`Weight (1000kg)`, na.rm = TRUE))  # Summarizing total weight for each group
view(ReS_TRADE_2018_sum_by_continent)

# Filter the data frame to remove rows with NA values in specific columns
ReS_TRADE_2018_sum_by_continent <- ReS_TRADE_2018_sum_by_continent %>%
  # Use filter() to keep only rows where both conditions are true
  filter(
    # Check if 'Exporter_continent' is not NA
    !is.na(Exporter_continent) & 
      # Check if 'Importer_continent' is not NA
      !is.na(Importer_continent)
  )

# Export df 
write.xlsx(ReS_TRADE_2018_sum_by_region, "ReS_TRADE_2018_sum_by_region.xlsx")
write.xlsx(ReS_TRADE_2018_sum_by_continent, "ReS_TRADE_2018_sum_by_continent.xlsx")

write.xlsx(ReS_TRADE_2018_no_importer_continent, "ReS_TRADE_2018_no_importer_continent.xlsx")
write.xlsx(ReS_TRADE_2018_no_importer, "ReS_TRADE_2018_no_importer.xlsx")

write.xlsx(ReS_TRADE_2018_no_exporter_continent, "ReS_TRADE_2018_no_exporter_continent.xlsx")
write.xlsx(ReS_TRADE_2018_no_exporter, "ReS_TRADE_2018_no_exporter.xlsx")

write.xlsx(ReS_TRADE_2018_no_importer_Asia_continent, "ReS_TRADE_2018_no_importer_Asia_continent.xlsx")
write.xlsx(ReS_TRADE_2018_no_importer_Asia, "ReS_TRADE_2018_no_importer_Asia.xlsx")

write.xlsx(ReS_TRADE_2018_no_exporter_Asia_continent, "ReS_TRADE_2018_no_exporter_Asia_continent.xlsx")
write.xlsx(ReS_TRADE_2018_no_exporter_Asia, "ReS_TRADE_2018_no_exporter_Asia.xlsx")



###############################
 # 2. Data preparation before Chord diagram - by continent:
view(ReS_TRADE_2018_sum_by_continent)
colnames(ReS_TRADE_2018_sum_by_continent)

# Renaming the continent names in the specified columns
ReS_TRADE_2018_sum_by_continent <- ReS_TRADE_2018_sum_by_continent %>%
  mutate(Exporter_continent = str_replace_all(Exporter_continent, 
                                              c("Central America" = "America - Central",
                                                "North America" = "America - North",
                                                "South America" = "America - South")),
         Importer_continent = str_replace_all(Importer_continent, 
                                              c("Central America" = "America - Central",
                                                "North America" = "America - North",
                                                "South America" = "America - South")))


# Pivot the data to create a matrix format suitable for a chord diagram
continent_matrix <- ReS_TRADE_2018_sum_by_continent %>%
  pivot_wider(names_from = Importer_continent, values_from = Total_Weight_1000kg, values_fill = 0) %>%
  column_to_rownames(var = "Exporter_continent")

# Convert to matrix format for circlize
continent_matrix_final <- as.matrix(continent_matrix)

view(continent_matrix_final)
colnames(continent_matrix_final)

# Convert values from tons to Kilotons (kt) + round the value
continent_matrix_final_kt <- round(continent_matrix_final / 1e3, 2)


view(continent_matrix_final_kt)

# Export df 
write.xlsx(continent_matrix_final_kt, "continent_matrix_final_kt.xlsx")
write.xlsx(continent_matrix_final, "continent_matrix_final_ton.xlsx")


# I took the code from: https://bioinfo4all.wordpress.com/2021/03/13/tutorial-7-how-to-do-chord-diagram-using-r/

# Define colors for each continent
continent_colors <- c(
  "Oceania" = "#85929e",      
  "Asia" = "#FA8072",         
  "Eurasia" = "#f896e6",      
  "Europe" = "#8e44ad",       
  "Africa" = "#f4d03f",       
  "America - North" = "#2471a3",
  "America - Central" = "#77e7e1",  
  "America - South" = "#62c7e7" 
)

# Check row and column names of the matrix
print(union(rownames(continent_matrix_final_kt), colnames(continent_matrix_final_kt)))

continent_order <- c(
  "Asia",
  "Oceania",
  "Africa",
  "America - South",
  "America - Central",
  "America - North",
  "Europe",
  "Eurasia"
)

# Open the JPEG device before creating the plot
jpeg(filename = 'continent_chord_plot.jpeg', width = 10, height = 8, units = "in", res = 500)

# Create the chord diagram with the specified order
chordDiagram(continent_matrix_final_kt, grid.col = continent_colors, order = continent_order,
             annotationTrack = "grid", preAllocateTracks = 1, transparency = 0.5) # Adjust transparency (0 = fully opaque, 1 = fully transparent)

# Add labels and axis on a new track for each sector
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  # Print continent names
  circos.text(mean(xlim), ylim[1] + 3, sector.name,  # Adjusted distance from the diagram
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 1.2)
  
  # Set ticks to cover the entire possible range, not just the data range
  ticks <- seq(from = 0, to = max(c(12000, max(xlim))), by = 1000)  # Ensure ticks go up to at least 20,000
  
  
  # Draw the complete axis line with ticks for each sector
  circos.axis(h = "top", 
              labels.cex = 0.5, 
              major.tick.percentage = 0.2, 
              major.at = ticks,  # Use fixed ticks up to a high value
              labels = function(x) formatC(x, format = "f", big.mark = ",", digits = 0),  # Format labels with commas
              sector.index = sector.name, 
              track.index = 2)
}, bg.border = NA)

# Add a title to the plot using title()
#title("Flow of Refined Sugar by Continents (Trade)", line = -2, cex.main = 1.5) # Adjust line to position the title correctly


# Use grid.text() to add a title below the plot
#grid.text("Global trade flow of refined sugar by continents in 2018 (kt)", 
#x = unit(0.5, "npc"), y = unit(0.02, "npc"), # Position below the figure
# gp = gpar(fontsize = 20))

# Close the JPEG device to finalize the plot
dev.off()




################################################################
# The top 10 exporter and top 10 importer from each continent:

view(ReS_TRADE_2018_final)
colnames(ReS_TRADE_2018_final)

# 1.Find the top 10 exporters by continent and their importing countries
top_exporters <- ReS_TRADE_2018_final %>%
  group_by(Exporter_continent, Exporter, Exporter_region, Importer) %>%
  summarise(Total_Value = sum(`Weight (1000kg)`), .groups = 'drop') %>%
  arrange(Exporter_continent, desc(Total_Value)) %>%
  group_by(Exporter_continent) %>%
  slice_head(n = 10)

# 2. Find the top 10 importers by continent and their exporting countries
top_importers <- ReS_TRADE_2018_final %>%
  group_by(Importer_continent, Importer, Importer_region, Exporter) %>%
  summarise(Total_Value = sum(`Weight (1000kg)`), .groups = 'drop') %>%
  arrange(Importer_continent, desc(Total_Value)) %>%
  group_by(Importer_continent) %>%
  slice_head(n = 10)

# Combine the top exporters and top importers into one DataFrame for analysis
top_trade_2018 <- bind_rows(top_exporters, top_importers)

# Display the combined DataFrame
view(top_trade_2018)

# Export df 
write.xlsx(top_trade_2018, "top_trade_2018.xlsx")


#######################################################################
# Chord diagram - PDF format

# Check row and column names of the matrix
print(union(rownames(continent_matrix_final_kt), colnames(continent_matrix_final_kt)))

continent_order <- c(
  "Asia",
  "Oceania",
  "Africa",
  "America - South",
  "America - Central",
  "America - North",
  "Europe",
  "Eurasia"
)

# Open the PDF device before creating the plot
pdf(file = 'continent_chord_plot.pdf', width = 10, height = 8)

# Create the chord diagram with the specified order
chordDiagram(continent_matrix_final_kt, grid.col = continent_colors, order = continent_order,
             annotationTrack = "grid", preAllocateTracks = 1, transparency = 0.5)

# Add labels and axis on a new track for each sector
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
    # Print continent names
  circos.text(mean(xlim), ylim[1] + 3, sector.name,  # Adjusted distance from the diagram
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 1.2)
  
  # Set ticks to cover the entire possible range, not just the data range
  ticks <- seq(from = 0, to = max(c(12000, max(xlim))), by = 1000)  # Ensure ticks go up to at least 20,000
  
  # Draw the complete axis line with ticks for each sector
  circos.axis(h = "top", 
              labels.cex = 0.5, 
              major.tick.percentage = 0.2, 
              major.at = ticks,  # Use fixed ticks up to a high value
              labels = function(x) formatC(x, format = "f", big.mark = ",", digits = 0),  # Format labels with commas
              sector.index = sector.name, 
              track.index = 2)
}, bg.border = NA)

# Use grid.text() to add a title below the plot
#grid.text("Global trade flow of refined sugar by continents in 2018 (kt)", 
        #  x = unit(0.5, "npc"), y = unit(0.02, "npc"), # Position below the figure
        #  gp = gpar(fontsize = 20))

# Close the PDF device to finalize the plot
dev.off()

################################################


# Top 10 exporters regardless of continent
top_exporters_overall <- ReS_TRADE_2018_final %>%
  group_by(Exporter) %>%
  summarise(Total_Export_Value = sum(`Weight (1000kg)`), .groups = 'drop') %>%
  arrange(desc(Total_Export_Value)) %>%
  slice_head(n = 10)

# Top 10 importers regardless of continent
top_importers_overall <- ReS_TRADE_2018_final %>%
  group_by(Importer) %>%
  summarise(Total_Import_Value = sum(`Weight (1000kg)`), .groups = 'drop') %>%
  arrange(desc(Total_Import_Value)) %>%
  slice_head(n = 10)


# Combine the top exporters and importers into one DataFrame for analysis
top_trade_overall <- bind_rows(
  top_exporters_overall %>% mutate(Role = "Exporter"),
  top_importers_overall %>% mutate(Role = "Importer")
)

# Display the combined DataFrame
View(top_trade_overall)

# Export df 
write.xlsx(top_trade_overall, "top_trade_overall_2018_24-9-4.xlsx")

########################################

# Horizontal bar chart
# Load required libraries

# Split the data into two separate dataframes: one for exporters, one for importers
top_exporters <- top_trade_overall %>% filter(Role == "Exporter")
top_importers <- top_trade_overall %>% filter(Role == "Importer")

# Create a horizontal bar chart for exporters
exporters_plot <- ggplot(top_exporters, aes(x = reorder(Exporter, Total_Export_Value), y = Total_Export_Value, fill = Role)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Exporters in 2018",
    x = "Country",
    y = "Total Export Value (1000 kg)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  scale_fill_manual(values = "blue") +  # Customize color for exporters
  coord_flip()

# Create a horizontal bar chart for importers
importers_plot <- ggplot(top_importers, aes(x = reorder(Importer, Total_Import_Value), y = Total_Import_Value, fill = Role)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Importers in 2018",
    x = "Country",
    y = "Total Import Value (1000 kg)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  scale_fill_manual(values = "red") +  # Customize color for importers
  coord_flip()

# Display the plots
print(exporters_plot)
print(importers_plot)

###########################################################

# Treemap figure - exporter and importer
# Split the data into two separate dataframes: one for exporters, one for importers
top_exporters <- top_trade_overall %>% filter(Role == "Exporter")
top_importers <- top_trade_overall %>% filter(Role == "Importer")


# Convert export and import values to megatons (1 megaton = 1,000,000 tons)
top_exporters <- top_trade_overall %>%
  filter(Role == "Exporter") %>%
  mutate(Total_Export_Megatons = Total_Export_Value / 1e6)  # Convert to megatons

top_importers <- top_trade_overall %>%
  filter(Role == "Importer") %>%
  mutate(Total_Import_Megatons = Total_Import_Value / 1e6)  # Convert to megatons

# Plot Treemap for Exporters using ggplot2 and treemapify
ggplot(top_exporters, aes(area = Total_Export_Megatons, fill = Total_Export_Megatons, 
                          label = paste(Exporter, "\n", round(Total_Export_Megatons, 2), "Mt"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", size = 15, place = "center", grow = FALSE) + # grow = TRUE: This parameter allows the text size to grow proportionally with the size of the treemap box. 
  scale_fill_gradient(low = "#aeb6bf", high = "#34495e") +
  labs(title = "Refined sugar: top 10 exporters in 2018", fill = "Mt") +
  theme_minimal() +
  theme(
    legend.position = "bottom",         # Position the legend at the bottom
    legend.title = element_text(size = 10),    # Smaller title size for the legend
    legend.text = element_text(size = 8),      # Smaller text size for the legend items
    legend.key.size = unit(0.5, "cm")          # Smaller size for the legend keys
  )

# Plot Treemap for Importers using ggplot2 and treemapify
ggplot(top_importers, aes(area = Total_Import_Megatons, fill = Total_Import_Megatons, 
                          label = paste(Importer, "\n", round(Total_Import_Megatons, 2), "Mt"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white",size = 15, place = "center", grow = FALSE) +
  scale_fill_gradient(low = "#99a3a4", high = "#424949") +
  labs(title = "Refined sugar: top 10 importers in 2018", fill = "Mt") +
  theme_minimal() +
  theme(
    legend.position = "bottom",         # Position the legend at the bottom
    legend.title = element_text(size = 10),    # Smaller title size for the legend
    legend.text = element_text(size = 8),      # Smaller text size for the legend items
    legend.key.size = unit(0.5, "cm")          # Smaller size for the legend keys
  )

# Save the treemap for exporters with specific dimensions
#ggsave("exporters_treemap.png", width = 6, height = 4)  # Adjust width and height as needed

# Save the treemap for importers with specific dimensions
#ggsave("importers_treemap.png", width = 6, height = 4)  # Adjust width and height as needed





###############################
# 3. Data preparation before Chord diagram - by region:
view(ReS_TRADE_2018_sum_by_region)
colnames(ReS_TRADE_2018_sum_by_region)

# Pivot the data to create a matrix format suitable for a chord diagram
region_matrix <- ReS_TRADE_2018_sum_by_region %>%
  pivot_wider(names_from = Importer_region, values_from = Total_Weight_1000kg, values_fill = 0) %>%
  column_to_rownames(var = "Exporter_region")

# Convert to matrix format for circlize
region_matrix_final <- as.matrix(region_matrix)

view(region_matrix_final)
colnames(region_matrix_final)

# Convert values from tons to Kilotons (kt) + round the value
region_matrix_final_kt <- round(region_matrix_final / 1e3, 2)


view(region_matrix_final_kt)
# Export df 
write.xlsx(region_matrix_final, "region_matrix_final.xlsx")
write.xlsx(region_matrix_final_kt, "region_matrix_final_kt.xlsx")


# chord diagram by region - not for use:

# Define base colors for each continent
continent_colors <- c(
  "Oceania" = "#85929e",            # A shade of cyan
  "Asia" = "#FA8072",               # A shade of green
  "Eurasia" = "#f896e6",            # A shade of magenta
  "Europe" = "#8e44ad",             # A shade of yellow
  "Africa" = "#f4d03f",             # A shade of red
  "America - North" = "#2471a3",    # A shade of orange
  "America - Central" = "#77e7e1",  # A shade of blue
  "America - South" = "#62c7e7"     # Blue violet
)

# Assign colors to each region, making lighter or darker shades
regions_colors <- c(
  # Oceania (cyan shades)
  "Oceania" = continent_colors["Oceania"],
  "Melanesia" = adjustcolor(continent_colors["Oceania"], 0.8),
  "Micronesia" = adjustcolor(continent_colors["Oceania"], 0.6),
  "Polynesia" = adjustcolor(continent_colors["Oceania"], 0.4),
  
  # Asia (salmon shades)
  "East Asia" = continent_colors["Asia"],
  "South Asia" = adjustcolor(continent_colors["Asia"], 0.8),
  "Southeast Asia" = adjustcolor(continent_colors["Asia"], 0.6),
  "Middle East - Levant" = adjustcolor(continent_colors["Asia"], 0.5),
  "Middle East - Arabian Peninsula" = adjustcolor(continent_colors["Asia"], 0.4),
  "Middle East - Southwestern Asia" = adjustcolor(continent_colors["Asia"], 0.3),
  "Central Asia" = adjustcolor(continent_colors["Asia"], 0.2),
  
  # Eurasia (magenta shades)
  "Eastern Europe and Northern Asia" = continent_colors["Eurasia"],
  "South Caucasus (Southwestern Asia)" = adjustcolor(continent_colors["Eurasia"], 0.8),
  
  # Europe (purple shades)
  "Northern Europe" = continent_colors["Europe"],
  "Western Europe" = adjustcolor(continent_colors["Europe"], 0.8),
  "Central Europe" = adjustcolor(continent_colors["Europe"], 0.6),
  "Southern Europe" = adjustcolor(continent_colors["Europe"], 0.4),
  "Southeast Europe" = adjustcolor(continent_colors["Europe"], 0.3),
  "Southwestern Europe" = adjustcolor(continent_colors["Europe"], 0.2),
  "Eastern Europe" = adjustcolor(continent_colors["Europe"], 0.1),
  
  # Africa (yellow shades)
  "North Africa" = continent_colors["Africa"],
  "West Africa" = adjustcolor(continent_colors["Africa"], 0.8),
  "Central Africa" = adjustcolor(continent_colors["Africa"], 0.6),
  "East Africa" = adjustcolor(continent_colors["Africa"], 0.4),
  "Southeast Africa" = adjustcolor(continent_colors["Africa"], 0.3),
  "Southern Africa" = adjustcolor(continent_colors["Africa"], 0.2),
  
  # America - North (blue shades)
  "North America" = continent_colors["America - North"],
  
  # America - Central (light blue shades)
  "Caribbean" = continent_colors["America - Central"],
  "Central America" = adjustcolor(continent_colors["America - Central"], 0.8),
  
  # America - South (darker blue shades)
  "South America" = continent_colors["America - South"],
  
  # Oceans (dark turquoise shades)
  "North Atlantic" = adjustcolor("#00CED1", 0.8),      # North Atlantic using dark turquoise
  "South Atlantic Ocean" = adjustcolor("#00CED1", 0.6),# South Atlantic Ocean using lighter turquoise
  "Indian Ocean" = adjustcolor("#00CED1", 0.4)         # Indian Ocean using even lighter turquoise
)

# Open the JPEG device before creating the plot
jpeg(filename = 'region_chord_plot.jpeg', width = 12, height = 12, units = "in", res = 500)

# Create a chord diagram without labels initially
chordDiagram(region_matrix_final_kt, grid.col = regions_colors, annotationTrack = "grid", preAllocateTracks = 1)

# Add labels and axis on a new track for each sector
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  # Print continent names
  circos.text(mean(xlim), ylim[1] + 3, sector.name,  # Adjusted distance from the diagram
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.8)
  
  # Set ticks to cover the entire possible range, not just the data range
  ticks <- seq(from = 0, to = max(c(20000, max(xlim))), by = 1000)  # Ensure ticks go up to at least 20,000
  
  # Draw the complete axis line with ticks for each sector
  circos.axis(h = "top", 
              labels.cex = 0.5, 
              major.tick.percentage = 0.2, 
              major.at = ticks,  # Use fixed ticks up to a high value
              labels = function(x) formatC(x, format = "f", big.mark = ",", digits = 0),  # Format labels with commas
              sector.index = sector.name, 
              track.index = 2)
}, bg.border = NA)

# Close the JPEG device to finalize the plot
dev.off()


############################################################################
#########################################################################################

# Consumption Figure:
# A. Map - sugar consumption per capita (kg) by continent

view(GDD_female_2018_FIN)
view(GDD_male_2018_FIN)
view(GDD_female_2018_FIN_TOTAL)
view(GDD_male_2018_FIN_TOTAL)

write.xlsx(GDD_female_2018_FIN, "GDD_female_2018_FIN_3.6.24.xlsx")
write.xlsx(GDD_male_2018_FIN, "GDD_male_2018_FIN_3.6.24.xlsx")
write.xlsx(GDD_female_2018_FIN_TOTAL, "GDD_female_2018_FIN_TOTAL_3.6.24.xlsx")
write.xlsx(GDD_male_2018_FIN_TOTAL, "GDD_male_2018_FIN_TOTAL_3.6.24.xlsx")

# 1. Total sugar consumption (ton) in 2018 - by country - map fig.
# arranging data:
# Male
colnames(GDD_male_2018_FIN_TOTAL)

# Delete the specified columns
GDD_male_2018_for_fig <- GDD_male_2018_FIN_TOTAL[ , !(names(GDD_male_2018_FIN_TOTAL) %in% c("gender", "sugar_cons_perc", "calorie_intake", "Total_Sugar_cons_day_ton"))]

# Rename columns
names(GDD_male_2018_for_fig)[names(GDD_male_2018_for_fig) == "Total_pop"] <- "Total_pop_male"
names(GDD_male_2018_for_fig)[names(GDD_male_2018_for_fig) == "Total_Sugar_cons_year_ton"] <- "Total_Sugar_cons_year_ton_male"

view(GDD_male_2018_for_fig)

# Female
colnames(GDD_female_2018_FIN_TOTAL)

# Delete the specified columns
GDD_female_2018_for_fig <- GDD_female_2018_FIN_TOTAL[ , !(names(GDD_female_2018_FIN_TOTAL) %in% c("gender", "sugar_cons_perc", "calorie_intake", "Total_Sugar_cons_day_ton"))]

# Rename columns
names(GDD_female_2018_for_fig)[names(GDD_female_2018_for_fig) == "Total_pop"] <- "Total_pop_female"
names(GDD_female_2018_for_fig)[names(GDD_female_2018_for_fig) == "Total_Sugar_cons_year_ton"] <- "Total_Sugar_cons_year_ton_female"

view(GDD_female_2018_for_fig)

# Merge male and female data by the columns "Countries_code" and "UN_countries"
Sugar_cons_2018_by_country <- merge(GDD_female_2018_for_fig, GDD_male_2018_for_fig, by = c("Countries_code", "UN_countries"))

colnames(Sugar_cons_2018_by_country)

# Sum the sugar consumption: male+female
Sugar_cons_2018_by_country$Total_Sugar_cons_year_ton <- Sugar_cons_2018_by_country$Total_Sugar_cons_year_ton_male + Sugar_cons_2018_by_country$Total_Sugar_cons_year_ton_female
# Sum the country pop: male+female
Sugar_cons_2018_by_country$Total_pop <- Sugar_cons_2018_by_country$Total_pop_male + Sugar_cons_2018_by_country$Total_pop_female


# Convert values from tons to Kilotons (kt) + round the value - in a new column
Sugar_cons_2018_by_country$Total_Sugar_cons_year_kt <- round(Sugar_cons_2018_by_country$Total_Sugar_cons_year_ton / 1000, 2)

view(Countries_and_region)
colnames(Countries_and_region)
colnames(Sugar_cons_2018_by_country)

# Delete the specified columns
Countries_and_region_for_cons <- Countries_and_region[ , !(names(Countries_and_region) %in% c("Importer ISO3"))]

# Rename columns
names(Countries_and_region_for_cons)[names(Countries_and_region_for_cons) == "Exporter ISO3"] <- "Countries_code"
colnames(Countries_and_region_for_cons)

# Merge Countries_and_region with the Sugar_cons_2018_by_country data by the columns "Countries_code" and "UN_countries"
Sugar_cons_2018_by_country_FIN <- merge(Countries_and_region_for_cons, Sugar_cons_2018_by_country, by = c("Countries_code", "UN_countries"))

view(Sugar_cons_2018_by_country_FIN)
colnames(Sugar_cons_2018_by_country_FIN)

# Normalize consumption by population:
# Calculate per capita sugar consumption (in ton per person in a year)
Sugar_cons_2018_by_country_FIN$Sugar_per_capita_ton <- Sugar_cons_2018_by_country_FIN$Total_Sugar_cons_year_ton / Sugar_cons_2018_by_country_FIN$Total_pop

# Calculate per capita sugar consumption (in kg per person per year)
Sugar_cons_2018_by_country_FIN$Sugar_per_capita_kg <- Sugar_cons_2018_by_country_FIN$Sugar_per_capita_ton * 1000

#export data:
write.xlsx(Sugar_cons_2018_by_country_FIN, "Sugar_cons_2018_by_country_FIN_30.8.24.xlsx")

##################### building the map fig.#########################3

# Ensure country codes are consistent with map data
Sugar_cons_2018_by_country_FIN$Countries_code <- toupper(Sugar_cons_2018_by_country_FIN$Countries_code) #  toupper function converts all characters in a string to uppercase
#####################
# verify that all 185 countries from my data are represented on the map
# Load the world map with low resolution
world_map <- getMap(resolution = "low")

# Extract the country codes from the map
map_countries <- as.character(world_map$ISO3)

# Extract the country codes from your data
data_countries <- unique(Sugar_cons_2018_by_country_FIN$Countries_code)

# Check which countries are missing in the map
missing_in_map <- setdiff(data_countries, map_countries)
missing_in_map

# Check which countries from the map are missing in your data
missing_in_data <- setdiff(map_countries, data_countries)
missing_in_data
#################################

# Get the world map using rnaturalearth
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Fix missing ISO code for France and Norway
world_map$iso_a3[world_map$name == "France"] <- "FRA"
world_map$iso_a3[world_map$name == "Norway"] <- "NOR"


# Standard cleanup: ensure proper string format
Sugar_cons_2018_by_country_FIN$Countries_code <- trimws(toupper(Sugar_cons_2018_by_country_FIN$Countries_code))
world_map$iso_a3 <- trimws(toupper(world_map$iso_a3))

# # Merge map data with sugar data
sugar_con_map_data <- merge(
  world_map,
  Sugar_cons_2018_by_country_FIN,
  by.x = "iso_a3",
  by.y = "Countries_code",
  all.x = TRUE
)

# double-check both countries
sugar_con_map_data[sugar_con_map_data$iso_a3 %in% c("FRA", "NOR"), c("iso_a3", "Sugar_per_capita_kg")]


################
# Plot the map using geom_sf
ggplot(data = sugar_con_map_data) +
  geom_sf(aes(fill = Sugar_per_capita_kg), color = "black") +
  scale_fill_gradientn(
    colours = c("#fdedec", "#a93226"),
    name = "Annual normalized\nsugar consumption\nper capita\nby country\nin 2018 (kg)",
    na.value = "grey",   # Color for NA values
    limits = c(0, NA),   # Ensure the scale starts from 0
    breaks = seq(0, max(sugar_con_map_data$Sugar_per_capita_kg, na.rm = TRUE), by = 10),
    labels = c("0", "10", "20", "30", "40", "50", "60"),
    guide = guide_colorbar(
      frame.colour = "black",  # Add grey stroke around the color bar
      frame.linewidth = 0.3,  # Set the stroke width
      ticks.colour = "black"   # set tick color to grey
    )
  ) +
  theme_minimal() +
  labs(title = "") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.046, 0.35),  # Position the legend on the left side
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = "left"
  ) +
  coord_sf(ylim = c(-60, 90)) +
  annotate("rect", xmin = -232, xmax = -222, ymin = -60, ymax = -54, fill = "grey", color = "black", size = 0.3) +
  annotate("text", x = -220, y = -57, label = "No data", hjust = "left", size = 3, color = "black")

###############
# Plot the map using geom_sf - new color - from blue to red - made at 20.6.25 for publication
ggplot(data = sugar_con_map_data) +
  geom_sf(aes(fill = Sugar_per_capita_kg), color = "black") +
  scale_fill_gradientn(
    colours = c("#00bfff","#ff0255"),  # Blue → Red
    name = "Annual normalized\nsugar consumption\nper capita\nby country\nin 2018 (kg)",
    na.value = "grey",
    limits = c(0, NA),
    breaks = seq(0, max(sugar_con_map_data$Sugar_per_capita_kg, na.rm = TRUE), by = 10),
    labels = c("0", "10", "20", "30", "40", "50", "60"),
    guide = guide_colorbar(
      frame.colour = "black",
      frame.linewidth = 0.3,
      ticks.colour = "black"
    )
  ) +
  theme_minimal() +
  labs(title = "") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.046, 0.35),  # Position the legend on the left side
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = "left"
  ) +
  coord_sf(ylim = c(-60, 90)) +
  annotate("rect", xmin = -232, xmax = -222, ymin = -60, ymax = -54, fill = "grey", color = "black", size = 0.3) +
  annotate("text", x = -220, y = -57, label = "No data", hjust = "left", size = 3, color = "black")


##################################################################
# Plot the map using  from blue to red - discrete colors- option 1 - made at 23.8.25 for publication

# Step 1: Create bins with labels that already include "kg"
sugar_con_map_data <- sugar_con_map_data %>%
  mutate(Sugar_cat = case_when(
    is.na(Sugar_per_capita_kg) ~ "No data",
    Sugar_per_capita_kg <= 20 ~ "0–20 kg",
    Sugar_per_capita_kg > 20 & Sugar_per_capita_kg < 25 ~ "20–25 kg",
    Sugar_per_capita_kg >= 25 ~ "25–60 kg"
  ))

# Step 2: Plot
ggplot(data = sugar_con_map_data) +
  geom_sf(aes(fill = Sugar_cat), color = "black", size = 0.1) +
  scale_fill_manual(
    values = c(
      "0–20 kg" = "#00bfff",   # Blue
      "20–25 kg" = "#00ff00",  # Green
      "25–60 kg" = "#ff0255",  # Red
      "No data" = "grey"
    ),
    breaks = c("0–20 kg", "20–25 kg", "25–60 kg", "No data"),  # enforce order
    name = "Annual normalized\nsugar consumption\nper capita\nby country\nin 2018"
  ) +
  theme_minimal() +
  labs(title = "") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.046, 0.35),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = "left"
  ) +
  coord_sf(ylim = c(-60, 90))

###############################################
# Plot the map using  from blue to red - discrete colors- option 2 - made at 23.8.25 for publication

library(dplyr)
library(scales)   # for rescale()

# 1) Three discrete bins + within-bin alpha (normalized per bin)
sugar_con_map_data <- sugar_con_map_data %>%
  mutate(
    Sugar_cat = case_when(
      is.na(Sugar_per_capita_kg) ~ "No data",
      Sugar_per_capita_kg <= 20 ~ "0–20 kg",
      Sugar_per_capita_kg > 20 & Sugar_per_capita_kg < 25 ~ "20–25 kg",
      Sugar_per_capita_kg >= 25 ~ "25–60 kg"
    ),
    alpha_raw = case_when(
      is.na(Sugar_per_capita_kg) ~ 1,
      Sugar_cat == "0–20 kg"  ~ (Sugar_per_capita_kg - 0)  / (20 - 0),   # 0..20
      Sugar_cat == "20–25 kg" ~ (Sugar_per_capita_kg - 20) / (25 - 20),  # 20..25
      Sugar_cat == "25–60 kg" ~ (Sugar_per_capita_kg - 25) / (60 - 25)   # 25..60
    ),
    alpha_within = pmin(pmax(alpha_raw, 0), 1)  # clamp to [0,1]
  )

# 2) Plot: discrete color fill + within-bin alpha
ggplot(data = sugar_con_map_data) +
  geom_sf(aes(fill = Sugar_cat, alpha = alpha_within), color = "black", size = 0.1) +
  scale_fill_manual(
    values = c(
      "0–20 kg"  = "#0144eb",  # Blue
      "20–25 kg" = "#14d592",  # Green
      "25–60 kg" = "#d8043b",  # Red
      "No data"  = "grey"
    ),
    breaks = c("0–20 kg", "20–25 kg", "25–60 kg", "No data"),
    name = "Annual normalized\nsugar consumption\nper capita\nby country\nin 2018"
  ) +
  scale_alpha(
    range = c(0.6, 1),  # stronger contrast for print
    guide = guide_legend(title = "Within-bin intensity\n(higher = more opaque)")
  ) +
  theme_minimal() +
  labs(title = "") +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = c(0.046, 0.35),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = "left"
  ) +
  coord_sf(ylim = c(-60, 90))

###########################
# B. Heat map - sugar consumption by sex and group age: top countries by continent (annual total consumption - not normalized)

view(Sugar_cons_2018_by_country_FIN)
colnames(Sugar_cons_2018_by_country_FIN)

# Create a new df with only the top country by sugar consumption (total per yaer) from each continent
top_cons_countries_by_continent <- Sugar_cons_2018_by_country_FIN %>%
  group_by(Continent) %>%
  slice_max(order_by = Total_Sugar_cons_year_ton, n = 1) %>%
  ungroup()  # Remove the grouping

# View the resulting df
view(top_cons_countries_by_continent)

# Remove the specified columns
top_cons_countries_by_continent <- top_cons_countries_by_continent %>%
  select(-Total_Sugar_cons_year_kt, -Sugar_per_capita_ton, -Sugar_per_capita_kg)

view(GDD_female_2018_FIN)
view(GDD_male_2018_FIN)
colnames(GDD_female_2018_FIN)

# Remove the specified columns
GDD_female_2018_for_merge <- GDD_female_2018_FIN %>%
  select(-Total_Sugar_cons_day_ton, -Total_pop, -Total_Sugar_cons_year_ton)

# Filter merged_data to retain only those countries that are in the original top_cons_countries_by_continent
GDD_female_2018_for_merge <- GDD_female_2018_for_merge %>%
  filter(UN_countries %in% top_cons_countries_by_continent$UN_countries)

view(GDD_female_2018_for_merge)

# Remove the specified columns
GDD_male_2018_for_merge <- GDD_male_2018_FIN %>%
  select(-Total_Sugar_cons_day_ton, -Total_pop, -Total_Sugar_cons_year_ton)

# Filter merged_data to retain only those countries that are in the original top_cons_countries_by_continent
GDD_male_2018_for_merge <- GDD_male_2018_for_merge %>%
  filter(UN_countries %in% top_cons_countries_by_continent$UN_countries)


# Combine the data frames by rows (GDD_male_2018_for_merge and GDD_female_2018_for_merge)
GDD_2018_for_heat_map <- rbind(GDD_female_2018_for_merge, GDD_male_2018_for_merge)

view(GDD_2018_for_heat_map)
colnames(GDD_2018_for_heat_map)

# Calculate sugar consumption for year by person (and convert from gram to kg)
GDD_2018_for_heat_map <- GDD_2018_for_heat_map %>%
  mutate(sugar_cons_kg_year_person = (sugar_cons_grams_person / 1000) * 365)


# remove specified prefixes from the group_age column
GDD_2018_for_heat_map$group_age <- gsub("^(fe_|ma_)", "", GDD_2018_for_heat_map$group_age)

write.xlsx(GDD_2018_for_heat_map, "GDD_2018_for_heat_map.xlsx")

# Generate the heatmap
#heatmap <- ggplot(GDD_2018_for_heat_map, aes(x = group_age, y = UN_countries, fill = sugar_cons_kg_year_person)) +
  #geom_tile() +  # This will create the heatmap tiles
  #facet_wrap(~gender, ncol = 1, scales = "free_y") +  # Separate panels for each gender
  #scale_fill_gradient(low = "white", high = "blue") +  # You can change colors as needed
  #labs(x = "Age Group", y = "Country", title = "Annual Sugar Consumption per Person by Gender and Age Group") +
  #theme_minimal() + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

#print(heatmap)

# Set the correct order for age groups
GDD_2018_for_heat_map$group_age <- factor(GDD_2018_for_heat_map$group_age, levels = c(
  "0-11 mo", "12-23 mo", "2-5 years", "6-10 years", "11-14 years", "15-19 years", 
  "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", 
  "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", 
  "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95+ years"
))

# Verify the levels of the factor
print(levels(GDD_2018_for_heat_map$group_age))

# Define the desired order of countries and continents
country_order <- c("United States of America", "Russian Federation", "Guatemala", 
                   "France", "Brazil", "Australia", "Egypt", "India")

continent_info <- c(
  "United States of America" = "North America",
  "Russian Federation" = "Eurasia",
  "Guatemala" = "North America",
  "France" = "Europe",
  "Brazil" = "South America",
  "Australia" = "Oceania",
  "Egypt" = "Africa",
  "India" = "Asia"
)

# Create a new column combining country, continent, and gender in the desired format
GDD_2018_for_heat_map <- GDD_2018_for_heat_map %>%
  mutate(
    UN_countries = factor(UN_countries, levels = country_order),  # Set the order of countries
    continent = continent_info[UN_countries],  # Add continent information
    country_gender = paste(UN_countries, "(", continent, ")", "-", gender)  # Combine country, continent, and gender
  )

# Custom function to create y-axis labels
y_labels_function <- function(labels) {
  # Detect if label contains 'male' and modify
  male_labels <- grepl("male", labels)
  labels[male_labels] <- sub(" - male", " - male", labels[male_labels])  # Modify 'male' labels
  
  # For 'female' labels, replace with 'female'
  female_labels <- grepl("female", labels)
  labels[female_labels] <- "- female"  # Replace with 'female' only
  
  return(labels)
}

# Generate the heatmap with customized y-axis labels
heatmap <- ggplot(GDD_2018_for_heat_map, aes(x = group_age, y = country_gender, fill = sugar_cons_kg_year_person)) +
  geom_tile() +  # Create the heatmap tiles
  scale_fill_gradientn(
    colors = c("white", "#fdedec", "#fadbd8", "#f5b7b1", "#f1948a", "#ec7063", "#e74c3c", "#cb4335", "#b03a2e", "#943126", "#78281f"),
    values = scales::rescale(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)),
    limits = c(0, 80),
    name = "sugar (kg)",
    breaks = seq(0, 80, by = 10),  # Define specific points to display in the legend
    labels = seq(0, 80, by = 10),  # Numerical labels for these points
    guide = guide_colorbar(
      direction = "horizontal",  # Set the direction of the legend to horizontal
      frame.colour = "black",  # Add black stroke around the color bar
      frame.linewidth = 0.3,  # Set the stroke width
      ticks.colour = "black",  # Set tick color to black
      barwidth = 7,  # Adjust the width of the color bar
      barheight = 1,  # Adjust the height of the color bar
      title.position = "left",  # Move the legend title to the left of the legend
      title.hjust = 0.5  # Center the legend title vertically
    )
  ) +
  scale_x_discrete(limits = levels(GDD_2018_for_heat_map$group_age)) +
  scale_y_discrete(labels = y_labels_function) +  # Custom y-axis labels function
  labs(x = "Age Group", y = "", title = "Annual sugar consumption by age and gender: top countries by continent") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10, margin = margin(t = 5)),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size for better readability
    legend.position = c(-0.03, -0.05),  # Position the legend at the bottom of the plot
    legend.direction = "horizontal",  # Set the legend to horizontal layout
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = "left",  
    legend.title = element_text(size = 12, face = "plain"),  # Adjust legend title size and style
    legend.text = element_text(size = 8)  # Adjust legend text size
  )

# Print the heatmap
print(heatmap)



# Define the desired order of countries
country_order <- c("United States of America", "Russian Federation", "Guatemala", 
                   "France", "Brazil", "Australia", "Egypt", "India")

# Create a new column combining country and gender in the desired order and format
GDD_2018_for_heat_map <- GDD_2018_for_heat_map %>%
  mutate(
    UN_countries = factor(UN_countries, levels = country_order),  # Set the order of countries
    country_gender = paste(UN_countries, gender, sep = " - "),  # Combine country and gender
    country_gender = factor(country_gender, levels = unique(paste(rep(country_order, each = 2), c("male", "female"), sep = " - ")))  # Set the order of country_gender combinations
  )


# Generate the heatmap with separate colors for male and female
heatmap <- ggplot(GDD_2018_for_heat_map, aes(x = group_age, y = country_gender, fill = sugar_cons_kg_year_person)) +
  geom_tile() +  # Create the heatmap tiles
  scale_fill_gradientn(
    colors = c("white", "#fdedec", "#fadbd8", "#f5b7b1", "#f1948a", "#ec7063", "#e74c3c", "#cb4335", "#b03a2e", "#943126", "#78281f"),
    values = scales::rescale(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)),
    limits = c(0, 80),
    guide = guide_colorbar(frame.colour = "black", frame.linewidth = 0.5)  # Add a black frame to the color scale
  ) +
  scale_x_discrete(limits = levels(GDD_2018_for_heat_map$group_age)) +
  scale_y_discrete(limits = rev(levels(GDD_2018_for_heat_map$country_gender))) +  # Ensure the order of country_gender
  labs(x = "Age Group", y = "Country - Gender", title = "Annual Sugar Consumption per Person by Gender and Age Group") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 8),  # Adjust y-axis text size for better readability
    legend.position = "bottom",  # Move the legend to the bottom
    legend.title = element_text(size = 10),  # Adjust legend title size for better readability
    legend.text = element_text(size = 8)  # Adjust legend text size
  )

# Print the heatmap
print(heatmap)



#####################################################################

###### Method - distribution of the number of countries in the study according to: producer, trade, consumption

print(RawS_cane_beet_manufacturers_countries)

print(RawS_cane_trade_no_manufacturers_FIN)

print(refined_sugar_trade_no_manufacturers_FIN)

print(non_existent_countries_SC_SB_MFA)

print(GDD_male_2018_FIN_TOTAL)

print(GDD_female_2018_FIN_TOTAL)

print(SC_SB_MFA) ############### need to correct: vinass & ethanol
colnames(SC_SB_MFA)

#Extract specific columns from the df SC_SB_MFA
SC_SB_MFA_total_countries_name <- SC_SB_MFA[, c("UN_countries", "Countries_code", "Region", "Continent")]
view(SC_SB_MFA_total_countries_name)

# Find the countries that are in RawS_cane_beet_manufacturers_countries but not in SC_SB_MFA_total_countries_name
non_matching_countries <- RawS_cane_beet_manufacturers_countries %>%
  filter(!Countries_code %in% SC_SB_MFA_total_countries_name$Countries_code)

# Print the non-matching Countries_code
print(non_matching_countries$Countries_code)


# Perform a right join on the two dataframes by the 'Countries_code' column.
# This will keep all rows from SC_SB_MFA_total_countries_name and only matching rows from RawS_cane_beet_manufacturers_countries.
SC_SB_all_countries_by_category <- right_join(RawS_cane_beet_manufacturers_countries, SC_SB_MFA_total_countries_name, by = "Countries_code")

# Rename the column 'source' to 'manufacturer_by_source'
SC_SB_all_countries_by_category <- SC_SB_all_countries_by_category %>%
  rename(manufacturer_by_source = source)
view(SC_SB_all_countries_by_category)

# Remove the 'Country_name' column
SC_SB_all_countries_by_category <- SC_SB_all_countries_by_category[ , !(names(SC_SB_all_countries_by_category) %in% "Country_name")]


# Find the rows where Countries_code is in SC_SB_all_countries_by_category but not in SC_SB_MFA_total_countries_name
non_matching_countries <- SC_SB_all_countries_by_category %>%
  filter(!Countries_code %in% SC_SB_MFA_total_countries_name$Countries_code)

# Print the non-matching Countries_code and their corresponding country names
print(non_matching_countries$Countries_code)
print(non_matching_countries$Country_name)

# Find and display the rows with duplicate Countries_code
duplicates <- SC_SB_all_countries_by_category %>%
  group_by(Countries_code) %>%
  filter(n() > 1) %>%
  ungroup()

# Print the duplicate rows
print(duplicates)

# Count unique country codes
unique_country_count <- length(unique(GDD_male_2018_FIN_TOTAL$Countries_code))
print(unique_country_count)


# Extract unique country codes from GDD_male_2018_FIN_TOTAL
unique_countries <- unique(GDD_male_2018_FIN_TOTAL$Countries_code)

# Create the 'GDD_data' column based on whether the country code exists in the unique list
SC_SB_all_countries_by_category$GDD_data <- ifelse(SC_SB_all_countries_by_category$Countries_code %in% unique_countries, "YES", "NO")

# Check the first few rows to confirm the new column is added correctly
head(SC_SB_all_countries_by_category)

view(SC_SB_all_countries_by_category)


#export data:
write.xlsx(RawS_cane_beet_manufacturers_countries, "RawS_cane_beet_manufacturers_countries_1.11.24.xlsx")
write.xlsx(RawS_cane_trade_no_manufacturers_FIN, "RawS_cane_trade_no_manufacturers_FIN_1.11.24.xlsx")
write.xlsx(refined_sugar_trade_no_manufacturers_FIN, "refined_sugar_trade_no_manufacturers_FIN_1.11.24.xlsx")
write.xlsx(GDD_male_2018_FIN_TOTAL, "GDD_male_2018_FIN_TOTAL_1.11.24.xlsx")
write.xlsx(GDD_female_2018_FIN_TOTAL, "GDD_female_2018_FIN_TOTAL_1.11.24.xlsx")
write.xlsx(SC_SB_MFA, "SC_SB_MFA_1.11.24.xlsx")
write.xlsx(SC_SB_all_countries_by_category, "SC_SB_all_countries_by_category_1.11.24.xlsx")


############################################################################
############################################################

### Sectors Figure - Sunburst figure - NOT WORKING YET

# Load your Excel file 
Sectors <- read_excel("Sunburst_Data.xlsx")

# Split sectors into hierarchical levels for the sunburst
Sectors$path <- gsub(" -> ", "/", Sectors$Sector)  # Create path-like hierarchy
Sectors$Level1 <- sapply(strsplit(Sectors$path, "/"), `[`, 1)
Sectors$Level2 <- sapply(strsplit(Sectors$path, "/"), `[`, 2)
Sectors$Level3 <- sapply(strsplit(Sectors$path, "/"), `[`, 3)

# Prepare the data for ggplot
plot_data <- Sectors %>%
  tidyr::pivot_longer(cols = c(Level1, Level2, Level3),
                      names_to = "Level", values_to = "Name") %>%
  na.omit()

# Add cumulative flow for proportions
plot_data <- plot_data %>%
  group_by(Level, Name) %>%
  summarise(Flow = sum(`Flow (Gt)`)) %>%
  mutate(Percentage = Flow / sum(Flow) * 100)

# Create the static sunburst chart
ggplot(plot_data, aes(x = Level, y = Percentage, fill = Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Static Sunburst Chart", x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Save the chart as a static image
ggsave("sunburst_chart.jpg", width = 10, height = 10, dpi = 300)
ggsave("sunburst_chart.pdf", width = 10, height = 10)



############################################################################
###### Test 1:  covariance Using only countries that produces sugar

# Load the Excel file
file_path <- "Countries_producing_raw_sugar.xlsx"

# Read the sheets
sheet1 <- read_excel(file_path, sheet = 1)
sheet2 <- read_excel(file_path, sheet = 2)

# Rename columns for consistency
colnames(sheet1)[1:2] <- c("Country", "Country_Code")
colnames(sheet2)[1] <- "Country_Code"

# Merge the datasets on "Country_Code"
merged_data <- inner_join(sheet1, sheet2, by = "Country_Code")

# Select relevant columns for analysis
relevant_data <- merged_data %>%
  select(Country, Country_Code, `Sugar_per_capita_kg - normalized`)

view(relevant_data)

# Calculate covariance
cov_value <- cov(as.numeric(relevant_data$`Sugar_per_capita_kg - normalized`), seq_along(relevant_data$Country))

# Print covariance result
print(paste("Covariance:", cov_value))

# Create a scatter plot
ggplot(relevant_data, aes(x = seq_along(Country), y = `Sugar_per_capita_kg - normalized`)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(
    title = "Sugar Production and Consumption (Per Capita)",
    x = "Country Index (Producer)",
    y = "Sugar Consumption Per Capita (kg)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



###### Test 2: Using all countries in the consumption data

# Add a new column to sheet2 indicating whether the country is a producer
sheet2 <- sheet2 %>%
  mutate(Is_Producer = ifelse(Country_Code %in% sheet1$Country_Code, 1, 0))

# Calculate covariance
cov_value <- cov(sheet2$`Sugar_per_capita_kg - normalized`, sheet2$Is_Producer)

# Print covariance result
print(paste("Covariance:", cov_value))

# Create a scatter plot
ggplot(sheet2, aes(x = Is_Producer, y = `Sugar_per_capita_kg - normalized`)) +
  geom_jitter(width = 0.2, color = "blue", alpha = 0.7) +
  labs(
    title = "Sugar Consumption Per Capita vs. Production Status",
    x = "Is Producer (1 = Yes, 0 = No)",
    y = "Sugar Consumption Per Capita (kg)"
  ) +
  theme_minimal()


#########################################################################################

#  a stacked area chart showing the growth in yield of sugarcane and sugar beet from 1960 to 2020.
# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define file path
file_path <- "FAOSTAT_data_en_1960-2020 - sugar caneVS beet - by conti.xlsx"

# Read the first sheet of the Excel file
crops <- read_excel(file_path)

# Inspect column names
print(colnames(crops))

# Aggregate data: Sum yield across all continents per year
crops_summarized <- crops %>%
  filter(Item %in% c("Sugar cane", "Sugar beet")) %>%  # Keep only relevant crops
  group_by(Year, Item) %>%  # Group by Year and Crop type
  summarise(Yield = sum(Value, na.rm = TRUE) / 1e9, .groups = "drop")  # Convert to gigatonnes (Gt)

# Rename columns for clarity
crops_summarized <- crops_summarized %>%
  rename(Crop = Item)

# Ensure proper stacking order (Sugar cane at bottom, Sugar beet on top)
crops_summarized$Crop <- factor(crops_summarized$Crop, levels = c("Sugar cane", "Sugar beet"))

# Create the stacked area plot
ggplot(crops_summarized, aes(x = Year, y = Yield, fill = Crop)) +
  geom_area(color = "white", size = 0.5, alpha = 0.8) +  # White borders for separation
  scale_fill_manual(values = c("Sugar cane" = "#58d68d", "Sugar beet" = "#F08080")) +  # Assign colors
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +  # Show only every 10 years
  labs(title = "Growth in Yield of Sugarcane and Sugar Beet (1960-2020)",
       x = "Year", y = expression("Global harvest (Gt·yr"^{-1}*")"), fill = "Crop") +  # Updated y-axis title
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"))


##################################################################################

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Define file path for sugar source data
file_path <- "Countries producing raw sugar by source.xlsx"

# Read the Excel file (skip the first row, which is the header inside the file)
sugar_sources <- read_excel(file_path, skip = 1)

# Rename columns correctly
colnames(sugar_sources) <- c("Country_name", "iso_a3", "Source")  

# Convert country codes to uppercase (ensure consistency with map data)
sugar_sources$iso_a3 <- toupper(sugar_sources$iso_a3)

# Aggregate data: Identify if a country has sugar cane, sugar beet, or both
sugar_sources_summary <- sugar_sources %>%
  group_by(iso_a3) %>%
  summarise(Source = case_when(
    all(Source == "sugar cane") ~ "Sugar Cane",
    all(Source == "sugar beet") ~ "Sugar Beet",
    TRUE ~ "Both"
  ))

# Load the world map with country codes
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# --- COUNTRY NAME COMPARISON REPORT ---
# Extract country names in the map that match the sugar-producing countries
sugar_countries_in_map <- world_map %>%
  filter(iso_a3 %in% sugar_sources_summary$iso_a3) %>%
  select(iso_a3, admin, name)

# Identify mismatches: Countries in sugar data not found in the map
mismatch_list <- sugar_sources %>%
  left_join(sugar_countries_in_map, by = "iso_a3") %>%
  filter(is.na(admin)) %>%
  select(Country_name, iso_a3)

# Report mismatches
cat("Countries in sugar data but not found in the map:\n")
print(mismatch_list)

# Manual correction for known issues
world_map$iso_a3[world_map$name == "France"] <- "FRA"

# Merge map data with sugar source data again
sugar_map_data <- merge(world_map, sugar_sources_summary, by = "iso_a3", all.x = TRUE)

# Plot the world map using ggplot2
ggplot(data = sugar_map_data) +
  geom_sf(aes(fill = Source), color = "black", size = 0.2) +
  scale_fill_manual(
    values = c("Sugar Cane" = "#1b9e77", "Sugar Beet" = "#d95f02", "Both" = "#7570b3"),
    na.value = "grey"
  ) +
  labs(title = "Global Distribution of Sugar Cane and Sugar Beet Production",
       fill = "Sugar Source") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "right"
  ) +
  coord_sf(ylim = c(-60, 90))

colnames(sugar_map_data)

#######################################################
# sectors - Donut Chart
# look at that link - there is a movie: https://statdoe.com/pie-donut-chart-in-r/

library(ggplot2)
library(dplyr)
library(webr) # Required for PieDonut()

# Data preparation
# Read the Excel file
sugar_sectors <- read_excel("Donut_Chart_sectors_data.xlsx")

# Pie-Donut chart
PieDonut(sugar_sectors, aes(Category, Allocation, count = mass_Gt),
         r0=0.45, r1=1,
         #explode = 2,
         #explodeDonut=TRUE,
         title = "Sugar by Sectors")

?PieDonut

###########################################################

# Data preparation - Sugarcane
# Read the Excel file
sugarcan_sectors_new <- read_excel("Donut_Chart_sectors_data- sugarcane - new.xlsx")

# Pie-Donut chart
PieDonut(sugarcan_sectors_new, aes(Category, Allocation, count = mass_Gt),
         r0=0.45, r1=1,
         #explode = 2,
         #explodeDonut=TRUE,
         title = "Sugarcan by Sectors")

###########################################################

# Data preparation - Sugar beet
# Read the Excel file
sugar_beet_sectors_new <- read_excel("Donut_Chart_sectors_data- sugar beet - new.xlsx")

# Pie-Donut chart
PieDonut(sugar_beet_sectors_new, aes(Category, Allocation, count = mass_Gt),
         r0=0.45, r1=1,
         #explode = 2,
         #explodeDonut=TRUE,
         title = "Sugar beet by Sectors")

###########################################################

# Water usage - DONUT chart

sugar_water <- data.frame(
  Category = c("Total Blue Water in Agriculture", "Blue Water in Sugar"),
  Allocation = c("Total", "Sugar"),
  mass_Gt = c(1350, 190)
)

# Pie-Donut chart
PieDonut(sugar_water, aes(Category, Allocation, count = mass_Gt),
         r0 = 0.60, r1 = 1,         # Donut proportions
         title = "Blue Water Use in Agriculture vs. Sugar")


#########################################################################################


# 3 layes - not for use - from: https://stackoverflow.com/questions/76701848/donut-chart-with-3-levels-in-r
# Load data
Sugarcane_Sec <- read_excel("Donut_Chart_sectors_data- sugarcane.xlsx")

# Replace NAs with "Unknown"
Sugarcane_Sec <- Sugarcane_Sec %>%
  mutate(
    Category = replace_na(Category, "Unknown"),
    Allocation = replace_na(Allocation, "Unknown"),
    Crop = replace_na(Crop, "Unknown")
  )

# Level 0 (center node)
lvl0 <- tibble(name = "Sugar", value = 0, level = 0, fill = NA)

# Level 1 (Category)
lvl1 <- Sugarcane_Sec %>%
  group_by(name = Category) %>%
  summarise(value = sum(mass_Gt), .groups = "drop") %>%
  mutate(level = 1, fill = name)

# Level 2 (Allocation)
lvl2 <- Sugarcane_Sec %>%
  group_by(name = Allocation, fill = Category) %>%
  summarise(value = sum(mass_Gt), .groups = "drop") %>%
  mutate(level = 2)

# Level 3 (Crop)
lvl3 <- Sugarcane_Sec %>%
  group_by(name = Crop, fill = Allocation) %>%
  summarise(value = sum(mass_Gt), .groups = "drop") %>%
  mutate(level = 3)

# Combine levels
donut_data <- bind_rows(lvl0, lvl1, lvl2, lvl3) %>%
  mutate(level = as.factor(level))

# Reorder only levels 1–3 (not level 0)
donut_lvl0 <- donut_data %>% filter(level == 0)
donut_rest <- donut_data %>% filter(level != 0) %>%
  mutate(name = fct_reorder2(name, fill, value))
donut_data_final <- bind_rows(donut_lvl0, donut_rest)

# Plot the 3-level donut
ggplot(donut_data_final, aes(x = level, y = value, fill = fill, alpha = level)) +
  geom_col(width = 1, color = "gray90", size = 0.3, position = position_stack()) +
  geom_text(aes(label = name), size = 3, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = 0.7, "3" = 0.4), guide = FALSE) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_brewer(palette = "Set3", na.translate = FALSE) +
  labs(title = "Sugarcane – Category → Allocation → Crop", x = NULL, y = NULL) +
  theme_minimal()
