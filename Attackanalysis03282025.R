# Mahdi Army Attack Data
# 3/28/2025

library(readxl)
library(ggplot2)
library(dplyr)
library(stargazer)
library(xtable)
library(officer)
library(flextable)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)


setwd("C:/Users/MMcWeeney/Potomac Institute for Policy Studies/ICTS Terrorism - Documents/Kalaris Conference/Attack Analysis/MahdiArmyAttacks")

file_path <- "C:/Users/MMcWeeney/Potomac Institute for Policy Studies/ICTS Terrorism - Documents/Kalaris Conference/Mahdi Army Article Outline Sign-up.xlsx"

ma <- read_excel(file_path, sheet = 2)

#Descriptive Statistics of Duration, Killed, Wounded

colnames(ma) <- make.names(colnames(ma))  # Converts names to valid R format

colnames(ma)[colnames(ma) == "Duration..days."] <- "Duration"

ma$Duration <- as.numeric(ma$Duration)
ma$Killed <- as.numeric(ma$Killed)
ma$Wounded <- as.numeric(ma$Wounded)

ma_clean <- na.omit(ma[, c("Duration", "Killed", "Wounded")])

stats <- data.frame(
  Statistic = c("Mean", "St. Dev.", "Min", "Max", "N"),
  Duration = c(mean(ma$Duration, na.rm = TRUE),
               sd(ma$Duration, na.rm = TRUE),
               min(ma$Duration, na.rm = TRUE),
               max(ma$Duration, na.rm = TRUE),
               sum(!is.na(ma$Duration))),
  Killed = c(mean(ma$Killed, na.rm = TRUE),
             sd(ma$Killed, na.rm = TRUE),
             min(ma$Killed, na.rm = TRUE),
             max(ma$Killed, na.rm = TRUE),
             sum(!is.na(ma$Killed))),
  Wounded = c(mean(ma$Wounded, na.rm = TRUE),
              sd(ma$Wounded, na.rm = TRUE),
              min(ma$Wounded, na.rm = TRUE),
              max(ma$Wounded, na.rm = TRUE),
              sum(!is.na(ma$Wounded)))
)

print(xtable(stats, digits = 2), type = "latex", include.rownames = FALSE)

# Convert dataframe to a flextable
ft <- flextable(stats)

# Export to a Word document
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "Mahdi_Army_Stats.docx")

#Histograms
#Year Trend Line # of Attacks, Number of Fatalities, #Number of Wounded

attack_counts <- ma %>%
  group_by(Year) %>%
  summarise(Attack_Count = n())

print(attack_counts)

killed_per_year <- ma %>%
  group_by(Year) %>%        # Group data by Year
  summarise(Total_Killed = sum(Killed, na.rm = TRUE))  # Sum up "Killed" column

# View result
print(killed_per_year)

wounded_per_year <- ma %>%
  group_by(Year) %>%        # Group data by Year
  summarise(Total_Wounded = sum(Wounded, na.rm = TRUE))  # Sum up "Wounded" column

# View result
print(wounded_per_year)

merged_data <- full_join(attack_counts, killed_per_year, by = "Year")
merged_data2 <- full_join(merged_data, wounded_per_year, by = "Year")

long_data <- merged_data2 %>%
  pivot_longer(cols = c(Attack_Count, Total_Killed, Total_Wounded), 
               names_to = "Metric", values_to = "Count")

#2009 included for context

#Attack Counts Only 
ggplot(attack_counts, aes(x = Year, y = Attack_Count)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Trend of Mahdi Army Attacks per Year",
       x = "Year",
       y = "Attack Count") +
  theme_minimal()

filtered_data <- long_data %>%
  filter(Metric %in% c("Total_Killed", "Total_Wounded"))  # Exclude Attack_Count

#Killed and Wounded
ggplot(filtered_data, aes(x = Year, y = Count, color = Metric, group = Metric)) +
  geom_line(size = 1) +        # Line plot for trends
  geom_point(size = 2) +       # Points at each year
  labs(title = "Total Killed and Wounded Per Year",
       x = "Year",
       y = "Count",
       color = "Metric") +     # Legend title
  theme_minimal() +
  scale_color_manual(values = c("blue", "darkgreen"))  # Custom colors for clarity

#Weapon Type

ggplot(ma, aes(x = Year, fill = Weapon)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  labs(title = "Distribution of Weapons by Year",
       x = "Year",
       y = "Number of Attacks",
       fill = "Weapon") +
  theme_minimal()

#Type of Assault/Civilian Military - 16% or 6 were U.S. targeted

ma <- ma %>%
  mutate(
    Type.of.attack = trimws(Type.of.attack),
    Civilian.Military = trimws(Civilian.Military),
    Combined2 = paste(Type.of.attack, Civilian.Military, sep = "_")
  )

# View result
head(ma$Combined2)


ggplot(ma, aes(x = Year, fill = Combined2)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  labs(title = "Distribution of AttackType by Year",
       x = "Year",
       y = "Number of Attacks",
       fill = "AttackType") +
  theme_minimal()

#Map of Attacks
LatLong <- read_excel("LatLong.xlsx")
#merge lat long at ma by Location

colnames(LatLong)[colnames(LatLong) == "City"] <- "Location"

map_data <- full_join(ma, LatLong, by = "Location")

iraq_map <- ne_states(country = "Iraq", returnclass = "sf")

map_data <- map_data %>%
  filter(Location != "Unknown")

#Change Battle to Attack for plotting
map_data <- map_data %>%
  mutate(Combined2 = case_when(
    Combined2 == "Battle_Military" ~ "Attack_Military",
    Combined2 == "Terrorism Not In GTD_Civilian" ~ "Terrorism_Civilian",
    TRUE ~ Combined2  # Keep other values unchanged
  )) %>%
  filter(!is.na(Combined2))  # Remove NA values

map_data <- map_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

map_data <- map_data %>%
  filter(!is.na(Weapon) & !is.na(Combined2))

ggplot() +
  geom_sf(data = iraq_map, fill = "white", color = "black") +  # White map background
  geom_point(data = map_data, aes(x = Longitude, y = Latitude, 
                                  size = Killed, 
                                  color = Weapon, 
                                  shape = Combined2), 
             alpha = 0.8, stroke = 1.2) +  # Bolder symbols with increased stroke
  scale_size(range = c(4, 12), name = "Fatalities") +  # Bigger symbols
  scale_shape_manual(values = c(16, 17, 8, 18, 15, 3, 4, 7)) +  # Adjusted for more attack types
  labs(title = "Map of Attack Locations in Iraq (Fatalities, Weapon, and Attack Type)",
       x = "Longitude", 
       y = "Latitude",
       color = "Weapon Type",
       shape = "Attack Type") +  # Update legend labels
  theme_minimal() +  # Completely white background
  theme(legend.position = "right", legend.title = element_text(size = 10)) +  # Keep legends visible
  coord_sf(xlim = c(41, 47), ylim = c(31, 37))

#Appendix Table

ma2 <- ma[, 1:(ncol(ma) - 3)]

ft2 <- flextable(ma2) %>%
  set_table_properties(width = 1, layout = "autofit") %>%  # Autofit to reduce spaces
  fontsize(size = 8) %>%  # Set font size
  font(part = "all", fontname = "Times New Roman") %>%  # Set font type
  padding(padding = 2, part = "all")  # Reduce spacing in rows & columns

# Create a Word document with landscape orientation
doc2 <- read_docx() %>%
  body_add_par("Exported Data Table", style = "heading 1") %>%  # Add title
  body_add_flextable(ft2) %>%
  body_end_section_landscape()  # Set document to landscape

# Save the document
print(doc2, target = "DataTable_Landscape.docx")