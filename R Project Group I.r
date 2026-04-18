---
title: "ST661 (R for Data Analytics)- Group Project"
author: "Group I- BHANU NAGA SAI VAMSI BITRA,SILVIU-DANIEL FÁZEKAS,SANTHOSH PRABHAKARAN,ANJU BABY"
output:
  html_document:
    code_folding: hide
  fig_caption: true
  pdf_document: default
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,warning = FALSE, message = FALSE)
```

```{r,include=FALSE}
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(ggiraph)
library(lubridate)
```


# "Analyzing Trends and Patterns in Olympic Performance Using Athlete and Medal Data"{.tabset .tabset-fade .tabset-pills} 
## Introduction  {.tabset .tabset-fade .tabset-pills} 


In this project, we analyze two datasets:

**Athletes Dataset:** Contains detailed information about athletes, including their names, sports, countries, physical attributes (height, weight), and date of birth.

**Medals Dataset:** Provides details about the medals won, including the type of medal (Gold, Silver, or Bronze), the event, the athlete's country, and the date the medal was awarded.

These datasets are sourced from reliable Olympic data repositories, ensuring the accuracy and depth of the analysis. The goal is to uncover insights such as:

Patterns in medal distribution across countries.

Trends in age and performance.

The dominance of certain countries or athletes in specific sports.

Relationship between athlete heights and their sports.

**Dataset Structure:**

Variables such as Full_Name, Sports, Country, Medal_Type, Event, Medal_Date and Height are used for analysis.
These variables allow exploration of athlete characteristics, performance trends, and medal distributions.


__Dataset information:__

**Athletes:**

```{r}
# Define file paths for the datasets (correcting file extensions and slashes)
athletes_path <- "C:/Users/USER/OneDrive/Desktop/ST661[A] PROJECT GROUP I/athletes.csv"
medals_path <- "C:/Users/USER/OneDrive/Desktop/ST661[A] PROJECT GROUP I/medals.csv"

# Load datasets
library(readr)

# Load the datasets using read_csv
athletes_df <- read_csv(athletes_path, show_col_types = FALSE)
medals_df <- read_csv(medals_path, show_col_types = FALSE)

# Preview the first few rows of each dataset
head(athletes_df)


```

**Medals:**
```{r}
head(medals_df)

```

## Data Wrangling {.tabset .tabset-fade .tabset-pills}

In this project, we handled missing values and cleaned the datasets to prepare them for analysis.

***Handling Missing Values:***

Critical columns like Full_Name, Country, and Sports were filtered to remove rows with missing or irrecoverable values.

**Column Selection and Renaming:**

Unnecessary columns from both datasets were removed to focus only on relevant fields.
Columns were renamed for consistency and clarity (e.g., name_tv to Full_Name and medal_type to Medal_Type).

**Data Cleaning:**

Brackets, quotes, and formatting inconsistencies in categorical columns were removed.
Columns such as Birth_Date and Medal_Date were converted to proper date formats.

**Merging Datasets:**

The datasets were merged using common columns (Full_Name, Sports, Country) to form a unified dataset (combined_df).
This combined dataset includes all essential variables like Medal_Type, Event, Height, Weight, and Medal_Date.


```{r}


## Data cleaning

# Remove the specified columns in athletes 

df_clean_ath <- athletes_df %>%
  select(-c(current, name, name_short, country_code, country, 
            country_long, nationality_long, nationality_code, birth_place, 
            birth_country, residence_place, residence_country, nickname, 
            hobbies, occupation, education, family, lang, coach, reason, 
            hero, influence, philosophy, sporting_relatives, ritual, 
            other_sports, events))

# Remove the specified columns in medals

Me_cleaned <- medals_df %>%
  select(-c(medal_code,event_type,url_event,code,country_code,country_long,gender))
#Rename columns in athletes
df_ath <- df_clean_ath %>%
  rename(
    `Full_Name` = name_tv,
    Sports = disciplines,
    Country = nationality,
    Height = height,
    Weight = weight,
    Birth_Date = birth_date,
    Gender = gender,
    Code = code
  )

#Rename columns in medals

df_med <- Me_cleaned %>%
  rename(
    Medal_Type= medal_type,
    Medal_Date= medal_date,
    Full_Name = name,
    Sports= discipline,
    Country = country,
    Event =event
  )
# Athletes Data set

# Remove brackets and quotes from 'sports'

library(stringr)

df_cl <- df_ath %>%
  mutate(
    Sports = str_replace_all(Sports, "\\[|\\]|'", ""), 
  )

# Convert character to date in "dd-mm-yyyy" format
df_cl$Birth_Date <- as.Date(df_cl$Birth_Date, format="%d-%m-%Y")

#head(df_cl$Birth_Date)

#str(df_cl)
## Count NA and empty string values in the entire data frame

#count_na_empty <- apply(df_cl, 2, function(x) sum(is.na(x) | x == ""))

#count_na_empty
# removing rows with missing values for key columns

df_clea <- df_cl %>%
  filter(!is.na(`Full_Name`), !is.na(Country), !is.na(Sports), !is.na(Birth_Date), !is.na(Height), !is.na(Weight))


#count_na_empty <- apply(df_clea, 2, function(x) sum(is.na(x) | x == ""))

#count_na_empty

#Remove duplicate rows, if any


df_cleaned <- df_clea %>% distinct()

#checking data types of dataframe

#str(df_cleaned)

#convert char to date 

df_med$Medal_Date <- as.Date(df_med$Medal_Date, format="%d-%m-%Y")

#head(df_med$Medal_Date)

#str(df_med)
#checking null values 

#count_na_empty <- apply(df_med, 2, function(x) sum(is.na(x) | x == ""))

#count_na_empty

## Merge athletes and medals datasets using common columns
combined_df <- inner_join(df_med, df_cleaned, by = c("Full_Name", "Sports", "Country"))

head(combined_df)

#write.csv(combined_df, "combined_Clen_dataset.csv", row.names = FALSE)
#Transform the Data
#combined_df <- read.csv("E:/New folder (4)/olympics/combined_Clen_dataset1.csv")
# Assuming medals_df has a column 'medal_date' with values like '2024-07-27'
medals_df <- combined_df %>%
  mutate(year = year(as.Date(Medal_Date)))
#write.csv(combined_df, "combined_Clen_dataset1.csv", row.names = FALSE)
```
**Final Output:**

The cleaned and merged dataset has no missing values and is ready for analysis.
Numerical and categorical data were validated for correctness, and duplicates were removed.
This cleaned dataset provides a reliable foundation for uncovering trends and patterns in Olympic performance.




## Data Analysis {.tabset .tabset-pills}




### Story 1

__Mean Age of Medal-Winning Athletes by Sport and Gender __

The goal of this plot is to analyze the average age of medal-winning athletes across sports, highlighting differences between male and female athletes. It identifies sport-specific age trends and gender-based variations in peak performance. This insight helps understand how age impacts success in various disciplines.

```{r}
# Calculate the mean age and round to the nearest whole number
mean_age_df <- combined_df %>%
  mutate(Age = as.numeric(difftime(as.Date(Medal_Date), Birth_Date, units = "days")) / 365.25) %>%
  group_by(Sports, Gender) %>%
  summarise(
    Mean_Age = round(mean(Age, na.rm = TRUE)),  # Round the mean age
    .groups = "drop"
  )

# Create a custom color palette for gender
gender_colors <- c("Male" = "blue", "Female" = "red")

# Create the line plot using plotly
plot <- plot_ly(mean_age_df, 
                x = ~Sports, 
                y = ~Mean_Age, 
                color = ~Gender, 
                colors = gender_colors,  # Apply custom colors
                type = 'scatter', 
                mode = 'lines+markers') %>%
  layout(
    title = "Mean Age of Medal-Winning Athletes by Sport and Gender",
    xaxis = list(title = "Sports", tickangle = 45),  # Rotate x-axis labels
    yaxis = list(title = "Mean Age"),
    legend = list(title = list(text = "Gender"))
  )

# Print the plot
plot

```





### Story 2

__Athletes Who Won Multiple Medals In A Particular Sport__

The goal of this plot is to showcase athletes who have won multiple medals in a particular sport, emphasizing their exceptional performance and consistency. It highlights individual achievements and identifies athletes who dominate their respective disciplines. This insight helps recognize top performers and trends in medal-winning excellence.

```{r}


# Group by athlete to find athletes who won multiple medals
multi_event_medalists <- combined_df %>%
  group_by(Full_Name, Sports, Country) %>%
  summarise(medals_won = n_distinct(Event), .groups = "drop") %>%
  filter(medals_won > 1)  # Filter only athletes who won more than 1 medal



# Create Plotly scatter plot for multi-event medalists with both athlete's name, sport and country
multi_event_medalists <- multi_event_medalists %>%
  arrange(desc(medals_won))  # Ensure medals_won is ordered

# Create the plot
plot <- plot_ly(
  data = multi_event_medalists,
  x = ~medals_won,  # Number of medals on the x-axis
  y = ~reorder(Full_Name, medals_won),  # Reorder athletes by medals won
  type = 'scatter',
  mode = 'markers',  # Scatter plot with markers
  marker = list(color = 'purple', size = 10, opacity = 0.7),  # Marker customization
  text = ~paste(
    "Athlete: ", Full_Name,
    "<br>Sport: ", Sports,
    "<br>Country: ", Country,
    "<br>Medals Won: ", medals_won
  ),  # Tooltip text
  hoverinfo = "text"  # Display tooltip text
) %>%
  layout(
    title = 'Athletes Who Won Multiple Medals In A Particular Sport',
    xaxis = list(title = 'Number of Medals Won'),
    yaxis = list(title = 'Athlete', categoryorder = "total ascending"),  # Sort y-axis
    template = 'plotly_white'
  )

# Display the plot
plot
```




### Story 3

__Top 10 Countries by Medal Count__  

The goal of this plot is to highlight the top 10 countries by total medal count, breaking it down by gold, silver, and bronze medals. It showcases which nations excel in international sports competitions and their relative strengths in achieving higher-tier medals. This insight reflects the countries' dominance and investment in athletics.

```{r}
# Summarize medal distribution by country and medal type
medal_distribution <- combined_df %>%
  group_by(Country, Medal_Type) %>%
  summarise(medal_count = n(), .groups = "drop")

# Summarize the total medal count per country
total_medals_by_country <- medal_distribution %>%
  group_by(Country) %>%
  summarise(total_medals = sum(medal_count), .groups = "drop") %>%
  arrange(desc(total_medals)) %>%
  slice_head(n = 10)  # Get top 10 countries

# Filter for the top 10 countries in the original medal distribution
top_10_medal_distribution <- medal_distribution %>%
  filter(Country %in% total_medals_by_country$Country)



# Set custom colors for the medals
medal_colors <- c("Gold Medal" = "gold", "Silver Medal" = "gray", "Bronze Medal" = "darkorange3")

# Ensure the Medal_Type column has the correct order
top_10_medal_distribution$Medal_Type <- factor(
  top_10_medal_distribution$Medal_Type,
  levels = c("Gold Medal", "Silver Medal", "Bronze Medal")
)

# Add tooltips for interactivity
top_10_medal_distribution <- top_10_medal_distribution %>%
  mutate(tooltip = paste0(
    "Country: ", Country, "<br>",
    "Medal Type: ", Medal_Type, "<br>",
    "Medal Count: ", medal_count
  ))
interactive_plot <- ggplot(top_10_medal_distribution, aes(
  x = reorder(Country, -medal_count),
  y = medal_count,
  fill = Medal_Type
)) +
  geom_bar_interactive(
    stat = "identity",
    position = "stack",
    aes(tooltip = tooltip, data_id = interaction(Country, Medal_Type))
  ) +
  theme_minimal() +
  labs(
    title = "Top 10 Countries by Medal Count",
    x = "Country",
    y = "Medal Count",
    fill = "Medal Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20) # Increase the font size of the title
  ) +
  scale_fill_manual(values = medal_colors)

# Render the interactive plot using 'girafe'
girafe(ggobj = interactive_plot, width_svg = 12, height_svg = 8)

```


### Story 4

__Analyzing the Relationship Between Athlete Heights and Their Sports__

The goal of this plot is to compare the height distributions of male and female athletes across different sports. It helps identify gender-based height differences and variations in height trends within each sport. This can provide insights into height's role in athletic performance and suitability for specific sports.

```{r}
# Load required libraries
library(ggplot2)
library(dplyr)

# Filter out rows where height is 0
filtered_data <- combined_df %>%
  filter(Height > 0)

# Create a tooltip column for interactivity (optional for advanced features)
filtered_data <- filtered_data %>%
  mutate(tooltip = paste("Athlete:", Full_Name, 
                         "<br>Gender:", Gender,
                         "<br>Sport:", Sports,
                         "<br>Height:", Height, "cm"))

# Create the boxplot
plot <- ggplot(filtered_data, aes(x = Sports, y = Height, fill = Gender)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Standard boxplot
  labs(
    title = "Boxplot of Height vs Sports Grouped by Gender",
    x = "Sport",
    y = "Height (cm)",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

# Display the plot
print(plot)

```




## Conclusion

 The analysis of Olympic datasets has provided significant insights into the performance trends of athletes and countries. Here are the key takeaways:

__1) Mean Age of Medal-Winning Athletes by Sport and Gender__

The analysis revealed significant variations in the average age of medal-winning athletes across different sports and genders. Certain sports, such as gymnastics for males(22) and skateboarding for females (16) tend to favor younger athletes, while others, like equestrian events for both males (40) and females (39), see more experienced participants excelling. The interactive visualization showcased these patterns, providing insights into age-specific competitiveness and the physical and mental readiness required for various disciplines.

__2) Athletes Who Won Multiple Medals in a Particular Sport__ 

This analysis identified athletes who secured multiple medals in a single sport, highlighting their exceptional performance and contribution to their country's success. The visualization emphasized individual dominance, allowing users to explore the achievements of these athletes. Athletes such as Summer McINTOSH from Canada and Leon MARCHAND from France both won four medals each highlighting exceptional achievement.

__3) Top 10 Countries by Medal Count:__ 

The analysis of the top 10 countries with the highest medal counts showcased their dominance in Olympic sports. Countries such as USA and China consistently performed well, excelling across various disciplines. Japan performed well in comparison to the gold medal ratio it has to the other countries. The stacked bar chart provided a clear breakdown of medal types (Gold, Silver, Bronze) for each country, offering a comprehensive understanding of their achievements.

__4) Height Correlation by Sport__

The analysis of athletes’ heights across various sports revealed notable trends. Sports like **athletics** and **badminton** were dominated by taller athletes, as their height offers a competitive edge in terms of reach and performance. On the other hand, sports such as **sport climbing** saw a higher prevalence of shorter athletes, where agility, flexibility, and a lower center of gravity are crucial for success. 

Additionally, gender-based differences in height were observed, with male athletes generally displaying greater height variability compared to female athletes. The box-plot visualization effectively showcased these patterns, emphasizing how physical demands shape the height distribution across sports and genders.

**Authors’ statements :**

"I ,BHANU NAGA SAI VAMSI BITRA had primary responsibility for the material in the DATA WRANGLING section, involving the conceptualization of insights and their visualization."

"I,SILVIU-DANIEL FAZEKAS, had primary responsibility for the material in ANALYTICS section."

"I, SANTHOSH PRABHAKARAN, had primary responsibility for the material in the ANALYTICS section."

"I, ANJU BABY, had primary responsibility for the material in the INTRODUCTION and CONCLUSION section."