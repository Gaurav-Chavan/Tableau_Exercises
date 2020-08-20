library(tidyverse)
library(plotly)


Female_Political_Representation <- read.csv("Viz5_August_Female_Political_Representation.csv", na.strings="", stringsAsFactors=FALSE)
colnames(Female_Political_Representation)[4] <-"Proportion of Seats"

head(Female_Political_Representation)


# Dealing with missing values ----
colSums(is.na(Female_Political_Representation))

# Missing values within group
missing_data <- filter(Female_Political_Representation, is.na(`Proportion of Seats`)) %>%
  group_by(Year) %>%       
  summarise(count = n()) %>%
  arrange(desc(count))


# Imputing Missing Value ----

Imputed_data <- Female_Political_Representation %>% 
                group_by(Country.Name,Country.Code) %>% # Group at Country Level
                fill(`Proportion of Seats`, .direction = "downup") # Missing Value Imputation  
  
colSums(is.na(Imputed_data)) # check for completeness


# difference vs Year Ago Compute ----

KPI1 <- Imputed_data %>% 
         group_by(Country.Name,Country.Code) %>%
         mutate(Diff_vs_Year_Ago = `Proportion of Seats` - lag(`Proportion of Seats`)) %>% # Difference vs Year Ago
         fill(Diff_vs_Year_Ago, .direction = "downup") %>%  # Missing Value Imputation  
         mutate(`Proportion of Seats` = round(`Proportion of Seats` *100,2),
                Diff_vs_Year_Ago = round(Diff_vs_Year_Ago*100,2) # Data Standardization
                )  


# Rank the differences at Yearly level ----
Rank1 <- KPI1 %>% 
         group_by(Year) %>% 
         mutate(rank_Diff_vs_Year_Ago = rank( - Diff_vs_Year_Ago, ties.method = "first"))



# Plot Top 10 Countries ----
top_10_plot <- Rank1 %>% 
  filter(Year == 2019 & rank < 11) %>% 
  ggplot(aes(x=reorder(Country.Name, Diff_vs_Year_Ago), y= Diff_vs_Year_Ago,fill = Country.Code)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none")  +
  xlab("Country") + 
  coord_flip()

# Convert to plotly
ggplotly(top_10_plot)


# Plot Bottom 10 Countries ----
bottom_10_plot <- Rank1 %>% 
  filter(Year == 2019 & rank >205) %>% 
  ggplot(aes(x=reorder(Country.Name, - Diff_vs_Year_Ago), y= Diff_vs_Year_Ago,fill = Country.Code)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none")  +
  xlab("Country") + 
  coord_flip()

# Convert to plotly
ggplotly(bottom_10_plot)




# Difference vs start Compute ----
KPI1 <- Rank1 %>%
  arrange(Year) %>%
  group_by(Country.Name) %>%
  mutate(Diff_vs_Start = `Proportion of Seats` - first(`Proportion of Seats`))


# Compute Rank basis at Year Yearly Basis
Rank2 <- KPI1 %>% 
         group_by(Year) %>% 
         mutate(rank_Diff_vs_Start = rank( - Diff_vs_Start, ties.method = "first"))



# Plot Top 10 Countries -----

top_10_plot1 <- Rank2 %>% 
  filter(Year == 2019 & rank_Diff_vs_Start < 11) %>% 
  ggplot(aes(x=reorder(Country.Name, Diff_vs_Start), y= Diff_vs_Start,fill = Country.Code)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none")  +
  xlab("Country") + 
  coord_flip()

# Convert to plotly
ggplotly(top_10_plot1)


# Plot Bottom 10 Countries ----
bottom_10_plot1 <- Rank2 %>% 
  filter(Year == 2019 & rank_Diff_vs_Start > 205) %>% 
  ggplot(aes(x=reorder(Country.Name, - Diff_vs_Start), y= Diff_vs_Start,fill = Country.Code)) +
  geom_bar(stat="identity")+
  theme_minimal() +
  theme(legend.position = "none")  +
  xlab("Country") + 
  coord_flip()

# Convert to plotly
ggplotly(bottom_10_plot1)


# Global Level Trend ----
Global_Insight <- KPI1 %>% 
                  group_by(Year) %>% 
                  summarize(
                    Avg_Proportion_of_Seats = mean(`Proportion of Seats`),
                    Avg_Diff_vs_Year_Ago = mean(Diff_vs_Year_Ago),
                    Avg_Diff_vs_Start = mean(Diff_vs_Start)
                    
                  )


# Area plot
Avg_Proportion_of_Seats_plot <-  ggplot(Global_Insight, aes(x = Year, y = Avg_Proportion_of_Seats)) + 
  geom_area(fill ="#00AFBB", alpha = 0.5, position = position_dodge(0.8))

ggplotly(Avg_Proportion_of_Seats_plot)


Avg_Diff_vs_Year_Ago_plot <-  ggplot(Global_Insight, aes(x = Year, y = Avg_Diff_vs_Year_Ago)) + 
  geom_area(fill ="#ff7a00", alpha = 0.5, position = position_dodge(0.8))

ggplotly(Avg_Diff_vs_Year_Ago_plot)

Avg_Diff_vs_Start_plot <-  ggplot(Global_Insight, aes(x = Year, y = Avg_Diff_vs_Start)) + 
  geom_area(fill ="#0dff00", alpha = 0.5, position = position_dodge(0.8))

ggplotly(Avg_Diff_vs_Start_plot)

# Combine 3 plots ----
library(ggpubr)

theme_set(theme_pubr())

figure <- ggarrange(Avg_Proportion_of_Seats_plot, Avg_Diff_vs_Year_Ago_plot, Avg_Diff_vs_Start_plot,
                    labels = c("Avg_Proportion_of_Seats", "Avg_Diff_vs_Year_Ago", "Avg_Diff_vs_Start"),
                    ncol = 1, nrow = 3)

figure

colnames(Rank2)[1] <-  "Country Name"
colnames(Rank2)[2] <-  "Country Code"  
colnames(Rank2)[4] <- "Proportion_of_seats"

write.csv(temp3,"Clean_data.csv",row.names = F)