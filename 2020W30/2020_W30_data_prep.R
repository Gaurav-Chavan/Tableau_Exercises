library(tidyverse)

Viz5_August_Female_Political_Representation <- read.csv("~/Viz5_August_Female_Political_Representation.csv", na.strings="", stringsAsFactors=FALSE)


# Dealing with missing values ----
colSums(is.na(Viz5_August_Female_Political_Representation))

# Missing values within group
missing_data <- filter(Viz5_August_Female_Political_Representation, is.na(Proportion.of.seats.held.by.women.in.national.parliaments....)) %>%
  group_by(Country.Name,Year) %>%       
  summarise(count = n()) %>%
  arrange(desc(count))


# difference vs Year Ago Compute ----

# The missing values are inconsistently missing by position within the group. 
# Use .direction = "downup" to fill missing values in both directions


temp2 <- Viz5_August_Female_Political_Representation %>% 
         group_by(Country.Name,Country.Code) %>%
         fill(Proportion.of.seats.held.by.women.in.national.parliaments...., .direction = "downup") %>%  # Missing Value Imputation  
         mutate(Diff_vs_Year_Ago = Proportion.of.seats.held.by.women.in.national.parliaments.... - lag(Proportion.of.seats.held.by.women.in.national.parliaments....)) %>% # Difference vs Year Ago
         fill(Diff_vs_Year_Ago, .direction = "downup") %>%  # Missing Value Imputation  
         mutate(Proportion.of.seats.held.by.women.in.national.parliaments.... = round(Proportion.of.seats.held.by.women.in.national.parliaments.... *100,2),
                Diff_vs_Year_Ago = round(Diff_vs_Year_Ago*100,2)
                )  


# Difference vs start Compute ----
temp2 <- temp2 %>%
  arrange(Year) %>%
  group_by(Country.Name) %>%
  mutate(Diff_vs_Start = Proportion.of.seats.held.by.women.in.national.parliaments.... - first(Proportion.of.seats.held.by.women.in.national.parliaments....))


# Compute Rank basis Diff_vs_Year_Ago,Diff_vs_Start ----
temp3 <- temp2 %>% 
         group_by(Country.Name) %>% 
         mutate(Rank_basis_Diff_vs_Year_Ago = order(order(Diff_vs_Year_Ago, decreasing=TRUE)),
                Rank_basis_Diff_vs_Start = order(order(Diff_vs_Start, decreasing=TRUE))
                )


colnames(temp3)[1] <-  "Country Name"
colnames(temp3)[2] <-  "Country Code"  
colnames(temp3)[4] <- "Proportion_of_seats"

write.csv(temp3,"~/Clean_data.csv",row.names = F)