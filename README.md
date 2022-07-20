# COVID-19-Data
LFD tests reported by NHS staff, England

## NHS.Rcode


## Install packages ##

source("general_functions.R")

packages <- c("data.table", "tidyverse", "readxl",  "openxlsx",  "janitor", "tictoc",
              "lubridate",  "scales",  "directlabels", "ggrepel",
              "DBI",  "dbplyr", "readODS", "writexl", "xlsx")

instPackages(packages)

## Source codes:


## Run Publication function code

Publication_function <- function(Table_x) {
  Publication_table <- t(Table_x)
  
  colnames(Publication_table) <- Publication_table[1, ]
  RowNames <- row.names(Publication_table[-1, ])
  
  Publication_table <- as_tibble(Publication_table[-1, ]) %>%
    mutate(Metric = RowNames) %>%
    select(Metric, everything())
  
  NumCols <- c(2:ncol(Publication_table))
  Publication_table[NumCols] <- sapply(Publication_table[NumCols], as.numeric)
  Publication_table
}



## Connect to databases


#### Filter and produce R versions of databases ####

#Collect Poc secondary data and primary/secondary/independent npex data
#Then Merge poc secondary and npex secondary data, sum up all secondary data  into one bucket
#Plot primary, secondary and independent sectors, but with secondary a week behind the rest, as their full data comes in a week later

##########Import data##############

#Collect relevant fields filtered from the database

poc_tests <- pheTBL %>%
  filter(lower(TestMethod) %LIKE% '%lateral%' %AND% lower(RequestingOrganisationType) %like% '%worker testing%',
         TestDate >= publicationStartForLFD & TestDate < publicationEndPlusOne ) %>%
  select(TestDate, TestMethod, RequestingOrganisationType, OrganismCodeTranslation) %>%
  mutate(Test_Result = case_when(
    OrganismCodeTranslation == "" ~ "Positive",
    OrganismCodeTranslation == "002" ~ "Negative",
    TRUE ~ "Unknown/Void")) %>% collect()


# format test results
poc_tests_weekly <- poc_tests %>%
  mutate(Week_End = (6- (as.numeric(as.Date(TestDate) - as.Date("")) %% 7)) + as.Date(TestDate)) %>%
  mutate(Week_Start = Week_End - 6) %>%
  mutate(Week = paste(format(Week_Start,"%d/%m/%y"), "-", format(Week_End,"%d/%m/%y"))) %>% 
  arrange(TestDate) 

#Group into Positive, Negative, and Unknown/Void
poc_Results <-poc_tests_weekly %>%
  group_by(Week) %>%
  dplyr::summarise(Positive_poc_secondary = length(Test_Result[Test_Result == "Positive"]),
            Negative_poc_secondary = length(Test_Result[Test_Result == "Negative"]),
            Unknown_void_poc_secondary = length(Test_Result[Test_Result == "Unknown/Void"]))

#Add column for sums
cols_to_sum = c("Positive_poc_secondary", "Negative_poc_secondary", "Unknown_void_poc_secondary")
poc_Results$Total_poc_secondary <- rowSums(poc_Results[,cols_to_sum])

#Re-arrange the column order
poc_Results <- poc_Results[c(1,5,2,3,4)]



##########Import data##############

#Create Positive, Negative flag as well as Primary Secondary and Independent care

#### Filter and produce R versions of databases ####


npex_tests <- npexTBL %>%
  filter(
    as.Date(SpecimenProcessedDate) >= publicationStartForLFD &
      as.Date(SpecimenProcessedDate) < publicationEndPlusOne, CountryCode %like% "[E]%",
    lower(TestKit) %LIKE% "lft",
    OrganisationType %IN% c('GP', 'GPS', 'PHA', 'DSU', 'Dentist', 'OPH',
                            'ACT', 'AMT', 'CMT', 'MHU', 'ONE')
  ) %>%
  select(TestID, SpecimenID, TestKit, OrganisationType, SpecimenProcessedDate, Snowmed_translated_code, CountryCode) %>%
  group_by(SpecimenID, TestID) %>%
  window_order(desc(as_datetime(SpecimenProcessedDate))) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(Test_Date = as_date(SpecimenProcessedDate),
         Channel = case_when(
           OrganisationType  %in% c("GP", "GPS", "PHA", "DSU", "Dentist", "OPH") ~ "Primary care",
           OrganisationType %in% c("ACT", "AMT", "CMT", "MHU") ~ "Secondary care",
           OrganisationType %in% c("ONE") ~ "Independent care",
           TRUE ~ "Other")) %>%
  select(-all_of(c("TestID", "SpecimenID", "CountryCode"))) %>%
  collect()




npex_tests_weekly <- npex_tests %>%
  mutate(Test_Result = Snowmed_translated_code) %>%
  mutate(Week_End = (6 - (as.numeric(as.Date(Test_Date) - as.Date("")) %% 7)) + as.Date(Test_Date)) %>%
  mutate(Week_Start = Week_End - 6) %>%
  mutate(Week = paste(format(Week_Start, "%d/%m/%y"), "-", format(Week_End, "%d/%m/%y"))) %>%
  arrange(Test_Date)


#Primary results

npex_Results_primary <- npex_tests_weekly %>%
  filter(Channel=="Primary care") %>%
  group_by(Week) %>%
  summarise(Positive_npex_primary = length(Test_Result[Test_Result == "Positive"]),
            Negative_npex_primary = length(Test_Result[Test_Result == "Negative"]),
            Unknown_void_npex_primary = length(Test_Result[Test_Result == "Unknown/Void"]))


#Add column for sums
cols_to_sum = c("Positive_npex_primary", "Negative_npex_primary", "Unknown_void_npex_primary")
npex_Results_primary$Total_npex_primary <- rowSums(npex_Results_primary[,cols_to_sum])

#Re-arrange the column order
npex_Results_primary <- npex_Results_primary[c(1,5,2,3,4)]


#Secondary results
npex_Results_secondary <- npex_tests_weekly %>%
  filter(Channel=="Secondary care") %>%
  group_by(Week) %>%
  summarise(Positive_npex_secondary = length(Test_Result[Test_Result == "Positive"]),
            Negative_npex_secondary = length(Test_Result[Test_Result == "Negative"]),
            Unknown_void_npex_secondary = length(Test_Result[Test_Result == "Unknown/Void"]))

# Columns to sum  
cols_to_sum = c("Positive_npex_secondary", "Negative_npex_secondary", "Unknown_void_npex_secondary")
npex_Results_secondary$Total_npex_secondary <- rowSums(npex_Results_secondary[,cols_to_sum])

#Re-arrange the column order
npex_Results_secondary <- npex_Results_secondary[c(1,5,2,3,4)]

#Independent results
npex_Results_independent <- npex_tests_weekly %>%
  filter(Channel=="Independent care") %>%
  group_by(Week) %>%
  summarise(Positive_npex_independent = length(Test_Result[Test_Result == "Positive"]),
            Negative_npex_independent = length(Test_Result[Test_Result == "Negative"]),
            Unknown_void_npex_independent = length(Test_Result[Test_Result == "Unknown/Void"]))


#Add column for sums
cols_to_sum = c("Positive_npex_independent", "Negative_npex_independent", "Unknown_void_npex_independent")
npex_Results_independent$Total_npex_independent <- rowSums(npex_Results_independent[,cols_to_sum])

#Re-arrange the column order
npex_Results_independent <- npex_Results_independent[c(1,5,2)]

#Merge poc and npex primary on Week
MergeOne <- merge(poc_Results, npex_Results_primary, by ="Week", all=TRUE)

#Merge npex secondary as well on Week
MergeTwo <- merge(MergeOne, npex_Results_secondary, by ="Week", all=TRUE)

#Merge poc and npex primary/secondary with Independent on Week
MergeThree <- merge(MergeTwo, npex_Results_independent, by ="Week", all=TRUE)

#Re-arrange the column order
Reordered_MergeThree <- MergeThree[c(1,6,7,8,9)]

#Covert NA's to 0 
Reordered_MergeThree <- Reordered_MergeThree %>% mutate_all(list(~replace_na(.,0)))

#Arrange in date order
Reordered_MergeThree$Week_temporary <- as_date(str_sub(Reordered_MergeThree$Week,12,19),format="%d/%m/%y")
Reordered_MergeThree <- arrange(Reordered_MergeThree, Week_temporary)
#Reordered_MergeThree <- subset(Reordered_MergeThree, select = -c(Week_temporary))

#All data 
All_NHS <- Reordered_MergeThree %>%
  mutate(Total_secondary = Total_npex_secondary + Total_poc_secondary) %>%
  mutate(Positive_secondary = Positive_npex_secondary + Positive_poc_secondary) %>%
  mutate(Negative_secondary = Negative_npex_secondary + Negative_poc_secondary) %>%
  mutate(Unknown_void_secondary = Unknown_void_npex_secondary + Unknown_void_poc_secondary) %>%
  select(everything(), -c(Total_npex_secondary, Total_poc_secondary,
                          Positive_npex_secondary, Positive_poc_secondary,
                          Negative_npex_secondary, Negative_poc_secondary,
                          Unknown_void_npex_secondary, Unknown_void_poc_secondary))


# Transpose to publication format, suppress rows and columns
Table_13 <- All_NHS %>%
  filter(!Week_temporary <= "") %>%
  select(
    Week, Positive_npex_primary,
    Negative_npex_primary, Unknown_void_npex_primary
  ) %>%
  adorn_totals(c("row", "col"), name = "Total") %>%
  Publication_function() %>%
  slice(c(4, 1:3)) %>%
  suppressData() %>%
  suppressColumns() %>%
  mutate(across(matches(c("/", "Total")), function(x) as.numeric(gsub("\\D", NA, x))))

# Do not report on the current week for NHS secondary care
Table_14 <- All_NHS %>%
  filter(!Week_temporary == max(Week_temporary),!Week_temporary <= "") %>%
  select(
    Week, Positive_secondary,
    Negative_secondary, Unknown_void_secondary
  ) %>%
  adorn_totals(c("row", "col"), name = "Total") %>%
  Publication_function() %>%
  slice(c(4, 1:3)) %>%
  suppressData() %>%
  suppressColumns() %>%
  mutate(across(matches(c("/", "Total")), function(x) as.numeric(gsub("\\D", NA, x))))

Table_15 <- All_NHS %>%
  filter(!Week_temporary <= "2020-12-16") %>%
  select(
    Week, Positive_npex_independent,
    Negative_npex_independent, Unknown_void_npex_independent
  ) %>%
  adorn_totals(c("row", "col"), name = "Total") %>%
  Publication_function() %>%
  slice(c(4, 1:3)) %>%
  suppressData() %>%
  suppressColumns() %>%
  mutate(across(matches(c("/", "Total")), function(x) as.numeric(gsub("\\D", NA, x))))



# Prepare Plots -----------------------------------------------------------


#Prepare the All_NHS data for Figure6 plot
All_NHS_subset <- All_NHS %>%
  select(Week, Total_npex_primary, Total_npex_independent, Total_secondary, Week_temporary) %>%
  pivot_longer(!c(Week,Week_temporary),names_to = "Sector", values_to ="Tests") %>%
  arrange(Week_temporary) 

df15 <- All_NHS_subset %>%
  mutate(Sector = as.factor(ifelse(Sector == "Total_npex_primary", "Primary\ncare", 
                                    ifelse(Sector == "Total_npex_independent", "Other NHS", "NHS Trusts"))))




#Filtering out the entire last week for NHS Trust data 
df15$Week_temporary[df15$Sector=="NHS Trusts"][(length(df15$Sector)/3)] <- NA
df15$Tests[df15$Sector=="NHS Trusts"][(length(df15$Sector)/3)] <- NA

# Create evenly spaced x-axis that begins on first date and ends on last date.

StartingPoint <-1

if        ((length(df15$Week_temporary)-3)%%7 ==0 & ((length(df15$Week_temporary)-3)/7)%%3==0)  {
  labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=8)]
} else if ((length(df15$Week_temporary)-3)%%6 ==0 & ((length(df15$Week_temporary)-3)/6)%%3==0)  {
  labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=7)]                  
} else if ((length(df15$Week_temporary)-3)%%5 ==0 & ((length(df15$Week_temporary)-3)/5)%%3==0)  {
  labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=6)]  
} else {
  StartingPoint <- StartingPoint + 3
  if        ((length(df15$Week_temporary)-6)%%7 ==0 & ((length(df15$Week_temporary)-6)/7)%%3==0)  {
    labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=8)]
  } else if ((length(df15$Week_temporary)-6)%%6 ==0 & ((length(df15$Week_temporary)-6)/6)%%3==0)  {
    labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=7)]                  
  } else if ((length(df15$Week_temporary)-6)%%5 ==0 & ((length(df15$Week_temporary)-6)/5)%%3==0)  {
    labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=6)]  
  } else {
    StartingPoint <- StartingPoint + 3
    if        ((length(df15$Week_temporary)-9)%%7 ==0 & ((length(df15$Week_temporary)-9)/7)%%3==0)  {
      labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=8)]
    } else if ((length(df15$Week_temporary)-9)%%6 ==0 & ((length(df15$Week_temporary)-9)/6)%%3==0)  {
      labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=7)]                  
    } else if ((length(df15$Week_temporary)-9)%%5 ==0 & ((length(df15$Week_temporary)-9)/5)%%3==0)  {
      labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=6)]  
    } else {
      StartingPoint <- StartingPoint + 3
      if        ((length(df15$Week_temporary)-12)%%7 ==0 & ((length(df15$Week_temporary)-12)/7)%%3==0)  {
        labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=8)]
      } else if ((length(df15$Week_temporary)-12)%%6 ==0 & ((length(df15$Week_temporary)-12)/6)%%3==0)  {
        labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=7)]                  
      } else if ((length(df15$Week_temporary)-12)%%5 ==0 & ((length(df15$Week_temporary)-12)/5)%%3==0)  {
        labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=6)]  
      } else {
        StartingPoint <- StartingPoint + 3
        if        ((length(df15$Week_temporary)-15)%%7 ==0 & ((length(df15$Week_temporary)-15)/7)%%3==0)  {
          labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=8)]
        } else if ((length(df15$Week_temporary)-15)%%6 ==0 & ((length(df15$Week_temporary)-15)/6)%%3==0)  {
          labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=7)]                  
        } else if ((length(df15$Week_temporary)-15)%%5 ==0 & ((length(df15$Week_temporary)-15)/5)%%3==0)  {
          labels_to_plot<- df15$Week_temporary[seq(StartingPoint,length(df15$Week_temporary)-2,length.out=6)]  
        } else {
        #In case all attempts at spacing fail, return the entire date column
        labels_to_plot<- df15$Week_temporary
        }
      }     
    }  
  }
}


Figure6 <- {
  ggplot(data=df15, aes(x=Week_temporary, y=Tests, group = Sector, colour=Sector))+
    geom_line(aes(color=Sector),lineend="round", size=5)+ 
    scale_color_manual(values=c('#21569E','#999999','#DCA138'))+  
    ylab("Number of tests conducted")+
    xlab("Week ending")+
    geom_text_repel(aes(label = Sector), nudge_x=2, force=1,
                    hjust=0,size=15,fontface=2,na.rm = TRUE,
                    box.padding=0.5, segment.alpha=0, data = df15 %>% group_by(Sector) %>% arrange(Sector) %>%
                                            top_n(1, Week_temporary)) +
    scale_y_continuous(labels = scales::comma,expand = c(0,0), limits=c(0,max(df15$Tests,na.rm = TRUE)+50000),breaks=seq(0,50000+max(df15$Tests,na.rm = TRUE),by=50000))+
    scale_x_date(expand=c(0,1,0.25,0),date_labels = "%d %b %y",breaks = labels_to_plot)+

    theme_classic()+
    theme(axis.line=element_line(color = "black",size = 2))+
    theme(legend.position = "none", legend.text = element_text(size = 45),text = element_text(face="bold",size=20),
          axis.text.x = element_text(color = "black",size = 45),
          axis.title.x = element_text(color = "black",hjust = 0.9,vjust = 0.5,size=45),
          axis.text.y = element_text(color = "black",size=45),
          axis.title.y = element_text(color = "black",vjust=1.5,size=45),
          axis.ticks = element_line(color="black", size = 2),axis.ticks.length = unit(0.7,"cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}




