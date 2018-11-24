library(readr)
library(stringr)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(ggplot2)

# Setup file paths
databaseName <- "OHDSI5PercentSampleDatabaseV522.sqlite"

# Create connection to database
loadedDatabase <- DBI::dbConnect(RSQLite::SQLite(), databaseName)
# Get overview of db
src_dbi(loadedDatabase)

# Access table names
src_tbls(src_dbi(loadedDatabase))

# Access column names for specific table
colnames(tbl(loadedDatabase, "relationship"))


# Define query: Option 1, dplyr syntax
# Get a specific table
care_siteTable <- tbl(loadedDatabase, "care_site")
# Define queries
result1<- care_siteTable %>% select(care_site_id)%>%summarize(count=n()) %>% collect()
result2 <- care_siteTable%>% filter(care_site_id==1) %>% collect()

# Define query: Option 2, SQL syntax
# Define query
tempQuery <- dbSendQuery(loadedDatabase, "SELECT * FROM care_site WHERE care_site_id = 1")
# Get results
result3<-dbFetch(tempQuery)
# Clear query
dbClearResult(tempQuery)


# Examples:

# Set table references
person <- tbl(loadedDatabase, "person")
concept <- tbl(loadedDatabase, "concept")




# Example 1: Analyse gender_concept

# Get number of persons in the database
numberPersons<- person %>%  
  summarise(countPersons = n()) %>%
  collect()
print(numberPersons)
numberPersons<-as.numeric(numberPersons)


# Get count for each gender concept
genderConcepts<- person %>%  
  group_by(gender_concept_id) %>%
  summarise(countGenderConcepts = n()) %>%
  collect()
print(genderConcepts)
genderConceptIDs<-as.numeric(genderConcepts$gender_concept_id)

# Get gender concept description from concept table
conceptDescription<-concept %>%
                      select(concept_id,concept_name) %>%
                      filter(concept_id %in% genderConceptIDs) %>%
                      collect()
  
# Get distribution of genders
maleFemaleDistribution<- person %>%  
  group_by(gender_concept_id) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /numberPersons) %>%
  select(gender_concept_id,percentage)%>%
  collect()

# Join tables to get names of gender
maleFemaleDistributionInclDescription<-left_join(x=maleFemaleDistribution,y=conceptDescription,by=c("gender_concept_id"="concept_id"))
print(maleFemaleDistributionInclDescription)
# Plot distribution
sexPlot<-ggplot(maleFemaleDistributionInclDescription,aes(x=concept_name,y=percentage))+
  ggtitle("Gender distribution")+
  xlab("Gender")+
  geom_col()


# Example 2: Analyse age distribution
# Get number of persons in the database
numberPersons<- person %>%  
  summarise(countPersons = n()) %>%
  collect()
print(numberPersons)
numberPersons<-as.numeric(numberPersons)

# Get number of missing values
missingYearOfBirth<- person %>%  
  select(year_of_birth) %>%
  summarise(countMissing = sum(is.na(year_of_birth),na.rm=TRUE)) %>%
  collect()

# Get distribution of year_of_birth
yearOfBirthDistribution<- person %>%  
  group_by(year_of_birth) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /numberPersons) %>%
  select(year_of_birth,percentage)%>%
  collect()
print(yearOfBirthDistribution)
# Plot age distribution
yearPlot<-ggplot(yearOfBirthDistribution,aes(x=year_of_birth,y=percentage))+
  ggtitle("Distribution of year of birth")+
  xlab("Year of birth")+
  geom_col()

# Get distribution of month_of_birth
monthOfBirthDistribution<- person %>%  
  group_by(month_of_birth) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /numberPersons) %>%
  select(month_of_birth,percentage)%>%
  collect()
print(monthOfBirthDistribution)
# Plot distribution of month_of_birth
monthPlot<-ggplot(monthOfBirthDistribution,aes(x=month_of_birth,y=percentage))+
  ggtitle("Distribution of birth of month")+
  xlab("Month of birth")+
  geom_col()

# Example 3: Race distribution
# Get distribution of race_concept
raceConceptDistribution<- person %>%  
  group_by(race_concept_id) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /numberPersons) %>%
  select(race_concept_id,percentage)%>%
  collect()
print(raceConceptDistribution)

# Get corresponding concept names
conceptDescription<-concept %>%
  select(concept_id,concept_name) %>%
  filter(concept_id %in% raceConceptDistribution$race_concept_id) %>%
  collect()
#
raceDistributionInclDescription<-left_join(x=raceConceptDistribution,y=conceptDescription,by=c("race_concept_id"="concept_id"))
raceDistributionInclDescription[1,3]="Missing"
# Plot distribution of month_of_birth
racePlot<-ggplot(raceDistributionInclDescription,aes(x=concept_name,y=percentage))+
  ggtitle("Race Distribution")+
  xlab("Race")+
  geom_col()

# Example 4: Ethnicity distribution
# Get distribution of race_concept
ethnicityConceptDistribution<- person %>%  
  group_by(ethnicity_concept_id) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /numberPersons) %>%
  select(ethnicity_concept_id,percentage)%>%
  collect()
print(ethnicityConceptDistribution)

# Get corresponding concept names
conceptDescription<-concept %>%
  select(concept_id,concept_name) %>%
  filter(concept_id %in% ethnicityConceptDistribution$ethnicity_concept_id) %>%
  collect()
# Connect tables
ethnicityDistributionInclDescription<-left_join(x=ethnicityConceptDistribution,y=conceptDescription,by=c("ethnicity_concept_id"="concept_id"))
# Plot distribution of month_of_birth
ethnicityPlot<-ggplot(ethnicityDistributionInclDescription,aes(x=concept_name,y=percentage))+
  ggtitle("Ethnicity Distribution")+
  xlab("Ethnicity")+
  geom_col()

# Save plots
pdf("exploratoryAnalysisPersonTable.pdf")
sexPlot
yearPlot
monthPlot
racePlot
ethnicityPlot
dev.off()
# Close connection
dbDisconnect(loadedDatabase)
