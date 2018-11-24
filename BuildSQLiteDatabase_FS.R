library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(dbplyr)
library(RSQLite)

# General description:
# Create an SQLite database for 5%SynPUF sample data from OHDSI Website

# Requirements for usage:
# Part 1: Have the file "Data dictionary_OMOP_12Nov2018.xlsx" in your working directory
# Part 2: Have the "synpuf5pct_20180710" folder in your working directory

# Instructions:
# 1.) Run this script with loop indexes "1:length(fileList)"
# 2.) If the for loop fails at some point (in my experience, if it does, typically during the iteration i=10, which is the "cost.csv"),
#     this will be due to a (full) memory error.
#     Look at the print output and remember the last finished iteration (e.g. 9)
#     Close Rstudio and reopen it to free up the memory.
#     (To my knowledge this is a problem with R/RStudio garbage collection).
# 3.) Adjust the for loop to run from the last unloaded index (e.g. 10, "10:length(fileList)") and run the script.
# 4.) Repeat if necessary

# Author: Fabian Schildmann

# Adjust paths here if necessary:
pathsWD="./"
  
# Setup database
databaseName <- "OHDSI5PercentSampleDatabaseV522.sqlite"
OHDSIDatabase <- src_sqlite(databaseName, create = TRUE)

# Define filelist
fileList<-c("care_site.csv","cdm_source.csv","concept.csv",
            "concept_ancestor.csv","concept_class.csv","concept_relationship.csv",
            "concept_synonym.csv","condition_era.csv","condition_occurrence.csv",
            "cost.csv","death.csv","device_exposure.csv",
            "domain.csv","drug_era.csv","drug_exposure.csv",
            "drug_strength.csv","location.csv","measurement.csv",
            "observation.csv","observation_period.csv","payer_plan_period.csv",
            "person.csv","procedure_occurrence.csv","provider.csv",
            "relationship.csv","visit_occurrence.csv","vocabulary.csv")

# Adjust loop index here, if necessary
for (i in 1:length(fileList)){
# for (i in 10:10){
  # Update current file
  chosenFile<-fileList[[i]]
  
  # Read in full data dictionary
  dataDic<-read_excel(paste(pathsWD,"Data dictionary_OMOP_12Nov2018.xlsx",sep=""),sheet="OMOP CDM_v5.2")  
  # Get subset of dictionary for chosen file
  subDataDic<-dataDic[sapply(dataDic[,2], tolower)==str_split(chosenFile,".csv")[[1]][1],]
  # Create vector which uses known column types from OMOP-Model and create R equivalent specification
  colTypes<-str_to_upper(subDataDic$Type)
  colTypes<-str_replace(colTypes,"BIG INTEGER","i")
  colTypes<-str_replace(colTypes,"CLOB","c")
  colTypes[str_detect(colTypes,fixed("DATE")) & !str_detect(colTypes,fixed("DATETIME"))]<-"D"
  colTypes<-str_replace(colTypes,"DATETIME","T")
  colTypes<-str_replace(colTypes,"FLOAT","d")
  colTypes<-str_replace(colTypes,"INTEGER","i")
  colTypes<-str_replace(colTypes,"RBDMS DEPENDENT TEXT","c")
  colTypes[str_detect(colTypes,"STRING")]<-"c"
  colTypes[str_detect(colTypes,"VARCHAR")]<-"c"
  # Get column descriptions
  colDescriptions<-subDataDic$Description

  # Load "cost.csv" in two steps due to memory problems
  if (i==10){
    OHDSIDatabase <- dbConnect(RSQLite::SQLite(), databaseName)
    # First part of the dataset
    # Load file
    loadedFile<-read_tsv(paste(pathsWD,"synpuf5pct_20180710/",chosenFile,sep=""),
                         col_names=subDataDic$Field,
                         col_types = as.list(colTypes),n_max=2*10^7)
    # Add current table to database
    # copy_to(OHDSIDatabase,df=loadedFile,append=TRUE,overwrite=TRUE,name = str_split(chosenFile,".csv")[[1]][1])
    dbWriteTable(OHDSIDatabase,name = str_split(chosenFile,".csv")[[1]][1],value = loadedFile,row.names=FALSE, overwrite=TRUE)
    # Free memory
    rm(loadedFile)
    gc() 
    
    # Second part of the dataset
    # Load file
    loadedFile<-read_tsv(paste(pathsWD,"synpuf5pct_20180710/",chosenFile,sep=""),
                         col_names=subDataDic$Field,
                         col_types = as.list(colTypes),skip=2*10^7)
    # Add current table to database
    # copy_to(OHDSIDatabase,df=loadedFile,append=TRUE,temporary=FALSE,name = str_split(chosenFile,".csv")[[1]][1])
    dbWriteTable(OHDSIDatabase,row.names=FALSE, value = loadedFile, name = str_split(chosenFile,".csv")[[1]][1], append=TRUE)
    rm(loadedFile)
    gc() 
    dbDisconnect(OHDSIDatabase)
  # Read in all other tables the following way:
  } else {
    OHDSIDatabase <- src_sqlite(databaseName, create = TRUE)
    # Load file
    loadedFile<-read_tsv(paste(pathsWD,"synpuf5pct_20180710/",chosenFile,sep=""),
                         col_names=subDataDic$Field,
                         col_types = as.list(colTypes))
    # Add current table to database
    copy_to(OHDSIDatabase,df=loadedFile,append=TRUE,overwrite=TRUE,temporary=FALSE,name = str_split(chosenFile,".csv")[[1]][1])
    rm(loadedFile)
    gc() 
  }
  
  print(paste(paste(i, " of ", length(fileList)," is finished.",sep=""),
              paste(" This is file: ",fileList[[i]],sep=""),sep=""))
}

rm(OHDSIDatabase)
gc()