library(dplyr)
library(acs)
library(datapkg)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Housing Tenure by Race and Ethnicity by Town
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Get state data
geography=geo.make(state=09)
yearlist=c(2019:2019)
span = 5
col.names="pretty" 
key="ed0e58d2538fb239f51e01643745e83f380582d7"
options(scipen=999)

tables <- c("", "A", "B", "C", "D", "E", "F", "G", "H", "I")
races <- c("All", "White Alone", "Black or African American Alone", "American Indian and Alaska Native Alone", 
           "Asian Alone", "Native Hawaiian and Other Pacific Islander", "Some Other Race Alone", 
           "Two or More Races", "White Alone Not Hispanic or Latino", "Hispanic or Latino")

state_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    variable =list()      
    for (k in seq_along(1:3)) {
     number = number=paste0("B25003", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }  
    variable <- as.character(variable)
    Sys.sleep(4)
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                    variable = variable, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$NAME <- NULL
    total <- data[,1]
    acs.colnames(total) <- "Number:Total"
    renter <- data[,3]
    acs.colnames(renter) <- "Number:Renter Occupied"
    percent.renter <- divide.acs(renter, total, method = "proportion")
    acs.colnames(percent.renter) <- "Percent_Race:Renter Occupied"
    owner <- data[,2]
    acs.colnames(owner) <- "Number:Owner Occupied"
    percent.owner <- divide.acs(owner, total, method = "proportion")
    acs.colnames(percent.owner) <- "Percent_Race:Owner Occupied"
     numberEstimates <- data.table(
            geo, 
            estimate(total),
            estimate(renter),
            estimate(owner),
            year,
            race,
            "Number",
            "Occupied Housing Units"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(renter) * 1.645,
            standard.error(owner) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Number:Total",
            "Number:Renter Occupied",
            "Number:Owner Occupied",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)
    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Housing Units",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    percentEstimates <- data.table(
            geo,
            estimate(percent.renter),
            estimate(percent.owner),
            year,
            race,
            "Percent",
            "Occupied Housing Units"
        )
    percentMOES <- data.table(
                geo,
                standard.error(percent.renter) * 1.645,
                standard.error(percent.owner) * 1.645,
                year,
                race,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "FIPS",
            "Percent_Race:Renter Occupied",
            "Percent_Race:Owner Occupied",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimates, percentMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Housing Units",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )

    inter_data <- rbind(inter_data, numbersData.melt, percentData.melt)
    
   if (race == "All") {
        all.total <- data[,1]
        acs.colnames(all.total) <- "Total"
    } else {
        # use all.total as denominator, add to dataset
        percent_state.total <- divide.acs(total, all.total, method = "proportion")
        acs.colnames(percent_state.total) <- "Percent_State:Total"

        percent_state.renter <- divide.acs(renter, all.total, method = "proportion")
        acs.colnames(percent_state.renter) <- "Percent_State:Renter Occupied"

        percent_state.owner <- divide.acs(owner, all.total, method = "proportion")
        acs.colnames(percent_state.owner) <- "Percent_State:Owner Occupied"
      
        percent_stateEstimates <- data.table(
                geo,
                estimate(percent_state.total),
                estimate(percent_state.renter),
                estimate(percent_state.owner),
                year,
                race,
                "Percent_State",
                "Occupied Housing Units"
            )
        percent_stateMOES <- data.table(
                    geo,
                    standard.error(percent_state.total) * 1.645,
                    standard.error(percent_state.renter) * 1.645,
                    standard.error(percent_state.owner) * 1.645,
                    year,
                    race,
                    "Percent_State",
                    "Margins of Error"
                )
        percent_stateNames <- c(
                "FIPS",
                "Percent_State:Total",
                "Percent_State:Renter Occupied",
                "Percent_State:Owner Occupied",
                "Year",
                "Race/Ethnicity",
                "Measure Type",
                "Variable"
             )
        setnames(percent_stateEstimates, percent_stateNames)
        setnames(percent_stateMOES, percent_stateNames)
        percent_stateData.melt <- melt(
                rbind(percent_stateEstimates, percent_stateMOES),
                id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
                variable.name="Housing Units",
                variable.factor = F,
                value.name="Value",
                value.factor = F
             )
        
    inter_data <- rbind(inter_data, percent_stateData.melt)
    }
  }
  state_data <- rbind(state_data, inter_data)
}

#Get town data
geography=geo.make(state=09, county="*", county.subdivision = "*")

town_data <- data.table()
for (i in seq_along(yearlist)) {
  endyear = yearlist[i]
  inter_data <- data.table()
  for (j in seq_along(tables)) {
    tbl <- tables[j]
    race <- races[j]
    variable =list()      
    for (k in seq_along(1:3)) {
     number = number=paste0("B25003", tbl, "_", sprintf("%03d",k))
     variable = c(variable, number)
     k=k+1
    }  
    variable <- as.character(variable)
    Sys.sleep(4)
    data <- acs.fetch(geography=geography, endyear=endyear, span=span, 
                    variable = variable, key=key)
    year <- data@endyear
    print(paste("Processing: ", year, race))
    year <- paste(year-4, year, sep="-")
    geo <- data@geography
    geo$county <- sprintf("%02d", geo$county)
    geo$county <- gsub("^", "090", geo$county)
    geo$FIPS <- paste0(geo$county, geo$countysubdivision)
    geo$state <- NULL
    geo$NAME <- NULL
    geo$countysubdivision <- NULL
    geo$county <- NULL
    total <- data[,1]
    acs.colnames(total) <- "Number:Total"
    renter <- data[,3]
    acs.colnames(renter) <- "Number:Renter Occupied"
    percent.renter <- divide.acs(renter, total, method = "proportion")
    acs.colnames(percent.renter) <- "Percent_Race:Renter Occupied"
    owner <- data[,2]
    acs.colnames(owner) <- "Number:Owner Occupied"
    percent.owner <- divide.acs(owner, total, method = "proportion")
    acs.colnames(percent.owner) <- "Percent_Race:Owner Occupied"
     numberEstimates <- data.table(
            geo, 
            estimate(total),
            estimate(renter),
            estimate(owner),
            year,
            race,
            "Number",
            "Occupied Housing Units"
        )
    numberMOES <- data.table(
            geo,
            standard.error(total) * 1.645,
            standard.error(renter) * 1.645,
            standard.error(owner) * 1.645,
            year,
            race,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Number:Total",
            "Number:Renter Occupied",
            "Number:Owner Occupied",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)
    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Housing Units",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    percentEstimates <- data.table(
            geo,
            estimate(percent.renter),
            estimate(percent.owner),
            year,
            race,
            "Percent",
            "Occupied Housing Units"
        )
    percentMOES <- data.table(
                geo,
                standard.error(percent.renter) * 1.645,
                standard.error(percent.owner) * 1.645,
                year,
                race,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "FIPS",
            "Percent_Race:Renter Occupied",
            "Percent_Race:Owner Occupied",
            "Year",
            "Race/Ethnicity",
            "Measure Type",
            "Variable"
         )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimates, percentMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
            variable.name="Housing Units",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )

    inter_data <- rbind(inter_data, numbersData.melt, percentData.melt)
    
   if (race == "All") {
        all.total <- data[,1]
        acs.colnames(all.total) <- "Total"
    } else {
        # use all.total as denominator, add to dataset
        percent_state.total <- divide.acs(total, all.total, method = "proportion")
        acs.colnames(percent_state.total) <- "Percent_State:Total"

        percent_state.renter <- divide.acs(renter, all.total, method = "proportion")
        acs.colnames(percent_state.renter) <- "Percent_State:Renter Occupied"

        percent_state.owner <- divide.acs(owner, all.total, method = "proportion")
        acs.colnames(percent_state.owner) <- "Percent_State:Owner Occupied"
      
        percent_stateEstimates <- data.table(
                geo,
                estimate(percent_state.total),
                estimate(percent_state.renter),
                estimate(percent_state.owner),
                year,
                race,
                "Percent_State",
                "Occupied Housing Units"
            )
        percent_stateMOES <- data.table(
                    geo,
                    standard.error(percent_state.total) * 1.645,
                    standard.error(percent_state.renter) * 1.645,
                    standard.error(percent_state.owner) * 1.645,
                    year,
                    race,
                    "Percent_State",
                    "Margins of Error"
                )
        percent_stateNames <- c(
                "FIPS",
                "Percent_State:Total",
                "Percent_State:Renter Occupied",
                "Percent_State:Owner Occupied",
                "Year",
                "Race/Ethnicity",
                "Measure Type",
                "Variable"
             )
        setnames(percent_stateEstimates, percent_stateNames)
        setnames(percent_stateMOES, percent_stateNames)
        percent_stateData.melt <- melt(
                rbind(percent_stateEstimates, percent_stateMOES),
                id.vars=c("FIPS", "Year", "Measure Type", "Variable", "Race/Ethnicity"),
                variable.name="Housing Units",
                variable.factor = F,
                value.name="Value",
                value.factor = F
             )
        
    inter_data <- rbind(inter_data, percent_stateData.melt)
    }
  }
  town_data <- rbind(town_data, inter_data)
}

tenure_data <- rbind(state_data, town_data)

#Merge in towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

tenure_data <- merge(tenure_data, towns, by = "FIPS")

tenure_data[,c("Measure Type", "Tenure"):=do.call(Map, c(f=c, strsplit(`Housing Units`, ":", fixed=T)))]
tenure_data[,`Housing Units` := NULL]

# re-code the proportions to percent of state total so it is less confusing
tenure_data$`Measure Type`[tenure_data$`Measure Type` == "Percent_Race" & tenure_data$`Race/Ethnicity` == "All"] <- "Percent_State"

# Re-code measure types
tenure_data[
        `Measure Type` == "Percent_State",
        `Measure Type` := "Percent of Total Housing Units"
    ][
        `Measure Type` == "Percent_Race",
        `Measure Type` := "Percent of Racial/Ethnic Subgroup"
    ]

# Round Values according to type/variable
tenure_data[`Measure Type` == "Number", Value := round(Value, 2)]
tenure_data[`Measure Type` != "Number", Value := round(Value*100, 2)]
tenure_data[Variable == "Margins of Error", Value := round(Value, 0)]

tenure_data$Tenure <- factor(tenure_data$Tenure, levels = c("Total", "Owner Occupied", "Renter Occupied"))

#Code NaNs to NAs
tenure_data$Value[tenure_data$Value == "NaN"] <- NA

#set final column order
tenure_data <- tenure_data %>% 
  select(Town, FIPS, Year, `Race/Ethnicity`, Tenure, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Race/Ethnicity`, Tenure, `Measure Type`, desc(Variable))

write.table (
  tenure_data,
  file.path(getwd(), "data", "housing_tenure_race_town_2019-only.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
