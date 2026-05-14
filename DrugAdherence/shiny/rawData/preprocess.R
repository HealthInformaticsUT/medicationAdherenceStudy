# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_medication_adherence = list(result_type = "summarise_medication_adherence"),
  summarise_medication_adherence_cma_over_0.8 = list(result_type = "summarise_medication_adherence_cma_over_0.8"),
  summarise_mean_continuous_adherence_window = list(result_type = "summarise_mean_continuous_adherence_window"),
  summarise_adherence_breaks = list(result_type = "summarise_adherence_breaks")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))


data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
