# Header ------------------------------------------------------------------
# regression_analysis.r
#
# This program reads data used in the California School Dashboard and performs
# linear regression analyses of percent of students who are socioeconomically
# disadvantaged on distance from standard on math and ELA among high schools.
#
# Written by Stephen Lew



# Load packages -----------------------------------------------------------
library(tidyverse)
library(modelr)



# Extract data ------------------------------------------------------------
# Extract data on total enrollment and percent of students who are
# socioeconomically disadvantaged
enrollment <- read_tsv("https://www3.cde.ca.gov/researchfiles/cadashboard/censusenrollratesdownload2024.txt", guess_max = Inf) |>
  rename_with(tolower) |>
  # Keep only records for schools. Drop district and state records.
  filter(rtype == "S") |>
  # Keep the record with data on socioeconomically disadvantaged students if possible.
  # Schools that do not have any socioeconomically disadvantaged students do not
  # have such a record. For those schools, keep the first record and then set the
  # percentage of socioeconomically disadvantaged students to zero.
  mutate(sed_record = if_else(studentgroup == "SED", 1, 0)) |>
  arrange(cds, desc(sed_record)) |>
  group_by(cds) |>
  slice(1)
  # Data is now one record per school uniquely identified by cds

enrollment <- enrollment |>
  rename(sed = rate) |>
  mutate(sed = if_else(sed_record == 0, 0, sed)) |>
  select(cds, schoolname, districtname, totalenrollment, sed)

# Extract data on distance from standard on math and ELA
extract_assessments <- function(subj, url) {
  read_tsv(url, guess_max = Inf) |>
    rename_with(tolower) |>
    # Keep only records for schools that have the data for all students.
    filter(rtype == "S" & studentgroup == "ALL") |>
    # Data is now one record per school uniquely identified by cds.
    mutate({{subj}} := currstatus) |>
    select(cds, {{subj}})
}
math <- extract_assessments(subj = math, url = "https://www3.cde.ca.gov/researchfiles/cadashboard/mathdownload2024.txt")
ela <- extract_assessments(subj = ela, url = "https://www3.cde.ca.gov/researchfiles/cadashboard/eladownload2024.txt")

# Extract a list of high schools. High schools have a record in the graduation
# rate data
hs <- read_tsv("https://www3.cde.ca.gov/researchfiles/cadashboard/graddownload2024.txt", guess_max = Inf) |>
  rename_with(tolower) |>
  # Keep only records for schools. Drop district and state records.
  filter(rtype == "S") |>
  select(cds) |>
  group_by(cds) |>
  slice(1)
  # Data is now one record per school uniquely identified by cds



# Integrate data ----------------------------------------------------------
analysis <- hs |>
  inner_join(enrollment, join_by(cds)) |>
  left_join(math, join_by(cds)) |>
  left_join(ela, join_by(cds))



# Data analysis ----------------------------------------------
# Linear regression analyses of percent of students who are socioeconomically
# disadvantaged on distance from standard on math and ELA among high schools. Total
# enrollment is used as the analytic weight.
math_model <- lm(math ~ sed, data = analysis, weights = totalenrollment)
ela_model <- lm(ela ~ sed, data = analysis, weights = totalenrollment)
analysis <- analysis |>
  add_predictions(math_model, var = "mathpredicted") |>
  add_residuals(math_model, var = "mathresid") |>
  add_predictions(ela_model, var = "elapredicted") |>
  add_residuals(ela_model, var = "elaresid") |>
  write_csv("C:/Users/Public/Documents/regression_analysis_r.csv")

ggplot() +
  geom_point(data = analysis, mapping = aes(x = sed, y = math)) +
  geom_smooth(data = analysis, mapping = aes(x = sed, y = mathpredicted))

ggplot() +
  geom_point(data = analysis, mapping = aes(x = sed, y = ela)) +
  geom_smooth(data = analysis, mapping = aes(x = sed, y = elapredicted))

