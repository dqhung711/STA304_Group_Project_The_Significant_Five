# Use the supplied aggregate demo by default. Team members with their own
# authorized copy of the survey can run the original live plot code instead.
csv <- Sys.getenv("STA304_SURVEY_CSV", unset = file.path("data", "The_Significant_Five_dataset.csv"))
if (nzchar(Sys.getenv("STA304_SURVEY_CSV")) && !file.exists(csv))
  stop("The file specified by STA304_SURVEY_CSV does not exist.")
entry <- if (file.exists(csv)) "survey-site.R" else "summary-site.R"
source(entry, local = TRUE, encoding = "UTF-8")$value
