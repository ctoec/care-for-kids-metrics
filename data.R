require(tidyverse)
require(lubridate)
require(readxl)

raw_calls = data.frame()
for (.file in list.files("original-data/cisco")) {
  raw_calls <- rbind(raw_calls, read_csv(
    file.path(".", "original-data/cisco", .file),
    col_types = cols(
      `Node ID - Session ID - Sequence Number` = col_character(),
      `Start Time` = col_character(),
      `End Time` = col_character(),
      `Contact\\Type` = col_factor(),
      `Contact\\Disposition` = col_factor(),
      `Originator\\Type` = col_factor(),
      `Originator\\ID` = col_character(),
      `Originator\\Directory Number` = col_character(),
      `Destination\\Type` = col_integer(),
      `Destination\\ID` = col_character(),
      `Destination\\Directory Number` = col_character(),
      `Called Number` = col_character(),
      `Original Called Number` = col_character(),
      `Application Name` = col_character(),
      `Queue Time` = col_integer(),
      `Talk Time` = col_integer(),
      `Hold Time` = col_integer(),
      `Work Time` = col_integer()
    )
  ))
}

calls <- raw_calls %>%
  transmute(
    caller = `Originator\\Directory Number`,
    date = as_date(parse_date_time(`Start Time`, c("abd HMS Y")))
  )

raw_redets <- read_xlsx(
    "original-data/Sept2019_Redets.xlsx",
    col_types = "text"
  ) %>%
  mutate(
    RECERT_DATE = as_date("2019-10-31")
  )

for (.sheet in 1:3) {
  raw_redets <- rbind(raw_redets, read_xlsx(
    "original-data/Skylight - ImpaCT Phone Numbers for Cases Up for Redet Jun-Aug2019.xlsx",
    sheet = .sheet,
    col_types = c("text", "text", "text", "text", "text", "text", "text", "date")
  ))
}

redets <- raw_redets %>%
  transmute(
    family_id = FAMILY_ID,
    phone = PHONE_NUMBER,
    recert_date = RECERT_DATE
  )

raw_pilot_participants <- read_csv(
  "original-data/Care 4 Kids Pilot Participants Tracker - Confirmed Participants.csv",
  col_types = cols(
    `#` = col_integer(),
    `Case/Family ID` = col_character(),
    `Phone Number` = col_character(),
    `Date Contacted` = col_character(),
    `Contacted By` = col_character(),
    `Confirmed?` = col_logical(),
    Notes = col_character(),
    query = col_character(),
    `added to database` = col_character()
  )
)

pilot_participants <- raw_pilot_participants %>%
  transmute(
    family_id = `Case/Family ID`
  ) %>%
  filter(!is.na(family_id)) %>%
  unique()

contacted <- read_csv("original-data/contacted.csv", col_types = list(col_character())) %>%
  unique()

raw_docuclass <- read_csv(
  "original-data/C4K Pilot Data Summary - raw-reporting-db-export.csv",
  col_types = cols(
    `Case ID` = col_character(),
    `Doc Type` = col_factor(levels = c(
      "NA – Application",
      "RP – Redetermination",
      "SP – Parent Supporting Document",
      "PS – Provider Supporting Document",
      "PR – W-9",
      "DR – DCFR form",
      "FR – FRED Document",
      "AC – ACAP Document"
    )),
    `Archived At` = col_datetime(format = "%Y-%m-%d %H:%M:%S %z"),
    Phone = col_character(),
    ExportedAt = col_datetime(format = "%Y-%m-%d %H:%M:%S %z"),
    ExportTime = col_time(),
    Source = col_factor(levels = c("Faxes", "Emails", "Scans")),
    `Document ID` = col_character()
  )
)

docuclass <- raw_docuclass %>%
  transmute(
    family_id = `Case ID`,
    type = `Doc Type`,
    date = as_date(ExportedAt),
    source = Source
  )
