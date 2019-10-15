source("data.r")

IGNORE_DOC_TYPE <- 1

redet_receipts <- redets %>%
  filter(recert_date == as_date("2019-10-31")) %>%
  inner_join(filter(docuclass, type == "RP – Redetermination" | 1==IGNORE_DOC_TYPE), by = "family_id") %>%
  group_by(family_id) %>%
  summarize(first_receipt = min(date)) %>%
  left_join(mutate(contacted, contacted = TRUE), by = "family_id") %>%
  left_join(mutate(pilot_participants, opted_in = TRUE), by = "family_id") %>%
  replace_na(list(contacted = FALSE, opted_in = FALSE))

table(redet_receipts$contacted, redet_receipts$opted_in)

most_recent_date_to_include <- min(max(calls$date), max(docuclass$date)) - 7
receipts_older_than_one_week <- redet_receipts %>%
  filter(first_receipt <= most_recent_date_to_include)

table(receipts_older_than_one_week$contacted, receipts_older_than_one_week$opted_in)

receipts_with_aftermath <- receipts_older_than_one_week %>%
  left_join(left_join(calls, redets, by = c("caller" = "phone")), by = "family_id") %>%
  filter(is.na(date) | (date >= first_receipt & date < first_receipt + 7)) %>%
  group_by(family_id, first_receipt, contacted, opted_in) %>%
  summarize(call_count = sum(!is.na(date))) %>%
  left_join(docuclass, by = "family_id") %>%
  filter(is.na(date) | (date >= first_receipt & date < first_receipt + 7)) %>%
  group_by(family_id, first_receipt, contacted, opted_in, call_count) %>%
  summarize(doc_count = sum(!is.na(date)))

receipts_with_aftermath %>%
  group_by(contacted, opted_in) %>%
  summarize(n = n(), mean(call_count))

receipts_with_aftermath %>%
  group_by(contacted) %>%
  summarize(n = n(), mean(call_count))

receipts_with_aftermath %>%
  group_by(contacted, opted_in) %>%
  summarize(n = n(), mean(doc_count))

receipts_with_aftermath %>%
  group_by(contacted) %>%
  summarize(n = n(), mean(doc_count))

t.test(call_count ~ contacted, data = receipts_with_aftermath)

t.test(doc_count ~ contacted, data = receipts_with_aftermath)

