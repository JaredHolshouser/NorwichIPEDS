library(tidyverse)
library(scales)

#Load the file
TenureStatus <- read_csv("TenureStatus.csv")

#Clean up the column names
TenureStatus <- janitor::clean_names(TenureStatus)

#Plot the total by the year
TenureStatus |>
  ggplot(aes(x = year, y = total)) +
  geom_line() +
  labs(
    y = "Total",
    x = "Year",
    title = "Total Academic Workforce Per Audit"
  ) +
  scale_x_continuous(
    breaks = seq(2002,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(180,440, by = 20)
  )

#pivot the data longer
TenureStatus <- TenureStatus |>
  select(!total) |>
  pivot_longer(
    cols = c(!year),
    names_to = "employment_status",
    values_to = "count"
  )

#Clean up the category names
TenureStatus <- TenureStatus |>
  mutate(employment_status = str_replace_all(employment_status,"_", " ")) |> 
  mutate(employment_status = gsub("(?<=\\b)([a-z])", "\\U\\1", employment_status, perl=TRUE))

#Column Plot
TenureStatus |>
  group_by(year,employment_status) |>
  summarize(percentage = sum(count)) |>
  mutate(percentage = percentage/sum(percentage)*100) |>
  ggplot(aes(x = year, y = percentage, fill = employment_status)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(2002,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0,100, by = 10)
  ) +
  labs(
    title = "Faculty Distribution per Audit",
    x = "Year",
    y = "Percent",
    fill = "Employment Status",
  ) +
  scale_fill_brewer(palette = "Spectral")

#Line Plot
TenureStatus |>
  ggplot(aes(x = year, y = count, color = employment_status)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(2002,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0,260, by = 20)
  ) +
  labs(
    title = "Faculty Headcount per Audit",
    x = "Year",
    y = "Headcount",
    color = "Employment Status",
  ) +
  scale_fill_brewer(palette = "Spectral")

#Build the pivot table of FTE Staff
TenureStatus_pivot <- TenureStatus |>
  pivot_wider(
    names_from = year,
    values_from = count
  )

write_csv(TenureStatus_pivot,"Tenure Status by Year.csv")

TenureStatus |>
  group_by(employment_status) |>
  mutate(
    percent_change = (count - lag(count))/lag(count)*100
  ) |>
  filter(!is.na(percent_change)) |>
  ggplot() +
  geom_bar(aes(x = year, y = percent_change, fill = employment_status),width = 0.6, position = "dodge", stat = "identity") +
  labs(
    x = "Year",
    y = "Percent Change",
    fill = "Employment Status",
    title = "Percent Change in Employment Status Over Previous Year"
  ) +
  scale_x_continuous(
    breaks = seq(2003,2022,by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(-30,120,by = 10)
  ) +
  scale_fill_brewer(palette = "Spectral")
