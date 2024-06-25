library(tidyverse)

#Load the file
expenses_pct <- read_csv("Expenses Percent.csv")

#Clean up the column names
expenses_pct <- janitor::clean_names(expenses_pct)

#Rename the columns
glimpse(expenses_pct)

expenses_pct <- expenses_pct |> rename(
  instruction = drvf2014_rv_instruction_expenses_as_a_percent_of_total_core_expenses_fasb,
  research = drvf2014_rv_research_expenses_as_a_percent_of_total_core_expenses_fasb,
  public_service = drvf2014_rv_public_service_expenses_as_a_percent_of_total_core_expenses_fasb,
  academic_support = drvf2014_rv_academic_support_expenses_as_a_percent_of_total_core_expenses_fasb,
  student_service = drvf2014_rv_student_service_expenses_as_a_percent_of_total_core_expenses_fasb,
  institutional_support = drvf2014_rv_institutional_support_expenses_as_a_percent_of_total_core_expenses_fasb,
  other = drvf2014_rv_other_core_expenses_as_a_percent_of_total_core_expenses_fasb,
  core_expenses_total_dollars = drvf2014_rv_core_expenses_total_dollars_fasb
)

#Pull the total per year
expenses_total <- expenses_pct |> group_by(year) |> summarize(total = sum(core_expenses_total_dollars))

#Plot the total by the year
expenses_total |>
  mutate(total = total/1000000) |>
  ggplot(aes(x = year, y = total)) +
  geom_line() +
  labs(
    y = "Total (in millions of dollars)",
    title = "Total Expenses per Audit",
    x = "Year"
  )

#Making a table of total expenses
expenses_total_pivot <- expenses_total |>
  mutate(total = total/1000000) |>
  pivot_wider(
    names_from = year,
    values_from = total
  )
write_csv(expenses_total_pivot,"Total Expenses by year.csv")

#pivot the data longer
expenses_pct <- expenses_pct |>
  select(!core_expenses_total_dollars) |>
  pivot_longer(
  cols = c(instruction,research,public_service,academic_support,student_service,institutional_support,other),
  names_to = "category",
  values_to = "expense_percent"
)

#Clean up the category names
expenses_pct <- expenses_pct |>
  mutate(category = str_replace_all(category,"_", " ")) |> 
  mutate(category = gsub("(?<=\\b)([a-z])", "\\U\\1", category, perl=TRUE))

#Column Plot
expenses_pct |>
  ggplot(aes(x = year, y = expense_percent, fill = category)) +
  geom_col() +
  labs(
    title = "Expense Distribution per Audit",
    y = "Percent",
    x = "Year"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(
    breaks = seq(2006,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0,100, by = 10)
  )

#Facet Plot
expenses_pct |> 
  ggplot(aes(x = year, y = expense_percent)) +
  geom_line() +
  facet_wrap(~category) +
  labs(
    title = "Percentage of Expenses per Audit by Category",
    y = "Percent",
    x = "Year"
  ) +
  scale_x_continuous(
    breaks = seq(2006,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0,100, by = 10)
  )

#Build the pivot table of expenses
expense_pivot <- expenses_pct |>
  pivot_wider(
    names_from = year,
    values_from = expense_percent
  )

expense_pivot <- expense_pivot|>rename(IPEDS_Percent_Distribution = category)

write_csv(expense_pivot,"Expenses by year.csv")

