library(tidyverse)

#Load the file
expenses <- read_csv("expenses.csv")

#Clean up the column names
expenses <- janitor::clean_names(expenses)

#Rename the columns
glimpse(expenses)

expenses <- expenses |> rename(
  instruction = drvf2017_rv_instruction_expenses_per_fte_fasb,
  research = drvf2017_rv_research_expenses_per_fte_fasb,
  public_service = drvf2017_rv_public_service_expenses_per_fte_fasb,
  academic_support = drvf2017_rv_academic_support_expenses_per_fte_fasb,
  student_service = drvf2017_rv_student_service_expenses_per_fte_fasb,
  institutional_support = drvf2017_rv_institutional_support_expenses_per_fte_fasb,
  other = drvf2017_rv_all_other_core_expenses_per_fte_fasb
)

#pivot the data longer
expenses <- expenses |> pivot_longer(
  cols = c(instruction,research,public_service,academic_support,student_service,institutional_support,other),
  names_to = "category",
  values_to = "expense_per_fte"
)

#Clean up the category names
expenses <- expenses |>
  mutate(category = str_replace_all(category,"_", " ")) |> 
  mutate(category = gsub("(?<=\\b)([a-z])", "\\U\\1", category, perl=TRUE))

#Column Plot
expenses |> 
  group_by(year,category) |> 
  summarize(percentage = sum(expense_per_fte)) |>
  mutate(percentage = percentage/sum(percentage)*100) |>
  ggplot(aes(x = year, y = percentage, fill = category)) +
  geom_col() +
  labs(
    title = "Expense Distribution Per Audit"
  ) +
  scale_fill_brewer(palette = "Dark2")

#Facet Plot
expenses |> 
  group_by(year,category) |> 
  summarize(percentage = sum(expense_per_fte)) |>
  mutate(total = percentage/sum(percentage)) |>
  ggplot(aes(x = year, y = percentage)) +
  geom_line() +
  facet_wrap(~category) +
  labs(
    title = "Percentage of Expenses Over Time by Category"
  )
