library(tidyverse)
library(scales)

#Load the file
revenue <- read_csv("revenue.csv")

#Clean up the column names
revenue <- janitor::clean_names(revenue)

#Rename the columns
glimpse(revenue)

revenue <- revenue |> rename(
  Tuition_and_Fees = drvf2016_rv_tuition_and_fees_as_a_percent_of_core_revenues_fasb,
  Grants_and_Contracts = drvf2016_rv_government_grants_and_contracts_as_a_percent_of_core_revenues_fasb,
  Investment_Income = drvf2016_rv_investment_return_as_a_percent_of_core_revenues_fasb,
  Contributions = drvf2016_rv_private_gifts_grants_contracts_contributions_from_affiliated_entities_as_a_percent_of_core_revenues_fasb,
  Other = drvf2016_rv_other_revenues_as_a_percent_of_core_revenues_fasb,
  total = drvf2016_rv_core_revenues_total_dollars_fasb 
)

#Pull the total per year
revenue_total <- revenue |> group_by(year) |> summarize(total = sum(total))

#Plot the total by the year
revenue_total |>
  mutate(total = total/1000000) |>
  ggplot(aes(x = year, y = total)) +
  geom_line() +
  labs(
    y = "Total (in Millions of Dollars)",
    title = "Total Revenue per Audit",
    x = "Year"
  ) +
  scale_x_continuous(
    breaks = seq(2006,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(50,250, by = 25)
  )

#Making a table of total revenue
revenue_total_pivot <- revenue_total |>
  pivot_wider(
    names_from = year,
    values_from = total
  )
write_csv(revenue_total_pivot,"Total Revenue by year.csv")

#Plotting Revenue and Expenses Together
revenue_total <- revenue_total |> mutate(cat = "Revenue")
expenses_total <- expenses_total |> mutate(cat = "Expense")

revenue_total |> 
  union(expenses_total) |>
  ggplot(aes(x = year, y = total, color = cat)) +
  geom_line() +
  scale_y_continuous(
    labels = label_dollar(scale = 1/1000000, suffix = "M"),
    breaks = seq(25000000, 225000000, by = 25000000)
  ) +
  scale_x_continuous(
    breaks = seq(2006,2022, by = 2)
  ) +
  labs(
    x = "Year",
    y = "Amount",
    title = "Expenses and Revenue per Audit",
    color = ""
  ) +
  scale_color_brewer(palette = "Set1")

#pivot the data longer
revenue <- revenue |>
  select(!total) |>
  pivot_longer(
    cols = c(Tuition_and_Fees,Grants_and_Contracts,Investment_Income,Contributions,Other),
    names_to = "category",
    values_to = "revenue_percent"
  )

#Clean up the category names
revenue <- revenue |>
  mutate(category = str_replace_all(category,"_", " ")) |> 
  mutate(category = gsub("(?<=\\b)([a-z])", "\\U\\1", category, perl=TRUE))

#Column Plot
revenue |>
  ggplot(aes(x = year, y = revenue_percent, fill = category)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(2006,2022, by = 2)
    ) +
  scale_y_continuous(
    breaks = seq(-100,200, by = 10)
  ) +
  labs(
    title = "Revenue Distribution per Audit",
    x = "Year",
    y = "Percent",
    fill = "Revenue Type",
  ) +
  scale_fill_brewer(palette = "Dark2")

#Facet Plot
revenue |> 
  ggplot(aes(x = year, y = revenue_percent)) +
  geom_line() +
  facet_wrap(~category) +
  labs(
    title = "Percentage of Revenue per Audit by Category",
    y = "Percent",
    x = "Year"
  ) +
  scale_x_continuous(
    breaks = seq(2006,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(-100,120, by = 20)
  )

#Build the pivot table of expenses
revenue_pivot <- revenue |>
  pivot_wider(
    names_from = year,
    values_from = revenue_percent
  )

revenue_pivot <- revenue_pivot|>rename(IPEDS_Percent_Distribution = category)

write_csv(revenue_pivot,"Revenue by year.csv")
