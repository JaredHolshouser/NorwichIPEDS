library(tidyverse)
library(scales)

#read in the allowances
allowances <- read_csv("Allowances.csv")

#pull in the revenue percentages
revenue |> filter(category == "Tuition And Fees")

#Use the allowance, revenue percentage, and total revenue to compute the discount rate
discount <- allowances |>
  inner_join(revenue_total, by = join_by(year)) |>
  inner_join(
    revenue |> filter(category == "Tuition And Fees"),
    by = join_by(year)
  ) |>
  select(year,allowance,total,revenue_percent) |>
  mutate(
    tuition = total*revenue_percent*.01,
    gross_tuition = tuition + allowance,
    discount_rate = allowance/gross_tuition*100
  ) |>
  select(
    year,
    allowance,
    tuition,
    gross_tuition,
    discount_rate
  )

#Plot the discount rate over time
discount |>
  ggplot(aes(x = year, y = discount_rate)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(2002,2022,by = 2)
  )+
  scale_y_continuous(
    breaks = seq(0,100,by = 5)
  ) +
  labs(
    x = "Year",
    y = "Discount Rate",
    title = "Discount Rate per Audit"
  )

#Build the pivot table of discount rate
discount_pivot = discount |>
  pivot_longer(
    cols = c(allowance, tuition, gross_tuition, discount_rate),
    names_to = "category",
    values_to = "amount"
  ) |>
  mutate(
    amount = if_else(
      amount > 100,
      amount/1000000,
      amount
      ),
    amount = format(amount, digits = 4),
    category = str_replace_all(category,"_", " "),
    category = gsub("(?<=\\b)([a-z])", "\\U\\1", category, perl=TRUE),
    amount = if_else(
      category == "Discount Rate",
      paste(amount,"%"),
      paste(amount,"M")
    ),
    amount = str_remove_all(amount, " ")
    ) |>
  pivot_wider(
    names_from = year,
    values_from = amount
  )

write_csv(discount_pivot,"Discount by Year.csv")
