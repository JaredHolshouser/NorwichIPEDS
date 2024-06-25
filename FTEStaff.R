library(tidyverse)
library(scales)

#Load the file
FTEStaff <- read_csv("FTEStaff.csv")

#Clean up the column names
FTEStaff <- janitor::clean_names(FTEStaff)

#Rename the columns
glimpse(FTEStaff)

FTEStaff <- FTEStaff |> 
  rename(
  total = drvhr2014_rv_total_fte_staff,
  instructional = drvhr2014_rv_instructional_fte,
  research = drvhr2014_rv_research_fte,
  public_service = drvhr2014_rv_public_service_fte,
  librarians_curators_and_archivists = drvhr2014_rv_librarians_curators_and_archivists_fte,
  student_and_academic_affairs_and_other_education_services = drvhr2014_rv_student_and_academic_affairs_and_other_education_services_fte,
  management = drvhr2014_rv_management_fte,
  business_and_financial_operations = drvhr2014_rv_business_and_financial_operations_fte,
  computer_engineering_and_science = drvhr2014_rv_computer_engineering_and_science_fte,
  community_service_legal_arts_and_media = drvhr2014_rv_community_service_legal_arts_and_media_fte,
  healthcare = drvhr2014_rv_healthcare_fte,
  other = drvhr2014_rv_service_sales_office_admin_support_natural_resources_construction_maintenance_production_transportation_materials_moving_fte,
) |>
  select(!drvhr2014_rv_instructional_research_and_public_service_fte) |>
  select(!drvhr2014_rv_librarians_curators_and_archivists_student_and_academic_affairs_and_other_education_services_fte) |>
  select(!drvhr2014_rv_service_fte:drvhr2014_rv_production_transportation_and_material_moving_fte)

#Plot the total by the year
FTEStaff |>
  ggplot(aes(x = year, y = total)) +
  geom_line() +
  labs(
    y = "Total",
    x = "Year",
    title = "Total FTE Staff per Audit"
  ) +
  scale_x_continuous(
    breaks = seq(2012,2022, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(500,740, by = 20)
  )

#pivot the data longer
FTEStaff <- FTEStaff |>
  select(!total) |>
  pivot_longer(
    cols = c(instructional:other),
    names_to = "occupational_category",
    values_to = "fte_staff"
  )

#Clean up the category names
FTEStaff <- FTEStaff |>
  mutate(occupational_category = str_replace_all(occupational_category,"_", " ")) |> 
  mutate(occupational_category = gsub("(?<=\\b)([a-z])", "\\U\\1", occupational_category, perl=TRUE))

#Column Plot
FTEStaff |>
  group_by(year,occupational_category) |>
  summarize(percentage = sum(fte_staff)) |>
  mutate(percentage = percentage/sum(percentage)*100) |>
  ggplot(aes(x = year, y = percentage, fill = occupational_category)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(2012,2022, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0,100, by = 10)
  ) +
  labs(
    title = "FTE Staff Percentages per Audit",
    x = "Year",
    y = "Percent",
    fill = "Occupational Category",
  ) +
  scale_fill_brewer(palette = "Spectral")

#Facet Plot
FTEStaff |>
  mutate(
    occupational_category = if_else(
      occupational_category == "Student And Academic Affairs And Other Education Services",
      "Educational Services",
      occupational_category
    ),
    occupational_category = if_else(
      occupational_category == "Community Service Legal Arts And Media",
      "Community Services...",
      occupational_category
    ),
    occupational_category = if_else(
      occupational_category == "Computer Engineering And Science",
      "Computer Engineering...",
      occupational_category
    )
  ) |>
  ggplot(aes(x = year, y = fte_staff)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(2012,2022, by = 2)
  ) +
  scale_y_continuous(
    breaks = seq(0,600, by = 50)
  ) +
  labs(
    title = "FTE Staff per Audit by Category",
    x = "Year",
    y = "FTE Staff",
  ) +
  facet_wrap(~occupational_category)

#Build the pivot table of FTE Staff
FTEStaff_pivot <- FTEStaff |>
  pivot_wider(
    names_from = year,
    values_from = fte_staff
  )

write_csv(FTEStaff_pivot,"FTE Staff by Year.csv")
  