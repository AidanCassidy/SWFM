# Produce internal report for policy

setwd("E:/dd/EAS7/Homelessness/Scottish Welfare Fund Monitoring/SWF - IT System/R projects/SWFM/Data")

SWFM_TOTALS <- readRDS("SWFM_TOTALS.rds") %>% 
  mutate(FY = ifelse(as.numeric(month(Date)) < 4, as.numeric(year(Date)), as.numeric(year(Date)+1))) %>% 
  arrange(Date)

SWFM_TOTALS_LA <- readRDS("SWFM_TOTALS_LA.rds") %>% 
  mutate(FY = ifelse(as.numeric(month(Date)) < 4, as.numeric(year(Date)), as.numeric(year(Date)+1))) %>% 
  arrange(Date)


# Create dataframe containing estimated totals for each month to go below actual figures
SWFM_EST_TOTALS <- SWFM_TOTALS %>%
  arrange(Date) %>%
  mutate(MY = as.yearmon(Date, "%m %Y")) %>%
  filter(Date >= as.Date("2021-04-01")) %>%
  select(21, ends_with("est")) %>%
  pivot_longer(cols = 2:10, names_to = "X", values_to = "n") %>%
  pivot_wider(names_from = "MY", values_from = "n")

# For sheet 1 - CCG applications

CCG_APPS.1 <- SWFM_TOTALS_LA %>%
  arrange(Date) %>% 
  filter(Date >= as.Date("2021-04-01")) %>% 
  select(MY, LANAME, CCG_APPS) %>% 
  pivot_wider(names_from = "MY", values_from = "CCG_APPS") %>% 
  adorn_totals(c("row"))

#Select correct estimated total
est_total <- SWFM_EST_TOTALS %>% 
  filter(X == "CCG_APPS_est") %>% 
  mutate(X = "Estimated total") %>% 
  rename("LANAME" = "X")

CCG_APPS.1 <- CCG_APPS.1 %>% 
  bind_rows(est_total)

CCG_APPS.2 <- CCG_APPS.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)

# For sheet 2 - CCG awards

CCG_PAIDOUT.1 <- SWFM_TOTALS_LA %>%
  arrange(Date) %>% 
  filter(Date >= as.Date("2021-04-01")) %>% 
  select(MY, LANAME, CCG_PAIDOUT) %>% 
  pivot_wider(names_from = "MY", values_from = "CCG_PAIDOUT") %>% 
  adorn_totals(c("row")) 

#Select correct estimated total
est_total <- SWFM_EST_TOTALS %>% 
  filter(X == "CCG_PAIDOUT_est") %>% 
  mutate(X = "Estimated total") %>% 
  rename("LANAME" = "X")

CCG_PAIDOUT.1 <- CCG_PAIDOUT.1 %>% 
  bind_rows(est_total)

CCG_PAIDOUT.2 <- CCG_PAIDOUT.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)

# For sheet 3 - CCG spend

CCG_SPEND.1 <- SWFM_TOTALS_LA %>%
  arrange(Date) %>% 
  filter(Date >= as.Date("2021-04-01")) %>% 
  select(MY, LANAME, CCG_SPEND) %>% 
  pivot_wider(names_from = "MY", values_from = "CCG_SPEND") %>% 
  adorn_totals(c("row")) 

#Select correct estimated total
est_total <- SWFM_EST_TOTALS %>% 
  filter(X == "CCG_SPEND_est") %>% 
  mutate(X = "Estimated total") %>% 
  rename("LANAME" = "X")

CCG_SPEND.1 <- CCG_SPEND.1 %>% 
  bind_rows(est_total)

CCG_SPEND.2 <- CCG_SPEND.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)


# For sheet 4 - CCG average award value

LA <- CCG_SPEND.1$LANAME
cnames <- colnames(CCG_SPEND.1)

#remove the LA name column form the spend and award tables in preparation for mapping against each other
CCG_SPEND.3 <- CCG_SPEND.1 %>% 
  select(-1)

CCG_PAIDOUT.3 <- CCG_PAIDOUT.1 %>% 
  select(-1)

CCG_VALUE.1 <- map2_dfc(CCG_SPEND.3, CCG_PAIDOUT.3, `/`) %>% 
  bind_cols(LA) %>% 
  select(last_col(), everything()) %>% 
  set_names(nm = cnames)

CCG_VALUE.2 <- CCG_VALUE.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)


# For sheet 5 - CCG award rate

LA <- CCG_PAIDOUT.1$LANAME
cnames <- colnames(CCG_PAIDOUT.1)

#remove the LA name column form the spend and award tables in preparation for mapping against each other

CCG_PAIDOUT.3 <- CCG_PAIDOUT.1 %>% 
  select(-1)

CCG_APPS.3 <- CCG_APPS.1 %>% 
  select(-1)

CCG_RATE.1 <- map2_dfc(CCG_PAIDOUT.3, CCG_APPS.3, `/`) %>% 
  bind_cols(LA) %>% 
  select(last_col(), everything()) %>% 
  set_names(nm = cnames)

CCG_RATE.2 <- CCG_RATE.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w) %>% 
  select(3)


# For sheet 6 - CRISIS applications

CRISIS_APPS.1 <- SWFM_TOTALS_LA %>%
  arrange(Date) %>% 
  filter(Date >= as.Date("2021-04-01")) %>% 
  select(MY, LANAME, CRISIS_APPS) %>% 
  pivot_wider(names_from = "MY", values_from = "CRISIS_APPS") %>% 
  adorn_totals(c("row"))

#Select correct estimated total
est_total <- SWFM_EST_TOTALS %>% 
  filter(X == "CRISIS_APPS_est") %>% 
  mutate(X = "Estimated total") %>% 
  rename("LANAME" = "X")

CRISIS_APPS.1 <- CRISIS_APPS.1 %>% 
  bind_rows(est_total)

CRISIS_APPS.2 <- CRISIS_APPS.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)

# For sheet 7 - CRISIS awards

CRISIS_PAIDOUT.1 <- SWFM_TOTALS_LA %>%
  arrange(Date) %>% 
  filter(Date >= as.Date("2021-04-01")) %>% 
  select(MY, LANAME, CRISIS_PAIDOUT) %>% 
  pivot_wider(names_from = "MY", values_from = "CRISIS_PAIDOUT") %>% 
  adorn_totals(c("row")) 

#Select correct estimated total
est_total <- SWFM_EST_TOTALS %>% 
  filter(X == "CRISIS_PAIDOUT_est") %>% 
  mutate(X = "Estimated total") %>% 
  rename("LANAME" = "X")

CRISIS_PAIDOUT.1 <- CRISIS_PAIDOUT.1 %>% 
  bind_rows(est_total)

CRISIS_PAIDOUT.2 <- CRISIS_PAIDOUT.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)

# For sheet 8 - CRISIS spend

CRISIS_SPEND.1 <- SWFM_TOTALS_LA %>%
  arrange(Date) %>% 
  filter(Date >= as.Date("2021-04-01")) %>% 
  select(MY, LANAME, CRISIS_SPEND) %>% 
  pivot_wider(names_from = "MY", values_from = "CRISIS_SPEND") %>% 
  adorn_totals(c("row")) 

#Select correct estimated total
est_total <- SWFM_EST_TOTALS %>% 
  filter(X == "CRISIS_SPEND_est") %>% 
  mutate(X = "Estimated total") %>% 
  rename("LANAME" = "X")

CRISIS_SPEND.1 <- CRISIS_SPEND.1 %>% 
  bind_rows(est_total)

CRISIS_SPEND.2 <- CRISIS_SPEND.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)


# For sheet 4 - CRISIS average award value

LA <- CRISIS_SPEND.1$LANAME
cnames <- colnames(CRISIS_SPEND.1)

#remove the LA name column form the spend and award tables in preparation for mapping against each other
CRISIS_SPEND.3 <- CRISIS_SPEND.1 %>% 
  select(-1)

CRISIS_PAIDOUT.3 <- CRISIS_PAIDOUT.1 %>% 
  select(-1)

CRISIS_VALUE.1 <- map2_dfc(CRISIS_SPEND.3, CRISIS_PAIDOUT.3, `/`) %>% 
  bind_cols(LA) %>% 
  select(last_col(), everything()) %>% 
  set_names(nm = cnames)

CRISIS_VALUE.2 <- CRISIS_VALUE.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w,
         z = y / w) %>% 
  select(3:4) %>% 
  mutate_all(round, 2)


# For sheet 5 - CRISIS award rate

LA <- CRISIS_PAIDOUT.1$LANAME
cnames <- colnames(CRISIS_PAIDOUT.1)

#remove the LA name column form the spend and award tables in preparation for mapping against each other

CRISIS_PAIDOUT.3 <- CRISIS_PAIDOUT.1 %>% 
  select(-1)

CRISIS_APPS.3 <- CRISIS_APPS.1 %>% 
  select(-1)

CRISIS_RATE.1 <- map2_dfc(CRISIS_PAIDOUT.3, CRISIS_APPS.3, `/`) %>% 
  bind_cols(LA) %>% 
  select(last_col(), everything()) %>% 
  set_names(nm = cnames)

CRISIS_RATE.2 <- CRISIS_RATE.1 %>% 
  select(w = last_col(offset = 1), x = last_col()) %>% 
  mutate(y = x - w) %>% 
  select(3) %>% 
  mutate_all(round, 2)


# MONTHLY SPEND CHART

xgridlines <- seq(from = ymd("2021-04-01"), to = ymd(max(SWFM_TOTALS_LA$Date)), by = "month")
ygridlines <- 10^6 * seq(from = 0.5, to = plyr::round_any((max(SWFM_TOTALS$SWF_SPEND)/10^6), 0.5, f = floor), by = 0.5)

month(max(SWFM_TOTALS_LA$Date), label = TRUE)

SWFM_TOTALS %>% 
  filter(Date >= "2021-04-01") %>%
  select(Date, SWF_SPEND, SWF_SPEND_est, CCG_SPEND, CCG_SPEND_est, CRISIS_SPEND, CRISIS_SPEND_est) %>% 
  pivot_longer(cols = 2:7, names_to = "X", values_to = "Y") %>% 
  mutate(Type = case_when(
    X == "CCG_SPEND"     | X == "CRISIS_SPEND"     | X == "SWF_SPEND" ~ "Returned",
    X == "CCG_SPEND_est" | X == "CRISIS_SPEND_est" | X == "SWF_SPEND_est" ~ "Estimated"),
    X = case_when(
      str_detect(X, pattern = "CCG",    negate = FALSE) ~ "Community Care Grants",
      str_detect(X, pattern = "CRISIS", negate = FALSE) ~ "Crisis Grants",
      str_detect(X, pattern = "SWF",    negate = FALSE) ~ "SWF total")) %>%
  mutate(Type = as_factor(Type),
         X = as_factor(X),
         Type = fct_relevel(Type),
         X = fct_relevel(X)) %>%
  ggplot(aes(x = Date, y = Y, colour = X, linetype = Type)) + 
  scale_x_date(date_labels = "%b %y", breaks = "month", minor_breaks = xgridlines) +
  scale_y_continuous(labels = paste0("£", seq(from = 0.5, to = 6.0, by = 0.5), "m"), 
                     breaks = ygridlines, minor_breaks = ygridlines) +
  geom_line(size = 1.5) +
  scale_color_manual(values=c("#FA0606", "#1CFA06", "#0654FA")) + 
  scale_linetype_manual(values=c(1,4), labels = c("Complete returns", "Missing returns estimated")) +
  labs(title = "Chart 1 - Monthly Spend",
       subtitle = "Management Information") +
  xlab("Month") +
  ylab("Expenditure") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

setwd("E:/dd/EAS7/Homelessness/Scottish Welfare Fund Monitoring/SWF - IT System/R projects/SWFM/Output")
ggsave("swfm_plot_1.png", last_plot(), width = 30, height = 20, units = "cm", dpi = 300, limitsize = TRUE)

# PRODUCING BUDGET SHEET

#selecting SWF_SPEND  
SWFM.1 <- SWFM_TOTALS_LA %>% 
  select(LANAME, MY, Date, FY, SWF_SPEND) %>% 
  # pivot_wider and add Scotland total
  pivot_wider(names_from = "LANAME", values_from = SWF_SPEND) %>% 
  mutate(Scotland = rowSums(across(4:last_col()), na.rm = TRUE)) %>%
  # pivot_longer then pivot_wider again to get months as columns
  pivot_longer(cols = 4:36, names_to = "LANAME", values_to = "spend")

#selecting SWF_SPEND_est
SWFM.2 <- SWFM_TOTALS_LA %>% 
  select(LANAME, MY, Date, FY, SWF_SPEND_est) %>% 
  # pivot_wider and add Scotland total
  pivot_wider(names_from = "LANAME", values_from = SWF_SPEND_est) %>% 
  mutate(Scotland = rowSums(across(4:last_col()), na.rm = TRUE)) %>%
  # pivot_longer then pivot_wider again to get months as columns
  pivot_longer(cols = 4:36, names_to = "LANAME", values_to = "spend")

# latest financial year in SWFM:
FY1 <- ifelse(month(max(SWFM_TOTALS_LA$Date)) < 4, year(max(SWFM_TOTALS_LA$Date)), year(max(SWFM_TOTALS_LA$Date))+1)

# online SWF annual publication tables:
xlsxFile <-  "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2022/07/scottish-welfare-fund-statistics-annual-update-2021-222/documents/scottish-welfare-fund-statistics-tables-april-2013-march-2022/scottish-welfare-fund-statistics-tables-april-2013-march-2022/govscot%3Adocument/scottish-welfare-fund-statistics-tables-april-2013-march-2022.xlsx"

SWFQ.spend <- read.xlsx(xlsxFile = xlsxFile, sheet = "T37 - SWF EXPENDITURE M", startRow = 4, colNames = TRUE) %>% 
  # remove "(£)" from Month column and format as date
  mutate(Month = str_remove_all(Month, pattern = "[(£)]")) %>% 
  mutate(Month = as.Date(as.yearmon(Month, "%b %Y"))) %>% 
  filter(!is.na(Month)) %>% 
  #remove last row (total since 2013)
  mutate(FY = ifelse(as.numeric(month(Month)) < 4, as.numeric(year(Month)), as.numeric(year(Month)+1)),
         MY = as.yearmon(Month, "%m %Y")) %>% 
  relocate(2, .after = last_col()) %>% 
  select(1, 34:35, everything()) %>% 
  pivot_longer(cols = 4:36, names_to = "LANAME", values_to = "spend") %>%
  # clean LANAME
  mutate(LANAME = str_replace_all(LANAME, pattern = "[.]", " "),
         LANAME = str_remove_all(LANAME, pattern = "\\[Note |\\[note |\\]"),
         LANAME = str_remove_all(LANAME, pattern = "[:digit:]"),
         LANAME = str_trim(LANAME),
         LANAME = case_when(
           LANAME == "Edinburgh" ~ "Edinburgh City",
           LANAME == "Eilean Siar" ~ "Na h-Eileanan Siar",
           LANAME == "Glasgow" ~ "Glasgow City",
           LANAME == "Shetland" ~ "Shetland Islands",
           LANAME == "Scotland" ~ "Scotland total",
           TRUE ~ LANAME))

# read in and tidy SWF budgets and underspends table
SWFQ.budget <- read.xlsx(xlsxFile = xlsxFile, sheet = "T40 - SWF BUDGETS & UNDERSPENDS", startRow = 3, colNames = TRUE) %>% 
  # move Scotland total to last column
  relocate(3, .after = last_col()) %>% 
  # change year from "2013-14" to "2014" - did this so the for loop below would work (Y0 = i - 1), but there may be another way
  mutate(FY = paste0(20, str_sub(Year, -2))) %>%
  select(-Year) %>% 
  # remove "(£)" from Expenditure category column
  mutate(Expenditure.category = str_remove_all(Expenditure.category, pattern = "[ (£)]")) %>% 
  pivot_longer(cols = 2:34, names_to = "LANAME", values_to = "amount") %>% 
  pivot_wider(names_from = c("Expenditure.category", "FY"), names_sep = ".", values_from = "amount") %>% 
  # tidy up LA names
  mutate(LANAME = str_replace_all(LANAME, pattern = "[.]", " "),
         LANAME = str_remove_all(LANAME, pattern = "\\[Note |\\[note |\\]"),
         LANAME = str_remove_all(LANAME, pattern = "[:digit:]"),
         LANAME = str_trim(LANAME),
         LANAME = case_when(
           LANAME == "Edinburgh" ~ "Edinburgh City",
           LANAME == "Eilean Siar" ~ "Na h-Eileanan Siar",
           LANAME == "Glasgow" ~ "Glasgow City",
           LANAME == "Shetland" ~ "Shetland Islands",
           LANAME == "Scotland" ~ "Scotland total",
           TRUE ~ LANAME))

SWFQ_end_date <- max(SWFQ.spend$Month)

# If the latest month in the monthly data (SWFM) is April, May or June..
# then underspend from previous year (FY0) to be calculated by combining quarterly (SWFQ) and SWFM

if(month(max(SWFM.1$Date)) %in% 4:6) {
  
  # reshape SWFQ expenditure data frame:
  SWFQ.spend.1 <- SWFQ.spend %>% 
    filter(FY == FY1 - 1) %>% 
    select(3:5) %>% 
    pivot_wider(names_from = "MY", values_from = "spend")
  
  # reshape SWFQ expenditure data frame:
  SWFM.spend.1 <- SWFM.1 %>% 
    # only months after last month in quarterly data
    filter(Date > SWFQ_end_date & FY == FY1 - 1) %>% 
    select(1, 4, 5) %>% 
    pivot_wider(names_from = "MY", values_from = "spend")
  
  # combine monthly FY0 expenditure figures from quarterly data and MI
  SWF.spend.FY0 <- SWFQ.spend.1 %>% 
    left_join(SWFM.spend.1, by = "LANAME") %>% 
    adorn_totals(c("col")) %>% 
    select(1, last_col())
  
  SWFQ.budget.1 <- SWFQ.budget %>% 
    select(1, 
           starts_with("Allocated.") & ends_with(as.character(FY1-1)), 
           starts_with("Allocated+underspend.") & ends_with(as.character(FY1-1))) %>% 
    left_join(SWF.spend.FY0, by = "LANAME") %>% 
    setNames(c("LANAME", 
               "Allocated.FY0", 
               "Budget.FY0",
               "Spent.FY0")) %>%
    rowwise() %>% 
    mutate(Underspend.FY0 = max(0, Budget.FY0 - Spent.FY0),
           Allocated.FY1 = Allocated.FY0,
           Budget.FY1 = Allocated.FY1 + Underspend.FY0) %>% 
    select(-c(2:4))
  
  SWFQ.budget.2 <-SWFQ.budget %>% 
    select(-c(starts_with("Underspend.") & ends_with("FY0"))) %>% 
    left_join(SWFQ.budget.1, by = "LANAME") 
  
  
} else {
  
  SWFQ.budget.1 <- SWFQ.budget %>% 
    select(1, 
           starts_with("Allocated.") & ends_with(as.character(FY1-1)), 
           starts_with("Underspend.") & ends_with(as.character(FY1-1))) %>% 
    setNames(c("LANAME", 
               "Allocated.FY0", 
               "Underspend.FY0")) %>% 
    mutate(Allocated.FY1 = Allocated.FY0,
           Budget.FY1 = Allocated.FY1 + Underspend.FY0) %>% 
    select(-c(2))
  
  SWFQ.budget.2 <-SWFQ.budget %>% 
    select(-c(starts_with("Underspend.") & ends_with("FY1-1"))) %>% 
    left_join(SWFQ.budget.1, by = "LANAME")
  
}

# Add estimated total row to budget data frame for calculating % spent columns

SWFQ.budget.3 <- SWFQ.budget.1 %>% 
  pivot_longer(cols = 2:4, names_to = "names", values_to = "values") %>% 
  pivot_wider(names_from = "LANAME", values_from = "values") %>%
  select(1, last_col()) %>% 
  setNames(c("LANAME", "Scotland_total")) %>% 
  mutate(Scotland_total_copy = Scotland_total) %>% 
  pivot_longer(cols = 2:3, names_to = "names", values_to = "values") %>% 
  pivot_wider(names_from = "LANAME", values_from = "values") %>% 
  rename("LANAME" = names) %>% 
  filter(LANAME != "Scotland_total") %>% 
  mutate(LANAME = case_when(
    LANAME == "Scotland_total_copy" ~ "Scotland total (missing data estimated)"))

SWFQ.budget.1 <- SWFQ.budget.1 %>% 
  bind_rows(SWFQ.budget.3)


# Create table for FY1 budget sheet:

# reshape SWFM expenditure data frame:
SWFM.spend.2 <- SWFM.1 %>% 
  # only months after last month in quarterly data
  filter(FY == FY1) %>% 
  select(1, 4, 5) %>% 
  pivot_wider(names_from = "MY", values_from = "spend") %>% 
  mutate(LANAME = case_when(LANAME == "Scotland" ~ "Scotland total",
                            TRUE ~ LANAME))

SWFM.spend.3 <- SWFM.2 %>% 
  # only months after last month in quarterly data
  filter(FY == FY1) %>% 
  select(1, 4, 5) %>% 
  pivot_wider(names_from = "MY", values_from = "spend") %>% 
  filter(LANAME == "Scotland") %>% 
  mutate(LANAME = "Scotland total (missing data estimated)")

SWFM.spend.4 <- SWFM.spend.2 %>% 
  bind_rows(SWFM.spend.3) %>% 
  adorn_totals("col") 

SWF.FY1.table <- SWFQ.budget.1 %>% 
  left_join(SWFM.spend.4, by = "LANAME") %>% 
  mutate(perc.alloc.spent = Total / Allocated.FY1,
         perc.budget.spent = Total / Budget.FY1) %>% 
  relocate(last_col(offset = 2):last_col(), .after = 4)

###charts 2, 3, 4

if(month(max(SWFM.1$Date)) > 4) {
   
   line <- 100 * (round((month(max(SWFM.1$Date))-3)/12, 2))
   
}else if(month(max(SWFM.1$Date)) < 4){
  
  line <- 100 * (round((month(max(SWFM.1$Date))+9)/12, 2))

   }

BLUE <- "#076fa2"
RED  <- "#8B0000"
text_box.1 <- paste0("The bars show the % of the 2022/23 funding allocation (excluding
previous underspend) spent by end of the month. 
The vertical line (", line, "%) shows  the proportion we would
expect to be spent if spending was even across the year.")
text_box.2 <- paste0("The bars show the % of the estimated 2022/23 budget (including
previous underspend) spent by end of the month. 
The vertical line (", line, "%) shows  the proportion we would
expect to be spent if spending was even across the year.")

SWF.FY1.plot.1 <- SWF.FY1.table %>% 
  select(1, 6) %>% 
  setNames(c("LA", "spend")) %>% 
  mutate(spend = spend * 100) %>% 
  mutate(myline = line, 
         Scotland_flag = if_else(LA == "Scotland total", "yes", "no")) %>% 
  filter(LA != "Scotland total (missing data estimated)") %>% 
  group_by(LA)

SWF.FY1.plot.1 %>% 
  ggplot(aes(y = fct_reorder(LA, spend), x = spend, fill = Scotland_flag)) +
  geom_col() + 
  scale_fill_manual(values=c(BLUE, RED)) +
  geom_vline(aes(xintercept = line), linetype = 2, size = 1) + 
  scale_x_continuous(labels = paste0(seq(from = 0, to = 180, by = 10), "%"),
                     breaks = seq(from = 0, to = 180, by = 10)) +
  geom_label(x = 50, y = "Scotland total", label = text_box.1, fill = "white") +
  labs(x = "Allocation spent (%)", y = "Local authority", title = "Chart 2 - Allocation spent by local authority") + 
  theme(legend.position = "none")

ggsave("swfm_plot_2.png", last_plot(), width = 30, height = 20, units = "cm", dpi = 300, limitsize = TRUE)


SWF.FY1.plot.2 <- SWF.FY1.table %>% 
  select(1, 7) %>% 
  setNames(c("LA", "spend")) %>% 
  mutate(spend = spend * 100) %>% 
  mutate(myline = line, 
         Scotland_flag = if_else(LA == "Scotland total", "yes", "no")) %>% 
  filter(LA != "Scotland total (missing data estimated)") %>% 
  group_by(LA)

SWF.FY1.plot.2 %>% 
  ggplot(aes(y = fct_reorder(LA, spend), x = spend, fill = Scotland_flag)) +
  geom_col() + 
  scale_fill_manual(values=c(BLUE, RED)) +
  geom_vline(aes(xintercept = line), linetype = 2, size = 1) + 
  scale_x_continuous(labels = paste0(seq(from = 0, to = 180, by = 10), "%"),
                     breaks = seq(from = 0, to = 180, by = 10)) +
  geom_label(x = 50, y = "Aberdeen City", label = text_box.2, fill = "white") +
  labs(x = "Budget spent (%)", y = "Local authority", title = "Chart 3 - Budget spent by local authority") + 
  theme(legend.position = "none")

ggsave("swfm_plot_3.png", last_plot(), width = 30, height = 20, units = "cm", dpi = 300, limitsize = TRUE)

plotdata.1 <- SWFM_TOTALS_LA %>% 
  select(LANAME, FY, Date, MY, SWF_SPEND, SWF_SPEND_est) %>% 
  filter(FY == FY1) %>% 
  group_by(Date) %>% 
  summarise(sum.SWF_SPEND = sum(SWF_SPEND, na.rm = TRUE), sum.SWF_SPEND_est = sum(SWF_SPEND_est, na.rm = TRUE)) %>% 
  mutate(cumsum.SWF_SPEND = cumsum(sum.SWF_SPEND), cumsum.SWF_SPEND_est = cumsum(sum.SWF_SPEND_est),
         cumsum.diff = cumsum.SWF_SPEND_est - cumsum.SWF_SPEND,
         alloc = 35495000/12,
         alloc.profile = cumsum(alloc),
         budget = 39770193/12,
         budget.profile = cumsum(budget)) %>% 
  select(Date, cumsum.SWF_SPEND, cumsum.diff, alloc.profile, budget.profile) 

plotdata.3 <- plotdata.1 %>% 
  select(Date, cumsum.SWF_SPEND, cumsum.diff) %>% 
  pivot_longer(cols = 2:3, values_to = "spend", names_to = "SWF_spend") %>% 
  mutate(SWF_spend = if_else(SWF_spend == "cumsum.SWF_SPEND", "SWF spend", "Estimated SWF spend"))


xgridlines <- seq.Date(from = as.Date(paste(FY1-1, 4, 1, sep="/")), to = as.Date(paste(FY1, 3, 1, sep="/")), by = "month")

plotdata.2 <- as.data.frame(xgridlines) %>% 
  mutate(alloc = SWFQ.budget.3$Allocated.FY1[1]/12,
         alloc.profile = cumsum(alloc),
         budget = SWFQ.budget.3$Budget.FY1[1]/12,
         budget.profile = cumsum(budget)) %>% 
  select("Date" = xgridlines, alloc.profile, budget.profile) %>% 
  pivot_longer(cols = 2:3, names_to = "Profile", values_to = "values") %>% 
  mutate(Profile = if_else(Profile == "alloc.profile", "Allocation (excluding underspend)", "Budget (including underspend)"))

ygridlines <- 10^6 * seq(from = 0, to = plyr::round_any((max(plotdata.3$spend)/10^6), 1, f = floor), by = 5)
ygridlines <- 10^6 * seq(from = 0, to = plyr::round_any((SWFQ.budget.3$Budget.FY1[1]/10^6), 1, f = floor), by = 5)

ggplot(plotdata.3) +
  geom_col(aes(x = Date, y = spend, fill = SWF_spend)) +
  scale_y_continuous(labels = paste0("£", seq(from = 0, to = plyr::round_any((SWFQ.budget.3$Budget.FY1[1]/10^6), 1, f = floor), by = 5), "m"), 
                     breaks = ygridlines, minor_breaks = ygridlines) +
  scale_fill_manual(values=c(RED, BLUE)) +
  labs(x = "Month", y = "cummulative SWF spend", title = "Cummulative SWF spend compared to projected spend") +
  geom_line(data = plotdata.2, aes(x = Date, y = values, colour = Profile), size = 2) +
  scale_x_date(date_labels = "%b %y", breaks = "month", minor_breaks = xgridlines)

ggsave("swfm_plot_4.png", last_plot(), width = 30, height = 20, units = "cm", dpi = 300, limitsize = TRUE)


##############################

# Output to excel

# Save my output number formats

report_from <- as.Date("2021-04-01")

#Save my output number formats
monyear <- createStyle(numFmt = "mmm-yy", textDecoration = "Bold", border = "TopBottomLeftRight")
pct <- createStyle(numFmt="0%", border = "TopBottomLeftRight") #2 decimal percent
bold_pct <- createStyle(numFmt="0%", textDecoration = "Bold", border = "TopBottomLeftRight") #2 decimal percent
mynum <- createStyle(numFmt="#,##0", border = "TopBottomLeftRight") #Thousand's separator
gbp <- createStyle(numFmt = "£* #,##0", border = "TopBottomLeftRight") #financial
header_bold <- createStyle(textDecoration = "Bold", border = "TopBottomLeftRight")
header_bold_wrap <- createStyle(textDecoration = "Bold", wrapText = TRUE, border = "TopBottomLeftRight")
lastrow_bold_pct <- createStyle(numFmt="0%",textDecoration = "Bold", border = "TopBottomLeftRight")
bold_gbp <- createStyle(numFmt="£* #,##0",textDecoration = "Bold", border = "TopBottomLeftRight")
bold_mynum <- createStyle(numFmt="#,##0",textDecoration = "Bold", border = "TopBottomLeftRight")
title <- createStyle(textDecoration = "Bold", fontSize = 14)
mydate<-Sys.Date()
save_name <- paste0("SWFM Report - ", format(max(SWFM_TOTALS$Date), "%B %Y"),".xlsx")
FY0_full <- paste(as.character(FY1-2), as.character(FY1-2001), sep = "/")
FY1_full <- paste(as.character(FY1-1), as.character(FY1-2000), sep = "/")



#Define headings
headings.1 <- c("Local authority", as.character(as.yearmon(seq(from = report_from, by = "month", length.out = ncol(CCG_APPS.1)-1), "%m %Y")))
headings.2 <- c("number", "%")
headings.3 <- c("Change from previous month")
headings.4 <- c("Local authority", 
                paste0("Underspend carried forward from ", FY0_full),
                paste0("SG allocation ", FY1_full),
                paste0("Total ", FY1_full,  " funds available"),
                "Total YTD spend",
                "allocation spent (excl. previous underspend)",
                "budget Spent (inc. previous underspend)",
                as.character(as.yearmon(seq.Date(from = as.Date(paste(FY1-1, 04, 01, sep = "/")), to = max(SWFM.1$Date), by = "month"))))


wb <- createWorkbook("New Workbook")

###
#CCG Applications sheet

#Overwrite my variable names with new column headings
colnames(CCG_APPS.1) <- headings.1
colnames(CCG_APPS.2) <- headings.2

this_sheet <- "CCG Applications"
addWorksheet(wb, this_sheet)
sheet_title <- "Community Care Grants - Applications"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CCG_APPS.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = mynum, cols = 2:(ncol(CCG_APPS.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CCG_APPS.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = 2:(ncol(CCG_APPS.1)), rows = (nrow(CCG_APPS.1)+2):(nrow(CCG_APPS.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CCG_APPS.2, colNames = TRUE, startRow = 3, startCol = (ncol(CCG_APPS.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CCG_APPS.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = mynum, cols = (ncol(CCG_APPS.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CCG_APPS.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = (ncol(CCG_APPS.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CCG_APPS.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CCG_APPS.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CCG_APPS.1)+2):(ncol(CCG_APPS.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CCG_APPS.1)+2):(ncol(CCG_APPS.1)+3), rows = 2:3, gridExpand=TRUE)



#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CCG_APPS.1)+2), widths = 8)
setColWidths(wb, this_sheet, cols = (ncol(CCG_APPS.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)


###
#CCG Awards sheet

#Overwrite my variable names with new column headings
colnames(CCG_PAIDOUT.1) <- headings.1
colnames(CCG_PAIDOUT.2) <- headings.2

this_sheet <- "CCG Awards"
addWorksheet(wb, this_sheet)
sheet_title <- "Community Care Grants - Awards"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CCG_PAIDOUT.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = mynum, cols = 2:(ncol(CCG_PAIDOUT.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CCG_PAIDOUT.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = 2:(ncol(CCG_PAIDOUT.1)), rows = (nrow(CCG_PAIDOUT.1)+2):(nrow(CCG_PAIDOUT.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CCG_PAIDOUT.2, colNames = TRUE, startRow = 3, startCol = (ncol(CCG_PAIDOUT.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CCG_PAIDOUT.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = mynum, cols = (ncol(CCG_PAIDOUT.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CCG_PAIDOUT.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = (ncol(CCG_PAIDOUT.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CCG_PAIDOUT.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CCG_PAIDOUT.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CCG_PAIDOUT.1)+2):(ncol(CCG_PAIDOUT.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CCG_PAIDOUT.1)+2):(ncol(CCG_PAIDOUT.1)+3), rows = 2:3, gridExpand=TRUE)



#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CCG_PAIDOUT.1)+2), widths = 8)
setColWidths(wb, this_sheet, cols = (ncol(CCG_PAIDOUT.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)

###
#CCG Expenditure sheet

#Overwrite my variable names with new column headings
colnames(CCG_SPEND.1) <- headings.1
colnames(CCG_SPEND.2) <- headings.2

this_sheet <- "CCG Spend"
addWorksheet(wb, this_sheet)
sheet_title <- "Community Care Grants - Spend"
change_header <- "Difference from previous month"


#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CCG_SPEND.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = gbp, cols = 2:(ncol(CCG_SPEND.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CCG_SPEND.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = 2:(ncol(CCG_SPEND.1)), rows = (nrow(CCG_SPEND.1)+2):(nrow(CCG_SPEND.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CCG_SPEND.2, colNames = TRUE, startRow = 3, startCol = (ncol(CCG_SPEND.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CCG_SPEND.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = gbp, cols = (ncol(CCG_SPEND.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CCG_SPEND.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = (ncol(CCG_SPEND.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CCG_SPEND.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CCG_SPEND.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CCG_SPEND.1)+2):(ncol(CCG_SPEND.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CCG_SPEND.1)+2):(ncol(CCG_SPEND.1)+3), rows = 2:3, gridExpand=TRUE)


#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CCG_SPEND.1)+2), widths = 10)
setColWidths(wb, this_sheet, cols = (ncol(CCG_SPEND.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)


###
#CCG Award value sheet

#Overwrite my variable names with new column headings
colnames(CCG_VALUE.1) <- headings.1
colnames(CCG_VALUE.2) <- headings.2

this_sheet <- "CCG Award Values"
addWorksheet(wb, this_sheet)
sheet_title <- "Community Care Grants - Average award value"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CCG_VALUE.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = gbp, cols = 2:(ncol(CCG_VALUE.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CCG_VALUE.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = 2:(ncol(CCG_VALUE.1)), rows = (nrow(CCG_VALUE.1)+2):(nrow(CCG_VALUE.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CCG_VALUE.2, colNames = TRUE, startRow = 3, startCol = (ncol(CCG_VALUE.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CCG_VALUE.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = gbp, cols = (ncol(CCG_VALUE.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CCG_VALUE.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = (ncol(CCG_VALUE.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CCG_VALUE.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CCG_VALUE.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CCG_VALUE.1)+2):(ncol(CCG_VALUE.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CCG_VALUE.1)+2):(ncol(CCG_VALUE.1)+3), rows = 2:3, gridExpand=TRUE)


#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CCG_VALUE.1)+2), widths = 9)
setColWidths(wb, this_sheet, cols = (ncol(CCG_VALUE.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)


###
#CCG Award rates sheet

#Overwrite my variable names with new column headings
colnames(CCG_RATE.1) <- headings.1
colnames(CCG_RATE.2) <- headings.3

this_sheet <- "CCG Award Rates"
addWorksheet(wb, this_sheet)
sheet_title <- "Community Care Grants - Award Rates"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CCG_RATE.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = pct, cols = 2:(ncol(CCG_RATE.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CCG_RATE.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_pct, 
         cols = 2:(ncol(CCG_RATE.1)), rows = (nrow(CCG_RATE.1)+2):(nrow(CCG_RATE.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CCG_RATE.2, colNames = FALSE, startRow = 4, startCol = (ncol(CCG_RATE.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 3, startCol = (ncol(CCG_RATE.1)+2))

#Format RATE change column
#addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CCG_RATE.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CCG_RATE.1)+2), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows RATE change column
addStyle(wb, sheet = this_sheet, style = bold_pct, 
         cols = (ncol(CCG_RATE.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CCG_RATE.1)+2), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CCG_RATE.1)+2), rows = 4:37, type = "databar") 

#Format change header
#mergeCells(wb, sheet = this_sheet, cols = (ncol(CCG_RATE.1)+2):(ncol(CCG_RATE.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CCG_RATE.1)+2), rows = 3, gridExpand=TRUE)


#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = (ncol(CCG_RATE.1)+2), widths = 20)
setColWidths(wb, this_sheet, cols = 2:(ncol(CCG_RATE.1)), widths = 8)


#Set row height for change header
setRowHeights(wb, this_sheet, rows = 3, heights = 29)


#CRISIS Applications sheet

#Overwrite my variable names with new column headings
colnames(CRISIS_APPS.1) <- headings.1
colnames(CRISIS_APPS.2) <- headings.2

this_sheet <- "CRISIS Applications"
addWorksheet(wb, this_sheet)
sheet_title <- "Crisis Grants - Applications"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CRISIS_APPS.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = mynum, cols = 2:(ncol(CRISIS_APPS.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CRISIS_APPS.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = 2:(ncol(CRISIS_APPS.1)), rows = (nrow(CRISIS_APPS.1)+2):(nrow(CRISIS_APPS.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CRISIS_APPS.2, colNames = TRUE, startRow = 3, startCol = (ncol(CRISIS_APPS.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CRISIS_APPS.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = mynum, cols = (ncol(CRISIS_APPS.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CRISIS_APPS.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = (ncol(CRISIS_APPS.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CRISIS_APPS.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CRISIS_APPS.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CRISIS_APPS.1)+2):(ncol(CRISIS_APPS.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CRISIS_APPS.1)+2):(ncol(CRISIS_APPS.1)+3), rows = 2:3, gridExpand=TRUE)



#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CRISIS_APPS.1)+2), widths = 8)
setColWidths(wb, this_sheet, cols = (ncol(CRISIS_APPS.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)


###
#CRISIS Awards sheet

#Overwrite my variable names with new column headings
colnames(CRISIS_PAIDOUT.1) <- headings.1
colnames(CRISIS_PAIDOUT.2) <- headings.2

this_sheet <- "CRISIS Awards"
addWorksheet(wb, this_sheet)
sheet_title <- "Crisis Grants - Awards"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CRISIS_PAIDOUT.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = mynum, cols = 2:(ncol(CRISIS_PAIDOUT.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CRISIS_PAIDOUT.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = 2:(ncol(CRISIS_PAIDOUT.1)), rows = (nrow(CRISIS_PAIDOUT.1)+2):(nrow(CRISIS_PAIDOUT.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CRISIS_PAIDOUT.2, colNames = TRUE, startRow = 3, startCol = (ncol(CRISIS_PAIDOUT.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CRISIS_PAIDOUT.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = mynum, cols = (ncol(CRISIS_PAIDOUT.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CRISIS_PAIDOUT.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_mynum, 
         cols = (ncol(CRISIS_PAIDOUT.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CRISIS_PAIDOUT.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CRISIS_PAIDOUT.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CRISIS_PAIDOUT.1)+2):(ncol(CRISIS_PAIDOUT.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CRISIS_PAIDOUT.1)+2):(ncol(CRISIS_PAIDOUT.1)+3), rows = 2:3, gridExpand=TRUE)



#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CRISIS_PAIDOUT.1)+2), widths = 8)
setColWidths(wb, this_sheet, cols = (ncol(CRISIS_PAIDOUT.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)

###
#CRISIS Expenditure sheet

#Overwrite my variable names with new column headings
colnames(CRISIS_SPEND.1) <- headings.1
colnames(CRISIS_SPEND.2) <- headings.2

this_sheet <- "CRISIS Spend"
addWorksheet(wb, this_sheet)
sheet_title <- "Crisis Grants - Spend"
change_header <- "Difference from previous month"


#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CRISIS_SPEND.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = gbp, cols = 2:(ncol(CRISIS_SPEND.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CRISIS_SPEND.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = 2:(ncol(CRISIS_SPEND.1)), rows = (nrow(CRISIS_SPEND.1)+2):(nrow(CRISIS_SPEND.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CRISIS_SPEND.2, colNames = TRUE, startRow = 3, startCol = (ncol(CRISIS_SPEND.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CRISIS_SPEND.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = gbp, cols = (ncol(CRISIS_SPEND.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CRISIS_SPEND.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = (ncol(CRISIS_SPEND.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CRISIS_SPEND.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CRISIS_SPEND.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CRISIS_SPEND.1)+2):(ncol(CRISIS_SPEND.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CRISIS_SPEND.1)+2):(ncol(CRISIS_SPEND.1)+3), rows = 2:3, gridExpand=TRUE)


#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CRISIS_SPEND.1)+2), widths = 10)
setColWidths(wb, this_sheet, cols = (ncol(CRISIS_SPEND.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)


###
#CRISIS Award value sheet

#Overwrite my variable names with new column headings
colnames(CRISIS_VALUE.1) <- headings.1
colnames(CRISIS_VALUE.2) <- headings.2

this_sheet <- "CRISIS Award Values"
addWorksheet(wb, this_sheet)
sheet_title <- "Crisis Grants - Average award value"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CRISIS_VALUE.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = gbp, cols = 2:(ncol(CRISIS_VALUE.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CRISIS_VALUE.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = 2:(ncol(CRISIS_VALUE.1)), rows = (nrow(CRISIS_VALUE.1)+2):(nrow(CRISIS_VALUE.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CRISIS_VALUE.2, colNames = TRUE, startRow = 3, startCol = (ncol(CRISIS_VALUE.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 2, startCol = (ncol(CRISIS_VALUE.1)+2))

#Format value change column
addStyle(wb, sheet = this_sheet, style = gbp, cols = (ncol(CRISIS_VALUE.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CRISIS_VALUE.1)+3), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows value change column
addStyle(wb, sheet = this_sheet, style = bold_gbp, 
         cols = (ncol(CRISIS_VALUE.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CRISIS_VALUE.1)+3), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CRISIS_VALUE.1)+3), rows = 4:37, type = "databar") 

#Format change header
mergeCells(wb, sheet = this_sheet, cols = (ncol(CRISIS_VALUE.1)+2):(ncol(CRISIS_VALUE.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CRISIS_VALUE.1)+2):(ncol(CRISIS_VALUE.1)+3), rows = 2:3, gridExpand=TRUE)


#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = 2:(ncol(CRISIS_VALUE.1)+2), widths = 9)
setColWidths(wb, this_sheet, cols = (ncol(CRISIS_VALUE.1)+3), widths = 12)

#Set row height for change header
setRowHeights(wb, this_sheet, rows = 2, heights = 34)


###
#CRISIS Award rates sheet

#Overwrite my variable names with new column headings
colnames(CRISIS_RATE.1) <- headings.1
colnames(CRISIS_RATE.2) <- headings.3

this_sheet <- "CRISIS Award Rates"
addWorksheet(wb, this_sheet)
sheet_title <- "Crisis Grants - Award Rates"
change_header <- "Difference from previous month"

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)

#main part of table
writeData(wb, this_sheet, CRISIS_RATE.1, colNames = TRUE, startRow=3,startCol="A") 

#Format table body
addStyle(wb, sheet = this_sheet, style = pct, cols = 2:(ncol(CRISIS_RATE.1)), rows = 4:35, gridExpand=TRUE)
#Format header bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1:(ncol(CRISIS_RATE.1)), rows=3, gridExpand=TRUE)
#Format first column bold
addStyle(wb, sheet = this_sheet, style=header_bold, cols=1, rows=4:37, gridExpand=TRUE)

#Format bottom two rows (total and estimated total)
addStyle(wb, sheet = this_sheet, style = bold_pct, 
         cols = 2:(ncol(CRISIS_RATE.1)), rows = (nrow(CRISIS_RATE.1)+2):(nrow(CRISIS_RATE.1)+3), gridExpand=TRUE)

#Insert change columns
writeData(wb, sheet = this_sheet, CRISIS_RATE.2, colNames = FALSE, startRow = 4, startCol = (ncol(CRISIS_RATE.1)+2))

#Add change columns header
writeData(wb, sheet = this_sheet, change_header, startRow = 3, startCol = (ncol(CRISIS_RATE.1)+2))

#Format RATE change column
#addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CRISIS_RATE.1)+2), rows = 4:35, gridExpand = TRUE)
#Format percentage change column
addStyle(wb, sheet = this_sheet, style = pct, cols = (ncol(CRISIS_RATE.1)+2), rows = 4:35, gridExpand = TRUE)
#Format last 2 rows RATE change column
addStyle(wb, sheet = this_sheet, style = bold_pct, 
         cols = (ncol(CRISIS_RATE.1)+2), rows = 36:37, gridExpand = TRUE)
#Format last 2 rows percentage change column
addStyle(wb, sheet = this_sheet, style = lastrow_bold_pct, 
         cols = (ncol(CRISIS_RATE.1)+2), rows = 36:37, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = (ncol(CRISIS_RATE.1)+2), rows = 4:37, type = "databar") 

#Format change header
#mergeCells(wb, sheet = this_sheet, cols = (ncol(CRISIS_RATE.1)+2):(ncol(CRISIS_RATE.1)+3), rows = 2)
addStyle(wb, sheet = this_sheet, style=header_bold_wrap, cols=(ncol(CRISIS_RATE.1)+2), rows = 3, gridExpand=TRUE)


#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 21)
setColWidths(wb, this_sheet, cols = (ncol(CRISIS_RATE.1)+2), widths = 20)
setColWidths(wb, this_sheet, cols = 2:(ncol(CRISIS_RATE.1)), widths = 8)


#Set row height for change header
setRowHeights(wb, this_sheet, rows = 3, heights = 29)


###
#Chart 1 - monthly spend

this_sheet <- "Chart 1 (monthly spend)"
addWorksheet(wb, this_sheet)
sheet_title <- "Chart 1 - Monthly SWF Expenditure"

#Insert chart image from saved file
insertImage(wb, this_sheet, "swfm_plot_1.png", height = 20, width = 30, units = "cm", dpi = 300)


###
#CURRENT FINANCIAL YEAR BUDGET sheet

#Overwrite my variable names with new column headings
colnames(SWF.FY1.table) <- headings.4

this_sheet <- as.character(paste0(as.character(FY1-1), "-", as.character(FY1-2000)))
addWorksheet(wb, this_sheet)
sheet_title <- paste0("Budget & expenditure summary - ", FY1_full)

#Insert sheet title
writeData(wb, sheet = this_sheet, sheet_title, colNames = FALSE, startRow=1,startCol="A")
#Format sheet title
addStyle(wb, sheet = this_sheet, style = title, cols = 1, rows = 1)
#Format column headers
addStyle(wb, sheet = this_sheet, style = header_bold_wrap, cols = 2:ncol(SWF.FY1.table), rows = 3, gridExpand = TRUE)
#Format first row
addStyle(wb, sheet = this_sheet, style = header_bold, cols = 1, rows = 3:(nrow(SWF.FY1.table)+3), gridExpand = TRUE)

#main part of table
writeData(wb, this_sheet, SWF.FY1.table, colNames = TRUE, startRow=3,startCol="A")

#Format financial values
addStyle(wb, sheet = this_sheet, style = gbp, cols = 2:5, rows = 4:35, gridExpand=TRUE)
addStyle(wb, sheet = this_sheet, style = gbp, cols = 8:(ncol(SWF.FY1.table)), rows = 4:35, gridExpand=TRUE)

#Format percentages
addStyle(wb, sheet = this_sheet, style = pct, cols = 6:7, rows = 4:35, gridExpand = TRUE)
conditionalFormatting(wb, sheet = this_sheet, cols = 6:7, rows = 4:37, type = "databar", gridExpand=TRUE) 
#conditionalFormatting(wb, sheet = this_sheet, cols = 7, rows = 4:37, type = "databar") 

#Format bottom two rows
addStyle(wb, sheet = this_sheet, style = bold_gbp, cols = c(2:5, 8:ncol(SWF.FY1.table)), rows = 36:37, gridExpand = TRUE)
addStyle(wb, sheet = this_sheet, style = bold_pct, cols = 6:7, rows = 36:37, gridExpand = TRUE)

#Set column widths
setColWidths(wb, this_sheet, cols = 1, widths = 33)
setColWidths(wb, this_sheet, cols = 2:7, widths = 14)
setColWidths(wb, this_sheet, cols = 8:ncol(SWF.FY1.table), widths = 10)

###
#Chart 2 - % allocation spend

this_sheet <- "Chart 2 (allocation spent)"
addWorksheet(wb, this_sheet)
#sheet_title <- "Chart 2"

#Insert chart image from saved file
insertImage(wb, this_sheet, "swfm_plot_2.png", height = 20, width = 30, units = "cm", dpi = 300)

###
#Chart 3 - % budget spend

this_sheet <- "Chart 3 (budget spent)"
addWorksheet(wb, this_sheet)
#sheet_title <- "Chart 3"

#Insert chart image from saved file
insertImage(wb, this_sheet, "swfm_plot_3.png", height = 20, width = 30, units = "cm", dpi = 300)

###
#Chart 4 - % budget spend

this_sheet <- "Chart 4 (spend profile)"
addWorksheet(wb, this_sheet)
#sheet_title <- "Chart 3"

#Insert chart image from saved file
insertImage(wb, this_sheet, "swfm_plot_4.png", height = 20, width = 30, units = "cm", dpi = 300)




saveWorkbook(wb, save_name, overwrite = TRUE)



  