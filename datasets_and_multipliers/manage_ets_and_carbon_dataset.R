library(ggplot2)
library(dplyr)
library(ggthemes)  # For better themes
library(ggtext)  # For styled text
library(scales)
library(rsdmx)
library(dplyr)
library(lubridate)


files <- list.files(path = "WorldCarbonPricingDatabase/_dataset/data/CO2/national", 
                    pattern = "*.csv", full.names = TRUE)
data_list <- lapply(files, read.csv) 
names(data_list) <- gsub("/wcpd_co2_|.csv", "", files)

co2_data <- bind_rows(data_list, .id = "Country")

co2_data$Country <- NULL

write.csv(co2_data, "outputs/co2_data.csv", row.names = FALSE)

co2_datafact <- co2_data
co2_datafact$jurisdiction <- as.factor(co2_datafact$jurisdiction)
co2_datafact$year <- as.factor(co2_datafact$year)
co2_datafact$ipcc_code <- as.factor(co2_datafact$ipcc_code)
co2_datafact$ets_curr_code <- as.factor(co2_datafact$ets_curr_code)
co2_datafact$tax_curr_code <- as.factor(co2_datafact$tax_curr_code)

str(co2_datafact)
glimpse(co2_datafact)
summary(co2_datafact)

india <- co2_data %>%
  filter(jurisdiction == "India")

con_ipcc <- c(
  # Land transport and transport via pipelines
  "1A3",    # Transport (land)
  "1B2",    # Fugitive emissions from oil and natural gas (pipelines)
  
  # Basic metals
  "2C",     # Metal production
  
  # Coke and refined petroleum products
  "2A",     # Mineral products (includes coke production)
  "1B1",    # Fugitive emissions from solid fuels (coke production)
  
  # Construction
  "1A2",    # Manufacturing industries and construction
  
  # Other non-metallic mineral products
  "2A",     # Mineral products (cement, lime, glass, ceramics)
  
  # Water supply; sewerage, waste management and remediation activities
  "5A",     # Solid waste disposal
  "5B",     # Biological treatment of solid waste
  "5C",     # Incineration and open burning of waste
  "5D",     # Wastewater treatment and discharge
  
  # Chemicals and chemical products
  "2B",     # Chemical industry
  
  # Air transport
  "1A3C",   # Transport - Aviation
  
  # Electricity, gas, steam and air conditioning supply
  "1A1",    # Energy industries
  
  # Mining and quarrying: energy producing products
  "1B1",    # Fugitive emissions solid fuels (coal mining)
  "1B2",    # Fugitive emissions oil and natural gas
  
  # Water transport
  "1A3B",   # Transport - Marine
  
  # Mining and quarrying: non-energy producing products
  "2C"      # Metal production and related industrial processes (mining non-energy)
)

sweden <- co2_data %>%
  filter(jurisdiction == "Sweden") %>%
  filter(ipcc_code %in% con_ipcc)

poland <- co2_data %>%
  filter(jurisdiction == "Poland") %>%
  filter(ipcc_code %in% con_ipcc)
  
finland <- co2_data %>%
  filter(jurisdiction == "Finland") %>%
  filter(ipcc_code %in% con_ipcc)

# CARBON DATA
jurisdictions_by_currency <- list(
  #EUR = c("Estonia", "Finland", "France", "Ireland", "Latvia", "Luxembourg", "Netherlands", 
          #"Portugal", "Slovenia", "Sweden", "Norway", "Poland", "Liechtenstein", "Switzerland"),
  EUR = c("Sweden", "Poland", "Finland", "France", "Portugal", "Estonia", "Switzerland",
          "Norway")
)

# Funzione per scaricare e processare tassi di cambio da ECB (da valuta a EUR)
get_annual_fx_rate <- function(currency) {
  # costruiamo URL SDMX per tassi di cambio annuali, tipo SP00 (reference), da valuta a EUR
  url <- paste0("https://sdw-wsrest.ecb.europa.eu/service/data/EXR/D.", currency, ".EUR.SP00.A?format=sdmx-2.1")
  
  # scarica dati
  sdmx_data <- readSDMX(url)
  df <- as.data.frame(sdmx_data)
  
  # pulizia e aggregazione media annuale
  df_clean <- df %>%
    select(obsTime, obsValue) %>%
    mutate(anno = year(ymd(obsTime))) %>%
    group_by(anno) %>%
    summarise(tasso_medio = mean(obsValue, na.rm = TRUE)) %>%
    ungroup()
  
  return(df_clean)
}

# Scarica tassi annuali per CHF ed NZD
chf_rates <- get_annual_fx_rate("CHF") 

sek_rates <- get_annual_fx_rate("SEK")

pln_rates <- get_annual_fx_rate("PLN")

nok_rates <- get_annual_fx_rate("NOK")

exchange_rates <- tibble::tibble(
  year = 1989:2025,
  nok_to_eur = c(rep(NA, 10), nok_rates$tasso_medio),
  sek_to_eur = c(rep(NA, 10), sek_rates$tasso_medio),
  chf_to_eur = c(rep(NA, 10), chf_rates$tasso_medio),
  pln_to_eur = c(rep(NA, 10), pln_rates$tasso_medio)
)

currency = "EUR"
# Filter the dataset for the current currency's nations
co2_selected_nations <- co2_data %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]]) %>%
  mutate(
    currency = currency  
  )  %>%
  group_by(jurisdiction, year) %>%
  summarise(mean_tax = mean(tax_rate_incl_ex_clcu, na.rm = TRUE)) %>%
  mutate(
    mean_tax = ifelse(is.nan(mean_tax), NA, mean_tax)  # Convert NaN to NA
  ) %>%
  left_join(exchange_rates, by = "year") %>%
  mutate(
    mean_tax = case_when(
      jurisdiction == "Norway" ~ mean_tax / nok_to_eur,
      jurisdiction == "Poland" ~ mean_tax / pln_to_eur,
      jurisdiction == "Sweden" ~ mean_tax / sek_to_eur,
      jurisdiction == "Switzerland" ~ mean_tax / chf_to_eur,
      jurisdiction == "Liechtenstein" ~ mean_tax / chf_to_eur,
      TRUE ~ mean_tax
    )
  )

tax_data_subset <- co2_selected_nations[, 1:3]
write.csv(tax_data_subset, "outputs/tax_data.csv", row.names = FALSE)


#-------------------------------GET ETS DATA------------------------------------

jurisdictions_by_currency <- list(
  EUR = c("Sweden", "Poland", "Finland", "France", "Portugal", "Latvia", "Estonia", "Switzerland",
          "Norway", "Italy", "China", "New Zealand", "United States", "Germany")
)

ets_values <- co2_data %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]]) %>%
  mutate(
    currency = currency  # Add the currency to the data for reference
  ) %>%
  group_by(jurisdiction, year) %>%
  summarise(mean_tax = mean(ets_price, na.rm = TRUE))

jurisdiction_currency_map <- co2_data %>%
  group_by(jurisdiction) %>%
  summarise(
    ets_curr_code = first(tax_curr_code[!is.na(ets_curr_code)]),
    .groups = "drop"
  ) %>% 
  filter(jurisdiction %in% jurisdictions_by_currency[["EUR"]])

ets_data <- data.frame(year = ets_values$year[which(ets_values == "Estonia")], 
                       EUR = ets_values$mean_tax[which(ets_values == "Estonia")],
                       CHN = ets_values$mean_tax[which(ets_values == "China")],
                       US = ets_values$mean_tax[which(ets_values == "United States")],
                       NWZ = ets_values$mean_tax[which(ets_values == "New Zealand")],
                       CHE = ets_values$mean_tax[which(ets_values == "Switzerland")])


# Funzione per scaricare e processare tassi di cambio da ECB (da valuta a EUR)
get_annual_fx_rate <- function(currency) {
  # costruiamo URL SDMX per tassi di cambio annuali, tipo SP00 (reference), da valuta a EUR
  url <- paste0("https://sdw-wsrest.ecb.europa.eu/service/data/EXR/D.", currency, ".EUR.SP00.A?format=sdmx-2.1")
  
  # scarica dati
  sdmx_data <- readSDMX(url)
  df <- as.data.frame(sdmx_data)
  
  # pulizia e aggregazione media annuale
  df_clean <- df %>%
    select(obsTime, obsValue) %>%
    mutate(anno = year(ymd(obsTime))) %>%
    group_by(anno) %>%
    summarise(tasso_medio = mean(obsValue, na.rm = TRUE)) %>%
    ungroup()
  
  return(df_clean)
}

# Scarica tassi annuali per CHF ed NZD
chf_rates <- get_annual_fx_rate("CHF")
nzd_rates <- get_annual_fx_rate("NZD")
cny_rates <- get_annual_fx_rate("CNY")


# Uniamo i tassi a ets_data e convertiamo CHF->EUR e NZD->EUR
ets_data_converted <- ets_data %>%
  left_join(chf_rates, by = c("year" = "anno")) %>%
  rename(tasso_chf = tasso_medio) %>%
  left_join(nzd_rates, by = c("year" = "anno")) %>%
  rename(tasso_nzd = tasso_medio) %>%
  left_join(cny_rates, by = c("year" = "anno")) %>%
  rename(tasso_cny = tasso_medio) %>%
  mutate(
    CHE_EUR = ifelse(!is.na(CHE) & !is.na(tasso_chf), CHE / tasso_chf, NA),
    NWZ_EUR = ifelse(!is.na(NWZ) & !is.na(tasso_nzd), NWZ / tasso_nzd, NA),
    CNY_EUR = ifelse(!is.na(CHN) & !is.na(tasso_cny), CHN / tasso_cny, NA)
  ) %>%
  # Sostituisci tutti i NaN con NA
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# Visualizza risultato
print(ets_data_converted)

write.csv(ets_data_converted, "outputs/ets_data.csv", row.names = FALSE)


library(dplyr)
library(rlang)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)


tax_data <- read.csv("outputs/tax_data.csv")
ets_data <- read.csv("outputs/ets_data.csv")


# GRAFICI

# GRAFICO EXPONENTS BETA

# Set the parent folder where the country subfolders are located
parent_folder <- "outputs/GI_reg/"

# Find all files named "regression_GI_results" within subdirectories
files <- list.files(parent_folder, pattern = "regression_GI_results", 
                    recursive = TRUE, full.names = TRUE)

# GI BETA

second_cols <- list()
first_col <- NULL

# Loop over files
for (i in seq_along(files)) {
  df <- read.csv(files[i])  # adjust read.csv to read.table or read.delim as needed
  
  if (i == 1) {
    # Store the first column only once
    first_col <- df[[1]]
  }
  
  # Get country name from folder name
  country <- basename(dirname(files[i]))
  
  # Store the second column with country name
  second_cols[[country]] <- df[[2]]
}

# Combine into a single data frame
result_df <- data.frame(ID = first_col, second_cols)

write.csv(result_df, "outputs/beta_GI.csv", row.names = FALSE)

# CV data

second_cols <- list()
first_col <- NULL

# Loop over files
for (i in seq_along(files)) {
  df <- read.csv(files[i])  # adjust read.csv to read.table or read.delim as needed
  
  if (i == 1) {
    # Store the first column only once
    first_col <- df[[1]]
  }
  
  # Get country name from folder name
  country <- basename(dirname(files[i]))
  
  # Store the second column with country name
  second_cols[[country]] <- df[[6]]
}

# Combine into a single data frame
result_df <- data.frame(ID = first_col, second_cols)

write.csv(result_df, "outputs/CV_data.csv", row.names = FALSE)


all_countries <- c("Sweden", "Poland", "Finland", "France", "Portugal", "Estonia", "Switzerland",
                   "Norway", "Italy", "New Zealand", "Germany", "Spain", "United States", "China", "India",
                   "Europe")

# Palette di colori personalizzata
color_palette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "purple", "#A65628", 
  "#F781BF", "gold", "#999999", "#66C2A5", "black", "violet", "lightblue", "#33FF66","#FF9933", "black"
)

color_palette_named <- setNames(color_palette, all_countries)


result_df <- read.csv("outputs/beta_GI.csv")

long_df <- result_df %>%
  rename(`New Zealand` = New.Zealand) %>%
  pivot_longer(-ID, names_to = "Country", values_to = "Value")

# Make sure ID is numeric (in case it's a factor or character)
long_df$ID <- as.numeric(as.character(long_df$ID))

# Rimuovi China e Latvia
long_df_filtered <- long_df %>%
  filter(!Country %in% c("Latvia"))

# Ordina i livelli dei Paesi per una legenda ordinata
long_df_filtered$Country <- factor(long_df_filtered$Country)

x11()
ggplot(long_df_filtered, aes(x = ID, y = Value, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.0, alpha = 0.9, shape = 21, fill = "white", stroke = 1) +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed", size = 0.8) +   # qui aggiungi la riga rossa tratteggiata
  facet_wrap2(
    ~Country,
    ncol = 5,
    scales = "free_y",
    axes = "x"
  ) +
  scale_color_manual(values = color_palette_named) +
  theme_minimal(base_size = 14) +
  labs(
    title = expression("Exponent " * beta * " of the Power-Law by Country"),
    subtitle = "Annual estimates from 2000 to 2019",
    x = "Year",
    y = expression(beta)
  ) +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 20, face = "bold", color = "#222222"),
    plot.subtitle = element_text(size = 16, color = "#666666"),
    axis.title = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(size = 14, angle = 90, hjust = 0.5, vjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

result_df <- read.csv("outputs/CV_data.csv")

long_df <- result_df %>%
  rename(`New Zealand` = New.Zealand) %>%
  pivot_longer(-ID, names_to = "Country", values_to = "Value")

# Make sure ID is numeric (in case it's a factor or character)
long_df$ID <- as.numeric(as.character(long_df$ID))

# Rimuovi China e Latvia
long_df_filtered <- long_df %>%
  filter(!Country %in% c("Latvia"))

# Ordina i livelli dei Paesi per una legenda ordinata
long_df_filtered$Country <- factor(long_df_filtered$Country)

x11()
ggplot(long_df_filtered, aes(x = ID, y = Value, color = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.0, alpha = 0.9, shape = 21, fill = "white", stroke = 1) +
  facet_wrap2(
    ~Country,
    ncol = 5,
    scales = "free_y",
    axes = "x"
  ) +
  scale_color_manual(values = color_palette_named) +
  theme_minimal(base_size = 14) +
  labs(
    title = expression("Coefficient of Variation (CV) by Country"),
    subtitle = "Annual estimates from 2000 to 2019",
    x = "Year",
    y = "Coefficient of Variation (CV)"
  ) +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 20, face = "bold", color = "#222222"),
    plot.subtitle = element_text(size = 16, color = "#666666"),
    axis.title = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(size = 14, angle = 90, hjust = 0.5, vjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )



# GRAFICO CARBON TAX

tax_data_gr <- tax_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

glimpse(tax_data_gr)

# Filter data
filtered_data <- tax_data_gr %>%
  filter(year >= 2000 & year <= 2019) %>%
  filter(jurisdiction != "Latvia")

# Get number of unique jurisdictions for color palette
num_jurisdictions <- length(unique(filtered_data$jurisdiction))


# Plot for the current currency
currency <- "EUR"

x11()
ggplot(filtered_data, aes(x = year, y = mean_tax, color = jurisdiction, group = jurisdiction)) +
  geom_line(size = 1.2) +  # Solid lines for each nation
  geom_point(size = 2.5, alpha = 0.8, shape = 21, fill = "white", stroke = 1) +  # Points on the lines
  facet_wrap2(
    ~jurisdiction,
    ncol = 4,
    scales = "free_y",
    axes = "x"
  ) +
  scale_color_manual(values = color_palette_named) +
  theme_minimal(base_size = 16) +  # Minimal theme with larger text size
  coord_cartesian(xlim = c(2000, 2019)) +
  scale_x_continuous(breaks = seq(1989, 2022, 3)) +
  labs(
    title = paste("Carbon Tax Rates for EU Nations Over Time"),
    subtitle = paste("Data from 2000 to 2019"),
    x = "Year", 
    y = "Tax Rate (eur/ton CO2)",
    color = "Nation" 
  ) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 20, face = "bold", color = "#222222"),
    plot.subtitle = element_text(size = 16, color = "#666666"),
    axis.title = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(size = 14, angle = 90, hjust = 0.5, vjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))


# GRAFICO ETS

ets_data_gr <- ets_data %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

glimpse(tax_data_gr)

# Filter data
filtered_data <- ets_data_gr %>%
  filter(year >= 2000 & year <= 2019) 
filtered_data <- filtered_data[, c(1, 2, 10, 11)]
filtered_data <- filtered_data %>%
  pivot_longer(-year, names_to = "jurisdiction", values_to = "mean_tax") %>%
  mutate(jurisdiction = recode(jurisdiction,
                               "CHE_EUR" = "Switzerland",
                               "EUR" = "Europe",
                               "NWZ_EUR" = "New Zealand"))

# Plot for the current currency
currency <- "EUR"

x11()
ggplot(filtered_data, aes(x = year, y = mean_tax, color = jurisdiction, group = jurisdiction)) +
  geom_line(size = 1.2) +  # Solid lines for each nation
  geom_point(size = 2.5, alpha = 0.8, shape = 21, fill = "white", stroke = 1) +  # Points on the lines
  facet_wrap2(
    ~jurisdiction,
    ncol = 2,
    scales = "free_y",
    axes = "x"
  ) +
  theme_minimal(base_size = 16) +  # Minimal theme with larger text size
  coord_cartesian(xlim = c(2000, 2019)) +
  scale_color_manual(values = color_palette_named) +
  scale_x_continuous(breaks = seq(1989, 2022, 3)) +
  labs(
    title = paste("ETS Price Over Time"),
    subtitle = paste("Data from 2000 to 2019"),
    x = "Year", 
    y = "ETS Price (Local Curr/ton CO2)",
    color = "Nation" 
  ) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    legend.position = "none",
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 20, face = "bold", color = "#222222"),
    plot.subtitle = element_text(size = 16, color = "#666666"),
    axis.title = element_text(size = 16, face = "bold"), 
    axis.text.x = element_text(size = 14, angle = 90, hjust = 0.5, vjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))







