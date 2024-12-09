
# Libraries
library_file <- './libraries'

packages <- readLines(library_file)

lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
})

url <- <FILE_LINK>

# Read the CSV file from the URL
original_file <- read.csv(url)

write.csv(df, here("data", "cleaned_data.csv"), row.names = FALSE)


df <- original_file

write.csv(df, here("data", "original_data.csv"), row.names = FALSE)
clean_names(df)

# Convert date column to date data type
df$event_date <- as.Date(df$event.date)
df$publication_date <- as.Date(df$publication.date)

# Create ID column with the number for each report
# Use later to filter broken rows
df$report_id <- seq_len(nrow(df))

# Generate the full sequence of dates
# If we will create a TS file, there will no missed days.
full_dates <- seq(
  from = min(df$publication_date, na.rm = TRUE),
  to = max(df$publication_date, na.rm = TRUE),
  by = "day")


# Find missing dates
missing_dates <- setdiff(full_dates, df$publication_date)
missing_dates_df <- data.frame(publication_date = missing_dates)

# Merge with the original dataset
df <- merge(df, missing_dates_df, by = "publication_date", all = TRUE)

# Sort the data by publication date
df <- df %>%
  arrange(publication_date)


# Extract year, month, week, and day
df$year_report <- year(df$publication_date)
df$month_report <- month(df$publication_date)
df$week_report <- week(df$publication_date)
df$day_report <- day(df$publication_date)

# Extract month name and weekday
df$month_name <- month(df$publication_date, label = TRUE, abbr = TRUE)
df$weekday <- wday(df$publication_date, label = TRUE, abbr = TRUE)

# Convert disease/syndrome values to the lower case.
df$diseases <- tolower(df$diseases)
df$title <- tolower(df$title)
df$syndromes <- tolower(df$syndromes)


# List of diseases with commas in name
# It helps to correctly strip the data
diseases_with_commas <- c("hand, foot and mouth disease")

# Replace commas within disease names with placeholders
diseases_with_commas_placeholder <- gsub(",", "<COMMA>", diseases_with_commas)

# Create a named vector for replacement
names(diseases_with_commas_placeholder) <- diseases_with_commas

# Replace disease names in the dataset
for (original_name in names(diseases_with_commas_placeholder)) {
  placeholder_name <- diseases_with_commas_placeholder[original_name]
  df$diseases <- gsub(
    original_name, placeholder_name, df$diseases, fixed = TRUE)}


# Split diseases into a list
df$disease_name <- strsplit(df$diseases, ",")

# Trim white spaces before or after the disease name
df$disease_name <- lapply(df$disease_name, function(diseases) {
  trimws(diseases)})

# Replace placeholder with real name
df$disease_name <- lapply(df$disease_name, function(diseases) {
  sapply(diseases, function(disease) {
    gsub("<COMMA>", ",", disease)})
})

# Create additional column with only one syndrome per row
df <- df %>%
  unnest(disease_name)


# Split syndromes into a list as we did with diseases
df$syndrome_name <- strsplit(df$syndromes, ",")
# Trim white spaces
df$syndrome_name <- lapply(df$syndrome_name, function(syndromes) {
  trimws(syndromes)})

# Create a column that contains only one syndrome.
df <- df %>%
  unnest(syndrome_name)



# Extract the latitude and longitude from lat_long column
split_data <- strsplit(df$lat_long, ", ")
df$latitude <- as.numeric(sapply(split_data, `[`, 1))  # Extract and convert latitude
df$longitude <- as.numeric(sapply(split_data, `[`, 2))


# Check the coordinates range, create a flag for check.
df <- df %>%
  mutate(
    coordinates_valid = ifelse(
      latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180,
      TRUE,
      FALSE)
  )


# Check that coordinates math the countries in the data.
# We filter rows with missing data in lat_long column.
# Because st_as_sf function cannot work with missing data.
df_sf <- df %>%
  filter(!is.na(df$lat_long))


# Create sf object
df_sf <- st_as_sf(df_sf, coords = c("longitude", "latitude"), crs = 4326)

# Load world boundaries from rnatural package
world <- ne_countries(scale = "medium", returnclass = "sf")


# This part ensures that names of countries in our dataset match the
# names in rnatural package
df_sf <- df_sf %>%
  mutate(
    country = ifelse(
      country == "Russian Federation", "Russia",
      ifelse(country == "United States", "United States of America", country)
    )
  )


# Spatial join
result <- st_join(df_sf, world, join = st_within)

# Check if country matches
result <- result %>%
  mutate(valid = country == admin)

# View invalid points
invalid_points <- result %>%
  filter(valid == FALSE)

# Choose the IDs of inconsistent rows
bad_ids <- invalid_points$report_id

# Filter inconsistent rows out.
df <- df %>%
  filter(!report_id %in% bad_ids)


# Flag incomplete rows (missing both syndrome and diseases name)
df <- df %>%
  mutate(
    disease_syndrome_missing = ifelse(
      is.na(diseases) & is.na(syndromes), TRUE, FALSE
    )
  )


# Filter incomplete rows
df <- df %>% filter(disease_syndrome_missing == FALSE)




# Add a column for daily disease count across the world,
disease_count <- df %>%
  filter(!is.na(disease_name)) %>%  # Filter out rows where disease_name is NA
  group_by(publication_date, disease_name) %>%
  summarise(daily_disease_count = n_distinct(report_id), .groups = 'drop')  # Count unique articles

syndrome_count <- df %>%
  filter(!is.na(syndrome_name)) %>%  # Filter out rows where syndrome_name is NA
  group_by(publication_date, syndrome_name) %>%
  summarise(daily_syndrome_count = n_distinct(report_id), .groups = 'drop')  # Count unique articles


# Join daily disease counts back to the main dataset
df <- df %>%
  left_join(disease_count, by = c("publication_date", "disease_name"))

# Join daily syndrome counts back to the main dataset
df <- df %>%
  left_join(syndrome_count, by = c("publication_date", "syndrome_name"))

# Replace NAs with 0
df <- df %>%
  mutate(
    daily_disease_count = replace_na(daily_disease_count, 0),
    daily_syndrome_count = replace_na(daily_syndrome_count, 0)
  )



# Add a column for daily disease count PER CONTRY (previous column - calculates all the countries that were reported that day)
disease_count_per_country <- df %>%
  filter(!is.na(disease_name)) %>%  # Filter out rows where disease_name is NA
  group_by(publication_date, country, disease_name) %>%
  summarise(daily_disease_count_country = n_distinct(report_id), .groups = 'drop')  # Count unique articles

# Add a column for daily syndrome count PER CONTRY
syndrome_count_per_country <- df %>%
  filter(!is.na(syndrome_name)) %>%  # Filter out rows where syndrome_name is NA
  group_by(publication_date, country, syndrome_name) %>%
  summarise(daily_syndrome_count_country = n_distinct(report_id), .groups = 'drop')  # Count unique articles


# Join daily disease counts per country back to the main dataset
df <- df %>%
  left_join(disease_count_per_country, by = c("publication_date",
                                              "country",
                                              "disease_name"))




# Join daily syndrome counts per country back to the main dataset
df <- df %>%
  left_join(syndrome_count_per_country, by = c("publication_date",
                                               "country",
                                               "syndrome_name"))



# 7-day Rolling Average

df <- df %>%
  group_by(country, disease_name) %>%
  arrange(publication_date) %>%
  mutate(country_rolling_avg = round(as.numeric(rollmean(daily_disease_count_country, k = 7, fill = NA, align = 'right')), digits = 2)) %>%
  ungroup()



# Final conversion of data types
df$publication_date <- as.Date(df$publication_date)
df$event_date <- as.Date(df$event.date)


# Save cleaned data to the "data" folder within your project
write.csv(df, here("data", "cleaned_data.csv"), row.names = FALSE)













