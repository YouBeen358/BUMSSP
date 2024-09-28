
install.packages("data.table")
install.packages("lubridate")

# Load required libraries
library(data.table)
library(lubridate)

# Define the base URL components for NOAA data
file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

# Initialize an empty list to store data for each year
all_years_data <- list()

# Loop through each year from 1985 to 2023
for (year in 1985:2023) {
  
  # Construct the full URL for the current year
  path <- paste0(file_root, year, tail)
  
  # Try to process each year's data, and catch errors if they occur
  tryCatch({
    
    # Read the header line
    header <- scan(path, what = 'character', nlines = 1)
    
    # Determine how many lines to skip based on the year
    skip_lines <- if (year < 1990) 1 else 2
    
    # Read the buoy data, filling missing values and skipping the appropriate number of lines
    buoy_data <- fread(path, header = FALSE, skip = skip_lines, fill = TRUE)
    
    # Handle cases where the number of columns in data differs from the header
    if (length(header) != ncol(buoy_data)) {
      warning(paste("Mismatch in columns for year", year))
      
      # Adjust header or data if there is a mismatch
      if (length(header) > ncol(buoy_data)) {
        header <- header[1:ncol(buoy_data)]  # Trim the header
      } else {
        buoy_data <- buoy_data[, 1:length(header)]  # Trim the data
      }
    }
    
    # Assign column names to the data
    colnames(buoy_data) <- header
    
    # Validate the date columns (YY, MM, DD, hh) before parsing
    if (all(c("YY", "MM", "DD", "hh") %in% names(buoy_data))) {
      # Convert date components into a single Date column
      buoy_data$Date <- ymd_h(paste(buoy_data$YY, buoy_data$MM, buoy_data$DD, buoy_data$hh, sep = "-"))
    } else {
      warning(paste("Date columns missing for year", year))
      buoy_data$Date <- NA
    }
    
    # Store the buoy data for this year in the list
    all_years_data[[as.character(year)]] <- buoy_data
    
  }, error = function(e) {
    # If there's an error, print a message and continue with the next year
    warning(paste("Failed to process data for year", year, ":", e$message))
  })
}

# Combine all the data into a single data.table
combined_data <- rbindlist(all_years_data, use.names = TRUE, fill = TRUE)

# View the first few rows of the combined data
head(combined_data)

#save the csv file for 1985-2023
write.csv(combined_data, "buoy_44013_data_1985_2023.csv", row.names = FALSE)


nrow(combined_data)