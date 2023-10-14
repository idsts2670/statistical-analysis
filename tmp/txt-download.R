library("MASS")
library("tidyverse")
library("data.table")


# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "tmp")
file_content <- readLines(file.path(main_path, "problem.txt"))


# Use regex patterns to process the lines

# processed_lines <- gsub(
#   pattern = "(-?\\d+\\.\\d{4})\\s+(\\d{1,4})(\\d{1,2})",
#   replacement = "\\1 \\2 \\3",
#   x = file_content
# )


# processed_lines <- gsub(
#   pattern = "(-?\\d+\\.\\d{4})\\s+(\\d{1,3})(\\d{1,4})(\\d{1,2})",
#   replacement = "\\1 \\2\\3 \\4",
#   x = file_content
# )

# processed_lines <- gsub(
#   pattern = "(-?\\d+\\.\\d{4})\\s+(\\d{1,3}?)(\\d)(\\d{1})$",
#   replacement = "\\1 \\2\\3 \\4",
#   x = file_content
# )

# this should be the correct one
processed_lines <- gsub(
  pattern = "(-?\\d+\\.\\d{4})\\s+(\\d{1,3}?)(\\d{1,2})$",
  replacement = "\\1 \\2 \\3",
  x = file_content
)

# Write the processed content to a new temporary file
temp_file <- tempfile()
writeLines(processed_lines, temp_file)

# Read the data using fread
data <- fread(temp_file)

# Set the column names
setnames(data, c("X", "Y", "Z"))

print(data)
