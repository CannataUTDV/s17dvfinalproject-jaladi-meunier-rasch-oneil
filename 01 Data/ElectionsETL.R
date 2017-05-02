require(readr)
require(plyr)

# Set the Working Directory to the 00 Doc folder
file_path = "../01\ Data/finalproject_ElectionsData.csv"
elections <- readr::read_csv(file_path)
names(elections)

# keep if rename columns df plyr::rename(hydropower, c("Resource Assessment Site ID"="Site ID"))
df <- data.frame(elections)
names(elections)

dimensions <- c("State")

measures <- setdiff(names(df), dimensions)
measures

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

if( length(dimensions) > 0) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
  }
}

# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions, and change NA to 0.
if( length(measures) > 1) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    df[m] <- data.frame(lapply(df[m], function(x) as.numeric(as.character(x)))) # This is needed to turn measures back to numeric because gsub turns them into strings.
  }
}
str(df)

write.csv(df, gsub("Elections_clean", "", file_path), row.names=FALSE, na = "")
