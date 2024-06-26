# Set variables
CSV_DIRECTORY=C:/Users/resur/Documents/drg/Excel/
DATASET=grouper_v5

# List all CSV files
for FILE in $CSV_DIRECTORY/*.csv; do
    # Get the base name of the file
    BASE_NAME=$(basename $FILE .csv)
    
    # Load data into BigQuery
    bq load --source_format=CSV --autodetect $DATASET.$BASE_NAME $FILE
done

echo "All data loaded successfully."
