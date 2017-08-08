#!/usr/bin/env sh

# Removes header information
# Specifically, deletes everything up to and including the line "Events$"

if [ ! -d raw_data ]; then
    mkdir raw_data
    echo "Place your data in the folder data"
    exit 1
fi

# create folder if it doesn't exist
[ -d ~/formatted_data ] || mkdir formatted_data

for f in raw_data/*; do
    f=$(basename "$f")

    echo "state , time" > formatted_data/"$f"

    # Remove the header and extract relevant columns
    sed '1,/^Events/d' "raw_data/$f" | awk '{print $5,",",$2}' >> formatted_data/"$f"
    
done
