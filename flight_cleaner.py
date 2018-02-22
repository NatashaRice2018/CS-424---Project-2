"""
Quick script to clean the ontime flight data from:

http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time

And turn the CSV into a TSV with various lookup codes added.
"""

import csv

def airline_table(lookup):
    data = {}
    with open(lookup, 'r') as lookup:
        reader = csv.DictReader(lookup)
        for row in reader:
            descr, code = [a.strip() for a in row['Description'].split(':')]
            data[row['Code']] = {'airline': descr, 'code': code}
    return data

def carrier_table(lookup):
    data = {}
    with open(lookup, 'r') as lookup:
        reader = csv.DictReader(lookup)
        for row in reader:
            descr = row['Description'].split('(')[0].strip()
            data[row['Code']] = descr
    return data

def main():
    carrier_lookup = r"C:\Users\natas\Documents\cs 424\CS-424---Project-2\CARRIER_HISTORY.csv"
    airline_lookup = r"C:\Users\natas\Documents\cs 424\CS-424---Project-2\AIRLINE_ID.csv"
    flight_data    = r"C:\Users\natas\Documents\cs 424\CS-424---Project-2\April_Airport_Data.csv"

    airlines       = airline_table(airline_lookup)
    carriers       = carrier_table(carrier_lookup)

    with open(flight_data, 'r') as data:
        reader = csv.DictReader(data)
        with open('ontime_flights.cleaned.csv', 'w') as output:
            writer = csv.DictWriter(output, delimiter="\t", fieldnames=reader.fieldnames)
            for row in reader:
                row['AIRLINE_ID'] = airlines[row['AIRLINE_ID']]['code']
                row['CARRIER'] = carriers[row['CARRIER']]
                writer.writerow(row)

if __name__ == '__main__':
    main()
