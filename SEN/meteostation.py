#!/usr/bin/env python
import signal
import sys
import argparse
import time
import json
import gspread
from datetime import datetime
from oauth2client.client import SignedJwtAssertionCredentials
from DHT11 import DHT11

if __name__ == "__main__":
    # Manage arguments.
    parser = argparse.ArgumentParser(
        description='Meteostation - measure temperature and humidity.')
    parser.add_argument('--time', '-t', default=1, type=int,
                        help='Sampling frequency in miliseconds.')

    args = parser.parse_args()
    
    # init DHT11
    dht11 = DHT11(17)
    SLEEP_INTERVAL = args.time
    
    # load google API authentication file
    json_key = json.load(open('Meteostation-b107033494ae.json'))
    scope = ['https://spreadsheets.google.com/feeds']
    credentials = SignedJwtAssertionCredentials(json_key['client_email'], json_key['private_key'].encode(), scope)
    gc = gspread.authorize(credentials)

    # load current_temperature sheet
    current_sheet = gc.open("current_temp").sheet1
    current_sheet.resize(2, 3)
    current_sheet.update_cell(1, 1, "timestamp")
    current_sheet.update_cell(1, 2, "temperature")
    current_sheet.update_cell(1, 3, "humidity")

    # load history temperatures
    sheet = gc.open("temps").sheet1

    while True:
        try:
            # Update meteo values in sensor  
            dht11.startSensor()
            time.sleep(SLEEP_INTERVAL)

            # Prepare row to insert to historical data
            row = [ datetime.now().strftime('%Y-%m-%d %H:%M:%S'), dht11.getTemperature(), dht11.getHumidity() ]
            if sheet.row_count > 25:
                sheet.delete_row(2)

            # Save to historical data
            sheet.append_row(row)

            # Update the current values at second sheet
            current_sheet.update_cell(2, 1, datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
            current_sheet.update_cell(2, 2, dht11.getTemperature())
            current_sheet.update_cell(2, 3, dht11.getHumidity())

            # Print to stdout
            print(time.time(), float("%.2f" % dht11.getTemperature()), float("%.2f" % dht11.getHumidity()))
        except KeyboardInterrupt:
            print "Shutting down the meteo station. Bye."
            dht11.clean()
            sys.exit()

