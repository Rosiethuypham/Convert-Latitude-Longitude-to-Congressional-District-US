#!python3

# Install required libraries:
#    % pip3 install Fiona shapely
#
#  Download 2020 TIGER/Line Shapefiles: Congressional Districts (116) from:
#    https://www.census.gov/cgi-bin/geo/shapefiles/index.php

import csv
import fiona
from shapely.geometry import shape, Point
from pprint import pprint

# Read coordinates
with open('data/Codes.csv') as f:
	reader = csv.reader(f)
	next(reader)
	codes = list(reader)

# Load congressional districts
shapefile = fiona.open('data/tl_2020_us_cd116/tl_2020_us_cd116.shp')
districts = []
for i in iter(shapefile):
	state = i["properties"]["STATEFP"]
	cd116fp = i["properties"]["CD116FP"]
	geometry = shape(i["geometry"])
	districts.append({'STATE': state, 'CD116FP': cd116fp, 'GEOMETRY': geometry})

file = csv.writer(open('output.csv', 'w'))
file.writerow(["NUMBER", "LONGITUDE", "LATITUDE", "STATEFP", "CD116FP"])

for code in codes:
	number = code[0]
	longitude = float(code[1])
	latitude = float(code[2])
	point = Point(longitude, latitude)

	for district in districts:
		if point.within(district["GEOMETRY"]):
			file.writerow([number, longitude, latitude, district["STATE"], district["CD116FP"]])

