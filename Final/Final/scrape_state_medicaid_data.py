import xlrd
import csv

#"/Users/nikhilgopal/Downloads/financial-management-report-fy2018/FY 2018 FMR NET EXPENDITURES.xlsx"

workbook = xlrd.open_workbook("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/Final/data/2014/financial-management-report-fy2014/FMR Net Expenditures FY14.xlsx")

sheets = workbook.sheet_names()

list_of_values = []
counter = 0

for sheet in sheets:
    sh = workbook.sheet_by_name(sheet)
    list_of_values.append(sheet[6:])
    for col in sh.col_values(0):
        if col == 'Total VIII Group':
            list_of_values.append(sh.cell_value(counter, 3))
        else:
            counter += 1
    counter = 0


fh = open("/Users/nikhilgopal/Google Drive/Notability/Research and Design_ Data Analysis/Final/data/2014.txt", "w")

counter = 0
for index in list_of_values:
    if counter == 0:
        fh.write(list_of_values[0])
        fh.write(",")
        counter += 1
    elif counter == 1:
        fh.write(str(list_of_values[1]))
        fh.write("\n")
        counter += 1
    elif counter % 2 == 0:
        fh.write(str(list_of_values[counter]))
        fh.write(",")
        counter += 1
    elif counter % 2 != 0:
        fh.write(str(list_of_values[counter]))
        fh.write("\n")
        counter += 1

fh.close()
