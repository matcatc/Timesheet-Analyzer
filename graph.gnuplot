#!/usr/bin/gnuplot
#
# Generates several graphs from the temporary / intermediate CSV file generated
# by the "Computation and Analysis" component


# TODO: implement

# X axis is the time domain. Different units for different graphs though.
#set xdata time
#set timefmt "%Y-%m-%d %H:%M:%S"
#set format x "%Y-%m-%d %H:%M"
#set xlabel "time"
#set xtics rotate

# Y axis is always hours
set ylabel "hours"
# 0 to largest number
set yrange [0:]

# One input file for all graphs (the intermediate CSV file)
# CSV is comma separated
set datafile separator ","

