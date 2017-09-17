#!/usr/bin/gnuplot
#
# Generates several graphs from the temporary / intermediate CSV file generated
# by the "Computation and Analysis" component

reset
clear

# TODO: what size to use?
set terminal png size 640,480

# TODO: implement
# TODO: Using test_intermediate_timesheet.csv for development for now.

# Set colors for each category, which can be referenced by plot commands.  So
# as to keep colors consistent accross plots.
# TODO: tweak colors once we're farther along
set style line 1 lc rgb 'black' lt 1 lw 2        # total
set style line 2 lc rgb 'blue' lt 1 lw 2         # labor
set style line 3 lc rgb 'red' lt 1 lw 2          # sick
set style line 4 lc rgb 'turquoise' lt 1 lw 2    # vacation
set style line 5 lc rgb 'green' lt 1 lw 2        # holiday
set style line 6 lc rgb 'yellow' lt 1 lw 2       # comp

# X axis is the time domain. Different units for different graphs though.
#set xdata time
#set timefmt "%g"
#set format x "ww%g%v"
set xtics rotate


# Y axis is always hours
set ylabel "hours"
# 0 to largest number
set yrange [0:]
# Setup a grid
set grid


# One input file for all graphs (the intermediate CSV file)
# CSV is comma separated
set datafile separator ","

# set key
# TODO: tweak key once we're farther along
set key reverse Left outside


#################################################
# Plot 1: linechart of total week time, with breakdown (seperate lines for each category)
set output "total_week_time_linechart.png"
set title "Total Week Time"
set xlabel "Week"

# Separate lines - no stacking
plot "test_intermediate_timesheet.csv" using 1:49 title "total" with linespoints ls 1,\
    "" using 1:44 title "labor" with lines ls 2,\
    "" using 1:45 title "sick" with lines ls 3,\
    "" using 1:46 title "vacation" with lines ls 4,\
    "" using 1:47 title "holiday" with lines ls 5,\
    "" using 1:48 title "comp" with lines ls 6


#################################################
# Plot 2: stacked linechart of total week time, with breakdown (separate stacked lines for each category)
# Inspiration/code taken from https://stackoverflow.com/questions/40487935/gnuplot-draw-stacked-line-chart
set output "total_week_time_stacked_linechart.png"
set title "Total Week Time"
set xlabel "Week"

# stacked line chart
firstcol=44
cumulated(i)=((i>firstcol)?column(i)+cumulated(i-1):(i==firstcol)?column(i):1/0)
plot "test_intermediate_timesheet.csv" using 1:(cumulated(48)) title "comp" with filledcurves ls 6,\
    "" using 1:(cumulated(47)) title "holiday" with filledcurves ls 5,\
    "" using 1:(cumulated(46)) title "vacation" with filledcurves ls 4,\
    "" using 1:(cumulated(45)) title "sick" with filledcurves ls 3,\
    "" using 1:(cumulated(44)) title "labor" with filledcurves ls 2,\
    "" using 1:49 title "total" with linespoints ls 1


#################################################
# Plot 3: 
