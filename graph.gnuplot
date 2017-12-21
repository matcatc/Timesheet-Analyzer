#!/usr/bin/gnuplot
#
# Generates several graphs from the temporary / intermediate CSV file generated
# by the "Computation and Analysis" component

# Invoke this gnuplot program as so:
#  gnuplot -c graph.gnuplot <input_filename> <output_dir>
input_filename = ARG1
output_dir = ARG2

# input_filename is a variable passed into gnuplot on the command line which
# specifies what CSV file to read the data from. We verify it was set here.
if (input_filename eq "") {
    exit error "input_filename was not specified, so unable to read in data."
}

# output_dir is a variable passed into gnuplot on the command line which
# specifies what directory to store the generated graphs too. If not specified,
# will be set to current directory.
if (output_dir eq "") output_dir="."

print "input_filename = ".input_filename
print "output_dir = ".output_dir

# Ensure we're at a known good starting environment
reset
# Don't do clear here since that will create a popup window

# TODO: what size to use?
set terminal pngcairo size 640,480

# Set colors for each category, which can be referenced by plot commands.  So
# as to keep colors consistent accross plots.
# TODO: tweak colors once we're farther along
set style line 1 lc rgb 'black' lt 1 lw 2        # total
set style line 2 lc rgb 'blue' lt 1 lw 2         # labor
set style line 3 lc rgb 'red' lt 1 lw 2          # sick
set style line 4 lc rgb 'turquoise' lt 1 lw 2    # vacation
set style line 5 lc rgb 'green' lt 1 lw 2        # holiday
set style line 6 lc rgb 'olive' lt 1 lw 2       # comp
set style line 7 lc rgb 'purple' lt 1 lw 2       # threshold / annotations


# X axis is the time domain. Different units for different graphs though.
# Since in the CSV we're specifying the week just as a number (e.g.: 201736),
# no need to parse it or anything.  Histograms will of course need to setup
# their X axis specially.
set xtics rotate


# Y axis is always hours
set ylabel "hours"
# 0 to largest number
set yrange [0:*]
# Setup a grid
set grid
# Have ytics be in 8 hour increments (since a normal working day is 8 hours)
set ytics 8


# One input file for all graphs (the intermediate CSV file)
# CSV is comma separated
set datafile separator ","

# set key
# TODO: tweak key once we're farther along
set key reverse Left outside


# Run stats command in order to get the number of rows (records)
stats input_filename

# Number of xtics we want in the graph. Design constant.
number_xtics = 6

# How many labels to skip in order to get number_xtics in the graph (approx)
xtic_skip = int(STATS_records / number_xtics)

# Function to print out the xtics labels only every N labels
# Note: hardcodes the labels as being in column 1 (which they are)
# Note: "hardcodes" how often to print out xtics using data from stats computed above
# See: http://compgroups.net/comp.graphics.apps.gnuplot/way-to-limit-xtics-in-histogram/351857 for the inspiration
everyNth(dum) = (int(column(0)) % xtic_skip == 0) ? stringcolumn(1) : ""

# Note: to make the above function work in the plots, we need to make sure the
# using part of the plot command is correct. In particular, don't do "1:col",
# as this will pull in the label column (1) into the graph. Instead do "col:xtics(everyNth())"


#################################################
# Plot 1: linechart of total week time, with breakdown (seperate lines for each category)
set output output_dir."/total_week_time_linechart.png"
set title "Total Week Time"
set xlabel "Week"

# Smoothed separate lines - no stacking
# Use smoothing so the graph is a little easier on the eyes and to see. use
# mcsplines so that the lines go through all data points and don't "overshoot"
# (as csplines would).
plot input_filename using 49:xtic(everyNth(0)) title "total" smooth mcsplines with lines ls 1,\
    "" using 44 title "labor" smooth mcsplines with lines ls 2,\
    "" using 45 title "sick" smooth mcsplines with lines ls 3,\
    "" using 46 title "vacation" smooth mcsplines with lines ls 4,\
    "" using 47 title "holiday" smooth mcsplines with lines ls 5,\
    "" using 48 title "comp" smooth mcsplines with lines ls 6,\
    40 title "40" with lines ls 7,\
    "" using 49 title "" with points ls 1


#################################################
# Plot 2: stacked linechart of total week time, with breakdown (separate stacked lines for each category)
# Inspiration/code taken from https://stackoverflow.com/questions/40487935/gnuplot-draw-stacked-line-chart
set output output_dir."/total_week_time_stacked_linechart.png"
set title "Total Week Time"
set xlabel "Week"

# stacked line chart
# fill from x axis since we won't have any starting/ending weeks with "0" hours
firstcol=44
cumulated(i)=((i>firstcol)?column(i)+cumulated(i-1):(i==firstcol)?column(i):1/0)
plot input_filename using (cumulated(48)):xtic(everyNth(0)) title "comp" with filledcurves x ls 6,\
    "" using (cumulated(47)) title "holiday" with filledcurves x ls 5,\
    "" using (cumulated(46)) title "vacation" with filledcurves x ls 4,\
    "" using (cumulated(45)) title "sick" with filledcurves x ls 3,\
    "" using (cumulated(44)) title "labor" with filledcurves x ls 2,\
    "" using 49 title "total" with linespoints ls 1,\
    40 title "40" with lines ls 7


#################################################
# Plot 3: Smoothed linechart of labor showing "running average" of labor time.

set output output_dir."/average_week_labor_linechart.png"
set title "Averaged/Smoothed Week Labor Time"
set xlabel "Week"

plot input_filename using 44:xtic(everyNth(0)) title "smoothed labor" smooth bezier with lines ls 2,\
    "" using 44 title "labor" with points ls 2,\
    40 title "40" with lines ls 7


#################################################
# Plot 4: Smoothed linechart of total time showing "running average" of total time.

set output output_dir."/average_week_total_linechart.png"
set title "Averaged/Smoothed Week Total Time"
set xlabel "Week"

plot input_filename using 49:xtic(everyNth(0)) title "smoothed total" smooth bezier with lines ls 1 lw 2,\
    "" using 49 title "total" with points ls 1,\
    40 title "40" with lines ls 7
#    "" using 1:49 title "total" smooth mcsplines with lines ls 1 lw 1,\


#################################################
# Plot 5: Weekly earned comp time
# Note: the comp time earned each week can be negative in cases where comp time
# is spent/used. Need to allow for negative y values in this graph.

# set yrange so it will display negative values
set yrange [*:*]

set output output_dir."/week_earned_comp_time_linechart.png"
set title "Weekly earned comp time"
set xlabel "Week"

# add a line for x axis to help with visibility
plot input_filename using 50:xtic(everyNth(0)) title "earned comp time" with lines ls 6 lw 2,\
     0 title "" with lines ls 7

# Resetting yrange back to what it was before
# 0 to largest number
set yrange [0:*]

#################################################
# Plot 6: Weekly accrued comp time
# Note: since the comp time earned each week could be negative, the accrued
# value could also theoretically go negative (i.e.: borring comp time from the
# future.) So we need to allow for negative y values in this graph

# set yrange so it will display negative values
set yrange [*:*]

set output output_dir."/week_accrued_comp_time_linechart.png"
set title "Weekly accrued comp time"
set xlabel "Week"

# add a line for x axis to help with visibility
plot input_filename using 51:xtic(everyNth(0)) title "accrued comp time" with lines ls 6 lw 2,\
     0 title "" with lines ls 7

# Resetting yrange back to what it was before
# 0 to largest number
set yrange [0:*]


#################################################
# Plot 7: TODO

# TODO: implement more graphs
