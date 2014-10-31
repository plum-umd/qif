set macros
set term TERM
set key above

set pointsize 3
#set clip points
set boxwidth 0.8

set format y "2^{%L}"

#set ytics 

#set log y 2
set xrange [0:37]

set style line 1 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "gray"
set style line 2 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "black"
set style line 3 lt 1 lw 2 pt 1 ps 5 lc rgbcolor "black"

xcoord(x) = (x == 0) ? 45 : x
xlabel(x) = (x == 0) ? "{/Symbol \245}" : sprintf("%d", x)

#psize(x) = (x >= 1) ? 1.0 : x
#psize(x) = (x == 0) ? 0.0 : x + 0.5

#ptype(x) = (x == 0) ? 1 : (x <= 0.00002) ? 6 : (x <= 0.00003) ? 14 : (x <= 0.0002) ? 12 : 8

psize(x) = (x == 0) ? 0.2 : (x <= 0.00002) ? 0.4 : (x <= 0.00003) ? 0.6 : (x <= 0.0002) ? 0.8 : 1.2

rgb(r,g,b) = int(r)*65536 + int(g)*256 + int(b)
#pcol(x) = rgb(127 * psize(x),127 * psize(x), 127 * psize(x))
pcol(x) = rgb(0,0,0)

ifdomainthen(d,do) = sprintf("strcol(COL_STATS_DOMAIN) eq '%s' ? %s : 1/0", d, do)

set style fill solid

common_under(d,dname, pt, boxcolor, mediancolor, pointcolor) = "" \
. "'FILE_IN_STATS'" . " every ::1 using (xcoord($COL_STATS_PRECISION)):(" . ifdomainthen(d, '$COL_STATS_BELIEF_QLOWER') . "):COL_STATS_BELIEF_MIN:COL_STATS_BELIEF_MAX:COL_STATS_BELIEF_QUPPER with candlesticks ls 1 lw 4 lc rgb \"" . boxcolor . "\" title '' whiskerbars"

common_over(d,dname, pt, boxcolor, mediancolor, pointcolor) = "" \
. "'FILE_IN_STATS'" . " every ::1 using (xcoord($COL_STATS_PRECISION)):(" . ifdomainthen(d, '$COL_STATS_BELIEF_MEDIAN') . ") with points pt " . pt . " lw 4 lc rgb '" . pointcolor . "' ps 2 title \"" . dname . "\""

#darkred = "#aa0000"
#darkgreen = "#009900"
#darkblue = "#0000aa"

darkred = "#000000"
darkgreen = "#000000"
darkblue = "#000000"

lightgray = "#dddddd"

commonbover  = common_over( 'box', "median", "4", lightgray, darkblue, darkblue)
commonbunder = common_under('box', "median", "4", lightgray, darkblue, darkblue)

set key top
set output 'GRAPH_OUT'

#set grid
set size 1.6,1.0
set origin 0.0,0.0
#set multiplot
#set lmargin 10
#set rmargin 1
set tmargin 2
#set size 1.4,1.0
#set origin 0.0,0.6

#set yrange [-1:23]
#set yrange [0.45:0.62]
#set yrange [0.0001:1.5]
#set yrange [0.0002:0.01]
set yrange [0.0002:1.5]

#all_range = STATS_MAX_ALL - STATS_MIN_ALL

#set yrange [0-all_range*0.05:STATS_MAX_ALL+all_range*0.05]
set log y 10
#set yrange [0.0001:0.01]

set ylabel "max belief" offset -1,0
set xlabel "interval set size bound n"

set xtics 1,1,36

#set xtics ("1" 1, "2" 2, "3" 3, "4" 4, "5" 5, "6" 6, "7" 7, "8" 8, "9" 9, \
#           "10" 10, "15" 15, "20" 20, "25" 25, "30" 30, "35" 35, "40" 40, \
#	   "{/Symbol \245}" 45)

plot \
@commonbunder,\
@commonbover,\
'FILE_IN_BOX' every ::1 using (xcoord($COL_PREC)):COL_BELIEF with points ps 1 pt 1 title "samples"
