set macros
set term TERM
set key above

set pointsize 3
#set clip points
set boxwidth 0.6

#set format y "2^{%L}"

#set ytics 

#set log y 2
set xrange [-2:49]

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

set style fill empty

common_under(d,dname, pt, boxcolor, mediancolor, pointcolor) = "" \
. "'FILE_IN_STATS'" . " every ::1 using (xcoord($COL_STATS_PRECISION)):(" . ifdomainthen(d, '$COL_STATS_REALTIME_QLOWER') . "):COL_STATS_REALTIME_MIN:COL_STATS_REALTIME_MAX:COL_STATS_REALTIME_QUPPER with candlesticks ls 1 lw 4 lc rgb \"" . boxcolor . "\" title '' whiskerbars"

common_over(d,dname, pt, boxcolor, mediancolor, pointcolor) = "" \
. "'FILE_IN_STATS'" . " every ::1 using (xcoord($COL_STATS_PRECISION)):(" . ifdomainthen(d, '$COL_STATS_REALTIME') . "):($COL_STATS_PSIZE) with points pt " . pt . " ps variable lw 4 lc rgb '" . pointcolor . "' title \"" . dname . "\""

#. "'FILE_IN_STATS'" . " every ::1 using (xcoord($COL_STATS_PRECISION)):(" . ifdomainthen(d, '$COL_STATS_REALTIME') . "):xticlabels(xlabel($COL_STATS_PRECISION)) with points ls 3 ps 2 lw 2 lc rgb '" . mediancolor . "' title ''"

#darkred = "#aa0000"
#darkgreen = "#009900"
#darkblue = "#0000aa"

darkred = "#000000"
darkgreen = "#000000"
darkblue = "#000000"

commonpover = common_over('poly', "polygons", "14", "gray", darkred, darkred)
commonoover = common_over('octalatte', "octagons", "12", "gray", darkgreen, darkgreen)
commonbover = common_over('box', "intervals", "4", "gray", darkblue, darkblue)
commonpunder = common_under('poly', "polygons", "14", "gray", darkred, darkred)
commonounder = common_under('octalatte', "octagons", "12", "gray", darkgreen, darkgreen)
commonbunder = common_under('box', "intervals", "4", "gray", darkblue, darkblue)

set key top
set output 'GRAPH_OUT'

#set grid
set size 1.4,1.6
set origin 0.0,0.0
set multiplot
set lmargin 10
#set rmargin 1
set tmargin 2
set size 1.4,1.0
set origin 0.0,0.6

#set yrange [-1:23]
#set yrange [0.45:0.62]
#set yrange [-0.1:1.1]

all_range = STATS_MAX_ALL - STATS_MIN_ALL

set yrange [0-all_range*0.05:STATS_MAX_ALL+all_range*0.05]

set ylabel "time [seconds]" offset -1,0

set xtics ("1" 1, "2" 2, "3" 3, "4" 4, "5" 5, "6" 6, "7" 7, "8" 8, "9" 9, \
           "10" 10, "15" 15, "20" 20, "25" 25, "30" 30, "35" 35, "40" 40, \
	   "{/Symbol \245}" 45)

plot \
@commonpunder,\
@commonounder,\
@commonbunder,\
@commonpover,\
@commonoover,\
@commonbover

set size 1.4,0.6
set origin 0.0,0.0
set nokey
set tmargin 0

#set output 'GRAPH_OUT2'
#set yrange [-1:23]
#set yrange [0.45:0.62]
#set yrange [-0.1:1.1]


box_range = STATS_MAX_BOX - STATS_MIN_BOX

set yrange [STATS_MIN_BOX - box_range * 0.1 : STATS_MAX_BOX + box_range * 0.1]

set ylabel "time [seconds]" offset 1,0

#set bmargin 1.5
#unset xtics
#set noxtics
#unset xlabel

set xtics(1,2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45)
set format x ""
set xlabel "set size bound" offset 0,1.5

plot \
@commonpunder,\
@commonounder,\
@commonbunder,\
@commonpover,\
@commonoover,\
@commonbover

unset multiplot

