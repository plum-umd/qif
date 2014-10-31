set size 1.0,1.0
set term TERM
set key above

set pointsize 3
set clip points
set ylabel "probability"
#set xlabel "X checking policy about X's maxblief (X-Y)"
set boxwidth 0.6
set log y 2

set format y "2^{%L}"

#set ytics 

set xrange [0:7]
set yrange [0.1:1.5]
#set yrange [-0.1:1.1]
set output 'GRAPH_OUT'

set style line 1 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "gray"
set style line 2 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "black"
set style line 3 lt 1 lw 2 pt 1 ps 5 lc rgbcolor "black"

xcoord(y) = (y eq "P_2-P_1") ? 1 : \
	    (y eq "P_3-P_1") ? 2 : \
	    (y eq "P_1-P_2") ? 3 : \
	    (y eq "P_3-P_2") ? 4 : \
	    (y eq "P_1-P_3") ? 5 : \
	    (y eq "P_2-P_3") ? 6 : 7

xlabel(y) = (y eq "P_2-P_1") ? "{/Symbol d}_1^{x_1}" : \
	    (y eq "P_3-P_1") ? "{/Symbol d}_1^{x_3}" : \
	    (y eq "P_1-P_2") ? "{/Symbol d}_2^{x_1}" : \
	    (y eq "P_3-P_2") ? "{/Symbol d}_2^{x_3}" : \
	    (y eq "P_1-P_3") ? "{/Symbol d}_3^{x_1}" : \
	    (y eq "P_2-P_3") ? "{/Symbol d}_3^{x_2}" : ""

plot \
'FILE_IN' every ::1 using (xcoord(strcol(COL_POLICY))+0.0):COL_BELIEF:xticlabels(xlabel(strcol(COL_POLICY))) with points ls 1 title "belief", \
'FILE_IN_STATS' every ::1 using (xcoord(strcol(COL_STATS_POLICY))-0.0):COL_STATS_BELIEF_MEDIAN title "median" with points ls 2, \
'FILE_IN_STATS' every ::1 using (xcoord(strcol(COL_STATS_POLICY))-0.0):COL_STATS_BELIEF_QLOWER:COL_STATS_BELIEF_MIN:COL_STATS_BELIEF_MAX:COL_STATS_BELIEF_QUPPER with candlesticks ls 3 title "quartiles" whiskerbars
