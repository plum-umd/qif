set size 1.0,1.0
set term postscript eps enhanced "Helvetica" 20
set key above

set pointsize 3
set clip points
set ylabel "probability of most probable x_2"
set xlabel "x_1"
set boxwidth 0.6
set log y 2

set format y "2^{%L}"

#set ytics 

set xrange [7:103]
#set yrange [0.007:1.5]
set yrange [0.005:1.5]
set output 'beliefs_run_col1.eps'

set style line 1 lt 1 lw 2 pt 1 ps 2 lc rgbcolor "black"
set style line 2 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "black"
set style line 3 lt 1 lw 2 pt 1 ps 5 lc rgbcolor "black"

xcoord(y) = (y eq "P_2-P_1") ? 1 : \
	    (y eq "P_3-P_1") ? 2 : \
	    (y eq "P_1-P_2") ? 3 : \
	    (y eq "P_3-P_2") ? 4 : \
	    (y eq "P_1-P_3") ? 5 : \
	    (y eq "P_2-P_3") ? 6 : 7

xlabel(y) = (y eq "P_2-P_1") ? "{/Symbol d}_1^{x_2}" : \
	    (y eq "P_3-P_1") ? "{/Symbol d}_1^{x_3}" : \
	    (y eq "P_1-P_2") ? "{/Symbol d}_2^{x_1}" : \
	    (y eq "P_3-P_2") ? "{/Symbol d}_2^{x_3}" : \
	    (y eq "P_1-P_3") ? "{/Symbol d}_3^{x_1}" : \
	    (y eq "P_2-P_3") ? "{/Symbol d}_3^{x_2}" : ""

plot \
'data_run_col1.tsv' every ::1 using 4:6 with points ls 1 notitle
