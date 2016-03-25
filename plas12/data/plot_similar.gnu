set size 1.0,1.0
set term postscript eps enhanced "Helvetica" 20
set key above

set pointsize 3
set clip points
set ylabel "probability"
unset xlabel
set boxwidth 0.6
set log y 2

set format y "2^{%L}"

#set ytics 

set xrange [0:7]
set yrange [0.007:1.5]
#set yrange [-0.1:1.1]
set output 'beliefs_similar.eps'

set style line 1 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "gray"
set style line 2 lt 1 lw 2 pt 7 ps 2 lc rgbcolor "black"
set style line 3 lt 1 lw 2 pt 1 ps 5 lc rgbcolor "black"

xcoord(y,i) = (y eq "P_2-P_1") ? 1 + i : \
  	      (y eq "P_3-P_1") ? 2 + i : \
	      (y eq "P_1-P_2") ? 3 + i : \
	      (y eq "P_3-P_2") ? 4 + i : \
	      (y eq "P_1-P_3") ? 5 + i : \
	      (y eq "P_2-P_3") ? 6 + i : 7

xlabel(y,i) = (y eq "P_2-P_1") ? "{/Symbol d}^2_1" : \
	      (y eq "P_3-P_1") ? "{/Symbol d}^3_1" : \
	      (y eq "P_1-P_2") ? "{/Symbol d}^1_2" : \
	      (y eq "P_3-P_2") ? "{/Symbol d}^3_2" : \
	      (y eq "P_1-P_3") ? "{/Symbol d}^1_3" : \
	      (y eq "P_2-P_3") ? "{/Symbol d}^2_3" : ""

tlabel(t) = (t eq "") ? "{/Symbol d}^2_1" : \
	    (t eq "P_3-P_1") ? "{/Symbol d}^3_1" : \
	    (t eq "P_1-P_2") ? "{/Symbol d}^1_2" : \
	    (t eq "P_3-P_2") ? "{/Symbol d}^3_2" : \
	    (t eq "P_1-P_3") ? "{/Symbol d}^1_3" : \
	    (t eq "P_2-P_3") ? "{/Symbol d}^2_3" : ""

#plot \
#'data_similar2.tsv' every ::1 using (xcoord(strcol(1), 0)+0.0):6:xticlabels(xlabel(strcol(1))) with points ls 1 title "max belief", \
#'data_stats_similar2.tsv' every ::1 using (xcoord(strcol(1),0)-0.0):4 title "median" with points ls 2, \
#'data_stats_similar2.tsv' every ::1 using (xcoord(strcol(1),0)-0.0):6:8:9:7 with candlesticks ls 3 title "quartiles" whiskerbars, \
#'data_similar10.tsv' every ::1 using (xcoord(strcol(1),0)+0.0):6:xticlabels(xlabel(strcol(1))) with points ls 1 notitle, \
#'data_stats_similar10.tsv' every ::1 using (xcoord(strcol(1),0)-0.0):4 with points ls 2 notitle, \
#'data_stats_similar10.tsv' every ::1 using (xcoord(strcol(1),0)-0.0):6:8:9:7 with candlesticks ls 3 notitle whiskerbars 

plot \
'data_similar0.tsv' every ::1 using (1):6:xticlabels("w=0") with points ls 1 title "belief", \
'data_stats_similar0.tsv' every ::1 using (1):4 title "median" with points ls 2, \
'data_stats_similar0.tsv' every ::1 using (1):6:8:9:7 with candlesticks ls 3 title "quartiles" whiskerbars, \
'data_similar1.tsv' every ::1 using (2):6:xticlabels("w=1") with points ls 1 notitle, \
'data_stats_similar1.tsv' every ::1 using (2):4 with points ls 2 notitle, \
'data_stats_similar1.tsv' every ::1 using (2):6:8:9:7 with candlesticks ls 3 notitle whiskerbars, \
'data_similar2.tsv' every ::1 using (3):6:xticlabels("w=2") with points ls 1 notitle, \
'data_stats_similar2.tsv' every ::1 using (3):4 with points ls 2 notitle, \
'data_stats_similar2.tsv' every ::1 using (3):6:8:9:7 with candlesticks ls 3 notitle whiskerbars, \
'data_similar4.tsv' every ::1 using (4):6:xticlabels("w=4") with points ls 1 notitle, \
'data_stats_similar4.tsv' every ::1 using (4):4 with points ls 2 notitle, \
'data_stats_similar4.tsv' every ::1 using (4):6:8:9:7 with candlesticks ls 3 notitle whiskerbars, \
'data_similar8.tsv' every ::1 using (5):6:xticlabels("w=8") with points ls 1 notitle, \
'data_stats_similar8.tsv' every ::1 using (5):4 with points ls 2 notitle, \
'data_stats_similar8.tsv' every ::1 using (5):6:8:9:7 with candlesticks ls 3 notitle whiskerbars, \
'data_similar16.tsv' every ::1 using (6):6:xticlabels("w=16") with points ls 1 notitle, \
'data_stats_similar16.tsv' every ::1 using (6):4 with points ls 2 notitle, \
'data_stats_similar16.tsv' every ::1 using (6):6:8:9:7 with candlesticks ls 3 notitle whiskerbars
