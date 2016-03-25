set size 1.0,1.0
set term postscript eps 22
set key above

set pointsize 2
set clip points
set ylabel "time [s]"
set xlabel "num constraints"
set boxwidth 0.6
set log y

set output 'graph.eps'

plot \
'data_stats.tsv' every ::1 using 1:6:8:9:7 with candlesticks lw 0 title "quartiles", \
'data_stats.tsv' every ::1 using 1:4 with points ls 7 ps 1.5 title "median"

#'data.tsv' every ::1 using 2:4 with points ps 1 title "samples", \
