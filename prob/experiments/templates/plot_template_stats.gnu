set size 1.0,0.6
set term TERM
set key above

set pointsize 2
set clip points
set ylabel "max belief"
set xlabel "running time [s]"
set style line 1 linetype 3 pointtype 2 linewidth 1 pointsize 1
set style line 2 linetype 2 pointtype 1 linewidth 1 pointsize 2

#set yrange [0.0001:0.1]
#set xrange [5:44]

set log y
set output 'GRAPH_OUT'
unset key

#plot \
#'FILE_IN' using COL_TIME:COL_PROB:COL_PREC with labels left offset
#0,0 font "Helvetica,14"

plot \
'FILE_IN' every ::1 using COL_TIME:COL_PROB:COL_PREC with labels left offset 0,0

set output 'GRAPH2_OUT'

set xlabel "precision [# of polyhedra]"
set xrange [2.5:20.5]
#set yrange [0.00032:0.0016]
#unset log

plot \
'FILE_IN' every ::1 using COL_PREC:COL_PROB
