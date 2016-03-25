set term postscript eps 22
set output 'plot_bday.eps'

set pointsize 2
set clip points
#set log y
set ylabel "max belief"
set xlabel "running time [s]"
set xtics 4
set ytics 0,0.2,1
set style line 1 linetype 3 pointtype 2 linewidth 1 pointsize 1
set style line 2 linetype 2 pointtype 1 linewidth 1 pointsize 2
#set style point 1 pointtype 2 pointsize 1
#set style point 2 pointtype 1 pointsize 2
set yrange [-0.1:1.1]
set xrange [-1:18]

set size 1.0,0.7

set key above

plot \
'data_ps_bday.tsv' using 2:3 axes x1y1 title 'prob-scheme' with points ls 1,\
'data_bday_q0.tsv' using 4:9 axes x1y1 title 'prob-poly-set' with points ls 2

#'res_ps_bday.tsv' using 1:2 notitle with points ls 1,\
#'res_prob_bday.tsv' using 1:2 notitle with points ls 2

set output 'plot_bday_large.eps'
unset key
set size 1.0,0.6
#set log x
set xrange [-1:36]

plot \
'data_ps_bday_large.tsv' using 2:3 axes x1y1 title 'prob-scheme' with points ls 1 ,\
'data_bday_large_q0.tsv' using 4:9  axes x1y1 title 'prob-poly-set' with points ls 2
#'res_ps_bday_large.tsv' using 1:2 notitle with points ls 1,\
#'res_prob_bday_large.tsv' using 1:2 notitle with points ls 2

set yrange [0.00032:0.0016]
set xrange [18:77]
#set ytics 0,0.01,1
#unset yrange
set log y
set output 'plot_bday_seq.eps'
unset key
#plot \
#'res_prob_bday_seq.tsv' using 7:5:3 axes x1y2 with labels left offset -1,1 font "Helvetica,10" , \
#'res_prob_bday_seq.tsv' using 7:5 axes x1y2 with points ls 1

plot \
'data_bday_q2.tsv' using 4:9:1 with labels left offset 0,0 font "Helvetica,14"
