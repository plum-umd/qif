set term postscript eps 22
set output 'plot_pres_bday.eps'

set pointsize 2
set clip points
#set log y
set ylabel "precision"
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
'data_ps_bday.tsv' using 2:(1-$3) axes x1y1 title 'prob-scheme' with linespoints ls 1,\
'data_bday_q0.tsv' using 4:(1-$9) axes x1y1 title 'prob-poly-set' with poits ls 2

#'res_ps_bday.tsv' using 1:2 notitle with points ls 1,\
#'res_prob_bday.tsv' using 1:2 notitle with points ls 2

set output 'plot_pres_bday_large.eps'
unset key
set size 1.0,0.6
#set log x
set xrange [-1:36]

plot \
'data_ps_bday_large.tsv' using 2:3 axes x1y1 title 'prob-scheme' with points ls 1 ,\
'data_bday_large_q0.tsv' using 4:9  axes x1y1 title 'prob-poly-set' with points ls 2
#'res_ps_bday_large.tsv' using 1:2 notitle with points ls 1,\
#'res_prob_bday_large.tsv' using 1:2 notitle with points ls 2
