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

set size 1.0,0.7

set key above

plot \
'res_ps_bday.csv' using 1:2 axes x1y1 title 'probscheme' with points ls 1,\
'res_prob_bday.csv' using 1:2 axes x1y1 title 'prob' with points ls 2

#'res_ps_bday.csv' using 1:2 notitle with points ls 1,\
#'res_prob_bday.csv' using 1:2 notitle with points ls 2

set output 'plot_bday_large.eps'
unset key
set size 1.0,0.6

plot \
'res_ps_bday_large.csv' using 1:2 axes x1y1 title 'probscheme' with points ls 1 ,\
'res_prob_bday_large.csv' using 1:2  axes x1y1 title 'prob' with points ls 2
#'res_ps_bday_large.csv' using 1:2 notitle with points ls 1,\
#'res_prob_bday_large.csv' using 1:2 notitle with points ls 2

set yrange [0.0001:0.1]
set xrange [5:44]
#unset yrange
set log y
set output 'plot_bday_seq.eps'
unset key
#plot \
#'res_prob_bday_seq.csv' using 7:5:3 axes x1y2 with labels left offset -1,1 font "Helvetica,10" , \
#'res_prob_bday_seq.csv' using 7:5 axes x1y2 with points ls 1

plot \
'res_prob_bday_seq.csv' using 7:5:3 with labels left offset 0,0 font "Helvetica,14"

 
