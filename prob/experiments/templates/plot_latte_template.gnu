set size 1.0,1.0
set term TERM
set key above

set pointsize 2
set clip points
set ylabel "time [s]"
set xlabel "num constraints"
set boxwidth 0.6
set log y

set output 'FILE_OUT'

plot \
'FILE_IN1' every ::1 using COL_CONSS:COL_TIME with points ps 1 title "samples", \
'FILE_IN2' every ::1 using COL2_CONSS:COL2_QLOWER:COL2_MIN:COL2_MAX:COL2_QUPPER with candlesticks lw 1 title "quartiles", \
'FILE_IN2' every ::1 using COL2_CONSS:COL2_MEDIAN with points lw 2 title "median"
