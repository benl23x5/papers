
set title "Intel Lynnfield 4x2 2.8Ghz" 

set key off

set xlabel "threads (on 4 PEs)"
set xrange [0:8]
set xtics  (1,2, 3, 4, 5, 6, 7, 8)

set ylabel "runtime (ms)"
set yrange [0:2000]

set terminal pdf monochrome dashed font "Times-Roman" fsize 12 size 5,3.5

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set label "grey"	at 0.1,284 left
set label "blurX"	at 0.1,400 left
set label "blurY"	at 0.1,600 left
set label "diffX"	at 0.1,750 left
set label "diffY"	at 0.1,900 left
set label "mag" 	at 0.1,1100 left
set label "orient" 	at 0.1,1400 left
set label "supp" 	at 0.1,1800 left


plot 	'desire.ssv' using ($1):($2) with lines, \
	'' using ($1):($2+$3)			with lines lt 1, \
	'' using ($1):($2+$3+$4)		with lines lt 1, \
	'' using ($1):($2+$3+$4+$5)		with lines lt 1, \
	'' using ($1):($2+$3+$4+$5+$6)		with lines lt 1, \
	'' using ($1):($2+$3+$4+$5+$6+$7) 	with lines lt 1, \
	'' using ($1):($2+$3+$4+$5+$6+$7+$8)	with lines lt 1, \
	'' using ($1):($2+$3+$4+$5+$6+$7+$8+$9)	with lines lt 1 \
			