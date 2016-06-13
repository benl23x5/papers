
set title "Canny on 2xQuad-core 2.0GHz Intel Harpertown" 

set key on

set xlabel "threads (on 8 PEs)"
set xrange [-0.2:8.5]
set xtics  (1,2, 3, 4, 5, 6, 7, 8)

set ylabel "runtime (ms)"
set yrange [0:2600]

set terminal pdf monochrome dashed font "Times-Roman" fsize 12 size 5,3.5

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set label "supp" 	at 0,2450
set label "orient" 	at 0,2100
set label "mag" 	at 0,1600
set label "diffY"	at 0,1400
set label "diffX"	at 0,1200
set label "blurY"	at 0,1050
set label "blurX"	at 0,700
set label "grey"	at 0,500


plot 	'tesla-breakout.ssv' \
 		using ($1):($2)  			with lines lt 3 title "Culmative time for each stage", \
	'' 	using ($1):($2+$3)			with lines lt 3 title "", \
	'' 	using ($1):($2+$3+$4)			with lines lt 3 title "", \
	'' 	using ($1):($2+$3+$4+$5)		with lines lt 3 title "", \
	'' 	using ($1):($2+$3+$4+$5+$6)		with lines lt 3 title "", \
	'' 	using ($1):($2+$3+$4+$5+$6+$7) 		with lines lt 3 title "", \
	'' 	using ($1):($2+$3+$4+$5+$6+$7+$8)	with lines lt 3 title "", \
	'' 	using ($1):($2+$3+$4+$5+$6+$7+$8+$9)	with lines lt 3 title "", \
	'' 	using ($1):(2500 / $1) 			with lines lw 4 title "Runtime assuming perfect speedup"
		
