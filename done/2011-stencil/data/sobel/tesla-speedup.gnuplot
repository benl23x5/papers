
set key off
set xlabel "threads (on 8 PEs)"
set xrange [0.5:8.5]
set xtics  (1,2, 3, 4, 5, 6, 7, 8)

set ylabel "speedup"
set yrange [0.5:4.5]
set ytics  (1, 2, 3, 4)

set terminal pdf monochrome dashed font "Times-Roman" fsize 12 size 4.75,2.5

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set label "1:1" at 4.3,4.1
set label "1024x1024 image" at 5,3.2
set label " 768x768  image" at 5,2.3
set label " 512x512  image" at 5,1.5


plot 	'tesla.ssv' 	using ($1):(303/$2) 	title ""	with lines lt 1, \
	'' 		using ($1):(647/$3)	title ""	with lines lt 1, \
	'' 		using ($1):(1223/$4)	title ""	with lines lt 1, \
	''		using ($1):($1)		title ""	with lines lt 4
