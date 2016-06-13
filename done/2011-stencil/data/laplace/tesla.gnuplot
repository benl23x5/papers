
set title "Laplace on 2xQuad Core 2.0GHz Intel Harpertown" 

set terminal epslatex size 3.5,2.5
set output "data/laplace/tesla.tex"

# Fix the graph box size
set lmargin at screen 0.12
set bmargin at screen 0.1
set rmargin at screen 0.9
set tmargin at screen 0.9

set key off
set noxlabel
set xrange [0.9:8.5]
set xtics  (1,2, 3, 4, 5, 6, 7, 8) format ""

set ylabel "runtime (ms)"
set yrange [300:7000]
set ytics  	( "100"   100, "200"   200, ""      300,  "400"  400,      ""   500 \
		, ""      600, ""      700, "800"   800,  ""     900,  "1000"  1000 \
  		, "1000" 1000, "2000" 2000, ""     3000, "4000" 4000,      ""  5000 \
		, ""     6000, ""     7000, "8000" 8000, ""     9000, "10000" 10000)

set logscale x
set logscale y

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set label "Safe Get"   				at 1,4500 rotate by -27 left
set label "Unsafe Get" 				at 1,3300 rotate by -26 left 
set label "Unsafe Unrolled Get"			at 1,1680 rotate by -16 left 
set label "Safe Unrolled Stencil"		at 1,850  rotate by -6 left 
set label "Handwritten C with GCC 4.4.3"	at 1,430

plot 	'data/laplace/tesla.ssv' 	using ($1):($2) title "Safe Get"		with lines lt 4, \
	'' 		using ($1):($3)	title "Unsafe Get"		with lines lt 3, \
	'' 		using ($1):($4)	title "Unsafe Unrolled Get"	with lines lt 2, \
	'' 		using ($1):($5)	title "Safe Unrolled Stencil"	with lines lt 1, \
	''		using ($1):($6) title "Hand-written C with GCC 4.4.3" with lines lt 4