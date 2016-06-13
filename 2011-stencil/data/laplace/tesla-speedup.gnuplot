
set terminal epslatex size 3.5,1.5
set output "data/laplace/tesla-speedup.tex"

# Fix the graph box size
set lmargin at screen 0.12
set bmargin at screen 0.1
set rmargin at screen 0.9
set tmargin at screen 0.9

set key off
set xlabel "threads (on 8 PEs)"
set xrange [0.9:8.5]
set xtics  (1,2, 3, 4, 5, 6, 7, 8)

set ylabel "speedup"
set yrange [0.85:9]
set ytics  (1, 2, 4, 8)


set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set logscale x
set logscale y

plot 	'data/laplace/tesla.ssv' \
			using ($1):(4038/$2) 	title "Safe Get"		with lines lt 4, \
	'' 		using ($1):(2986/$3)	title "Unsafe Get"		with lines lt 3, \
	'' 		using ($1):(1519/$4)	title "Unsafe Unrolled Get"	with lines lt 2, \
	'' 		using ($1):(769/$5)	title "Safe Unrolled Stencil"	with lines lt 1, \
	''		using ($1):(394/$6) 	title "Hand-written C with GCC 4.4.3" with lines lt 4
