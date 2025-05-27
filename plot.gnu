set title "Temps d'exécution : Strassen vs Naïf"
set xlabel "Taille de la matrice (n x n)"
set ylabel "Temps (s)"
set grid
set logscale x 2
plot "times.dat" using 1:2 with linespoints title "Strassen", \
     "times.dat" using 1:3 with linespoints title "Naïf"
