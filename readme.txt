# compile on linux systems (I did not try others)
# run
rm -f matinv.o matinv.so
R CMD SHLIB matinv.f90 -llapack -lblas
nm -D matinv.so | grep -i solvef90     # must show:  solvef90
