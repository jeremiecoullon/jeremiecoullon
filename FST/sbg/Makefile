all: sbr.f

sbr.f: sbg.out sbg.beg
	wrun sbg

sbg.out:  sbg.f flx.f 
	f77 -O3 -o sbg.out -C -w sbg.f flx.f -lnag

clean:
	rm -f *.out
