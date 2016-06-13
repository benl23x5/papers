

# Compile Repa test with GHC, dumping the intermediate core program
# just before conversion to STG code.
all :
	@touch Test.hs
	@ghc -Odph \
	  -funfolding-use-threshold1000 -funfolding-keeness-factor1000 \
	  -fno-liberate-case \
	  --make Test.hs \
	  -dsuppress-all -dppr-case-as-let -ddump-prep  > dump.prep

clean :
	rm *.o *.hi dump.prep
