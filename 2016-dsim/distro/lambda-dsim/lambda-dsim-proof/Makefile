
# -- Config ---------------------------------------------------------
# Coq binaries to use when building
COQDEP		= coqdep
COQC		= coqc
THREADS		= 4


# -- Roots ----------------------------------------------------------
root =	src/Lambda/DSIM.vo


# -------------------------------------------------------------------
.PHONY : all
all:
	@$(MAKE) deps
	@$(MAKE) proof -j $(THREADS)


# Build the Coq proofs.
.PHONY: proof
proof: $(root)

# Start the Coq ide
.PHONY: start
start: 
	coqide -R src/Lambda -as Lambda & 

# Build dependencies for Coq proof scripts.
.PHONY: deps
deps: make/proof.deps

# Find Coq proof scripts
src_coq_v \
 =  	$(shell find src   -name "*.v" -follow) \

# Coqc makes a .vo and a .glob from each .v file.
src_coq_vo \
 = $(patsubst %.v,%.vo,$(src_coq_v))
	
make/proof.deps : $(src_coq_v)
	@echo "* Building proof dependencies"
	@$(COQDEP) -R src/Lambda  -as Lambda \
 	           $(src_coq_v) > make/proof.deps
	@cp make/proof.deps make/proof.deps.inc


# Clean up generated files.
.PHONY: clean
clean : 
	@rm -f make/proof.deps
	@rm -f make/proof.deps.inc

	@find .    -name "*.vo" \
		-o -name "*.glob" \
		-follow | xargs -n 1 rm -f

# Rules 
%.vo %.glob : %.v
	@echo "* Checking $<"
	@$(COQC) -R src/Lambda Lambda \
	         $<

# Include the dependencies.
-include make/proof.deps.inc
