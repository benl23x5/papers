TARGET		= bin/fluid
BUILD		= dist

GHC		= ghc
GHCFLAGS	= -Wall -threaded -fllvm -optlo-O3 -Odph -rtsopts \
		  -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 \
		  -odir $(BUILD) -hidir $(BUILD)

.PHONY: $(TARGET)
$(TARGET): Main.hs
	@mkdir -p $(BUILD)
	@mkdir -p $(dir $(TARGET))
	$(GHC) $(GHCFLAGS) $< -o $@

.PHONY: clean
clean:
	-rm -f $(wildcard $(BUILD)/*.o)
	-rm -f $(wildcard $(BUILD)/*.hi)
	-rm -f $(TARGET)
	-rmdir $(BUILD)

