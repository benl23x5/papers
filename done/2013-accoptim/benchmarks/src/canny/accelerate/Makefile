TARGET		= acc-canny
BUILD		= dist

GHC		= ghc
GHCFLAGS	= -Wall -threaded -fllvm -optlo-O3 -Odph -fno-liberate-case \
		  -funfolding-use-threshold100 -funfolding-keeness-factor100 \
		  -odir $(BUILD) -hidir $(BUILD)

.PHONY: $(TARGET)
$(TARGET): Main.hs
	@mkdir -p $(BUILD)
	$(GHC) $(GHCFLAGS) $< -o $@

.PHONY: clean
clean:
	-rm -f $(wildcard $(BUILD)/*.o)
	-rm -f $(wildcard $(BUILD)/*.hi)
	-rm -f $(TARGET)
	-rmdir $(BUILD)

