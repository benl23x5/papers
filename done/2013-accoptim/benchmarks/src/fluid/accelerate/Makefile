TARGET		= fluid
BUILD		= dist

GHC		= ghc
DEFINES		= ACCELERATE_CUDA_BACKEND
GHCFLAGS	= -Wall -threaded -fllvm -optlo-O3 -O2 $(DEFINES:%=-D%) -odir $(BUILD) -hidir $(BUILD)

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

