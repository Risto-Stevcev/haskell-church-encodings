GHC = ghc

.PHONY:
default: TestChurch

TestChurch:
	$(GHC) TestChurch.hs

.PHONY:
all: clean default

.PHONY:
clean:
	rm -f *.o *.hi *.*~ TestChurch
