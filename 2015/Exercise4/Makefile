
SDIR = secrets

GHCOPT = -package-db=secrets/ghc

default: tests

# Set up the GHC package database

tests: check-up-to-date ghc
	@echo "  I will now run your solution with some inputs, so you get an"
	@echo "  indication of the mark you might get."
	@echo ""
	runhaskell $(GHCOPT) TestBench


check-up-to-date:
	@grep -q "fromMove'" GameExercise.hs || ( \
		\
		echo; \
		echo "* Error: Since 3 December, you can choose to implement either fromMove or fromMove'. "; \
		echo; \
		echo "  Please add these two lines to your GameExercise.hs:"; \
		echo; \
		echo "      fromMove' :: Board -> Move -> (Int, Int, Player)"; \
		echo "      fromMove' b = fromMove"; \
		echo; \
		echo "  (but remove the spaces in front of the lines). Then you"; \
		echo "  can choose to implement either fromMove or fromMove'."; \
		echo; \
		echo "  This error message will then automatically go away."; \
		echo; \
		false )

ghc:
	rm -rf secrets/ghc
	cd secrets && ghc-pkg init ghc
	cd secrets && ghc-pkg update package-students.conf --package-db=ghc

ghci-secrets: ghc
	ghci $(GHCOPT)

.PHONY: default tests check-up-to-date