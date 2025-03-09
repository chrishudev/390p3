SCHEME ?= plt-r5rs

# Function to check for and warn about missing expected output
check_expected = (test -f $(1) || \
	(echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"; \
	echo "WARNING: missing expected output file" $(1) ; \
	echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"; \
	false))

# Function to check for expected output and do diff
check_and_diff = ! $(call check_expected,$(1).expect) || \
	(echo diff -q $(1).out $(1).expect ; diff -q $(1).out $(1).expect)

all: done

phase1: env_tests.internal

phase2: internal_phase2_tests.internal \
	$(patsubst %.scm,%.test,$(wildcard phase2*tests.scm))

phase3: internal_phase3_tests.internal \
	$(patsubst %.scm,%.test,$(wildcard phase3*tests.scm))

phase3_%: phase3_%_tests.test
	@: # null command

phase4: $(patsubst %.scm,%.test,$(wildcard phase4*tests.scm))

# Your own test cases that follow the test-*.scm naming pattern
custom: $(patsubst %.scm,%.test,$(wildcard test-*.scm))

%.internal:
	plt-r5rs $*.scm > $*.out
	@$(call check_and_diff,$*)

%.test:
	$(SCHEME) driver.scm < $*.scm | sed "s/Error:.*$$/Error:/g" > $*.out
	@$(call check_and_diff,$*)

done: phase1 phase2 phase3 phase4 custom
	@echo '##### All tests passed! #####'

clean:
	rm -vf *.out
