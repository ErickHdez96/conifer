SRCDIR = .
CMD = guile -L $(SRCDIR) --r6rs
TEST_FILES = $(wildcard *.spec.scm) $(wildcard ./**/*.spec.scm)

.PHONY: all
all:
	@echo $(TEST_FILES)
	$(CMD) -e main -s $(SRCDIR)/main.scm

.PHONY: testlog
testlog: test
	rg "result-kind: fail" *.log -B5 -A2 --no-config

.PHONY: test
test: $(TEST_FILES)

.PHONY: $(TEST_FILES)
$(TEST_FILES):
	$(CMD) -s $@

.PHONY: clean
clean:
	$(RM) *.log
