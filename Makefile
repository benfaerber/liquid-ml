.PHONY: build test clean coverage coverage-html

mli:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make mli FILE=<path/to/your_file.ml>"; \
		exit 1; \
	fi
	echo "Generating mli for $(FILE)" \
  OUTFILE=$${FILE%.ml}.mli; \
	dune exec -- ocaml-print-intf $(FILE) > $(OUTFILE) 


build:
	dune build

test:
	dune runtest

clean:
	dune clean
	rm -rf _coverage

format:
	dune fmt

runbin:
	dune build && dune exec "_build/default/bin/main.exe"

coverage:
	@mkdir -p _coverage
	@export BISECT_FILE=$$(pwd)/_coverage/bisect && \
	export BISECT_ENABLE=yes && \
	dune runtest --instrument-with bisect_ppx --force
	@echo ""
	@echo "Coverage Summary:"
	@echo "================"
	@bisect-ppx-report summary --coverage-path _coverage
	@echo ""
	@bisect-ppx-report html --coverage-path _coverage
	@echo "Coverage report: _coverage/index.html"

coverage-html: coverage
	@xdg-open _coverage/index.html 2>/dev/null || open _coverage/index.html 2>/dev/null || echo "Please open _coverage/index.html in your browser"
