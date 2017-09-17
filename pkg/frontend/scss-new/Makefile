default: generate-main once loop

.phony:

once: .phony
	sass main.scss main.css

loop: .phony
	sass --watch main.scss:main.css

generate-main: .phony
	for i in `ls *-*.scss`; do echo "@import '$$i';"; done > main.scss

generate-styleguide: .phony
	@echo "\n\nNOTE: if you change flags, you will have to rebuild the entire frontend library.  this is only worth it if you work on styling for a few iterations.\n\n"
	stack test --fast --flag refine-frontend:-build-refine-frontend --flag refine-frontend:-build-spec --flag refine-frontend:-build-bench

clean: .phony
	rm -f *~ */**~
