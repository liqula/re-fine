default: generate-main loop

.phony:

loop: .phony
	sass --watch main.scss:main.css

generate-main: .phony
	for i in `ls *-*.scss`; do echo "@import '$$i';"; done > main.scss

clean: .phony
	rm -f *~ */**~
