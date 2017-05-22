# this is a collection of poorly documented make goals that serve
# their purpose under very specific (and also poorly documented)
# circumstances.  copy to ../ to run.  ask if you want more details!
# ~fisx


.phony:

sensei.%: .phony
	cd pkg/$* && stack exec -- sensei -Wall -i../prelude/src -i../prelude/test -i../common/src -i../common/test -i./src -i./test ./test/Spec.hs $(SENSEI_ARGS)

sensei.p: .phony
	make sensei.prelude

sensei.c: .phony
	make sensei.common

sensei.b: .phony
	./build build-backend || true
	make sensei.backend

seito.p: .phony
	make -C pkg/prelude seito 2>&1 | perl -ne '/(.{0,300})/ && print "$$1\n"'

seito.c: .phony
	make -C pkg/common seito 2>&1 | perl -ne '/^(.{0,300})/ && print "$$1\n"'

seito.b: .phony
	make -C pkg/backend seito 2>&1 | perl -ne '/^(.{0,300})/ && print "$$1\n"'

seito.f: .phony
	make -C pkg/frontend seito 2>&1 | perl -ne '/^(.{0,300})/ && print "$$1\n"'

grepi.%: .phony
	git grep -Hni $*

grep.%: .phony
	git grep -Hn $*

tags: .phony
	hasktags -b pkg

test: .phony
	./build

hlint: .phony
	./build hlint

sensei.f: .phony
#	cd pkg/frontend && stack --stack-yaml=stack-no-js.yaml test --fast
	cd pkg/frontend && stack --stack-yaml=stack-no-js.yaml exec -- sensei -isrc -itest -i../common/src -i../prelude/src test/Main.hs --match=###

test.f: .phony
	( cd pkg/frontend && time stack test --fast --flag refine-frontend:-ghc-warn-error --test-arguments "$(SENSEI_ARGS)" ) || ( make noise-nay; false )
	make noise-yeay

debug.f:
	@echo
	@echo
	@echo 'use `debugger;` in your js code to stop execution.'
	@echo 'see also: https://medium.com/@paul_irish/debugging-node-js-nightlies-with-chrome-devtools-7c4a1b95ae27'
	@echo
	@echo
	( cd pkg/frontend && stack test --fast --flag refine-frontend:-ghc-warn-error --test-arguments "--match=#####" ) || true
	( cd pkg/frontend && node --inspect --debug-brk `stack path --dist-dir`/build/spec/spec.jsexe/all.js --match=### )

mk.f: .phony
	( cd pkg/frontend && time stack build --fast --flag refine-frontend:-ghc-warn-error --no-test && make ) || ( make noise-nay; false )
	make noise-yeay

noise-yeay: .phony
	mplayer /mnt/X/home/mf/backup/attic/home1/docs/snd/sounds_wavs/shiph.ogg

noise-nay: .phony
	mplayer /mnt/X/home/mf/backup/attic/home1/docs/snd/sounds_wavs/winny.ogg

clean: .phony
	find . -name '*~' -exec rm -f {} \;
