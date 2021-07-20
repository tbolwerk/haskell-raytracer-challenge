build:
	cabal clean
	# cabal new-build -O2 --enable-profiling  --enable-executable-profiling
	# cabal new-build -O2 --verbose=3
	cabal new-build -O2

render:
	rm -f *.ppm
	cabal new-build
	# time cabal new-run RayTracer-exe +RTS -p -s -RTS
	time cabal new-run RayTracer-exe +RTS -s -RTS
	open *.ppm

format:
	stylish-haskell -i src/*.hs *.hs test/*.hs app/*.hs
