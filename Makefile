all:
	cabal exec ghc fft
	rec -c1 -r8000 -c 1 -b 8 -t raw -e signed - 2>/dev/null | \
		./fft | stdbuf -o0 uniq | ./sin

init:
	# haven't tested this
	# cabal sandbox

packages:
	# haven't tested this
	# cabal install split fft
