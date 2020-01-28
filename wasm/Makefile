all:
	@./compile-all.sh ~/projects/external/gambit

clean:
	@echo "Cleaning WASM and intermediate files..."
	@rm -f app.data app.html app.js app.wasm
	@rm -Rf out/scheme out/scheme-libs out/scheme-stdlibs out/std.{c,bc} out/app_.{c,bc}

clean-libgambit:
	@echo "Cleaning Gambit WASM library..."
	@rm -f out/libgambit.bc
