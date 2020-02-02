all:
	@./build-project.sh ~/projects/external/gambit

gambit:
	@./build-gambit.sh ~/projects/external/gambit

clean:
	@echo "Cleaning WASM and intermediate files..."
	@rm -f app.data app.html app.js app.wasm
	@rm -Rf out/scheme out/scheme-libs out/scheme-stdlibs out/std.{c,bc} out/app_.{c,bc}

clean-gambit:
	@echo "Cleaning Gambit WASM library..."
	@rm -fr out/libgambit.bc include out/gambit
