strip .stack-work/dist/x86_64-linux-nix/Cabal-1.24.2.0/build/frame-generator-backend/frame-generator-backend
mkdir -p dist/bin
cp .stack-work/dist/x86_64-linux-nix/Cabal-1.24.2.0/build/frame-generator-backend/frame-generator-backend dist/bin
rm -rf static/tmp
tar czfv frame-generator.keter dist/bin/frame-generator-backend config static
