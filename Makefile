all: frontend-config-and-compile backend-compile

frontend-compile-only:
	cd frontend; ../reflex-platform/work-on ghcjs ./. --command "cabal build"
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/out.js      backend/static/editapp/
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/lib.js      backend/static/editapp/
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/rts.js      backend/static/editapp/
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/runmain.js  backend/static/editapp/

frontend-config-and-compile:
	cd frontend; ../reflex-platform/work-on ghcjs ./. --command "cabal configure --ghcjs; cabal build"
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/out.js      backend/static/editapp/
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/lib.js      backend/static/editapp/
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/rts.js      backend/static/editapp/
	cp frontend/dist/build/frame-generator-frontend/frame-generator-frontend.jsexe/runmain.js  backend/static/editapp/

backend-compile:
	cd backend; stack build
