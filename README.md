Frame Generator

Create a circular frame from a pattern and use this frame to create a nice
framed image.

Features
1. Create a circular frame from a user specified pattern image
  The use can edit the appearance of this frame by altering these parameters
  
    a. Number of patterns
  
    b. Scaling of patterns.
  
    c. Rotation

2. Once the frame is ready the user can put this frame around an image.
  The image will be nicely cut around the edges so as to fit the frame.
  User can edit the cut related parameters by modifying the mask used for
  cutting. 
  
    The following parameters can be used modified for mask
  
    a. Dilate - This alters the spacing between frame and image.
  
    b. Blur - This adds a blurred transparency at the cut to make it smooth.

Usage
1. Upload the pattern
2. Edit the frame
3. Upload the back ground image
4. Select a frame to create a framed image.

Installation from source

1. Backend
Currently this has a dependency on a modified diagrams-lib. With new version of
diagrams-lib (>1.4), this should be buildable from stack repo package.
Assuming this dependency is taken care of do...
```
cd backend; stack build;
```

2. Frontend
First create the nix-shell
```
git clone https://github.com/reflex-frp/reflex-platform.git;
./reflex-platform/workon ghcjs ./frontend
```

Then do this inside the nix-shell
```
cd frontend; cabal configure --ghcjs && cabal build;
```
