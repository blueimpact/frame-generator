# Frame Generator

## Create beautiful frames using patterns with this app

Features
1. Interactice editing of Frame foreground

  The user can edit the appearance of this frame by altering these parameters

    a. Number of patterns in a layer

    b. Scaling

    c. Rotation - Rotate individual pattern

    d. Radius

    e. Angle - Adjust the position of pattern in circle

2. Preview Widget

   This can automatically create a set of frame foregrounds from a given template and group of patterns.

   This is useful in exploring all the frames in an instant!

3. ForeGround widget

   This will create the foreground, along with the custom mask which can be used to create the full frame around an image.

For more details have a look at the [wiki](https://github.com/blueimpact/frame-generator/wiki)

## Installation

### Compiling

You need `stack` and `nix` installed on the system. The `Makefile` should be sufficient to build both frontend and backend.
```
> make
```

If you face any problem please report an issue.

### Setting up server

You need `zip` to be installed on the server for packaging the pngs in a zip archive.

```
> cd backend; stack exec frame-generator-backend
```

Open localhost:3000 to start the app.
