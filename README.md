# Modeller

Multibody simulation playground that utilizes Simbody for dynamics. Wrappers for Pybind11, SPICE, and MarsGRAM (with more to come!). Utilizes Docker, CMake, and Miniconda.

## Prerequisites

* Docker (so far only tested with Docker Desktop, WSL2 backend)
* Visual Studio Code (not necessary but makes development a ton easier!)

## Clone and Create Docker Development Image

So far, a "deployment" image has not yet been created, only a "development" image that can be used for active development. The idea is to eventually build up a much leaner "deployment" image that can be used to run more efficient simulations on clusters and other closed systems.

Clone this repository:

```
git clone https://devcentral.nasa.gov/jmeveret/modeller.git
```

Build the docker image:

```
cd ./modeller
docker built -t modeller-dev:1.0 .
```

## Developing and Running the Simulation Suite

My typical workflow involves Visual Studio code (see the `.devcontainer` and `.vscode` directories), but it is also possible to just work in the image directly. For development purposes, if using visual studio code, open this directory as a folder in Code and open up the command pallette (`Ctrl-Shift-P`) and select `Rebuild and Reopen in Container`. This will automatically bind-mount this repository.

The development image also comes with X11 support and small IDE's, so if it's desired, one can also run the image with:

```
docker run -e DISPLAY -ti -p 8080:8080 modeller-dev:1.0 /bin/bash
```

Passing your `DISPLAY` variable is only necessary if you plan on doing any sort of visualization.

