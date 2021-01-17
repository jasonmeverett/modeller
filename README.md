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

## Development


