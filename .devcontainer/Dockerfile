
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive 
ENV SPICE_HOME=/opt/SPICE/cspice
ENV SPICE_INCDIR=/opt/SPICE/cspice/include
ENV SPICE_LIBDIR=/opt/SPICE/cspice/lib 

RUN apt-get update && \
    apt-get upgrade --no-install-recommends -y && \
    apt-get install --no-install-recommends -y \
        csh \
        wget \
        build-essential \
        gdb \
        cmake \
        git \
        gfortran 

RUN apt-get install --no-install-recommends -y \
    libsimbody-dev \
    simbody-doc \
    nedit \
    python3.8 \
    python3-dev \
    python3-pip

RUN pip3 install \
    numpy \
    scipy \
    pandas \
    plotly \
    spiceypy 

RUN wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/cspice.tar.Z -P /opt/SPICE && \
    wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/importCSpice.csh -P /opt/SPICE && \
    cd /opt/SPICE && \
    /bin/csh ./importCSpice.csh && \
    cd / 
