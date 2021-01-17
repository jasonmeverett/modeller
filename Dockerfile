# ------------------------------------------------------------------------------------
#
#
#         ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
#         ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
#         ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
#         ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
#         ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
#         ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝
#
# 
#                                        v0.1
#
# This image is to be used for development purposes.
#
# ------------------------------------------------------------------------------------


# -------------------------------------------------------
#                    Base image and environment variables
# -------------------------------------------------------

FROM ubuntu:20.04

EXPOSE 8080
ENV DEBIAN_FRONTEND=noninteractive 
ENV SPICE_HOME=/opt/SPICE/cspice
ENV SPICE_INCDIR=/opt/SPICE/cspice/include
ENV SPICE_LIBDIR=/opt/SPICE/cspice/lib 
ENV LIBGL_ALWAYS_INDIRECT=0

# -------------------------------------------------------
#                                   Baseline dependencies
# -------------------------------------------------------


RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
        csh \
        wget \
        build-essential \
        gdb \
        cmake \
        git \
        gfortran \
        x11-apps \
        nedit \
        vim \
        gedit \
        liblapack-dev \
        freeglut3-dev \
        libxi-dev \
        libxmu-dev \
        doxygen \
        graphviz 


# -------------------------------------------------------
#                                            Simbody code
# -------------------------------------------------------

RUN git clone https://github.com/simbody/simbody.git /simbody-source && \  
    cd /simbody-source && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo .. && \
    make doxygen && \
    make -j && \
    make -j install && \
    ldconfig -v
    

# -------------------------------------------------------
#                         Miniconda && python environment
# -------------------------------------------------------

ADD ./modeller.yml /modeller/

RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    bash ./Miniconda3-latest-Linux-x86_64.sh -p /miniconda3 -b && \
    /miniconda3/bin/conda init && \
    cd /modeller && \
    /miniconda3/bin/conda env create -f ./modeller.yml && \
    echo "conda activate modeller" >> /root/.bashrc


# -------------------------------------------------------
#                                              NAIF SPICE
# -------------------------------------------------------

RUN wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/cspice.tar.Z -P /opt/SPICE && \
    wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/importCSpice.csh -P /opt/SPICE && \
    cd /opt/SPICE && \
    /bin/csh ./importCSpice.csh && \
    ldconfig -v

# -------------------------------------------------------
#                   Other X11 APIs for Simbody Visualizer
# -------------------------------------------------------

RUN apt-get install -y \
        mesa-common-dev \
        libgl1-mesa-dev \
        libglu1-mesa-dev \
        libxt-dev \
        mesa-utils \
        firefox \
        language-pack-en-base && \
    locale -a


# -------------------------------------------------------
#                                    Documentation Server
# -------------------------------------------------------

# RUN apt-get install npm && \
#     npm install --global http-server && \
#     echo "cd /usr/local/share/doc/simbody/api && nohup http-server . & && cd /" >> /root/.bashrc 