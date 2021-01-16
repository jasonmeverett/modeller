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

ENV DEBIAN_FRONTEND=noninteractive 
ENV SPICE_HOME=/opt/SPICE/cspice
ENV SPICE_INCDIR=/opt/SPICE/cspice/include
ENV SPICE_LIBDIR=/opt/SPICE/cspice/lib 


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
        gfortran 


# -------------------------------------------------------
#                         Miniconda && python environment
# -------------------------------------------------------

RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    bash ./Miniconda3-latest-Linux-x86_64.sh -p /miniconda3 -b && \
    /miniconda3/bin/conda init && \
    . /root/.bashrc

ADD ./modeller.yml /modeller/

RUN cd /modeller && \
    /miniconda3/bin/conda env create -f ./modeller.yml


RUN echo "conda activate modeller" >> /root/.bashrc

# -------------------------------------------------------
#                                              NAIF SPICE
# -------------------------------------------------------


# RUN wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/cspice.tar.Z -P /opt/SPICE && \
#     wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/importCSpice.csh -P /opt/SPICE && \
#     cd /opt/SPICE && \
#     /bin/csh ./importCSpice.csh && \
#     cd / 
