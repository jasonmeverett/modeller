# Modeller

Multibody sim playground w/ Simbody, Pybind, and SPICE.

## Prerequisites

Miniconda needs to be installed because of required access to newer GNU build versions. Configure a local working version of miniconda as follows:

```
cd ~
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh
```

Move throught the installation process by either hitting `Enter` or `yes` to all options. By default, miniconda3 will install at `/home/<user>` if using default options. Ensure that this new `conda` is being referenced properly:

```
[jmeveret@msgncc3 modeller]$ which conda
~/miniconda3/bin/conda
```

## Clone and Create Environment

```
git clone https://devcentral.nasa.gov/jmeveret/modeller.git
cd modeller/
conda env create -f ./modeller.yml
```

Note: the last command above may take several minutes!

## Build

```
conda activate modeller
mkdir build
cd build/
cmake ..
cmake --build . -j
```
