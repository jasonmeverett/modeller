#include <Modeller/SpiceInterface.hpp>


/**
 * @brief Constructor
 * 
 */
Modeller::Spice::SpiceEngine::SpiceEngine() {}

/**
 * @brief Load in a kernel file.
 * 
 * @param path_to_kernel_file 
 */
void Modeller::Spice::SpiceEngine::loadKernelFile(std::string kernelLoadFile)
{
    this->m_kernelLoadFile = kernelLoadFile;
    ConstSpiceChar * loadFile = this->m_kernelLoadFile.c_str();
    furnsh_c(loadFile);
    this->kernelFileLoaded = true;
}

/**
 * @brief Set the planetary body corresponding to "Ground" body for a simulation.
 * It's definitely important to switch this at some point - how does simbody
 * handle localizing errors?
 * 
 * @param groundBody 
 */
void Modeller::Spice::SpiceEngine::setGroundBody(std::string groundBody){
    this->groundBody = groundBody;
    this->groundBodySet = true;
}

/**
 * @brief Set the ground frame of the simulation. Same as above - let's make
 * this interchangeable.
 * 
 * @param groundFrame 
 */
void Modeller::Spice::SpiceEngine::setGroundFrame(std::string groundFrame){
    this->groundFrame = groundFrame;
    this->groundFrameSet = true;
}

/**
 * @brief Set the "zero" Epoch for the SPICE engine. We track in ET.
 * 
 * @param etstr 
 */
void Modeller::Spice::SpiceEngine::setStateZeroEpoch(std::string etstr){
    str2et_c(etstr.c_str(), &this->ET0);
    this->zeroEpochSet = true;
}

