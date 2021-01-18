#include <Modeller/output.hpp>



Modeller::Output::DataSet * Modeller::Output::DataBase::getDataSetByName(std::string name_in)
{
    DataSet * ds_out;

    for(DataSet * ds : this->datasets)
    {
        if(ds->getName() == name_in)
        {
            ds_out = ds;
            break;
        }    
    }

    return ds_out;
}