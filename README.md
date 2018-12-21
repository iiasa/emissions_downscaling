# IAM Emissions Downscaling

[![DOI](https://zenodo.org/badge/81960517.svg)](https://zenodo.org/badge/latestdoi/81960517)

These routines take emissions data from Integrated Assessment Models and do two things:

1. Downscale from native IAM regions to country level
2. Map those country level emissions to a global, spatial grid, and export those into netCDF files in CMIP6 standard format.

The user can specify just downscaling, or downscaling & gridding. See the [project wiki](https://github.com/iiasa/emissions_downscaling/wiki) for further details, including a quick guide to getting started.

#### *November 2018 Release Note*
The current release is operational for downscaling. Gridding requires additional input data files which we are currently posting to Zenodo. Links and instructions will be added to the wiki when this is completed. For further information contact [Steve Smith](https://www.pnnl.gov/contacts/staffinfo.asp?uid=4437DB8911651043BBCB149C0C52AE28).

---
### Example gridded emissions output:

![BC emissions example](/documentation/img/BC-anthro_emissions.png)

---

This software is provided with the following [licence](https://github.com/iiasa/emissions_downscaling/blob/master/LICENSE.txt)

Copyright 2018 Battelle Memorial Institute

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


DISCLAIMER

This material was prepared as an account of work sponsored by an agency of the United States Government.  Neither the United States Government nor the United States Department of Energy, nor Battelle, nor any of their employees, nor any jurisdiction or organization that has cooperated in the development of these materials, makes any warranty, express or implied, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness or any information, apparatus, product, software, or process disclosed, or represents that its use would not infringe privately owned rights.

Reference herein to any specific commercial product, process, or service by trade name, trademark, manufacturer, or otherwise does not necessarily constitute or imply its endorsement, recommendation, or favoring by the United States Government or any agency thereof, or Battelle Memorial Institute. The views and opinions of authors expressed herein do not necessarily state or reflect those of the United States Government or any agency thereof.

PACIFIC NORTHWEST NATIONAL LABORATORY
operated by
BATTELLE
for the
UNITED STATES DEPARTMENT OF ENERGY
under Contract DE-AC05-76RL01830
