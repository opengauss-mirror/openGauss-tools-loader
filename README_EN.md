# openGauss-tools-loader

#### Introduction

This tool is from pgloader, whose code comes from the latest release version (v3.6.2:https://github.com/dimitri/pgloader/releases/tag/v3.6.2) on GitHub.

pgloader uses the COPY command to load openGauss data for migration.

Compared with using only COPY or \copy and foreign data wrappers, pgloader's advantage lies in its transactional behavior. It retains a separate file for rejected data while continuing to copy valid data from the database, ensuring data integrity and consistency. Any error in the input data, like a file or remote database, will stop the entire batch loading of the table.

pgloader also reformats data. For example, it converts MySQL timestamps like 00000 000 and 00000 000:00:00 into openGauss NULL values, because a regular calendar never has a year zero.


#### Installation

1. Installation environment: Linux x86, freetds, epel, sbcl 1.2+, sqlite-devel, zlib-devel

   If Yum can be used, run `sh bootstrap-*.sh` to install the dependencies, where \* indicates the OS, such as CentOS, CentOS 7, and Debian.

2. Offline installation: `make pgloader`

3. Online installation: `make -f Makefile_online pgloader`

#### Packaging

1. Environment requirements: In addition to the provided installation environment, the rpmdevel dependency is required.
2. Offline packaging: `make rpm`
3. Online packaging: `make -f Makefile_online rpm`

#### Instruction

1.  For details, see https://mp.weixin.qq.com/s/2agfQnL-a7lxZhQNPWx7WA.

#### Contributions

1.  Fork this repository.
2.  Create a `Feat_*xxx*` branch.
3.  Commit code.
4.  Create a pull request (PR).
