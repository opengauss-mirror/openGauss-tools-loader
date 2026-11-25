#!/usr/bin/env bash

# Copyright (c) 2005-2017, The PostgreSQL Global Development Group
# Copyright (c) 2026 Huawei Technologies Co., Ltd.
# All rights reserved.
#
# Licensed under the The PostgreSQL License (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# https://opensource.org/license/postgresql
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

sudo yum -y install yum-utils rpmdevtools @"Development Tools" \
                    sqlite-devel zlib-devel

# SBCL 1.3, we'll overwrite the repo version of sbcl with a more recent one
sudo yum -y install epel-release
sudo yum install -y sbcl.x86_64 --enablerepo=epel

wget http://downloads.sourceforge.net/project/sbcl/sbcl/1.3.6/sbcl-1.3.6-source.tar.bz2
tar xfj sbcl-1.3.6-source.tar.bz2
cd sbcl-1.3.6
./make.sh --with-sb-thread --with-sb-core-compression --prefix=/usr > /dev/null 2>&1
sudo sh install.sh
cd

# Missing dependencies
sudo yum -y install freetds-devel

# prepare the rpmbuild setup
rpmdev-setuptree

# pgloader
#make -C /vagrant rpm
