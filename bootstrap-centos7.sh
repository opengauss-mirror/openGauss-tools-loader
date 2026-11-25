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

# Enable epel for sbcl
sudo yum -y install epel-release
sudo yum -y install sbcl 

# Missing dependency
sudo yum install freetds freetds-devel -y
sudo ln -s /usr/lib64/libsybdb.so.5 /usr/lib64/libsybdb.so

# prepare the rpmbuild setup
rpmdev-setuptree
