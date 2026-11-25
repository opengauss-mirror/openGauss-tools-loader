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

# GnuPG Agent
export GPG_TTY=`tty`
eval "$(gpg-agent --daemon)"

# DEBIAN
export DEBEMAIL="dim@tapoueh.org"
export DEBFULLNAME="Dimitri Fontaine"
export DEBSIGN_KEYID="60B1CB4E"
