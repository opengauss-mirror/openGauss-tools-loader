# openGauss-tools-loader

#### 介绍

该工具源自pgloader，代码来自Github最新release版本代码，版本v3.6.2:https://github.com/dimitri/pgloader/releases/tag/v3.6.2

pgloader 是一个使用 COPY 命令实现迁移的openGauss数据加载工具 。

与仅使用COPY或者是\copy以及使用外部数据包装器相比，其主要优势在于其事务行为，即保留一个单独的被拒绝数据文件，但继续尝试复制数据库中的良好数据，以此保证数据的完整性和一致性。 正因为这种事务性行为，输入数据（如文件或远程数据库）中的任何错误行都将导致表的整个批量加载的停止。

除此之外pgloader还实现了数据重新格式化如其能将 MySQL日期戳 00000 000 和00000 000:00:00 转换为 openGauss NULL 值（因为正常的日历从来没有年零）。


#### 安装教程

1. 安装环境：Linux x86，freetds，epel，sbcl 1.2+，sqlite-devel，zlib-devel

   如果可以使用yum，可以通过sh bootstrap-*.sh来安装依赖，其中\*代表的是当前的操作系统，如有centos、centos7以及debian三种。

2. 离线安装：make pgloader

3. 在线安装：make -f Makefile_online pgloader

#### 打包教程

1. 环境要求：除安装教程的安装环境之外，还需要额外的rpmdevel依赖
2. 离线打包：make rpm
3. 在线打包：make -f Makefile_online rpm

#### 使用说明

1.  如该文章所示：https://mp.weixin.qq.com/s/2agfQnL-a7lxZhQNPWx7WA

#### 参与贡献

1.  Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request
