@echo off
net stop msmysql
c:\ms\bin\mysqld-nt.exe --remove msmysql
