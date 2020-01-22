connect system/oracle@ORCLPDB1
@/u01/app/oracle/product/12.2.0/dbhome_1/rdbms/admin/dbmsstdx.sql
ALTER SESSION SET CURRENT_SCHEMA = system;
@/mnt/cloak/oracle_udfs.sql
