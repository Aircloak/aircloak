@/u01/app/oracle/product/11.2.0/xe/rdbms/admin/dbmsstdx.sql
CREATE USER compliance_safe IDENTIFIED BY oracle;
GRANT CREATE TABLE TO compliance_safe;
ALTER SESSION SET CURRENT_SCHEMA = compliance_safe;
@/mnt/cloak/oracle_udfs.sql
