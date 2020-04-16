# Test database schema for validity
psql -d postgres -c "drop database Shelley" ; psql -d postgres -c "create database Shelley" ; psql -d shelley < shelley-schema-v3.sql
