#!/bin/bash

set -ex

mkdir -p /tagbio-jars

curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_archive_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_azure_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_csv_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_flatfile_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_limited_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_protocol_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_s3_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_server.jar
curl -LO --output-dir /tagbio-jars https://platform-jars.dev.tag.bio/fc_sql_server.jar