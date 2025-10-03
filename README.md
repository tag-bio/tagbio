# Tagbio R SDK

This repository holds the R SDK and is consumed by tagbio-fc-jars.


How to create an update:
1. `git tag v1.1.##` your commit by incrementing the semantic version of the `tagbio` R package
2. In `tagbio/DESCRIPTION`: Update the `Version` string with the associated git tag
3. Create a new gzipped tar archive of the `tagbio/` package.
  From project root: `R CMD build tagbio`
4. Update the symbolic link of `tagbio_latest.tgz` to point to your new archive.
  `ln -sfn tagbio_VERSION_NUMBER.tar.gz tagbio_latest.tgz`
5. Want to test your new version? From `test/`:
  `./test-in-docker.sh`
  The result will log the version number to the console.
6. Want to run the image you just baked?
  `docker run -it --rm -p 8888:8888 tagbior:latest`