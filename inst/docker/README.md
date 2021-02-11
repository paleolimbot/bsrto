
# Update and test the Dockerfile

``` bash
# update the dockerfile to the latest set of dependencies
# and the latest GitHub master reference
Rscript inst/docker/update-dockerfile.R

# Rebuild the Docker image
docker build inst/docker --tag paleolimbot/bsrto-builder

# Make sure the Docker image actually works
docker run --rm -it \
  -v //c/Users/dunningtond/Documents/barrow/ftp:/bsrto/cache \
  --env R_BSRTO_FTP_SERVER=ftp://... \
  paleolimbot/bsrto-builder


# Push the image to Docker hub
docker push paleolimbot/bsrto-builder
```
