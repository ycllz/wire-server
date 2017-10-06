# Requires docker >= 17.05 (requires support for multi-stage builds)
# Requires to have created the wire-server-builder and wire-server-deps docker images (run `make` in this directory)
# Usage example:
#   (from wire-server root directory)
#   SERVICE=galley; docker build -f build/alpine/Dockerfile -t $SERVICE --build-arg service=$SERVICE .

#--- Builder stage ---
FROM wire-server-builder:alpine as builder

ARG service
COPY . /src/wire-server/

RUN cd /src/wire-server/services/${service} && make install

RUN apk add --no-cache \
        python \
        py-pip && \
    pip install jinja2-cli[yaml]

# ARGs are not available at runtime, use a j2 template to write an entrypoint specific to this service.
# As the bash script uses exec, we can see which service is run as PID 1
# more info: https://stackoverflow.com/questions/40902445/using-variable-interpolation-in-string-in-docker
RUN jinja2 -D service=${service} /src/wire-server/build/alpine/docker-entrypoint.sh.j2 > /src/docker-entrypoint.sh

#--- Minified stage ---
FROM wire-server-deps:alpine

ARG service
COPY --from=builder /src/wire-server/services/${service}/dist/* /usr/bin/

COPY --from=builder /src/wire-server/services/${service}/${service}.yaml /etc/wire/
# TODO: only some projects do schema migration
COPY --from=builder /src/wire-server/services/${service}/${service}-schema.yaml /etc/wire/

# TODO: hack to get the brig templates copied
COPY --from=builder /src/wire-server/services/brig/deb/opt/brig/templates/ /usr/share/wire/templates/

# TODO: move integration conf to common location
COPY --from=builder /src/wire-server/services/${service}/integration.yaml /etc/wire/

COPY --from=builder /src/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh

ENTRYPOINT ["/docker-entrypoint.sh"]
