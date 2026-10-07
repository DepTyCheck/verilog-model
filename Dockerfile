# syntax=docker/dockerfile:1
FROM ghcr.io/stefan-hoeck/idris2-pack:latest

ENV PACKAGE_NAME=verilog-model

ARG WORK_DIR
ARG PACK_STATE

WORKDIR ${WORK_DIR}

# Copy sources.
COPY . .

# Restore the same compiler, dependencies, configuration and build used by CI.
RUN --mount=type=bind,from=pack-state,target=/pack-state tar -xmf "/pack-state/${PACK_STATE}.tar" -C /

# Install
RUN pack install-app ${PACKAGE_NAME}

# Fix filesystem (・‿・)
RUN pack run ${PACKAGE_NAME} --coverage mcov -n 1 --seed 0,1 && rm mcov

CMD ["bash"]

HEALTHCHECK CMD pack run ${PACKAGE_NAME} -n 1 --seed 0,1 || exit 1
