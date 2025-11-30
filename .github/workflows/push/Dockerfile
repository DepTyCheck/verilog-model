FROM ghcr.io/stefan-hoeck/idris2-pack:latest

ENV BUILD_DIR=.build
ENV PACKAGE_NAME=verilog-model

WORKDIR /app

# Copy Idris sources and cached .build
COPY . .
COPY ${BUILD_DIR} ${BUILD_DIR}

# Switch latest to match cached build
RUN pack switch latest

# Touch files trying to force build from cache
RUN find ${BUILD_DIR} -type f -exec touch {} +

# Build
RUN pack build ${PACKAGE_NAME}

# Install
RUN pack install-app ${PACKAGE_NAME}

# Fix filesystem (・‿・)
RUN pack run ${PACKAGE_NAME} --coverage mcov -n 1 --seed 0,1 && rm mcov

CMD ["bash"]
