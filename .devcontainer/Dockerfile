FROM docker.io/library/debian

VOLUME /nix

RUN apt-get update
RUN apt-get -y install \
  sudo \
  xz-utils \
  curl \
  git \
  direnv

ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"
ENV DIRENV_CONFIG=/etc

# install bash config
COPY bash.bashrc /etc/bash.bashrc

# set env for non interactive shell to load nix
COPY envrc /etc/envrc
ENV ENV="/etc/envrc" BASH_ENV="/etc/envrc"

COPY ./nix.conf /etc/nix/nix.conf
COPY direnv.toml /etc

RUN --mount=type=secret,id=NIX_NETRC cp /run/secrets/NIX_NETRC /netrc && chmod a+r /netrc

# create non-root user and group and add it sudoers
ARG USERNAME=code
ARG USER_UID=1000
ARG USER_GID=${USER_UID}
RUN groupadd --gid ${USER_GID} ${USERNAME} && \
    useradd --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME} -s /bin/bash && \
    echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/devcontainer && \
    chmod 0440 /etc/sudoers.d/devcontainer

ARG NIX_INSTALL_SCRIPT=https://nixos.org/nix/install
RUN curl -L ${NIX_INSTALL_SCRIPT} | sudo -u code NIX_INSTALLER_NO_MODIFY_PROFILE=1 sh
