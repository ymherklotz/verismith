FROM nixos/nix

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

RUN nix-env -i yosys git tmux vim gcc iverilog cabal-install wget

COPY . /verismith

WORKDIR /verismith
RUN nix-shell --run "true"
