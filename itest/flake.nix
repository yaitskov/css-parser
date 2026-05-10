{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    flake-utils.url = "github:numtide/flake-utils";
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
    # frameworks for integration testing
    bootstrap = {
      url = "github:twbs/bootstrap";
      flake = false;
    };
    bulma = {
      url = "github:jgthms/bulma";
      flake = false;
    };
    sakura = {
      url = "github:oxalorg/sakura";
      flake = false;
    };
    ress = {
      url = "github:filipelinhares/ress";
      flake = false;
    };
    foundation = {
      url = "github:foundation/foundation-sites";
      flake = false;
    };
    uswds = {
      url = https://github.com/uswds/uswds/releases/download/v3.13.0/uswds-uswds-3.13.0.tgz;
      flake = false;
    };
    stack-overflow = {
      url = https://github.com/StackExchange/Stacks/archive/refs/tags/@stackoverflow/stacks-svelte@0.6.0.tar.gz;
      flake = false;
    };
    materialize = {
      url = "github:materializecss/materialize";
      flake = false;
    };
    beer = {
      url = "github:beercss/beercss";
      flake = false;
    };
    cirrus = {
      url = "github:Spiderpig86/Cirrus";
      flake = false;
    };

    # = {
    #   url = "github:";
    #   flake = false;
    # };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      { devShells = {
          default = pkgs.mkShell {
            buildInputs = [];
            shellHook = ''
              export CSS_FRAMEWORKS=(
                 ${inputs.cirrus} ${inputs.beer} ${inputs.materialize}
                 ${inputs.bulma} ${inputs.bootstrap} ${inputs.stack-overflow}
                 ${inputs.uswds}
                 ${inputs.ress} ${inputs.sakura} ${inputs.foundation})
              function err() { echo "Error: $@" ; exit 1; }
              function findcss() {
                for CSS_FR in $CSS_FRAMEWORKS ; do
                  find $CSS_FR -type f -name '*.css'
                done
              }
              function intest() {
                # set -x
                # set +eo pipefail
                CSS_PARSER=$(find ../dist-newstyle/build -type f -name css-parser | head -n 1)
                [ -x "$CSS_PARSER" ] || err "CSS_PARSER is not found"
                mkdir -p .css-hashes
                for CSS_FR in ''${CSS_FRAMEWORKS[@]} ; do
                  # echo "$CSS_FR framework"
                  find $CSS_FR -type f -name '*.css' | while read CSS_FILE ; do
                    # set +e
                    # echo CSSFILE: $CSS_FILE
                    CSS_FILE_HASH=$(md5sum <<< $CSS_FILE | while read A B ; do echo $A ; done)
                    if [ "$CSS_PARSER" -nt .css-hashes/$CSS_FILE_HASH ] ; then
                      echo "$CSS_FILE"
                      if $CSS_PARSER $CSS_FILE > /dev/null ; then
                        # echo " OK"
                        echo $CSS_FILE > .css-hashes/$CSS_FILE_HASH
                      else
                        : # echo " Failed"
                      fi
                    else
                      : # echo "Skip $CSS_FILE"
                    fi
                  done
                done
              }
              echo "Run [intest] to start integration tests"
            '';
          };
        };
      });
}
