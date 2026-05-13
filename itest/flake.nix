{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    flake-utils.url = "github:numtide/flake-utils";
    # frameworks for integration testing
    nf-test-bootstrap = {
      url = "github:twbs/bootstrap?shallow=1";
      flake = false;
    };
    nf-test-bulma = {
      url = "github:jgthms/bulma?shallow=1";
      flake = false;
    };
    nf-test-sakura = {
      url = "github:oxalorg/sakura?shallow=1";
      flake = false;
    };
    nf-test-ress = {
      url = "github:filipelinhares/ress?shallow=1";
      flake = false;
    };
    nf-test-foundation = {
      url = "github:foundation/foundation-sites?shallow=1";
      flake = false;
    };
    nf-test-uswds = {
      url = https://github.com/uswds/uswds/releases/download/v3.13.0/uswds-uswds-3.13.0.tgz;
      flake = false;
    };
    nf-test-stack-overflow = {
      url = https://github.com/StackExchange/Stacks/archive/refs/tags/@stackoverflow/stacks-svelte@0.6.0.tar.gz;
      flake = false;
    };
    nf-test-materialize = {
      url = "github:materializecss/materialize?shallow=1";
      flake = false;
    };
    nf-test-beer = {
      url = "github:beercss/beercss?shallow=1";
      flake = false;
    };
    nf-test-cirrus = {
      url = "github:Spiderpig86/Cirrus?shallow=1";
      flake = false;
    };
    nf-test-uikit = {
      url = "github:uikit/uikit?shallow=1";
      flake = false;
    };
    # open-props = {
    #   url = "github:argyleink/open-props";
    #   flake = false;
    # };

    primer = {
      url = "github:yaitskov/css?shallow=1";
      # package lock is out of sync in the origin repo
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
        inherit (pkgs) lib;
        inherit (lib) strings attrsets;
        inherit (strings) hasPrefix;
        inherit (attrsets) filterAttrs;

        npmPkg = i: h:
          let
            npmPackage = builtins.fromJSON (builtins.readFile "${i}/package.json");
          in
            pkgs.buildNpmPackage {
              pname = npmPackage.name;
              version = npmPackage.version;
              src = i;
              # Generate a new dependency hash using:
              # prefetch-npm-deps path/to/package-lock.json
              npmDepsHash = h;
              npmBuildScript = "build:css";
              postPatch = ''
                patchShebangs .
              '';
              installPhase = ''
                mkdir -p "$out/lib"
                cp -rv dist "$out/lib"
              '';

            };
        builtLibs =
          [
            (npmPkg (inputs.primer) "sha256-KfNF6DgS6L7X5y3pEbZtbuYPfPBfRjdQdilvsJa67B4=")
          ];
        nonFlakes = builtins.attrValues (filterAttrs (n: _: hasPrefix "nf-test-" n) inputs);
        dirsWithCss = lib.concatStringsSep " " (builtLibs ++ nonFlakes);
      in
        {
          # packages.default =
          #   npmPkg inputs.open-props "sha256-sasT+YFg+5P5sOkiEywTELT4govwE+JQv8H6SU2n6jw=";

          devShells = {
            default = pkgs.mkShell {
              buildInputs = [
                pkgs.nodejs               # npm & npx
                pkgs.prefetch-npm-deps
              ];
              shellHook = ''
                echo -n "primer SHA: "
                # prefetch-npm-deps "${inputs.primer}/package-lock.json"
                echo "commented - skipped"
                export CSS_FRAMEWORKS=( . ${dirsWithCss} )
                function err() { echo "Error: $@" ; exit 1; }
                function findcss() {
                  for CSS_FR in $CSS_FRAMEWORKS ; do
                    find $CSS_FR -type f -name '*.css'
                  done
                }
                function intest() {
                  CSS_PARSER=$(find ../dist-newstyle/build -type f -name css-parser | head -n 1)
                  [ -x "$CSS_PARSER" ] || err "CSS_PARSER is not found"
                  mkdir -p .css-hashes
                  for CSS_FR in ''${CSS_FRAMEWORKS[@]} ; do
                    find $CSS_FR -type f -name '*.css' | while read CSS_FILE ; do
                      CSS_FILE_HASH=$(md5sum <<< $CSS_FILE | while read A B ; do echo $A ; done)
                      if [ "$CSS_PARSER" -nt .css-hashes/$CSS_FILE_HASH ] ; then
                        echo "$CSS_FILE"
                        if $CSS_PARSER $CSS_FILE > /dev/null ; then
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
