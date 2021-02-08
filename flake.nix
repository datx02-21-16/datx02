{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (texlive.combine {
              inherit (texlive) scheme-basic latexmk metafont
                biblatex biber biblatex-ieee;
            })
          ];
        };
      });
}
