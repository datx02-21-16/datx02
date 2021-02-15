{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = { url = "github:justinwoo/easy-purescript-nix"; flake = false; };
  };

  outputs = { self, nixpkgs, flake-utils, easy-purescript-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; let
            latex = texlive.combine {
              inherit (texlive) scheme-basic latexmk metafont
                biblatex biblatex-ieee biber 
                enumitem glossaries mfirstuc xfor datatool xindy xkeyval textcase;
            };
            inherit (import easy-purescript-nix { inherit pkgs; }) purescript spago;
          in [
            latex

            nodejs-14_x
            purescript spago
          ];
        };
      });
}
