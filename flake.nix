{
  description = "org-loom: Virtual table top for solo roleplaying journaling";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        emacs-with-packages = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
          org
          package-lint
        ]);

        compile-script = pkgs.writeShellScriptBin "compile" ''
          ${emacs-with-packages}/bin/emacs -batch -L . -f batch-byte-compile org-loom.el
        '';

        test-script = pkgs.writeShellScriptBin "test" ''
          if [ -f test/org-loom-test.el ]; then
            ${emacs-with-packages}/bin/emacs -batch -L . -l org-loom.el -l test/org-loom-test.el -f ert-run-tests-batch-and-exit
          else
            echo "No test file found at test/org-loom-test.el"
            exit 1
          fi
        '';

        lint-script = pkgs.writeShellScriptBin "lint" ''
          ${emacs-with-packages}/bin/emacs -batch --eval "(progn (require 'package) (require 'package-lint) (package-lint-batch-and-exit))" org-loom.el
        '';

        clean-script = pkgs.writeShellScriptBin "clean" ''
          rm -f *.elc
        '';

        package-script = pkgs.writeShellScriptBin "package" ''
          VERSION=$(grep "Version:" org-loom.el | sed 's/.*Version: *//')
          ${pkgs.gnutar}/bin/tar --exclude-vcs --exclude='*.elc' --exclude='result*' --exclude='test/' -czf org-loom-$VERSION.tar.gz .
          echo "Created package: org-loom-$VERSION.tar.gz"
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacs-with-packages
            compile-script
            test-script
            lint-script
            clean-script
            package-script
            gnutar
            gzip
          ];

          shellHook = ''
            echo "org-loom development environment"
            echo "Available commands:"
            echo "  compile  - Byte-compile the package"
            echo "  test     - Run tests"
            echo "  lint     - Run package linting"
            echo "  clean    - Remove compiled files"
            echo "  package  - Create distribution package"
            echo ""
            echo "Emacs with required packages is available as 'emacs'"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "org-loom";
          version = "0.1.0";

          src = ./.;

          buildInputs = [ emacs-with-packages ];

          buildPhase = ''
            ${emacs-with-packages}/bin/emacs -batch -L . -f batch-byte-compile org-loom.el
          '';

          installPhase = ''
            mkdir -p $out/share/emacs/site-lisp
            cp *.el *.elc $out/share/emacs/site-lisp/
          '';

          meta = with pkgs.lib; {
            description = "Virtual table top for solo roleplaying journaling";
            homepage = "https://github.com/yourusername/org-loom";
            license = licenses.gpl3Plus;
            maintainers = [ ];
            platforms = platforms.all;
          };
        };
      });
} 