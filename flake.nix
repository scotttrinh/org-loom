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

        dev-script = pkgs.writeShellScriptBin "dev" ''
          set -e

          usage() {
            echo "Usage: dev <command>"
            echo ""
            echo "Available commands:"
            echo "  compile  - Byte-compile the package"
            echo "  test     - Run tests"
            echo "  lint     - Run package linting"
            echo "  clean    - Remove compiled files"
            echo "  package  - Create distribution package"
            echo "  help     - Show this help message"
          }

          case "''${1:-}" in
            compile)
              echo "Compiling org-loom.el..."
              ${emacs-with-packages}/bin/emacs -batch -L . -f batch-byte-compile org-loom.el
              ;;
            test)
              echo "Checking for test file..."
              if [ -f test/org-loom-test.el ]; then
                echo "Found test file, running tests..."
                echo "Current directory: $(pwd)"
                echo "Loading org-loom.el..."
                ${emacs-with-packages}/bin/emacs -batch -L . --eval "(progn (message \"Loading org-loom...\") (load \"org-loom.el\") (message \"org-loom loaded successfully\"))" 2>&1
                if [ $? -ne 0 ]; then
                  echo "Failed to load org-loom.el"
                  exit 1
                fi
                echo "Loading test file..."
                ${emacs-with-packages}/bin/emacs -batch -L . -l org-loom.el --eval "(progn (message \"Loading test file...\") (load \"test/org-loom-test.el\") (message \"Test file loaded successfully\"))" 2>&1
                if [ $? -ne 0 ]; then
                  echo "Failed to load test file"
                  exit 1
                fi
                echo "Running tests with verbose output..."
                ${emacs-with-packages}/bin/emacs -batch -L . -l org-loom.el -l test/org-loom-test.el --eval "(ert-run-tests-batch-and-exit t)" 2>&1
              else
                echo "No test file found at test/org-loom-test.el"
                echo "Current directory: $(pwd)"
                echo "Directory contents:"
                ls -la
                exit 1
              fi
              ;;
            lint)
              echo "Linting org-loom.el..."
              ${emacs-with-packages}/bin/emacs -batch --eval "(progn (require 'package) (require 'package-lint) (package-lint-batch-and-exit))" org-loom.el
              ;;
            clean)
              echo "Cleaning compiled files..."
              rm -f *.elc
              echo "Done."
              ;;
            package)
              echo "Creating package..."
              VERSION=$(grep "Version:" org-loom.el | sed 's/.*Version: *//')
              ${pkgs.gnutar}/bin/tar --exclude-vcs --exclude='*.elc' --exclude='result*' --exclude='test/' -czf org-loom-$VERSION.tar.gz .
              echo "Created package: org-loom-$VERSION.tar.gz"
              ;;
            help|"")
              usage
              ;;
            *)
              echo "Unknown command: $1"
              echo ""
              usage
              exit 1
              ;;
          esac
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacs-with-packages
            dev-script
            gnutar
            gzip
          ];

          shellHook = ''
            echo "org-loom development environment"
            echo ""
            echo "Use 'dev <command>' for development tasks:"
            echo "  dev compile  - Byte-compile the package"
            echo "  dev test     - Run tests"
            echo "  dev lint     - Run package linting"
            echo "  dev clean    - Remove compiled files"
            echo "  dev package  - Create distribution package"
            echo "  dev help     - Show detailed help"
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