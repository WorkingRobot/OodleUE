{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {nixpkgs, ...}: let
    forAllSystems = nixpkgs.lib.genAttrs [
      "aarch64-linux"
      "x86_64-linux"
    ];
  in {
    packages = forAllSystems (system: rec {
      default = oodle;
      oodle = nixpkgs.legacyPackages.${system}.callPackage ({
        stdenv,
        cmake,
        git,
        fetchFromGitHub,
        enableStatic ? stdenv.hostPlatform.isStatic,
        lib,
      }:
        stdenv.mkDerivation rec {
          pname = "oodle";
          version = "2.9.16";
          src = ./.;

          nativeBuildInputs = [
            cmake
            git
          ];

          cmakeFlags = [
            "-S ${src}/build"
            "-DFETCHCONTENT_SOURCE_DIR_SMAKE=${fetchFromGitHub {
              owner = "WorkingRobot";
              repo = "smake";
              rev = "ab947d3000e8f7d0b188118b4205db521449cdd0";
              sha256 = "sha256-kkjYOFZkBhstQ+YKtr5k5vwRriUfIuT3LVqKBg5YM3s=";
            }}"
          ];

          installPhase = ''
            runHook preInstall

            mkdir -p $out

            cp -r ar $out
            cp -r lib $out
            cp -r bin $out

            runHook postInstall
          '';

          meta = {
            description = "Oodle is a family of data compression solutions with incredible world-beating performance.";
            homepage = "https://www.radgametools.com/oodle.htm";
            platforms = ["x86_64-linux" "aarch64-linux"];
            mainProgram = "ozip";
          };
        }) {};
    });
  };
}
