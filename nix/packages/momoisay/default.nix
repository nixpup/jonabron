{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "momoisay";
  version = "1.1.1";

  src = pkgs.fetchFromGitHub {
    owner = "Mon4sm";
    repo = "momoisay";
    rev = "001f4a693c26c8aabbf0dc4f88100303ad93efc6";
    sha256 = "sha256-9exrPLoroOiSa6SD6LRjlYo7+uuDTFCbxQcXmLaX2JI=";
  };

  naviveBuildInputs = with pkgs; [
    patchelf
  ];
  buildInputs = with pkgs; [
    ncurses
  ];

  installPhase = ''
    mkdir -p $out/bin
    install -m755 -D ./bin/linux/${pname} $out/bin/${pname}
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${pkgs.lib.makeLibraryPath [ pkgs.ncurses ]}" \
      $out/bin/${pname}
  '';

  meta = with pkgs.lib; {
    homepage = "https://github.com/Mon4sm/momoisay";
    description = "A CLI program written in C featuring talking Saiba Momoi from Blue Archive";
    platforms = [
      "x86_64-linux"
    ];
    maintainers = [ maintainers.librepup ];
    license = licenses.gpl3;
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    mainProgram = "momoisay";
  };
}
