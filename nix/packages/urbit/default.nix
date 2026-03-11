{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "urbit";
  version = "4.3";
  src = pkgs.fetchzip {
    url = "https://github.com/urbit/vere/releases/download/vere-v${version}/linux-x86_64.tgz";
    sha256 = "sha256-z8gUCnLfk6oRTO+wCbxmtQ8lga7eZhjGcqcnk+ORNLk=";
  };

  installPhase = ''
    install -m755 -D ./vere* $out/bin/urbit
  '';

  passthru.updateScript = ./update-bin.sh;

  meta = with pkgs.lib; {
    homepage = "https://urbit.org";
    description = "A clean-slate OS and network for the 21st century.";
    platforms = [
      "x86_64-linux"
    ];
    maintainers = [ maintainers.librepup ];
    license = licenses.mit;
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    mainProgram = "urbit";
  };
}
