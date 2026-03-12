{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation rec {
  pname = "dangerousjungle-grub-theme";
  version = "1.0";
  src = ./theme;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/
    cp -r ./* $out/
    runHook postInstall
  '';
  meta = {
    description = "Dangerous Jungle (Vibrant) GRUB Bootloader Theme";
    homepage = "https://github.com/librepup/jonabron";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.linux;
    maintainers = [ maintainers.librepup ];
  };
}
