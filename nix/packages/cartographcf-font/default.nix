{ stdenv, lib }:
stdenv.mkDerivation {
  pname = "cartographcf-font";
  version = "1.0";

  src = ./files;

  dontBuild = true;
  installPhase = ''
    mkdir -p $out/share/fonts/opentype

    cp -v *.otf $out/share/fonts/opentype 2>/dev/null || true
  '';

  meta = with lib; {
    description = "cartographCF Font";
    license = licenses.gpl3;
    platforms = platforms.all;
    maintainers = [ maintainers.librepup ];
  };
}
