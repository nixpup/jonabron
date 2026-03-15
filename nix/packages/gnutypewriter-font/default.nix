{ stdenv, lib }:

stdenv.mkDerivation {
  pname = "gnutypewriter-font";
  version = "1.0";

  src = ./files;

  dontBuild = true;
  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    mkdir -p $out/share/fonts/opentype

    # install all supported formats if present
    cp -v *.ttf $out/share/fonts/truetype 2>/dev/null || true
    cp -v *.otf $out/share/fonts/opentype 2>/dev/null || true
  '';

  meta = with lib; {
    description = "GNU Typewriter Font";
    license = licenses.gpl3;
    platforms = platforms.all;
    maintainers = [ maintainers.librepup ];
  };
}
