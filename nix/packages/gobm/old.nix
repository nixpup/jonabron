
{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "gobm";
  version = "1.0";
  src = ./.;
  buildInputs = [
    pkgs.guile
    pkgs.guile-json
    pkgs.curl
    pkgs.makeWrapper
  ];
  installPhase = ''
    mkdir -p $out/bin
    cp gobm.scm $out/bin/gobm.scm
    chmod +x $out/bin/gobm.scm

    ${pkgs.makeWrapper}/bin/makeWrapper ${pkgs.guile} $out/bin/gobm \
    --set GUILE_LOAD_PATH "${pkgs.guile-json}/share/guile/site/3.0" \
    --run "exec \$program $out/bin/gobm.scm \"\$@\""
  '';
  meta = with pkgs.lib; {
    homepage = "https://github.com/librepup/geex";
    description = "Download osu! Beatmaps from a URL";
    license = licenses.gpl3Plus;
    maintainers = [ maintainers.librepup ];
    platforms = platforms.linux;
    mainProgram = "gobm";
  };
}
