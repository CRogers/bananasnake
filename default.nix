{ mkDerivation, base, random, reactive-banana, stdenv
, terminal-size
}:
mkDerivation {
  pname = "bananasnake";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base random reactive-banana terminal-size ];
  license = stdenv.lib.licenses.mit;
}
