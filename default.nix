{ mkDerivation, aeson, base, bytestring, containers, hpack
, http-conduit, lib, mr-env, text
}:
mkDerivation {
  pname = "wayther";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers http-conduit mr-env text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers http-conduit mr-env text
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
