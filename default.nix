{ mkDerivation, aeson, base, bytestring, containers, errors
, http-conduit, lib, mr-env, text
}:
mkDerivation {
  pname = "wayther";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers errors http-conduit mr-env text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers errors http-conduit mr-env text
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
