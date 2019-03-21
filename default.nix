with import <nixpkgs> {};
stdenv.mkDerivation  rec {
  name = "elm-emoji";
  env = buildEnv {
    name = name;
    paths = [
      elmPackages.elm
      elmPackages.elm-format
      nodejs-10_x
      yarn
    ];
  };
  unpackPhase = "true";
  postInstall = 
  ''
  yarn globabl add create-elm-app
  '';
}
