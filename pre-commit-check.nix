{ fourmolu, protoHooks }: {
  src = ./.;
  settings = {
    # FIXME: https://github.com/cachix/pre-commit-hooks.nix/issues/155
    ormolu.defaultExtensions = [
      "NoStarIsType"
      "BangPatterns"
      "BinaryLiterals"
      "ConstrainedClassMethods"
      "ConstraintKinds"
      "DataKinds"
      "DeriveAnyClass"
      "DeriveDataTypeable"
      "DeriveFoldable"
      "DeriveFunctor"
      "DeriveGeneric"
      "DeriveLift"
      "DeriveTraversable"
      "DerivingStrategies"
      "DerivingVia"
      "DoAndIfThenElse"
      "EmptyCase"
      "EmptyDataDecls"
      "EmptyDataDeriving"
      "ExistentialQuantification"
      "ExplicitForAll"
      "ExplicitNamespaces"
      "FlexibleContexts"
      "FlexibleInstances"
      "ForeignFunctionInterface"
      "GADTSyntax"
      "GeneralisedNewtypeDeriving"
      "HexFloatLiterals"
      "ImportQualifiedPost"
      "InstanceSigs"
      "KindSignatures"
      "LambdaCase"
      "MonomorphismRestriction"
      "MultiParamTypeClasses"
      "NamedFieldPuns"
      "NamedWildCards"
      "NumericUnderscores"
      "OverloadedRecordDot"
      "OverloadedStrings"
      "PartialTypeSignatures"
      "PatternGuards"
      "PolyKinds"
      "PostfixOperators"
      "RankNTypes"
      "RelaxedPolyRec"
      "ScopedTypeVariables"
      "StandaloneDeriving"
      "StandaloneKindSignatures"
      "TraditionalRecordSyntax"
      "TupleSections"
      "TypeApplications"
      "TypeFamilies"
      "TypeOperators"
      "TypeSynonymInstances"
    ];
  };

  excludes = [ "lambda-buffers-codegen/data/.*" "experimental/.*" ];

  hooks = {
    nixpkgs-fmt.enable = true;
    nix-linter.enable = true;
    cabal-fmt.enable = true;
    fourmolu.enable = true;
    shellcheck.enable = true;
    hlint.enable = true;
    # TODO: Enable hunspell
    typos.enable = true;
    markdownlint.enable = true;
    dhall-format.enable = true;
  } // protoHooks;

  tools = { inherit fourmolu; };
}
