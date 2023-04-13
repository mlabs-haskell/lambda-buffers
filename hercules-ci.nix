{ self, inputs, ... }: {
  imports = [
    inputs.hci-effects.flakeModule # Adds hercules-ci and herculesCI options
  ];

  hercules-ci.flake-update = {
    enable = true;
    updateBranch = "updated-flake-lock";
    # Next two parameters should always be set explicitly
    createPullRequest = true;
    autoMergeMethod = null;
    when = {
      # Perform update by Sundays at 12:45
      minute = 45;
      hour = 12;
      dayOfWeek = "Sun";
    };
  };
  hercules-ci.github-pages.branch = "main";
  perSystem = { pkgs, ... }: {
    hercules-ci.github-pages.settings.contents = pkgs.runCommand "lambda-buffers-book"
      {
        buildInputs = [ pkgs.mdbook ];
      } "mdbook build ${self.outPath}/docs --dest-dir $out";
  };

  herculesCI.ciSystems = [ "x86_64-linux" ];
}