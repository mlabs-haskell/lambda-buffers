{ inputs, ... }: {
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
  perSystem = { config, ... }: {
    hercules-ci.github-pages.settings.contents = config.packages.lambda-buffers-book;
  };

  herculesCI.ciSystems = [ "x86_64-linux" ];
}
