{
  description = "yukifrog's dotfiles with flakes + home-manager + chezmoi";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Home Manager configuration
      homeConfigurations.yukifrog = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          {
            home.username = "yuuki";
            home.homeDirectory = "/home/yuuki";
            home.stateVersion = "23.11";

            # Package management
            home.packages = with pkgs; [
              # Development tools
              emacs
              chezmoi
              git
              gh
              tmux

              # Modern CLI replacements
              bat           # cat replacement with syntax highlighting
              eza           # ls replacement with colors and git status
              fd            # find replacement
              ripgrep       # grep replacement
              fzf           # fuzzy finder
              zoxide        # cd replacement with smart jumping
              delta         # git diff with syntax highlighting

              # System utilities
              age
              curl
              wget
              tree
              htop
              btop          # htop replacement (better than bottom)
              dust          # du replacement
              procs         # ps replacement
              hyperfine     # benchmarking tool
              jq            # JSON processor
              yq            # YAML processor

              # Development environments
              nodejs
              python3
              rustc
              cargo
              go

              # Additional development tools
              direnv        # automatic environment loading
              just          # command runner
              starship      # shell prompt

              # Build and compilation tools
              gcc           # C/C++ compiler
              gnumake       # build tool
              cmake         # cross-platform build system

              # Git and version control
              tig           # text-mode interface for git
              gitleaks      # detect secrets in git repos

              # Network and HTTP tools
              httpie        # user-friendly HTTP client
              bandwhich     # network utilization by process

              # Editors and text processing
              vim           # modal text editor
              nano          # simple text editor

              # AI and productivity
              aider         # AI pair programming
            ];

            # Git configuration (basic - detailed config in chezmoi)
            programs.git = {
              enable = true;
              userName = "yukifrog";
              userEmail = "yukifrog@users.noreply.github.com";
            };

            # Modern CLI tools configuration
            programs.bat = {
              enable = true;
              config = {
                theme = "zenburn";
                style = "numbers,changes,header";
              };
            };

            programs.fzf = {
              enable = true;
              enableBashIntegration = true;
            };

            programs.zoxide = {
              enable = true;
              enableBashIntegration = true;
            };

            programs.direnv = {
              enable = true;
              enableBashIntegration = true;
              nix-direnv.enable = true;
            };

            programs.starship = {
              enable = true;
              enableBashIntegration = true;
            };

            # Bash configuration (basic - detailed in chezmoi)
            programs.bash = {
              enable = true;
              enableCompletion = true;
              shellAliases = {
                # Modern CLI replacements
                cat = "bat";
                ls = "eza --icons --git";
                ll = "eza -l --icons --git";
                la = "eza -la --icons --git";
                find = "fd";
                grep = "rg";
                ps = "procs";
                top = "btop";
                du = "dust";

                # Git shortcuts
                g = "git";
                gs = "git status";
                gc = "git commit";
                gp = "git push";
                gl = "git log --oneline";

                # Additional shortcuts
                http = "httpie";
                secrets = "gitleaks detect";
                network = "bandwhich";
              };
            };

            # Home Manager can also manage dotfiles, but we let chezmoi handle detailed configs
            home.file = {
              # Create .nix-channels for compatibility
              ".nix-channels".text = ''
                https://nixos.org/channels/nixos-unstable nixpkgs
              '';
            };

            # Allow unfree packages (for some development tools)
            nixpkgs.config.allowUnfree = true;
          }
        ];
      };

      # Development shell for working on dotfiles
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixpkgs-fmt
          nil  # Nix LSP
        ];

        shellHook = ''
          echo "🎯 dotfiles development environment"
          echo "Available commands:"
          echo "  nixpkgs-fmt flake.nix  # Format Nix code"
          echo "  chezmoi status         # Check dotfiles status"
          echo "  home-manager switch --flake .#yukifrog  # Apply home-manager config"
        '';
      };

      # Formatter for nix code
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}