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

              # System utilities
              age
              curl
              wget
              tree
              htop

              # Development environments
              nodejs
              python3
              rustc
              cargo
            ];

            # Git configuration (basic - detailed config in chezmoi)
            programs.git = {
              enable = true;
              userName = "yukifrog";
              userEmail = "yukifrog@users.noreply.github.com";
            };

            # Bash configuration (basic - detailed in chezmoi)
            programs.bash = {
              enable = true;
              enableCompletion = true;
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