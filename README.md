# Dotfiles

❄️ **Nix Flakes + home-manager + chezmoi** で管理する現代的なdotfiles設定です。Claude Code統合を重視した開発環境。

## 構成

```
~/.local/share/chezmoi/     # chezmoiソースディレクトリ
├── flake.nix               # Nix flakes設定 (パッケージ管理)
├── flake.lock              # 依存関係固定ファイル
├── dot_bashrc              # ~/.bashrc
├── dot_config/             # ~/.config/
│   ├── emacs/init.el       # ~/.config/emacs/init.el
│   └── nix/nix.conf        # ~/.config/nix/nix.conf
├── dot_gitconfig.tmpl      # ~/.gitconfig (テンプレート)
└── README.md               # このファイル
```

## 特徴

### ❄️ Nix Flakes統合
- **完全な再現性**: flake.lockで依存関係固定
- **宣言的パッケージ管理**: home-managerでEmacs、開発ツール管理
- **開発環境**: `nix develop`で即座に開発シェル起動
- **Git統合**: GitHub URLから直接環境構築可能

### ✨ chezmoi管理
- **詳細なdotfiles**: テンプレート機能と暗号化対応
- **クロスプラットフォーム**: 複数環境での統一設定
- **役割分担**: パッケージ(Nix) + dotfiles(chezmoi)

### 🎯 Claude Code統合
- **Emacs設定**: Claude Code IDE連携最適化
- **起動高速化**: GC調整と遅延読み込み
- **モジュラー設計**: 機能別設定分割

## セットアップ

### 🚀 新環境でのインストール（推奨: Flakes方式）
```bash
# 1. Nix flakesが有効でない場合、有効化
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf

# 2. flakeから直接環境構築
nix run github:yukifrog/dotfiles#homeConfigurations.yukifrog.activationPackage

# 3. または段階的セットアップ
git clone https://github.com/yukifrog/dotfiles.git ~/.local/share/chezmoi
cd ~/.local/share/chezmoi
nix develop  # 開発環境に入る
```

### 📦 従来方式（Nix未使用環境）
```bash
# chezmoiのみでセットアップ
chezmoi init https://github.com/yukifrog/dotfiles.git
chezmoi apply
```

### 日常の使用

#### Nix環境管理
```bash
# 開発環境起動
cd ~/.local/share/chezmoi && nix develop

# パッケージ更新
nix flake update

# home-manager適用 (パッケージ変更時)
home-manager switch --flake .#yukifrog
```

#### dotfiles管理 (chezmoi)
```bash
# 設定ファイル編集
chezmoi edit ~/.bashrc
chezmoi edit ~/.config/emacs/init.el

# 変更確認・適用
chezmoi status && chezmoi diff && chezmoi apply

# Git管理
chezmoi cd
git add . && git commit -m "Update config" && git push
```

## ファイル詳細

### Emacs設定 (`~/.config/emacs/init.el`)
- Claude Code IDE連携機能
- straight.elパッケージ管理
- LSP・開発ツール設定
- 起動高速化調整

### Git設定 (`~/.gitconfig`)
- テンプレート変数使用
- GitHub認証設定
- 環境固有の設定対応

### Bash設定 (`~/.bashrc`)
- 開発環境向けカスタマイズ
- 効率的なコマンドエイリアス

## テンプレート変数

`~/.config/chezmoi/chezmoi.toml` で設定:
```toml
[data]
name = "yukifrog"
email = "yukifrog@users.noreply.github.com"
```

## 高度な使用

### 新しいファイルの追加
```bash
chezmoi add ~/.tmux.conf          # 通常ファイル
chezmoi add --template ~/.gitconfig # テンプレートファイル
chezmoi add --encrypt ~/.ssh/config  # 暗号化ファイル
```

### テンプレート作成
```bash
# ホスト固有設定の例
{{- if eq .hostname "work-laptop" }}
[http]
  proxy = http://proxy.company.com:8080
{{- end }}
```

## 技術スタック

### 🏗️ インフラ
- **パッケージ管理**: Nix Flakes + home-manager
- **dotfiles管理**: chezmoi
- **依存関係固定**: flake.lock
- **暗号化**: age

### 🛠️ 開発環境
- **エディタ**: Emacs (XDG Base Directory対応)
- **Emacsパッケージ**: straight.el
- **Git統合**: GitHub CLI + Magit
- **開発シェル**: `nix develop`

### ⚡ 現代的CLIツール
- **bat**: `cat`の代替（シンタックスハイライト付き）
- **eza**: `ls`の代替（アイコン・Git状態表示）
- **fd**: `find`の代替（高速・直感的）
- **ripgrep**: `grep`の代替（高速・Git対応）
- **fzf**: ファジーファインダー
- **zoxide**: `cd`の代替（スマートジャンプ）
- **delta**: Git diff（シンタックスハイライト付き）
- **bottom**: `htop`の代替
- **starship**: 高機能シェルプロンプト

### 🎯 特殊機能
- **Claude Code統合**: 専用Emacs設定
- **テンプレート**: 環境固有設定の変数化
- **世代管理**: Nixの安全なロールバック機能

## 開発コマンド

```bash
# 開発環境起動 (Nix LSP + フォーマッター含む)
nix develop

# Nixコード整形
nixpkgs-fmt flake.nix

# 統合テスト
chezmoi status && nix flake check
```

---

❄️ **次世代dotfiles管理** - Nix Flakes + home-manager + chezmoi による完全再現可能な開発環境