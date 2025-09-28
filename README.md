# Dotfiles

chezmoiで管理するdotfiles設定です。Claude Code統合を重視した開発環境。

## 構成

```
~/.local/share/chezmoi/     # chezmoiソースディレクトリ
├── dot_bashrc              # ~/.bashrc
├── dot_config/             # ~/.config/
│   └── emacs/
│       └── init.el         # ~/.config/emacs/init.el
├── dot_gitconfig.tmpl      # ~/.gitconfig (テンプレート)
└── README.md               # このファイル
```

## 特徴

### ✨ chezmoi管理
- **テンプレート機能**: 環境固有の設定を変数化
- **暗号化対応**: 秘密ファイルの安全な管理
- **クロスプラットフォーム**: 複数環境での統一設定

### 🎯 Claude Code統合
- **Emacs設定**: Claude Code IDE連携最適化
- **起動高速化**: GC調整と遅延読み込み
- **モジュラー設計**: 機能別設定分割

## セットアップ

### 新環境でのインストール
```bash
# chezmoiインストール (Nixの場合)
nix-env -iA nixpkgs.chezmoi

# dotfiles初期化・適用
chezmoi init https://github.com/yukifrog/dotfiles.git
chezmoi apply
```

### 日常の使用
```bash
# 設定ファイル編集
chezmoi edit ~/.bashrc
chezmoi edit ~/.config/emacs/init.el

# 変更確認・適用
chezmoi status
chezmoi diff
chezmoi apply

# Git管理
chezmoi cd
git add . && git commit -m "Update config"
git push
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

- **dotfiles管理**: chezmoi
- **エディタ**: Emacs (XDG Base Directory対応)
- **パッケージ管理**: straight.el
- **Git統合**: GitHub CLI + Magit
- **暗号化**: age

---

🚀 **現代的なdotfiles管理** - シンボリンクではなく、chezmoiによる統一管理