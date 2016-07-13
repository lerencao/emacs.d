#!/bin/sh

if [ ! -d ~/.cask ]
then
    echo "Clone Cask Repo"
    git clone https://github.com/cask/cask.git ~/.cask
fi

source_file=""
case $(basename $SHELL) in
    zsh)
        source_file='.zshrc'
        ;;
    bash)
        source_file='.bashrc'
        ;;
    *)
        echo "unrecognized shell $shell_name" >&2
        exit 1
        ;;
esac

if [ ! "$(grep '.cask/bin' ~/$source_file)" ]
then
    echo "Adding \$HOME/.cask/bin to \$PATH in ~/$source_file"
    echo "" >> ~/$source_file
    echo "# added by ~/.emacs.d/setup.sh" >> ~/$source_file
    echo "export PATH=\$HOME/.cask/bin:\$PATH" >> ~/$source_file
fi

export PATH=$HOME/.cask/bin:$PATH

cd ~/.emacs.d
cask install
git clone https://github.com/rust-lang/rust.git customizations/rust

curl https://raw.githubusercontent.com/ppareit/graphviz-dot-mode/master/graphviz-dot-mode.el -o customizations/23-graphviz-dot-mode.el
