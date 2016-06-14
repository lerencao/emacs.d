if [[ ! -e ~/.cask ]]
then
    echo "Clone Cask Repo"
    git clone https://github.com/cask/cask.git ~/.cask
fi

if [[ $(grep ".cask/bin" ~/.zshrc) == "" ]]
then
    echo "Adding \$HOME/.cask/bin to \$PATH in ~/.zshrc"
    echo "" >> ~/.zshrc
    echo "# added by ~/.emacs.d/setup.sh" >> ~/.zshrc
    echo "export PATH=\$HOME/.cask/bin:\$PATH" >> ~/.zshrc
fi

export PATH=$HOME/.cask/bin:$PATH

cd ~/.emacs.d
cask install
git clone https://github.com/rust-lang/rust.git customizations/rust
