# Hanabi

Standard ML implementation of Hanabi, by Carlo Angiuli.

This is intended primarily as a testing ground for various strategies.
Contributions are welcome!

# Installation

We recommend `rlwrap` to run the code. To install the required packages on Linux, execute
```
sudo apt-get install smlnj rlwrap
```
This repository uses `cmlib` as submodule. To clone the repository, run
```
git clone --recursive git@github.com:cangiuli/hanabi.git
```
If you've already cloned it without the submodule, run the following command to get `cmlib`
```
git submodule update --init --recursive
```
To run the code and play a single game of hanabi, execute
```
rlwrap sml -m sources.cm
```