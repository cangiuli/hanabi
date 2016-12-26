# Hanabi

Standard ML implementation of Hanabi, by Carlo Angiuli.

This is intended primarily as a testing ground for various strategies.
Contributions are welcome!

# Scores

**Simple Player**

Average score +/- 2 * standard error over 10000 games:
* 2 players: 24.080 +/- 0.059.
* 3 players: 22.315 +/- 0.043.
* 4 players: 22.717 +/- 0.042.
* 5 players: 21.841 +/- 0.034.

**Advanced Player**

Average score +/- 2 * standard error over 10000 games:
* 2 players: 25.214 +/- 0.058.
* 3 players: 24.859 +/- 0.040.
* 4 players: 24.216 +/- 0.036.
* 5 players: 22.822 +/- 0.030.

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
To run the code, execute
```
rlwrap sml -m sources.cm
```
This will play games specified in `src/main.sml`. Press `ctrl-D` to exit the interactive sml session.