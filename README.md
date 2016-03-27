# hnem
a naive implementation of netease music player using haskell

This is a toy project inspired by [netease-musicbox](https://github.com/darknessomi/musicbox) (written in python), its purpose is just practice.
At the time only login (only tested with phone login) and user playlist playback are implemented, other functions such as discover/search are not
included.Behind the scene it makes `mplayer` in slave mode to play music, so `mplayer` is a dependency.

The code became very messy (I'm new to haskell and programming), especially UI.hs, so this may not be te the `good code` for other new learners to read.
