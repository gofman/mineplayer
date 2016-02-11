# mineplayer

## About
	A program which plays Minesweeper. Minesweeper window is captured and analyzed, and mouse clicks are sent to the Window. The program auto detects cells screen sizes (though too small cells may cause problem). Digits recognition is based on its color. A simle color statistics is computed and then a simple heuristics is used to match the statistics to digit recognition tables. Gnome Mines and KMines color schemes are currently recongnized (the choice between Gnome/KMines is based on window title).
	Mine assumes 30x16 field sizes.
	Mine uses xdotool command line tool for finding Minesweeper window and sending mouse events to it.

	Note: the latest version of gnome mines has gray digits for now, so the program will not work for them.

## Prerequisites

# Build
ghc, cabal 
	Haskell packages: base, array, vector, mtl, process, gtk, data-ordlist, gd 
	'cabal install' should download and install missing packages (libgtk and gd development libraries 
	should be installed for gtk & gd)

# Runtime
	Haskell packages binary deps, including libgtk, libgd

	'xdotool' program should be installed and available in PATH
	

## Compiling
Build: 
	cabal install
or
	cabal configure; cabal build; cabal install

I would recommend to remove ghc and cabal caches (rm -R ~/.ghc/*; rm -R ~/.cabal/*) and run 'cabal update' before building to avoid package conflicts.

## Running
'mine' without options displays a brief help on options.
'mine -': finds Minesweeper window, grabs it, tries to recognize the field and establishes a cell to open. If there is a sure choice, the program will issue mouse click and open the cell.If no cell can be chosen for sure, a message box is displayed showing the probability of the best guess. After the message box is closed mouse pointer is positioned to the best guess cell.
'mine -r': the same as 'mine -', but if sure choice cell is found and open, repeats the action (until the entire field is open or no sure guess can be made)
'mine -rg': the same as 'mine -r', but always opens a cell (based on best probability guess if there is no sure choice). Can loose a game, though the only way to get 4-5 seconds best times.

'mine -e': display cell color statistics for each cell in the field which can be used to update recognition tables in the program source.

The program searches Minesweeper using substring 'Mine' in Window header, so make sure that no other windows with such title is open (e. g. browser with page having Mine in title)

