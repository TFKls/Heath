# Heath
***A simple scheme-like language.***

[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE)

## About
This is a not-yet complete interpreter for a scheme-like language. 

I've made this as a learning experience, because I've wanted to learn the Parsec library for some time now.  
It will most likely never be fully compatible with any of the RxRS standards, but I will try to add as much features as I can.

This interpreter has been written alongside the [Write Yourself a Scheme in 48 Hours](https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf) book, 
however multiple modifications and additional features have been made, and the code was rewritten due to differrences in the Haskell language between different reports and compiler versions.

## Usage
As of right now, the executable provides only a simple REPL, due to the limitations of the current version (no state implemented yet).  
Support for evaluating a file or a command line argument is however planned.

## Building
Check the Github Actions CI to check if the latest commit builds. (it should)

To build using stack use `stack build`, cabal using `cabal build all`.  
As of right now, there are no tests yet, they will most likely be added after state and executable updates.

## Contributing
If you are knowledgable about Haskell, it would be very helpful for me if you were to point out some mistakes I've made and help me get better. I would be extremely grateful.

If you don't want to do that and just want to contribute, you can create a pull request fixing some issue, or possibly fixing your own. I do however ask you, if you want to fix a problem you've encountered, create an issue first as it helps with organizing all of that.  
If you don't know how this stuff works but still would like to see some change, submit an issue.

I kindly ask you to follow the [code of conduct](CODE_OF_CONDUCT.md) while contributing.

## License
Heath - a simple scheme-like language  
Copyright (C) 2021 Tomasz "TFKls" Kulis

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program [here](LICENSE). If not, see <https://www.gnu.org/licenses/gpl.html>.  

