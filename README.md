# SVG Interface Creator Tool



# Dependencies 

The system requires Haskell and uses its build and package mamager **Stack**. If you don't already have Stack installed, then on  Un*x operating systems, including Mac OS, run the following command:
```
curl -sSL https://get.haskellstack.org/ | sh
```

and on Window you can download and install the [Windows 64-bit Installer.](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

Additionally to utilize the live editing demo, then you need to install [Steel Over Seer](https://github.com/mitchellwrosen/steeloverseer) and [ghcid](https://github.com/ndmitchell/ghcid), which again are Haskell programs and can be easily installed using the following Stack commands:

```
stack install steeloverseer
stack install ghcid
```

You need a text editor to work with SVG interface files, extension **.mi**, and I myself would recommend Microsoft's [Visual Studio Code](https://code.visualstudio.com/). Of course, if you already have one, then it's likely to be just fine. One thing to note is that as interface files are really just Haskell source hiding, then it is possible to use one of the many excellent VS Code Haskell extensions to get the benifits of syntax colouring and so on. Personally I use [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell). 

Once installed open examples/lightpad.mi within VS Code and in the status bar bottom let it will say **Plain Text**, click on this and in the drop down menu select Haskell as the associated language to get the benifits of Haskell source checking and so on.

Finally, if your using VS Code it can be useful to visualize generated SVGs directly and for this install the extension [SVG Viewer](https://marketplace.visualstudio.com/items?itemName=cssho.vscode-svgviewer). Right clicking on an SVG in VS Code's browser provides a menu option to view SVG. 

# Building

Clone the repo and change into its root directory, then run the following command:

```
stack build
```

# Using it

First k

First let's try editing an example, open up ```examples/lightpad.mi``` from within 

# License

Some of the code for the underlying SVG DSL comes from [svg-builder](https://github.com/diagrams/svg-builder), and even in the case of changes to those original files its license applies.

Otherwise licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 * [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)

at your option.

Dual MIT/Apache2 is strictly more permissive