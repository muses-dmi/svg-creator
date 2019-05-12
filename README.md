# <span style="color:#F3B73B">SVG Interface Creator Tool</span>

SVG interfaces (```.mi``` files) are a simple approach to specifiying control
interfaces for Digitial Musical Instruments or DMIs. SVG Interfaces generate OSC 
control messages and as such as indepdendent of any particular audio engine and were
designed within the context of the [Muses](https://muses-dmi.github.io/) project.

The SVG Interface
[documentation](https://github.com/muses-dmi/svg-creator/blob/master/docs/interfaces.md)
provides details of the spefication and how to create them. The tool descibed
here builds SVG interfaces from a small Domain Specific Language (DSL) for interfaces. SVG
interfaces are a portable format for describing control interfaces that communicate over OSC, 
however, they in themselves do not descibe how they are to be mapped or imagined in hardware. 

As noted this tool is part of the larger [Muses](https://muses-dmi.github.io/)
project and is a component within a larger set of tools for working with SVG
interfaces.

#  <span style="color:#F3B73B">Dependencies</span> 

The system requires Haskell and uses its build and package mamager **Stack**. If
you don't already have Stack installed, then on  Un*x operating systems,
including Mac OS, run the following command:
```
curl -sSL https://get.haskellstack.org/ | sh
```

and on Window you can download and install the [Windows 64-bit
Installer.](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

Additionally to utilize the live editing demo, then you need to install [Steel
Over Seer](https://github.com/mitchellwrosen/steeloverseer) and
[ghcid](https://github.com/ndmitchell/ghcid), which again are Haskell programs
and can be easily installed using the following Stack commands:

```
stack install steeloverseer
stack install ghcid
```

You need a text editor to work with SVG interface files, extension **.mi**, and
I myself would recommend Microsoft's [Visual Studio
Code](https://code.visualstudio.com/). Of course, if you already have one, then
it's likely to be just fine. One thing to note is that as interface files are
really just Haskell source hiding, then it is possible to use one of the many
excellent VS Code Haskell extensions to get the benifits of syntax colouring and
so on. Personally I use [Haskell Syntax
Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell).


Once installed open examples/lightpad.mi within VS Code and in the status bar
bottom let it will say **Plain Text**, click on this and in the drop down menu
select Haskell as the associated language to get the benifits of Haskell source
checking and so on.

Finally, if your using VS Code it can be useful to visualize generated SVGs
directly and for this install the extension [SVG
Viewer](https://marketplace.visualstudio.com/items?itemName=cssho.vscode-svgviewer).
Right clicking on an SVG in VS Code's browser provides a menu option to view
SVG. 

#  <span style="color:#F3B73B">Building</span>

Clone the repo and change into its root directory, then run the following
command:

```
stack build
```

#  <span style="color:#F3B73B">Using it</span>

In this section we assume the VS Code setup, as described above, but it should
not be to hard follow along with another text editor.

Firstly, begin by opening two terminal windows and change into the repo's
directory, then run the command:

```
sos
```

in one and 

```
ghcid --command="stack exec ghci app/Demo.hs" --test="gen"
```
in the other. 

Next right-click, in VS Code's browser window, on the **examples** directory
and select **New File** and create a file with the extension **.mi**, e.g.
**example1.mi**, and open it for editing.

OK, let's add our first control by adding the following to interface file:

```haskell
lightpad = pad ! #x 0 ! #y 0 ! #size 3 
               ! #address "/midicc" # iargs [100]
               # fill "rgb(217,137,188)"

interface = lightpad
```

saving the file will cause it to be automactially compiled to
examples/examples1.svg. Assuming everything went as planned open the SVG file
and run the command (Shift-CMD-P) **SVG Viewer** to preview the rendered 
interface in VS Code.

For a walk through building a complete interface see the
[tutorial](https://muses-dmi.github.io/projects_src/svg_interfaces/).

#  <span style="color:#F3B73B">More Information</span>

Parent project

   - [Muses](https://muses-dmi.github.io/).

Tool and documentation for specification of interfaces as SVGs:

   - [SVG Creator tool](https://github.com/muses-dmi/svg-creator). (This repo.)
   - [SVG Interface Documentation](https://github.com/muses-dmi/svg-creator/blob/master/docs/interfaces.md).

Tools for translating SVG Interfaces to the JSON intermidiate representation and different backends:

   - [SVG Interface to IR tool](https://github.com/muses-dmi/svg_interface).
   - [Interface IR to Littlefoot tool](https://github.com/muses-dmi/svg-littlefoot).
   - [SVG Sensel Driver](https://github.com/muses-dmi/sensel_osc).

#  <span style="color:#F3B73B">License</span>

Some of the code for the underlying SVGß DSL comes from
[svg-builder](https://github.com/diagrams/svg-builder), and even in the case of
changes to those original files its license applies.

Otherwise licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 * [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)

at your option.

Dual MIT/Apache2 is strictly more permissive