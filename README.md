# Chemonitor

Implements a dashboard that can be used for the visualization of e.g. analytical parameters in a chemical production process.
This is currently in a very early stage of development.
I use this repo as a testing ground to explore possible solutions and patterns I've noticed while developing dashboards professionally.
Parts of this app may be completely over-engineered or some abstractions might not make sense - well, **it's a playground**!


Currently, I'm working on trying to find a way to distribute the selection
of certain parameters to the different UI elements (data selection, filter and analyis)
in a general and abstract way, such that it will make sense for each different type of
analysis to be performed while keeping an intuitive UI for the user.


## Notes

- Implements a shiny dashboard as a package
- Uses shiny modules to segregate UI components
- Uses S3 classes `chemonitor_<x>` to
  - Clean-data (data validation)
  - Define filter variables and choices
  - Plot the data
  - Enable selecting data from the plot
- Allows upload of custom data sets

By packing this logic into the class methods, the dashboard does not need to implement complex `if-else` logic.
