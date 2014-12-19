GRAPHDEP
========

GraphDep creates SVG graph from coming xml dependency analysis result in your home directory called depGraph.svg. It uses SBT so to run it, simple use : `sbt "run xml_file_absolute_path"`

Currently, you can only generate one kind of graph which shows you the amount of new and deleted dependencies day by day :

__Legend :__
* White circles are days where there are no dependency changes
* Black circle are days where there are dependency changes
* Blue vertical lines correspond to the number of new dependencies (above, you have the precise number)
* Red vertical line, as the previous item, correspond to the number of deleted dependencies
