Datatree API for scala.
=======================

This is an API for managing partially structured, partically typed data as an intermediate between formats like RDF/XML and JSON
on the one hand and a class hierarchy on the other. It models data as trees of typed nodes, each with named properties and property
values. Datatree is designed to support round-tripping to cannonical RDF/XML documents which enforce specific patterns of resource
nesting, ideal for use with non-RDF tooling that needs to map the XML to/from an object model without any knowledge of the semantics
of RDF.

