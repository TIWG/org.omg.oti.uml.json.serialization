# OTI UML Json serialization

This project provides support for constructing a Json representation of a set of OTI Documents corresponding
to UML Packages or Profiles. The Json representation is based on simple data structures
corresponding to a normalized database schema where:


- Every UML metaclass in the CMOF metamodel maps to a database table with a single primary key,
  the element's tool-specific ID and columns for all the scalar data properties in the metaclass
  (i.e. UML Properties typed by a primitive type or a UML Enumeration)

- Every UML association in the CMOF metamodel maps to a database relation with two foreign keys
  corresponding to the primary keys of the directed association's first and second ends plus an
  index if the end is ordered.

This project provides a small library enabling a tool-specific OTI UML Adapter to map

## Relationship to OMG's XMI serialization

At the OMG, UML models are serialized according to OMG's XMI serialization format,
which is an XML Schema-based tree representation of the contents of the UML model.
This serialization has been a significant source of complexity that led to several attempts
to simplify it -- see for example OMG's XMI 2.5 Canonical XMI appendix.

### Differences in terms of the serialization format:

- XMI serialization is a tree, the depth of the tree corresponds to the nesting of UML element ownership

- OTI UML Json serialization is very shallow; the depth is independent of the nesting of UML element ownership

### Differences in terms of serialization/deserialization performance:

- XMI serialization/deserialization is inherently slow because extensive cross-references within a
  single XMI document (i.e. `xmi:id` / `xmi:idref`) limit the effectiveness of parallelism.

- The normalized database tables & relations involved in OTI UML Json provide opportunities to
  exploit parallelism for serialization and deserialization.

### Proof of concept example.

Loading a ~ 560K UML element (Europa Architecture + Framework, version from September 2015) in MagicDraw 18.0 SP5
takes ~ 4 minutes.

Serializing all the UML information for a significant subset of that model to Json is significantly faster:

```
JsonExportAsOTIDocumentSetConfiguration.overall (20 OTI document packages totalling 536596 elements) in 1 minute, 12 seconds, 982 milliseconds
```

Note that this serialization does not include (yet) the Json representation corresponding to
stereotypes applied to UML elements and to their tag property values.
