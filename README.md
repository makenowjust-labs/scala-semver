# scala-semver

> A simple [semver](https://semver.org/) parser/comparator implementation.

## Example

```scala
scala> import codes.quine.labo.semver._
import codes.quine.labo.semver._

scala> val Some(v1) = Version.parse("1.2.3-alpha.1")
v1: Version = 1.2.0-alpha.1

scala> val Some(v2) = Version.parse("1.2.3-beta")
v2: Version = 1.2.3-beta

scala> val Some(v3) = Version.parse("1.2.3")
v3: Version = 1.2.3

scala> v1 < v2
res0: Boolean = true

scala> v2 < v3
res1: Boolean = true

scala> val Some(set) = VersionSet.parse("~1.2.3")
set: VersionSet = >=1.2.3 <1.3.0-0

scala> set.contains(v1)
res2: Boolean = false

scala> set.contains(v3)
res3: Boolean = true
```

## License

MIT License.

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
