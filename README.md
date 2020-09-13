# scala-semver

> A simple [semver](https://semver.org/) parser/comparator implementation.

## Example

```scala
scala> import codes.quine.labo.semver._
import codes.quine.labo.semver._

scala> val Some(v1) = Version.parse("1.2.3-alpha.1")
val v1: codes.quine.labo.semver.Version = 1.2.3-alpha.1

scala> val Some(v2) = Version.parse("1.2.3-beta")
val v2: codes.quine.labo.semver.Version = 1.2.3-beta

scala> val Some(v3) = Version.parse("1.2.3")
val v3: codes.quine.labo.semver.Version = 1.2.3

scala> v1 < v2
val res0: Boolean = true

scala> v2 < v3
val res1: Boolean = true

scala> val Some(set) = VersionSet.parse("~1.2")
val set: codes.quine.labo.semver.VersionSet = >=1.2.0 <1.3.0-0

scala> set.contains(v1)
val res2: Boolean = false

scala> set.contains(v3)
val res3: Boolean = true
```

## License

MIT License.

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
