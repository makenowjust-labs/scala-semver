# scala-labo-semver

> simple semantic versioning parser/comparator implementations.

## Example

```scala
scala> import codes.quine.labo.semver._
import codes.quine.labo.semver._

scala> val v1 = Version.parse("1.2.3-alpha.1")
val v1: Option[codes.quine.labo.semver.Version] = Some(1.2.3-alpha.1)

scala> val v2 = Version.parse("1.2.3-beta")
val v2: Option[codes.quine.labo.semver.Version] = Some(1.2.3-beta)

scala> val v3 = Version.parse("1.2.3")
val v3: Option[codes.quine.labo.semver.Version] = Some(1.2.3)

scala> v1 < v2
val res0: Boolean = true

scala> v2 < v3
val res1: Boolean = true
```

## License

[CC-0 1.0](https://creativecommons.org/publicdomain/zero/1.0/)

2020 (C) TSUYUSATO "MakeNowJust" Kitsune