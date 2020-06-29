package codes.quine.labo.semver

import scala.collection.mutable

import minitest.SimpleTestSuite

import Version._
import scala.util.Random

object VersionSuite extends SimpleTestSuite {
  test("Version.parse, Version#toString") {
    val testCases = mutable.LinkedHashMap(
      "0.0.0" -> Version(0, 0, 0, Prerelease.empty, None),
      "1.0.0" -> Version(1, 0, 0, Prerelease.empty, None),
      "1.2.3" -> Version(1, 2, 3, Prerelease.empty, None),
      "12.34.56" -> Version(12, 34, 56, Prerelease.empty, None),
      "1.2.3-pre.1" -> Version(1, 2, 3, Prerelease(List(Right("pre"), Left(1))), None),
      "1.2.3+build" -> Version(1, 2, 3, Prerelease.empty, Some("build"))
    )

    for ((string, version) <- testCases) {
      val result = Version.parse(string)
      assertEquals(result, Some(version))
      assertEquals(result.get.toString, string)
    }
  }

  test("Version#compare") {
    val list = List(
      Version(0, 0, 0, Prerelease(List(Right("dev"))), None),
      Version(0, 0, 0, Prerelease.empty, None),
      Version(0, 0, 1, Prerelease.empty, None),
      Version(0, 0, 2, Prerelease.empty, None),
      Version(0, 0, 2, Prerelease.empty, None),
      Version(0, 1, 0, Prerelease.empty, None),
      Version(0, 1, 1, Prerelease.empty, None),
      Version(0, 2, 0, Prerelease.empty, None),
      Version(0, 2, 1, Prerelease.empty, None),
      Version(1, 0, 0, Prerelease(List(Right("alpha"))), None),
      Version(1, 0, 0, Prerelease(List(Right("alpha"), Left(1))), None),
      Version(1, 0, 0, Prerelease(List(Right("alpha"), Left(2))), None),
      Version(1, 0, 0, Prerelease(List(Right("beta"), Left(1))), None),
      Version(1, 0, 0, Prerelease.empty, None),
      Version(1, 1, 0, Prerelease.empty, None),
      Version(2, 0, 0, Prerelease.empty, None)
    )
    assertEquals(list.sorted, list)
    assertEquals(list.reverse.sorted, list)
  }
}
