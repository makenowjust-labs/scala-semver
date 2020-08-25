package codes.quine.labo.semver

import minitest.SimpleTestSuite

import Version._
import VersionSet._

object VersionSetSuite extends SimpleTestSuite {
  val Version011p0: Version = Version(0, 1, 1, Prerelease(List(Right("p0"))))
  val Version111p0: Version = Version(1, 1, 1, Prerelease(List(Right("p0"))))

  val AnyVersionSet: VersionSet = VersionSet(List(List.empty))
  val EmptyVersionSet: VersionSet = VersionSet(List(List(Constraint(Op.LT, Version(0, 0, 0, Prerelease.zero)))))
  val Version1Set: VersionSet = VersionSet(
    List(List(Constraint(Op.GE, Version(1, 0, 0)), Constraint(Op.LT, Version(2, 0, 0, Prerelease.zero))))
  )
  val Version11Set: VersionSet = VersionSet(
    List(List(Constraint(Op.GE, Version(1, 1, 0)), Constraint(Op.LT, Version(1, 2, 0, Prerelease.zero))))
  )
  val Version111Set: VersionSet = VersionSet(List(List(Constraint(Op.EQ, Version(1, 1, 1)))))
  val Version1And2Set: VersionSet = VersionSet(
    List(
      List(
        Constraint(Op.GE, Version(1, 0, 0)),
        Constraint(Op.LT, Version(2, 0, 0, Prerelease.zero)),
        Constraint(Op.GE, Version(2, 0, 0)),
        Constraint(Op.LT, Version(3, 0, 0, Prerelease.zero))
      )
    )
  )

  test("VersionSet.parse") {
    val testCases = List(
      "" -> AnyVersionSet,
      "  " -> AnyVersionSet,
      "*" -> AnyVersionSet,
      " * " -> AnyVersionSet,
      "1" -> Version1Set,
      "1.*" -> Version1Set,
      "1.x" -> Version1Set,
      "1.X" -> Version1Set,
      "1.*.1" -> Version1Set,
      "1.*.1-pre" -> Version1Set,
      "1.1" -> Version11Set,
      "1.1.*" -> Version11Set,
      "1.1.x" -> Version11Set,
      "1.1.X" -> Version11Set,
      "1.1.1" -> Version111Set,
      "=1" -> Version1Set,
      "=1.1" -> Version11Set,
      "=1.1.1" -> Version111Set,
      "= 1" -> Version1Set,
      "= 1.1" -> Version11Set,
      "= 1.1.1" -> Version111Set,
      "1 2" -> Version1And2Set,
      " 1 2 " -> Version1And2Set,
      "=1 =2 " -> Version1And2Set,
      ">=1.0.0 <2.0.0-0" -> Version1Set,
      ">=1.0.0 <2.0.0-0 >=2.0.0 <3.0.0-0" -> Version1And2Set,
      "<1.1.1" -> VersionSet(List(List(Constraint(Op.LT, Version(1, 1, 1))))),
      "< 1.1.1" -> VersionSet(List(List(Constraint(Op.LT, Version(1, 1, 1))))),
      "<=1.1.1" -> VersionSet(List(List(Constraint(Op.LE, Version(1, 1, 1))))),
      "<= 1.1.1" -> VersionSet(List(List(Constraint(Op.LE, Version(1, 1, 1))))),
      ">1.1.1" -> VersionSet(List(List(Constraint(Op.GT, Version(1, 1, 1))))),
      "> 1.1.1" -> VersionSet(List(List(Constraint(Op.GT, Version(1, 1, 1))))),
      ">=1.1.1" -> VersionSet(List(List(Constraint(Op.GE, Version(1, 1, 1))))),
      ">= 1.1.1" -> VersionSet(List(List(Constraint(Op.GE, Version(1, 1, 1))))),
      "<1.1.1-p0" -> VersionSet(List(List(Constraint(Op.LT, Version111p0)))),
      "<=1.1.1-p0" -> VersionSet(List(List(Constraint(Op.LE, Version111p0)))),
      ">1.1.1-p0" -> VersionSet(List(List(Constraint(Op.GT, Version111p0)))),
      ">=1.1.1-p0" -> VersionSet(List(List(Constraint(Op.GE, Version111p0)))),
      ">*" -> EmptyVersionSet,
      "<*" -> EmptyVersionSet,
      ">=*" -> AnyVersionSet,
      "<=*" -> AnyVersionSet,
      ">1" -> VersionSet(List(List(Constraint(Op.GE, Version(2, 0, 0))))),
      ">=1" -> VersionSet(List(List(Constraint(Op.GE, Version(1, 0, 0))))),
      "<1" -> VersionSet(List(List(Constraint(Op.LT, Version(1, 0, 0, Prerelease.zero))))),
      "<=1" -> VersionSet(List(List(Constraint(Op.LT, Version(2, 0, 0, Prerelease.zero))))),
      ">1.1" -> VersionSet(List(List(Constraint(Op.GE, Version(1, 2, 0))))),
      ">=1.1" -> VersionSet(List(List(Constraint(Op.GE, Version(1, 1, 0))))),
      "<1.1" -> VersionSet(List(List(Constraint(Op.LT, Version(1, 1, 0, Prerelease.zero))))),
      "<=1.1" -> VersionSet(List(List(Constraint(Op.LT, Version(1, 2, 0, Prerelease.zero))))),
      "~1" -> Version1Set,
      "~ 1" -> Version1Set,
      "~1.1" -> Version11Set,
      "~1.1.1" -> VersionSet(
        List(List(Constraint(Op.GE, Version(1, 1, 1)), Constraint(Op.LT, Version(1, 2, 0, Prerelease.zero))))
      ),
      "~1.1.1-p0" -> VersionSet(
        List(List(Constraint(Op.GE, Version111p0), Constraint(Op.LT, Version(1, 2, 0, Prerelease.zero))))
      ),
      "^1" -> Version1Set,
      "^ 1" -> Version1Set,
      "^1.1" -> VersionSet(
        List(List(Constraint(Op.GE, Version(1, 1, 0)), Constraint(Op.LT, Version(2, 0, 0, Prerelease.zero))))
      ),
      "^1.1.1" -> VersionSet(
        List(List(Constraint(Op.GE, Version(1, 1, 1)), Constraint(Op.LT, Version(2, 0, 0, Prerelease.zero))))
      ),
      "^1.1.1-p0" -> VersionSet(
        List(List(Constraint(Op.GE, Version111p0), Constraint(Op.LT, Version(2, 0, 0, Prerelease.zero))))
      ),
      "^0.1" -> VersionSet(
        List(List(Constraint(Op.GE, Version(0, 1, 0)), Constraint(Op.LT, Version(0, 2, 0, Prerelease.zero))))
      ),
      "^0.1.1" -> VersionSet(
        List(List(Constraint(Op.GE, Version(0, 1, 1)), Constraint(Op.LT, Version(0, 2, 0, Prerelease.zero))))
      ),
      "^0.1.1-p0" -> VersionSet(
        List(List(Constraint(Op.GE, Version011p0), Constraint(Op.LT, Version(0, 2, 0, Prerelease.zero))))
      ),
      "1 - 2" -> VersionSet(
        List(List(Constraint(Op.GE, Version(1, 0, 0)), Constraint(Op.LT, Version(3, 0, 0, Prerelease.zero))))
      ),
      "1.2.3 - 3.4" -> VersionSet(
        List(List(Constraint(Op.GE, Version(1, 2, 3)), Constraint(Op.LT, Version(3, 5, 0, Prerelease.zero))))
      ),
      "1.2.3 - 3.4.5" -> VersionSet(
        List(List(Constraint(Op.GE, Version(1, 2, 3)), Constraint(Op.LE, Version(3, 4, 5))))
      ),
      "1.2.3 || 4.5.6" -> VersionSet(
        List(List(Constraint(Op.EQ, Version(1, 2, 3))), List(Constraint(Op.EQ, Version(4, 5, 6))))
      ),
      "1.2.3 || " -> VersionSet(List(List(Constraint(Op.EQ, Version(1, 2, 3))), List())),
      "|| 4.5.6" -> VersionSet(List(List(), List(Constraint(Op.EQ, Version(4, 5, 6)))))
    )

    for ((string, set) <- testCases) {
      val result = VersionSet.parse(string)
      assertEquals(result, Some(set))
    }

    val errorCases = List("a", "1a", "1.2.3.4", "1-2", "1.2.3x", "<>1.1.1", "a - b")

    for (string <- errorCases) {
      val result = VersionSet.parse(string)
      assertEquals(result, None)
    }
  }

  test("VersionSet#contains") {
    val testCases = List(
      AnyVersionSet -> (
        Set(Version(0, 0, 0, Prerelease.zero), Version(0, 1, 1), Version(1, 0, 0), Version(2, 0, 0)),
        Set.empty,
      ),
      EmptyVersionSet -> (
        Set.empty,
        Set(Version(0, 0, 0, Prerelease.zero), Version(0, 1, 1), Version(1, 0, 0), Version(2, 0, 0)),
      ),
      Version1Set -> (
        Set(Version(1, 0, 0), Version(1, 1, 0), Version(1, 1, 1)),
        Set(Version(1, 0, 0, Prerelease.zero), Version(0, 0, 0), Version(2, 0, 0)),
      )
    )

    for ((set, (ok, no)) <- testCases) {
      for (v <- ok) {
        assertEquals(set.contains(v), true)
      }
      for (v <- no) {
        assertEquals(set.contains(v), false)
      }
    }
  }
}
