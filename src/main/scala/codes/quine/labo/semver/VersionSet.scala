package codes.quine.labo.semver

import Version._
import VersionSet._

final case class VersionSet(set: List[List[Condition]]) {
  def contains(v: Version): Boolean =
    set.exists(_.forall(_.contains(v)))
}

object VersionSet {
  sealed abstract class Op

  object Op {
    final case object LT extends Op
    final case object GT extends Op
    final case object LE extends Op
    final case object GE extends Op
    final case object EQ extends Op
  }

  final case class Condition(op: Op, value: Version) {
    def contains(v: Version): Boolean =
      op match {
        case Op.LT => v < value
        case Op.LE => v <= value
        case Op.GT => v > value
        case Op.GE => v >= value
        case Op.EQ => v == value
      }
  }

  def parse(string: String): Option[VersionSet] =
    traverse(orSepR.split(string).toList) { s =>
      traverse(spaceSepR.split(hyphenSepR.replaceAllIn(s, "-")).toList)(parseCondition(_))
        .map(_.flatten)
    }.map(VersionSet(_))

  private[this] def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
    var xs = list
    val ys = List.newBuilder[B]

    while (xs.nonEmpty) {
      val x = xs.head
      xs = xs.tail
      f(x) match {
        case None    => return None
        case Some(y) => ys.addOne(y)
      }
    }

    Some(ys.result())
  }

  private[this] val orSepR = raw"\s*\|\|\s*".r
  private[this] val hyphenSepR = raw"\s*-\s*".r
  private[this] val spaceSepR = raw"\s+".r

  private[this] def partial(digits: String): String =
    // major.minor.patch (minor and patch are optional)
    raw"""($digits)(?:\.($digits)(?:\.($digits)""" +
      // -prerelease (optional)
      """(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?""" +
      // +build (optional)
      """(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)?)?"""

  private[this] val simpleR = raw"^([<>]=?|=|~|\^)?${partial(raw"[0xX*]|[1-9]\d*")}$$".r
  private[this] val hyphenR = s"^${partial(raw"0|[1-9]\d*")}-${partial(raw"0|[1-9]\d*")}$$".r

  private[this] def parseInt(s: String): Option[Int] = Option(s).flatMap(_.toIntOption)

  private[this] def parseCondition(string: String): Option[List[Condition]] =
    string match {
      case simpleR(op, major, minor, patch, prerelease, _) => {
        val list = Option(op) match {
          case Some("<") =>
            parseLT(major, minor, patch, Prerelease.parse(prerelease))
          case Some("<=") =>
            parseLE(major, minor, patch, Prerelease.parse(prerelease))
          case Some(">") =>
            parseGT(major, minor, patch, Prerelease.parse(prerelease))
          case Some(">=") =>
            parseGE(major, minor, patch, Prerelease.parse(prerelease))
          case Some("~") =>
            parseTilde(major, minor, patch, Prerelease.parse(prerelease))
          case _ =>
            parseEQ(major, minor, patch, Prerelease.parse(prerelease))
        }
        Some(list)
      }
      case hyphenR(major1, minor1, patch1, prerelease1, _, major2, minor2, patch2, prerelease2, _) => {
        val v1 = parseGE(major1, minor1, patch1, Prerelease.parse(prerelease1))
        val v2 = parseLE(major2, minor2, patch2, Prerelease.parse(prerelease2))
        Some(v1 ++ v2)
      }
      case _ => None
    }

  private[this] def parseEQ(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List.empty
      case (Some(major), None, _) =>
        List(Condition(Op.GE, Version(major, 0, 0)), Condition(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(
          Condition(Op.GE, Version(major, minor, 0)),
          Condition(Op.LT, Version(major, minor + 1, 0, Prerelease.zero))
        )
      case (Some(major), Some(minor), Some(patch)) =>
        List(Condition(Op.EQ, Version(major, minor, patch, prerelease)))
    }

  private[this] def parseLT(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List(Condition(Op.LT, Version(0, 0, 0, Prerelease.zero)))
      case (Some(major), None, _) =>
        List(Condition(Op.LT, Version(major, 0, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(Condition(Op.LT, Version(major, minor, 0, Prerelease.zero)))
      case (Some(major), Some(minor), Some(patch)) =>
        List(Condition(Op.LT, Version(major, minor, patch, prerelease)))
    }

  private[this] def parseLE(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List.empty
      case (Some(major), None, _) =>
        List(Condition(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(Condition(Op.LT, Version(major, minor + 1, 0, Prerelease.zero)))
      case (Some(major), Some(minor), Some(patch)) =>
        List(Condition(Op.LE, Version(major, minor, patch, prerelease)))
    }

  private[this] def parseGT(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List(Condition(Op.LT, Version(0, 0, 0, Prerelease.zero)))
      case (Some(major), None, _) =>
        List(Condition(Op.GE, Version(major + 1, 0, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(Condition(Op.GE, Version(major, minor + 1, 0, Prerelease.zero)))
      case (Some(major), Some(minor), Some(patch)) =>
        List(Condition(Op.GT, Version(major, minor, patch, prerelease)))
    }

  private[this] def parseGE(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List.empty
      case (Some(major), None, _) =>
        List(Condition(Op.GE, Version(major, 0, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(Condition(Op.GE, Version(major, minor, 0, Prerelease.zero)))
      case (Some(major), Some(minor), Some(patch)) =>
        List(Condition(Op.GE, Version(major, minor, patch, prerelease)))
    }

  private[this] def parseTilde(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List.empty
      case (Some(major), None, _) =>
        List(Condition(Op.GE, Version(major, 0, 0)), Condition(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(
          Condition(Op.GE, Version(major, minor, 0)),
          Condition(Op.LT, Version(major, minor + 1, 0, Prerelease.zero))
        )
      case (Some(major), Some(minor), Some(patch)) =>
        List(
          Condition(Op.GE, Version(major, minor, patch, prerelease)),
          Condition(Op.LT, Version(major, minor + 1, 0, Prerelease.zero))
        )
    }

  private[this] def parseCaret(major: String, minor: String, patch: String, prerelease: Prerelease): List[Condition] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (None, _, _) => List.empty
      case (Some(major), None, _) =>
        List(Condition(Op.GE, Version(major, 0, 0)), Condition(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
      case (Some(0), Some(minor), None) =>
        List(Condition(Op.GE, Version(0, minor, 0)), Condition(Op.LT, Version(0, minor + 1, 0, Prerelease.zero)))
      case (Some(0), Some(minor), Some(patch)) =>
        List(Condition(Op.GE, Version(0, minor, patch)), Condition(Op.LT, Version(0, minor + 1, 0, Prerelease.zero)))
      case (Some(major), Some(minor), None) =>
        List(
          Condition(Op.GE, Version(major, minor, 0)),
          Condition(Op.LT, Version(major + 1, minor, 0, Prerelease.zero))
        )
      case (Some(major), Some(minor), Some(patch)) =>
        List(
          Condition(Op.GE, Version(major, minor, patch, prerelease)),
          Condition(Op.LT, Version(major + 1, minor, 0, Prerelease.zero))
        )
    }

  private[this] def parseVersion(major: String, minor: String, patch: String, prerelease: Prerelease): Option[Version] =
    (parseInt(major), parseInt(minor), parseInt(patch)) match {
      case (Some(major), Some(minor), Some(patch)) =>
        Some(Version(major, minor, patch, prerelease))
      case _ => None
    }
}
