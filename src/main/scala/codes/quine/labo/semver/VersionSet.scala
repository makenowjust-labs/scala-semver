package codes.quine.labo.semver

import Version._
import VersionSet._

/**
  * This represents set of Semantic versioning versions.
  *
  * It holds constraints, and its elements satisfy them.
  *
  * @param constraints DNF list of Constraint
  */
final case class VersionSet(constraints: List[List[Constraint]]) {

  /** Check inclusion of a Version in this set. */
  def contains(v: Version): Boolean =
    constraints.exists(_.forall(_.satisfy(v)))

  override def toString: String =
    constraints.map(_.mkString(" ")).mkString(" || ")
}

object VersionSet {

  /** This represents contraints operators. */
  sealed abstract class Op

  object Op {
    final case object LT extends Op
    final case object GT extends Op
    final case object LE extends Op
    final case object GE extends Op
    final case object EQ extends Op
  }

  /**
    * This represents version contraint like =1.2.3 or >=1.2.3.
    */
  final case class Constraint(op: Op, value: Version) {

    /** Check whether a version satisy this constraint or not. */
    def satisfy(v: Version): Boolean =
      op match {
        case Op.LT => v < value
        case Op.LE => v <= value
        case Op.GT => v > value
        case Op.GE => v >= value
        case Op.EQ => v == value
      }

    override def toString: String =
      op match {
        case Op.LT => s"<$value"
        case Op.LE => s"<=$value"
        case Op.GT => s">$value"
        case Op.GE => s">=$value"
        case Op.EQ => s"=$value"
      }
  }

  /**
    * Parse a string as version set.
    *
    * @param string a version set string.
    * @return a result of parsing.
    *   It returns [[scala.Some Some]] value with [[VersionSet]] object if parsing is succeeded.
    *   Otherwise, when it is failed on parsing, it returns [[scala.None None]] value.
    * @see [[https://github.com/npm/node-semver#ranges]]
    */
  def parse(string: String): Option[VersionSet] =
    traverse(unionSepR.pattern.split(string, -1).toList)(Constraint.parse(_)).map(VersionSet(_))

  /**
    * Traverse a list with [[scala.Option]] effect.
    *
    * This is specialized version of [[https://typelevel.org/cats/typeclasses/traverse.html cats.Traverse]] method.
    *
    * @param list a list to traverse
    * @param f processing function
    * @return [[scala.Some Some]] with result list if all calls are succeeded.
    *   Otherwise, if any call is failed, it returns [[scala.None None]] immediately.
    */
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

  private[this] val unionSepR = raw"\s*\|\|\s*".r

  object Constraint {

    /**
      * Parse a string as version constraints.
      *
      * @param string a version constraint string
      * @return [[scala.Some Some]] when parsing is succeeded, or [[scala.None None]] for failure.
      *   A value of result is a list of contraint.
      *   In addition, an empty list means tautology constrait, which is always true.
      */
    def parse(string: String): Option[List[Constraint]] = {
      var tokens = tokenSepR
        .split(opR.replaceAllIn(string, m => s"${m.group(1)} ").trim())
        .toList
        .filterNot(_.isBlank())

      // 1. Case of `1.2.3 - 4.5.6`:
      if (tokens.size == 3 && tokens(1) == "-") {
        return tokens match {
          case List(
                partialR(major1, minor1, patch1, pre1, _),
                "-",
                partialR(major2, minor2, patch2, pre2, _)
              ) =>
            val constraints1 = parseGE(major1, minor1, patch1, Prerelease.parse(pre1))
            val constraints2 = parseLE(major2, minor2, patch2, Prerelease.parse(pre2))
            Some(constraints1 ++ constraints2)
          case _ => None
        }
      }

      // 2. Case of `>1.2.3 4.5.6`:
      val constraints = List.newBuilder[Constraint]

      while (tokens.nonEmpty) {
        tokens match {
          case partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseEQ(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case "=" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseEQ(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case "<" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseLT(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case "<=" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseLE(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case ">" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseGT(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case ">=" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseGE(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case "~" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseTilde(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case "^" :: partialR(major, minor, patch, pre, _) :: xs =>
            constraints.addAll(parseCaret(major, minor, patch, Prerelease.parse(pre)))
            tokens = xs
          case _ => return None
        }
      }

      Some(constraints.result())
    }

    private[this] val opR = raw"([<>]=?|=|\^|~)\s*".r
    private[this] val tokenSepR = raw"\s+".r

    private[this] val partialR =
      (
        // major.minor.patch (minor and patch are optional)
        """([0xX*]|[1-9]\d*)(?:\.([0xX*]|[1-9]\d*)(?:\.([0xX*]|[1-9]\d*)""" +
          // -prerelease (optional)
          """(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?""" +
          // +build (optional)
          """(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)?)?"""
      ).r

    private[this] def parseInt(s: String): Option[Int] = Option(s).flatMap(_.toIntOption)

    private[this] def parseEQ(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List.empty
        case (Some(major), None, _) =>
          List(Constraint(Op.GE, Version(major, 0, 0)), Constraint(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
        case (Some(major), Some(minor), None) =>
          List(
            Constraint(Op.GE, Version(major, minor, 0)),
            Constraint(Op.LT, Version(major, minor + 1, 0, Prerelease.zero))
          )
        case (Some(major), Some(minor), Some(patch)) =>
          List(Constraint(Op.EQ, Version(major, minor, patch, pre)))
      }

    private[this] def parseLT(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List(Constraint(Op.LT, Version(0, 0, 0, Prerelease.zero)))
        case (Some(major), None, _) =>
          List(Constraint(Op.LT, Version(major, 0, 0, Prerelease.zero)))
        case (Some(major), Some(minor), None) =>
          List(Constraint(Op.LT, Version(major, minor, 0, Prerelease.zero)))
        case (Some(major), Some(minor), Some(patch)) =>
          List(Constraint(Op.LT, Version(major, minor, patch, pre)))
      }

    private[this] def parseLE(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List.empty
        case (Some(major), None, _) =>
          List(Constraint(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
        case (Some(major), Some(minor), None) =>
          List(Constraint(Op.LT, Version(major, minor + 1, 0, Prerelease.zero)))
        case (Some(major), Some(minor), Some(patch)) =>
          List(Constraint(Op.LE, Version(major, minor, patch, pre)))
      }

    private[this] def parseGT(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List(Constraint(Op.LT, Version(0, 0, 0, Prerelease.zero)))
        case (Some(major), None, _) =>
          List(Constraint(Op.GE, Version(major + 1, 0, 0)))
        case (Some(major), Some(minor), None) =>
          List(Constraint(Op.GE, Version(major, minor + 1, 0)))
        case (Some(major), Some(minor), Some(patch)) =>
          List(Constraint(Op.GT, Version(major, minor, patch, pre)))
      }

    private[this] def parseGE(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List.empty
        case (Some(major), None, _) =>
          List(Constraint(Op.GE, Version(major, 0, 0)))
        case (Some(major), Some(minor), None) =>
          List(Constraint(Op.GE, Version(major, minor, 0)))
        case (Some(major), Some(minor), Some(patch)) =>
          List(Constraint(Op.GE, Version(major, minor, patch, pre)))
      }

    private[this] def parseTilde(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List.empty
        case (Some(major), None, _) =>
          List(Constraint(Op.GE, Version(major, 0, 0)), Constraint(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
        case (Some(major), Some(minor), None) =>
          List(
            Constraint(Op.GE, Version(major, minor, 0)),
            Constraint(Op.LT, Version(major, minor + 1, 0, Prerelease.zero))
          )
        case (Some(major), Some(minor), Some(patch)) =>
          List(
            Constraint(Op.GE, Version(major, minor, patch, pre)),
            Constraint(Op.LT, Version(major, minor + 1, 0, Prerelease.zero))
          )
      }

    private[this] def parseCaret(major: String, minor: String, patch: String, pre: Prerelease): List[Constraint] =
      (parseInt(major), parseInt(minor), parseInt(patch)) match {
        case (None, _, _) => List.empty
        case (Some(major), None, _) =>
          List(Constraint(Op.GE, Version(major, 0, 0)), Constraint(Op.LT, Version(major + 1, 0, 0, Prerelease.zero)))
        case (Some(0), Some(minor), None) =>
          List(Constraint(Op.GE, Version(0, minor, 0)), Constraint(Op.LT, Version(0, minor + 1, 0, Prerelease.zero)))
        case (Some(0), Some(minor), Some(patch)) =>
          List(
            Constraint(Op.GE, Version(0, minor, patch, pre)),
            Constraint(Op.LT, Version(0, minor + 1, 0, Prerelease.zero))
          )
        case (Some(major), Some(minor), None) =>
          List(
            Constraint(Op.GE, Version(major, minor, 0)),
            Constraint(Op.LT, Version(major + 1, 0, 0, Prerelease.zero))
          )
        case (Some(major), Some(minor), Some(patch)) =>
          List(
            Constraint(Op.GE, Version(major, minor, patch, pre)),
            Constraint(Op.LT, Version(major + 1, 0, 0, Prerelease.zero))
          )
      }
  }
}
