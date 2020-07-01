package codes.quine.labo.semver

import scala.util.matching.Regex

import Version._

/**
  * This represents semantic versioning version.
  *
  * @param major major.X.X part number
  * @param minor X.minor.X part number
  * @param patch X.X.patch part number
  * @param prerelease X.X.X-prerelease part
  * @param build X.X.X+build part
  * @see [[https://semver.org/]]
  */
final case class Version(
    major: Int,
    minor: Int,
    patch: Int,
    prerelease: Prerelease = Prerelease.empty,
    build: Option[String] = None
) extends Ordered[Version] { self =>
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(major).append('.').append(minor).append('.').append(patch)
    if (prerelease.parts.nonEmpty) sb.append('-').append(prerelease)
    if (build.isDefined) sb.append('+').append(build.get)
    sb.result()
  }

  def compare(that: Version): Int = Ordering[Version].compare(self, that)
}

object Version {
  // This is copied from https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string and modified.
  private[this] val syntaxR =
    (
      // major.minor.patch
      """(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)""" +
        // -prerelease (optional)
        """(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?""" +
        // +build (optional)
        """(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?"""
    ).r

  /**
    * Parse a string as semantic versioning version.
    *
    * @param string a semantic versioning version string
    * @return a result of parsing.
    *   It returns [[scala.Some Some]] value with [[Version]] object if parsing is succeeded.
    *   Otherwise, when it is failed on parsing, it returns [[scala.None None]] value.
    * @see [[https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions]]
    */
  def parse(string: String): Option[Version] =
    string match {
      case syntaxR(major, minor, patch, prerelease, build) => {
        val version = Version(
          major.toInt,
          minor.toInt,
          patch.toInt,
          Prerelease.parse(prerelease),
          Option(build)
        )
        Some(version)
      }
      case _ => None
    }

  implicit val OrderingInstanceForVersionPrelease: Ordering[Prerelease] =
    // The empty prerelease is the greatest.
    Ordering[List[Either[Int, String]]] {
      case (List(), List()) => 0
      case (List(), _)      => 1
      case (_, List())      => -1
      case (_, _)           => 0
      // In other case, they are compared by usual list comparison mannar.
    }.orElse(Ordering.Implicits.seqOrdering(Ordering[Either[Int, String]] {
      case (Left(_), Right(_))  => 1
      case (Right(_), Left(_))  => -1
      case (Left(x), Left(y))   => x.compare(y)
      case (Right(x), Right(y)) => x.compare(y)
    }))
      .on(_.parts)

  implicit val OrderingInstanceForVersion: Ordering[Version] =
    Ordering.by((_: Version).major).orElseBy(_.minor).orElseBy(_.patch).orElseBy(_.prerelease)

  /**
    * This represents a prereleas part of semantic versioning version.
    *
    * @param parts parts of prerelease.
    *   It can contain either [[scala.Int Int]] or [[java.lang.String String]].
    *   When String contains only digits, it must becoma Int.
    */
  final case class Prerelease(parts: List[Either[Int, String]]) extends Ordered[Prerelease] {
    self =>
    override def toString: String = parts.map(_.fold(_.toString, identity(_))).mkString(".")
    def compare(that: Prerelease): Int = Ordering[Prerelease].compare(self, that)
  }

  object Prerelease {

    /** An empty Prerelease object. Note that it is the greatest object of any Prerelease. */
    def empty: Prerelease = Prerelease(List.empty)

    /** A Prerelease object contains a zero. Note that it is the least object of any Prerelease. */
    def zero: Prerelease = Prerelease(List(Left(0)))

    /**
      * Parse X.X.X-prerelease part of semantic versioning version.
      *
      * @param string a prerelease part string (null is possible for some reasons)
      * @return a result of parsing. It is succeeded always, so it may produces invalid Prerelease in some cases.
      */
    def parse(string: String): Prerelease =
      if (string == null) Prerelease.empty
      else {
        val parts =
          string.split('.').map(part => part.toIntOption.map(Left(_)).getOrElse(Right(part))).toList
        Prerelease(parts)
      }
  }
}
