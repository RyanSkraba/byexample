package com.skraba.byexample.scala.ammonite.validator

import com.skraba.docoptcli.AnsiConsole

/** A single configuration value.
  *
  * @param v
  *   An optional explicit value to be assigned to this configuration. This is usually done programmatically or from the
  *   command line.
  * @param props
  *   A list of properties that can be used to determine the value of configurations in this system.
  * @param propKey
  *   The property key that is linked to this value.
  * @param propFn
  *   A function that turns the string (if any) in the properties into the correct value type, if properties are being
  *   used to construct the value.
  * @param doc
  *   Some user-friendly documentation for how the configuration value is used.
  * @param defaultFn
  *   A function to generate the default value for this configuration.
  * @tparam T
  *   The type of the configuration value.
  */
case class Cfg[T](
    v: Option[T],
    props: Map[String, String],
    propKey: String,
    propFn: String => T,
    doc: String
)(
    defaultFn: => T
) {

  /** Evaluate the default value once. */
  lazy val d = defaultFn

  /** True if this value is the default. */
  lazy val isDefault: Boolean = get.equals(d)

  /** The evaluated value, either the explicit value, from the properties or the default. */
  lazy val get: T = v.orElse(props.get(propKey).map(propFn)).getOrElse(d)

  override def toString: String = get.toString

  /** Provides a colourful representation of the value, notably different if the default or non-default is used. */
  def toString(cfg: AnsiConsole): String = if (isDefault) cfg.green(this) else cfg.right(this)
}

object Cfg {

  def str(v: Option[String], props: Map[String, String], propKey: String, doc: String)(d: => String = ""): Cfg[String] =
    Cfg(v, props, propKey, identity, doc)(d)

  def bool(v: Option[Boolean], props: Map[String, String], propKey: String, doc: String)(
      d: => Boolean = false
  ): Cfg[Boolean] =
    Cfg(v, props, propKey, _.toBoolean, doc)(d)

  def path(resolveFrom: os.Path, v: Option[String], props: Map[String, String], propKey: String, doc: String)(
      d: => os.Path
  ): Cfg[os.Path] =
    Cfg(
      v.map(os.FilePath(_).resolveFrom(resolveFrom)),
      props,
      propKey,
      p => os.FilePath(p).resolveFrom(resolveFrom),
      doc
    )(d)

  def properties(cfg: AnsiConsole, cfgs: Cfg[?]*): String =
    cfgs.map(c => s"${cfg.left(c.propKey)}=${c.toString(cfg)}").mkString("\n")
}
