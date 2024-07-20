package com.skraba.byexample.scala.ammonite.validator

import com.skraba.docoptcli.AnsiConsole

/** A setting configures a named value, like a variable or context or configuration. There are many libraries that do
  * this. This is mine for ammonite.
  *
  * @param v
  *   An optional explicit value to be assigned to this setting. This is usually done programmatically or from the
  *   command line.
  * @param props
  *   A list of properties that can be used to determine the value of a setting in this system by looking it up by the
  *   propKey.
  * @param propKey
  *   The property key (or name) that is linked to this value.
  * @param propFn
  *   A function that turns the string (if any) in the properties into the correct value type, but only if properties
  *   are being used to construct the value.
  * @param doc
  *   Some user-friendly documentation for how the setting value is used.
  * @param defaultFn
  *   A function to generate the default value for this setting.
  * @tparam T
  *   The type of the setting value.
  */
case class Setting[T](
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
  def toString(out: AnsiConsole): String = if (isDefault) out.green(this) else out.right(this)
}

object Setting {

  /** @param v
    *   An optional explicit string value to be assigned to this setting.
    * @param props
    *   A list of properties that can be used to determine the value of a setting in this system by looking it up by the
    *   propKey.
    * @param propKey
    *   The property key (or name) that is linked to this value.
    * @param doc
    *   Some user-friendly documentation for how the setting value is used.
    * @param d
    *   A function to generate the default string value for this setting.
    */
  def str(v: Option[String], props: Map[String, String], propKey: String, doc: String)(
      d: => String = ""
  ): Setting[String] =
    Setting(v, props, propKey, identity, doc)(d)

  /** @param v
    *   An optional explicit boolean value to be assigned to this setting.
    * @param props
    *   A list of properties that can be used to determine the value of a setting in this system by looking it up by the
    *   propKey.
    * @param propKey
    *   The property key (or name) that is linked to this value.
    * @param doc
    *   Some user-friendly documentation for how the setting value is used.
    * @param d
    *   A function to generate the default boolean value for this setting.
    */
  def bool(v: Option[Boolean], props: Map[String, String], propKey: String, doc: String)(
      d: => Boolean = false
  ): Setting[Boolean] =
    Setting(v, props, propKey, _.toBoolean, doc)(d)

  /** @param base
    *   The base directory to use in case the discovered value is not an absoluate path.
    * @param v
    *   An optional explicit string value to be assigned to this setting as a path, resolving if necessary.
    * @param props
    *   A list of properties that can be used to determine the value of a setting in this system by looking it up by the
    *   propKey.
    * @param propKey
    *   The property key (or name) that is linked to this value. If relative, this will be resolved from the base
    *   directory.
    * @param doc
    *   Some user-friendly documentation for how the setting value is used.
    * @param d
    *   A function to generate the default path value for this setting.
    */
  def path(base: os.Path, v: Option[String], props: Map[String, String], propKey: String, doc: String)(
      d: => os.Path
  ): Setting[os.Path] =
    Setting(v.map(os.FilePath(_).resolveFrom(base)), props, propKey, p => os.FilePath(p).resolveFrom(base), doc)(
      d
    )

  def properties(out: AnsiConsole, cfgs: Setting[?]*): String =
    cfgs.map(c => s"${out.left(c.propKey)}=${c.toString(out)}").mkString("\n")
}
