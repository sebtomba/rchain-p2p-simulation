package coop.rchain.graphz

import java.io.FileOutputStream

import cats._
import cats.effect.Sync
import cats.implicits._
import cats.mtl.MonadState

trait GraphSerializer[F[_]] {
  def push(str: String, suffix: String = "\n"): F[Unit]
}

class StringSerializer[F[_]: Applicative: MonadState[?[_], StringBuffer]]
    extends GraphSerializer[F] {
  def push(str: String, suffix: String): F[Unit] =
    MonadState[F, StringBuffer].modify(sb => sb.append(str + suffix))
}

class FileSerializer[F[_]: Sync](fos: FileOutputStream) extends GraphSerializer[F] {
  def push(str: String, suffix: String): F[Unit] = Sync[F].delay {
    fos.write(str.getBytes)
    fos.flush()
  }
}

sealed trait GraphType
case object Graph   extends GraphType
case object DiGraph extends GraphType

sealed trait GraphShape
case object Circle       extends GraphShape
case object DoubleCircle extends GraphShape
case object Box          extends GraphShape
case object PlainText    extends GraphShape
case object Msquare      extends GraphShape
case object Record       extends GraphShape

sealed trait GraphRank
case object Same   extends GraphRank
case object Min    extends GraphRank
case object Source extends GraphRank
case object Max    extends GraphRank
case object Sink   extends GraphRank

sealed trait GraphRankDir
case object TB extends GraphRankDir
case object BT extends GraphRankDir
case object LR extends GraphRankDir
case object RL extends GraphRankDir

sealed trait GraphStyle
case object Solid  extends GraphStyle
case object Bold   extends GraphStyle
case object Filled extends GraphStyle
case object Invis  extends GraphStyle
case object Dotted extends GraphStyle
case object Dashed extends GraphStyle

sealed trait GraphArrowType
case object NormalArrow extends GraphArrowType
case object InvArrow    extends GraphArrowType
case object NoneArrow   extends GraphArrowType

sealed trait GraphOutputMode
case object BreadthFirst extends GraphOutputMode
case object NodesFirst   extends GraphOutputMode
case object EdgesFirst   extends GraphOutputMode

object Graphz {

  implicit val showShape: Show[GraphShape] = {
    case Circle       => "circle"
    case DoubleCircle => "doublecircle"
    case Box          => "box"
    case PlainText    => "plaintext"
    case Msquare      => "Msquare"
    case Record       => "record"
  }

  def smallToString[A]: Show[A] = (a: A) => a.toString.toLowerCase

  implicit val showStyle: Show[GraphStyle]     = smallToString[GraphStyle]
  implicit val showRank: Show[GraphRank]       = smallToString[GraphRank]
  implicit val showRankDir: Show[GraphRankDir] = Show.fromToString[GraphRankDir]
  implicit val showArrowType: Show[GraphArrowType] = {
    case NormalArrow => "normal"
    case InvArrow    => "inv"
    case NoneArrow   => "none"
  }
  implicit val showOutputMode: Show[GraphOutputMode] = smallToString[GraphOutputMode]

  def DefaultShape: GraphShape = Circle

  def apply[F[_]: Monad](
      name: String,
      gtype: GraphType,
      subgraph: Boolean = false,
      level: Int = 0,
      comment: Option[String] = None,
      label: Option[String] = None,
      outputorder: Option[GraphOutputMode] = None,
      size: Option[String] = None,
      margin: Option[String] = None,
      style: Option[String] = None,
      splines: Option[String] = None,
      color: Option[String] = None,
      bgcolor: Option[String] = None,
      node: Map[String, String] = Map.empty,
      edge: Map[String, String] = Map.empty
  )(
      implicit ser: GraphSerializer[F]
  ): F[Graphz[F]] = {

    val indent = tabs(level + 1)

    def insert(str: Option[String], v: String => String): F[Unit] =
      str.fold(().pure[F])(s => ser.push(indent + v(s)))

    for {
      _ <- comment.fold(().pure[F])(c => ser.push(s"// $c"))
      _ <- ser.push(head(gtype, subgraph, level, name))
      _ <- insert(label, l => s"label=${quote(l)}")
      _ <- insert(style, s => s"style=$s")
      _ <- insert(size, s => s"size=${quote(s)}")
      _ <- insert(margin, s => s"margin=$s")
      _ <- insert(splines.map(_.show), s => s"splines=$s")
      _ <- insert(outputorder.map(_.show), s => s"outputorder=${quote(s)}")
      _ <- insert(color, s => s"color=${quote(s)}")
      _ <- insert(bgcolor, s => s"bgcolor=${quote(s)}")
      _ <- insert(attrMkStr(node), n => s"node $n")
      _ <- insert(attrMkStr(edge), n => s"edge $n")
    } yield new Graphz[F](gtype, indent)
  }

  def subgraph[F[_]: Monad](
      name: String,
      gtype: GraphType,
      level: Int,
      label: Option[String] = None,
      style: Option[String] = None,
      color: Option[String] = None,
      bgcolor: Option[String] = None,
      node: Map[String, String] = Map.empty,
      edge: Map[String, String] = Map.empty
  )(implicit ser: GraphSerializer[F]): F[Graphz[F]] =
    apply[F](
      name,
      gtype,
      subgraph = true,
      level = level,
      label = label,
      style = style,
      color = color,
      bgcolor = bgcolor,
      node = node,
      edge = edge
    )

  private def head(gtype: GraphType, subgraph: Boolean, level: Int, name: String): String = {
    val prefix = (gtype, subgraph) match {
      case (_, true)    => s"${tabs(level)}subgraph"
      case (Graph, _)   => s"graph"
      case (DiGraph, _) => s"digraph"
    }
    if (name == "") s"$prefix {" else s"""$prefix "$name" {"""
  }

  def quote(str: String): String = str match {
    case _ if str.startsWith("\"") => str
    case _                         => s""""$str""""
  }

  def attrMkStr(attr: Map[String, String]): Option[String] =
    if (attr.isEmpty) None
    else
      Some("[" + attr.map(t => t._1 + "=" + t._2).mkString(" ") + "]")

  def nodesMkStr(nodes: Set[String]): String = {
    val str = nodes.map(quote)
    if (str.size == 1) str.head
    else str.mkString("{", ",", "}")
  }

  val tab = "  "
  def tabs(level: Int): String = {
    assert(level >= 0, "Level must not be negative")
    (0 until level).map(_ => tab).mkString
  }
}

class Graphz[F[_]: Monad](gtype: GraphType, t: String)(implicit ser: GraphSerializer[F]) {

  private val locale    = new java.util.Locale("en", "US")
  private val formatter = java.text.NumberFormat.getInstance(locale)

  def edge(edg: (String, String)): F[Unit] = edges(Set(edg._1), Set(edg._2))
  def edges(
      src: Set[String],
      dst: Set[String],
      style: Option[GraphStyle] = None,
      arrowHead: Option[GraphArrowType] = None
  ): F[Unit] =
    if (src.nonEmpty && dst.nonEmpty) {
      import Graphz.{showArrowType, showStyle}
      val attrStyle: Map[String, String] =
        style.map(s => Map("style" -> s.show)).getOrElse(Map.empty)
      val attrArrowHead: Map[String, String] =
        arrowHead.map(s => Map("arrowhead" -> s.show)).getOrElse(Map.empty)
      val attrs: Map[String, String] = attrStyle |+| attrArrowHead
      ser.push(
        edgeMkStr.format(
          Graphz.nodesMkStr(src),
          Graphz.nodesMkStr(dst),
          Graphz.attrMkStr(attrs).map(a => " " + a).getOrElse("")
        )
      )
    } else ().pure[F]

  def edges(src: String, dst: Set[String]): F[Unit] =
    edges(Set(src), dst)

  def edges(src: Set[String], dst: String): F[Unit] =
    edges(src, Set(dst))

  def node(
      name: String,
      shape: GraphShape = Circle,
      style: Option[GraphStyle] = None,
      height: Option[Float] = None,
      color: Option[String] = None,
      fillcolor: Option[String] = None,
      label: Option[String] = None
  ): F[Unit] = {
    import Graphz.{showShape, showStyle}
    val attrShape: Map[String, String] =
      if (shape == Graphz.DefaultShape) Map.empty else Map("shape" -> shape.show)
    val attrStyle: Map[String, String] = style.map(s => Map("style" -> s.show)).getOrElse(Map.empty)
    val attrColor: Map[String, String] =
      color.map(c => Map("color" -> Graphz.quote(c))).getOrElse(Map.empty)
    val attrFillcolor: Map[String, String] =
      fillcolor.map(c => Map("fillcolor" -> Graphz.quote(c))).getOrElse(Map.empty)
    val attrHeight: Map[String, String] =
      height.map(c => Map("height" -> formatter.format(c))).getOrElse(Map.empty)
    val attrLabel: Map[String, String] = label.map(c => Map("label" -> c)).getOrElse(Map.empty)

    val attrs: Map[String, String] =
      attrShape |+| attrColor |+| attrFillcolor |+| attrLabel |+| attrStyle |+| attrHeight
    ser.push(t + Graphz.quote(name) + Graphz.attrMkStr(attrs).map(a => " " + a).getOrElse(""))
  }

  def subgraph(sub: F[Graphz[F]]): F[Unit] = sub >>= (_ => ser.push(""))
  def close: F[Unit]                       = ser.push(s"${t.substring(Graphz.tab.length)}}", suffix = "")

  private def edgeMkStr: String = gtype match {
    case Graph   => s"$t%s -- %s%s"
    case DiGraph => s"$t%s -> %s%s"
  }
}
