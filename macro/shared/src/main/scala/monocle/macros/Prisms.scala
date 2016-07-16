package monocle.macros

import scala.reflect.macros.blackbox

class Prisms(prefix: String = "") extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PrismsImpl.prismsAnnotationMacro
}

@macrocompat.bundle
private[macros] class PrismsImpl(val c: blackbox.Context) {

  def prismsAnnotationMacro(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    // TODO this boilerplate is just to access the annotation's `prefix` argument. Isn't there an easier way?
    val prefix = c.macroApplication match {
      case Apply(Select(Apply(Select(New(Ident(TypeName("Prisms"))), t), args), _), _) if t == termNames.CONSTRUCTOR => args match {
        case Literal(Constant(s: String)) :: Nil => s
        case _ => ""
      }
      case _ => ""
    }

    /**
      * This doesn't work at all, because the macro annotation runs before the typer.
      * This means that knownDirectSubclasses can't find the subclasses and will always return an empty Set.
      *
      * I'm pretty confident it would be fixed by Miles's fix for SI-7046 (https://github.com/scala/scala/pull/5284)
      */
    def findSubclasses(typeName: TypeName): Set[TypeName] = {
      // Note: disable macros to prevent stack overflow caused by infinite typing loop!
      val tpe = c.typecheck(Ident(typeName), mode = c.TYPEmode, silent = true, withMacrosDisabled = true)

      tpe.symbol.asClass.knownDirectSubclasses.map(_.asType.name)
    }

    def prisms(parentTypename: TypeName, childNames: Set[TypeName]): Set[Tree] = childNames.map { childName =>
      val prismName = TermName(prefix + childName.decodedName.toString.toLowerCase)
      q"""val $prismName =
        monocle.macros.GenPrism.apply[$parentTypename, $childName]"""
    }

    /*
    A note about what's going on in this match, thanks to Brendan McAdams:

    - If you annotate a class with a companion object, both are passed in as `annottees`.
      - If you annotate an object without a companion class, only the object is passed in.
    - You must return both from your macro.

    https://speakerdeck.com/bwmcadams/nescala-16-scala-macros-for-mortals-or-how-i-learned-to-stop-worrying-and-mumbling-wtf
    (slide 22)

    We also need to match sealed traits and sealed abstract classes as separate cases.
     */
    val result = annottees map (_.tree) match {
      // Sealed abstract class
      case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
        :: Nil if mods.hasFlag(Flag.SEALED) =>
        val name = tpname.toTermName
        q"""
         $classDef
         object $name {
           ..${prisms(tpname, findSubclasses(tpname))}
         }
         """
      case (classDef @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
        :: q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        :: Nil if mods.hasFlag(Flag.SEALED) =>
        q"""
         $classDef
         object $objName extends { ..$objEarlyDefs} with ..$objParents { $objSelf =>
           ..${prisms(tpname, findSubclasses(tpname))}
           ..$objDefs
         }
         """

      // Sealed trait
      case (traitDef @ q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
        :: Nil if mods.hasFlag(Flag.SEALED) =>
        val name = tpname.toTermName
        q"""
         $traitDef
         object $name {
           ..${prisms(tpname, findSubclasses(tpname))}
         }
         """
      case (traitDef @ q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
        :: q"object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
        :: Nil if mods.hasFlag(Flag.SEALED) =>
        q"""
         $traitDef
         object $objName extends { ..$objEarlyDefs} with ..$objParents { $objSelf =>
           ..${prisms(tpname, findSubclasses(tpname))}
           ..$objDefs
         }
         """

      case _ => c.abort(c.enclosingPosition, "Invalid annotation target: must be a sealed trait or sealed abstract class")
    }

    c.Expr[Any](result)
  }
}

