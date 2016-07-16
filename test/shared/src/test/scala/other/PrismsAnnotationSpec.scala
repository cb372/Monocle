package other

import org.scalatest._

@monocle.macros.Prisms sealed trait Foo
case object A extends Foo
case object B extends Foo

class PrismsAnnotationSpec extends FlatSpec with Matchers {

  it should "work" in {
  // Commented out because I ran afoul of https://github.com/scalatest/scalatest/pull/669

//    """
//    @monocle.macros.Prisms sealed trait Foo
//    case object A extends Foo
//    case object B extends Foo
//    """ should compile
  }

}
