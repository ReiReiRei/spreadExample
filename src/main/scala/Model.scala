sealed trait Expr
sealed trait Delimiter extends Expr
case object LeftParenthesis extends Delimiter
case object RightParenthesis extends Delimiter

case object Literal extends Expr
case object Ref extends Expr
case object Add extends Expr
case object Sub extends Expr
case object Multiply extends Expr
case object Divide extends Expr

final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Add(a: Expr, b: Expr) extends Expr
final case class Sub(a: Expr, b: Expr) extends Expr
final case class Multiply(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr
