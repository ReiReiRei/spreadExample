sealed trait Trampoline[A] {
  def map[B](f: A => B): Trampoline[B] =
    flatMap(a => More(() => Done(f(a))))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    Cont(this, f)

  def run: A = {
    var cur: Trampoline[_] = this
    var stack: List[Any => Trampoline[A]] = List()
    var result: Option[A] = None
    while (result.isEmpty) {
      cur match {
        case Done(a) =>
          stack match {
            case Nil => result = Some(a.asInstanceOf[A])
            case c :: cs => {
              cur = c(a)
              stack = cs
            }
          }
        case More(t) => cur = t()
        case Cont(a, f) => {
          cur = a
          stack = f.asInstanceOf[Any => Trampoline[A]] :: stack
        }
      }
    }
    result.get
  }
}

case class Done[A](a: A) extends Trampoline[A]

case class More[A](a: () => Trampoline[A]) extends Trampoline[A]

case class Cont[A, B](a: Trampoline[A], f: A => Trampoline[B])
    extends Trampoline[B]
