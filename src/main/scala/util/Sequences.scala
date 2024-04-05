package util
import Optionals.Optional.*
import util.Optionals.Optional

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

    override def toString: String = this match
      case Cons(h, t) => s"$h :: $t"
      case Nil() => "nil"

  object Sequence:
    def apply[A](elements: A*): Sequence[A] =
      if elements.isEmpty then Nil()
      else Cons(elements.head, apply(elements.tail*))

    def empty[A]: Sequence[A] = Nil()

    extension [A](sequence: Sequence[A])
      def head: Optional[A] = sequence match
        case Cons(h, _) => Just(h)
        case _ => Empty()

      def concat(other: Sequence[A]): Sequence[A] = sequence match
        case Cons(h, t) => Cons(h, t.concat(other))
        case _ => other

      def flatMap[B](f: A => Sequence[B]): Sequence[B] = sequence match
        case Cons(h, t) => f(h).concat(t.flatMap(f))
        case _ => Nil()

      def map[B](f: A => B): Sequence[B] = sequence.flatMap(x => Cons(f(x), Nil()))

      def filter(f: A => Boolean): Sequence[A] = sequence.flatMap:
        case x if f(x) => Cons(x, Nil())
        case _ => Nil()
@main def trySequences =
  import Sequences.* 
  val sequence = Sequence(1, 2, 3)
  println(sequence)
  println(sequence.head)
  println(sequence.map(_ * 2))
  println(sequence.flatMap(x => Sequence(x, x * 2)))
  println(sequence.filter(_ % 2 == 0))
  println(sequence.concat(Sequence(4, 5, 6)))

