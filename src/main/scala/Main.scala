object Main extends App {
  println("Hello, World!")
}

trait Functor[F[_]] {
  def map[A, B](x: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f
}


object ExprInit extends App {
  // expression problem
  trait Expr
  case class Val(x: Int) extends Expr
  case class Add(e1: Expr, e2: Expr) extends Expr
  def eval(exp: Expr): Int = exp match {
    case Add(e1, e2) => eval(e1) + eval(e2)
    case Val(x) => x
  }

  def render(exp: Expr): String = exp match {
    case Add(e1, e2) => "(" + render(e1) + " + " + render(e2) + ")"
    case Val(x) => x.toString
  }

  println(eval(Add(Val(3), Add(Val(5), Val(2)))))
  println(render(Add(Val(3), Add(Val(5), Val(2)))))
}

// what if we need mul? Add a case class and change eval and render
// "Expression problem"

// datatype alacarte
object Expr {
  abstract class Expr[F[_]]
  case class In[F[_]](x: F[Expr[F]]) extends Expr[F]

  case class Val[E](x: Int)
  implicit def ValFunctor = new Functor[Val] {
    def map[A, B](x: Val[A])(f: A => B): Val[B] = Val[B](x.x)
  }
  case class Add[E](e1: E, e2: E) 
  implicit def AddFunctor = new Functor[Add] {
    def map[A, B](x: Add[A])(f: A => B): Add[B] = Add(f(x.e1), f(x.e2))
  }

  type ValExpr = In[Val]
  type AddExpr = In[Add]

  abstract class Coprod[F[_], G[_], A]
  case class Left[F[_], G[_], A](fa: F[A]) extends Coprod[F, G, A]
  case class Right[F[_], G[_], A](ga: G[A]) extends Coprod[F, G, A]

  type :+:[F[_], G[_]] = ({type t[A] = Coprod[F, G, A]})

  implicit def CoproductFunctor[F[_]: Functor, G[_]: Functor]: Functor[(F :+: G)#t] =
    new Functor[(F :+: G)#t] {
      def map[A, B](x: Coprod[F, G, A])(f: A => B): Coprod[F, G, B] = x match {
        case Left(x) => Left(Functor[F].map(x)(f))
        case Right(x) => Right(Functor[G].map(x)(f))
      }
    }

  // we compose two data types!
  // (Val :+: Add)#t is for type level lambda function
  val addExample: Expr[(Val :+: Add)#t] = In[(Val :+: Add)#t](Right(Add(In(Left(Val(118))), In(Left(Val(1219))))))
}

object Eval {
  import Expr._
  // FIXME: e In or Expr?
  def foldExpr[F[_]: Functor, A](f: F[A] => A)(e: Expr[F]): A =
    f(Functor[F].map(e.asInstanceOf[In[F]].x)(foldExpr(f)))

  abstract class Eval[F[_]: Functor] {
    def evalAlgebra(x: F[Int]): Int
  }

  object Eval {
    def apply[F[_]](implicit f: Eval[F]): Eval[F] = f
  }

  implicit def EvalAdd = new Eval[Add] {
    def evalAlgebra(x: Add[Int]): Int = x.e1 + x.e2
  }

  implicit def EvalVal = new Eval[Val] {
    def evalAlgebra(x: Val[Int]): Int = x.x
  }

  implicit def EvalCoprod[F[_]: Eval: Functor, G[_]: Eval : Functor] = new Eval[(F :+: G)#t] {
    def evalAlgebra(x: Coprod[F, G, Int]): Int = x match {
      case Left(fa) => Eval[F].evalAlgebra(fa)
      case Right(ga) => Eval[G].evalAlgebra(ga)
    }
  }

  def eval[F[_]: Eval : Functor](e: Expr[F]): Int =
    foldExpr(Eval[F].evalAlgebra(_))(e)
}

object Evaluation extends App {
  import Expr._, Eval._
  println(eval(addExample))
}
