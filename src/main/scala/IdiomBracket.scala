package com.github.jedesah

import scalaz._
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.collection.mutable.ListBuffer

object IdiomBracket {

  val merelyLiftedMsg = "IdiomBracket merely lifted expression, there is a probably a better more explicit way of achieving the same result"

  def apply[F[_]: Applicative,T](x: T): F[T] = macro idiomBracket[T,F]
  //def apply2[T](x: T): Option[Option[T]] = macro idiomBracket2[T]
  def monad[F[_]: Monad,T](x: T): F[T] = macro idiomBracketMonad[T,F]

  def control(x: String): Option[String] = macro controlImpl

  @compileTimeOnly("`extract` must be enclosed in an `IdiomBracket`")
  def extract[F[_], T](applicative: F[T]): T = sys.error(s"extract should have been removed by macro expansion!")

  def debug(x: Any): Unit = macro debugImpl

  object auto {
    @compileTimeOnly("`extract` must be enclosed in an `IdiomBracket`")
    implicit def extract[T](option: Option[T]): T = sys.error(s"extract should have been removed by macro expansion!")
  }

  import scala.reflect.macros.blackbox.Context

  def debugImpl(c: Context)(x: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    println(show(x.tree))
    println(showRaw(x.tree))
    val message = Literal(Constant(show(x.tree) + ": "))
    val result = q"""println($message + (${x.tree}))"""
    c.Expr[Unit](result)
  }

  def idiomBracket[T: c.WeakTypeTag, F[_]](c: Context)(x: c.Expr[T])(ap: c.Expr[Applicative[F]]): c.Expr[F[T]] = {
    import c.universe._
    val applicativeInstance = ap.tree
    val result = transform(c.universe)(ContextSubset(c),x.tree, applicativeInstance, monadic = false)
    if (!result.isDefined) c.warning(c.enclosingPosition,merelyLiftedMsg)
    c.Expr[F[T]](result.getOrElse(q"$applicativeInstance.pure(${x.tree})"))
  }

  def idiomBracketMonad[T, F[_]](c: Context)(x: c.Expr[T])(m: c.Expr[Monad[F]]): c.Expr[F[T]] = {
    import c.universe._
    val monadInstance = m.tree
    val result = transform(c.universe)(ContextSubset(c),x.tree, monadInstance, monadic = true)
    if (!result.isDefined) c.warning(c.enclosingPosition, merelyLiftedMsg)
    c.Expr[F[T]](result.getOrElse(q"$monadInstance.pure(${x.tree})"))
  }

  def controlImpl(c: Context)(x: c.Expr[String]): c.Expr[Option[String]] = {
    import c.universe._
    val result = x.tree match {
      case Match(expr, cases) =>
        println(cases)
        //val matchz = Match(q"""List("hello")""", cases.map(c.untypecheck(_).asInstanceOf[CaseDef]))
        val matchz = Match(q"""List("hello")""", cases.map{case cq"$x1 => $x2" => cq"$x1 => $x2"})
        q"Some($matchz)"
        q"""Some(List("5")).map{a => a;$matchz}"""
      case a => q"Some($a)"
    }
    c.Expr[Option[String]](c.untypecheck(result))
  }

  /*def idiomBracket2[T: c.WeakTypeTag](c: Context)(x: c.Expr[T]): c.Expr[Option[Option[T]]] = {
    import c.universe._
    val result = transformAST(c.universe, c)(x.tree, q"scalaz.Applicative[Option]", monadic = false)
    if (!result.isDefined) c.warning(c.enclosingPosition, merelyLiftedMsg)
    c.Expr[Option[Option[T]]](result.getOrElse(q"Some(Some($x.tree))"))
  }*/

  trait ContextSubset[U <: scala.reflect.api.Universe] {
    def freshName(): String
    def abort(pos: U#Position, msg: String): Nothing
    def enclosingPosition: U#Position
  }

  object ContextSubset {
    def apply(c: Context) = new ContextSubset[c.universe.type] {
      def freshName(): String = c.freshName()
      def abort(pos: c.Position, msg: String): Nothing = c.abort(pos, msg)
      def enclosingPosition: c.Position = c.enclosingPosition
    }
  }

  /**
   *
   * @param u The universe of the Trees. Required to operate under current Scala reflection architecture. Trees cannot
   *          exist without a universe.
   * @param c Context to use. Typically supplied by the macro definition
   * @param tree Tree to transform
   * @return Some(Tree) if the tree was transformed or none if it was not transformed
   */
  def transform(u: scala.reflect.api.Universe)(c: ContextSubset[u.type], ast: u.Tree, applicativeInstance: u.Tree, monadic: Boolean): Option[u.Tree] = {
    import u._

    /**
     * Lifts the following expression to an Applicative either by removing an extract function (it can be deeply nested)
     * or by simply adding a call to the pure function of the applicativeInstance if the expression contained no extract
     * functions.
     * @param expr Expression to be lifted by an AST transformation
     * @return New expression that has been lifted
     */
    def lift(expr: u.Tree, flatten: Boolean = false): (u.Tree, Int) = expr match {
      case fun: Apply if isExtractFunction(fun) =>
        val extracted = fun.args(0)
        // directly nested extracts: extract(extract(a))
        if (isExtractFunction(extracted)) c.abort(c.enclosingPosition, "It is not possible to lift directly nested extracts")
        // indirectly nested extracts: extract(do(extract(a))); this is fine but we need to be monadic
        if (extractsArePresent(extracted) && !monadic) c.abort(c.enclosingPosition, "It is not possible to lift nested extracts in non monadic context")
        if (extractsArePresent(extracted) && monadic) {
          lift(extracted, true)
        } else {
          (extracted, 1)
        }
      case Apply(ref, args) =>
        val (ref1, args1) =
        // There are not extracts in the LHS of this function/method application
          if (!extractsArePresent(ref)) {
            ref match {
              // This is a method invocations and we need to treat it separately because of Scala quirks.
              // This will make an anonymous function that does not do much
              // List(1,3).take(3) -> ($1) => List(1,3).take($1)
              case Select(exprRef, methodName) => (createMethodWithLHS(methodName, exprRef, args.size), args)
              // It's just a plain old reference to a function, so we can use the reference as is
              // com.github.jedesah.foo -> com.github.jedesah.foo
              case _ => (ref, args)
            }
            // There is an extract in the LHS of this function/method application
            // We need to create an anonymous function to transform the LHS into an argument of a plain old function
            // extract(a).take(4) -> ($1, $2) => $1.take($2), then List(a,4) are arguments we can lift and we will
            // ultamitely end up with Applicative.apply2(a,Applicative.pure(4))(($1,$2) => $1.take($2))
          } else {
            val Select(exprRef, methodName) = ref
            (createMethod(methodName, args.size), exprRef :: args)
          }
        val liftedArgs = args1.map(lift(_)._1)
        val applyTerm = getApplyTerm(liftedArgs.length, flatten)
        if (liftedArgs.forall(!isExtractFunction(_))) (q"$applicativeInstance.$applyTerm(..$liftedArgs)($ref1)", 1)
        else {
          val names: List[u.TermName] = List.fill(liftedArgs.size)(c.freshName()).map(TermName(_))
          val transformedArgs = liftedArgs.zip(names).map { case (arg, name) =>
            val ident = Ident(name)
            if (extractsArePresent(arg)) ident
            else q"$applicativeInstance.pure($ident)"
          }
          val inner = createFunction(q"$applicativeInstance.$applyTerm(..$transformedArgs)($ref1)", names)
          val reLiftedArgs = liftedArgs.map(lift(_))
          (q"$applicativeInstance.$applyTerm(..$reLiftedArgs)($inner)", 2)
        }
      case Block(exprs, finalExpr) => {
        var arityLastTransform: Int = 0
        val newExprs = (exprs :+ finalExpr).foldLeft[(Map[String, Int], List[u.Tree])]((Map(), Nil)) { (accu, expr) =>
          val (names, exprs) = accu
          expr match {
            // We need to remember the name of the value definition so that we can add extract methods later so that the right thing happens
            case ValDef(mods, name, tpt, rhs) =>
              val (tRHS, transformArity) = lift(addExtractR(rhs, names))
              arityLastTransform = transformArity
              (names + (name.toString -> transformArity), exprs :+ ValDef(mods, name, TypeTree(), tRHS))
            // If it's just an identifier, let's leave it as is but reconstruct it so that it looses it's type.
            case ident: Ident =>
              arityLastTransform = names(ident.name.toString)
              (names, exprs :+ Ident(TermName(ident.name.toString)))
            // Anything else, we need to add extracts to identifiers of transformed `ValDef`s because we lifted the type of the symbol they refer to.
            case _ =>
              val (transformed, transformArity) = lift(addExtractR(expr, names))
              arityLastTransform = transformArity
              (names, exprs :+ transformed)
          }
        }._2
        (Block(newExprs.init, newExprs.last), arityLastTransform)
      }
      // TODO: Figure out why unchanged case pattern seems to go bonky in macro
      case Match(expr, cases) =>
        val (tCases, argsWithWhatTheyReplace: List[List[(u.TermName, u.Tree)]]@unchecked) = cases.map { case cq"$x1 => $x2" =>
          val (newX1, argsWithWhatTheyReplace1) = replaceExtractWithRefInPatternMatch(x1)
          val (newX2, argsWithWhatTheyReplace2) =
            if (extractsArePresent(x2)) {
              val paramName = TermName(c.freshName())
              (Ident(paramName), (List(paramName,x2)))
            }
            else (x2, (Nil))
          (cq"$newX1 => $newX2", argsWithWhatTheyReplace1 ++ argsWithWhatTheyReplace2)
        }.unzip
        val (names, args) = argsWithWhatTheyReplace.flatten.unzip
        val allArgs = (expr :: args).map(lift(_)._1)
        val applyTerm = getApplyTerm(allArgs.size)
        val lhsName = TermName(c.freshName())
        val function = createFunction(q"$lhsName match { case ..$tCases}", lhsName :: names)
        (q"$applicativeInstance.$applyTerm(..$allArgs)($function)", 1)
      case If(expr, trueCase, falseCase) =>
        if (!monadic) {
          val liftedParts = List(expr, trueCase, falseCase).map(lift(_)._1)
          (q"$applicativeInstance.apply3(..$liftedParts)(if(_) _ else _)", 1)
        }
        else {
          val List(exprT, trueCaseT, falseCaseT) =
            if (flatten) List(lift(expr)._1, trueCase, falseCase)
            else List(expr, trueCase, falseCase).map(lift(_)._1)
          (q"$applicativeInstance.bind($exprT)(if(_) $trueCaseT else $falseCaseT)", 1)
        }
      case Select(qual, name) =>
        val lifted = lift(qual)._1
        (q"$applicativeInstance.map($lifted)(_.${name.toTermName})", 1)
      case _ => (q"$applicativeInstance.pure($expr)", 1)
    }

    def getApplyTerm(arity: Int, flatten: Boolean = false) = {
      if (arity > 12)
        c.abort(c.enclosingPosition, "scalaz does not define an apply13 or more which is necessary of our rewrite to work. Reformat your code to avoid functions receiving more than 12 parameters.")
      val applyFunName =
        if (flatten)
          if (arity == 1) s"bind" else s"bind$arity"
        else
        if (arity == 1) "map" else s"apply$arity"
      TermName(applyFunName)
    }

    def createFunction(rhs: u.Tree, args: List[u.TermName]) = {
      val lhs = args.map( name => ValDef(Modifiers(Flag.PARAM), name, TypeTree(), EmptyTree))
      Function(lhs, rhs)
    }

    def createMethod(methodName: u.Name, nArgs: Int) = {
      val names = List.fill(nArgs + 1)(c.freshName())
      val lhs = names.map( name => ValDef(Modifiers(Flag.PARAM | Flag.SYNTHETIC), TermName(name), TypeTree(), EmptyTree))
      val args = names.map(name => Ident(TermName(name)))
      val rhs = Apply(Select(args.head, methodName), args.tail)
      Function(lhs, rhs)
    }

    def createMethodWithLHS(methodName: u.Name, select: u.Tree, nArgs: Int) = {
      val names = List.fill(nArgs)(c.freshName())
      val lhs = names.map( name => ValDef(Modifiers(Flag.PARAM | Flag.SYNTHETIC), TermName(name), TypeTree(), EmptyTree))
      val args = names.map(name => Ident(TermName(name)))
      val rhs = Apply(Select(select, methodName), args)
      Function(lhs, rhs)
    }

    def replaceExtractWithRefInPatternMatch(pattern: u.Tree): (u.Tree, (List[(u.TermName,u.Tree)])) = {
      val namesWithReplaced = ListBuffer[(u.TermName, u.Tree)]()
      object ReplaceExtract extends Transformer {
        override def transform(tree: u.Tree): u.Tree = tree match {
          case fun: Apply if isExtractFunction(fun) =>
            val name = TermName(c.freshName())
            namesWithReplaced += ((name, fun))
            q"`$name`"
          case _ => super.transform(tree)
        }
      }
      val result = ReplaceExtract.transform(pattern)
      (result, namesWithReplaced.toList)
    }

    def addExtractR(expr: u.Tree, names: Map[String, Int]): u.Tree = {
      object AddExtract extends Transformer {
        override def transform(tree: u.Tree): u.Tree = tree match {
          case ident@Ident(name) => {
            val untypedIdent = Ident(TermName(name.toString))
            if (names.keys.toList.contains(name.toString))
              (0 until names(name.toString)).foldLeft[u.Tree](untypedIdent)((tree, _) => addExtract(tree))
            else ident
          }
          case _ => super.transform(tree)
        }
      }
      AddExtract.transform(expr)
    }

    def addExtract(expr: u.Tree): u.Tree = {
      q"com.github.jedesah.IdiomBracket.extract($expr)"
    }

    def extractsArePresent(expr: u.Tree): Boolean = {
      var result = false
      object FindExtract extends Traverser {
        override def traverse(tree: u.Tree): Unit =
          if(isExtractFunction(tree)) result = true
          else super.traverse(tree)
      }
      FindExtract.traverse(expr)
      result
    }

    def isExtractFunction(tree: u.Tree): Boolean = {
      val idiomBracketPath = "com.github.jedesah.IdiomBracket"
      val extractMethodNames = List(s"$idiomBracketPath.extract", s"$idiomBracketPath.auto.extract")
      tree match {
        case extract if extract.symbol != null && extractMethodNames.contains(extract.symbol.fullName) => true
        case q"com.github.jedesah.IdiomBracket.extract($_)" => true
        case _ => false
      }
    }

    val (result, transformArity) = lift(ast)
    if (transformArity == 0) None else Some(result)
  }
}