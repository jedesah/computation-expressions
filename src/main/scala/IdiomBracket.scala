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

  // I do not know why I need this... It has to do with the reflective toolbox
  @compileTimeOnly("`extract` must be enclosed in an `IdiomBracket`")
  def extract[T](applicative: Option[T]): T = sys.error(s"extract should have been removed by macro expansion!")

  def debug(x: Any): Unit = macro debugImpl

  object auto {
    @compileTimeOnly("`extract` must be enclosed in an `IdiomBracket`")
    implicit def extract[F[_], T](option: F[T]): T = sys.error(s"extract should have been removed by macro expansion!")
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

  def idiomBracket[T: c.WeakTypeTag, F[_]](c: Context)(x: c.Expr[T])(ap: c.Expr[Applicative[F]])(implicit tag: c.WeakTypeTag[F[_]]): c.Expr[F[T]] = {
    import c.universe._
    val applicativeInstance = ap.tree
    val result = transform(c.universe)(ContextSubset(c),x.tree, applicativeInstance, tag.tpe, monadic = false)
    if (!result.isDefined) c.warning(c.enclosingPosition,merelyLiftedMsg)
    c.Expr[F[T]](result.getOrElse(q"$applicativeInstance.pure(${x.tree})"))
  }

  def idiomBracketMonad[T, F[_]](c: Context)(x: c.Expr[T])(m: c.Expr[Monad[F]])(implicit tag: c.WeakTypeTag[F[_]]): c.Expr[F[T]] = {
    import c.universe._
    val monadInstance = m.tree
    val result = transform(c.universe)(ContextSubset(c),x.tree, monadInstance, tag.tpe, monadic = true)
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
   * @param ast AST to transform
   * @return Some(Tree) if the tree was transformed or none if it was not transformed
   */
  def transform(u: scala.reflect.api.Universe)(c: ContextSubset[u.type], ast: u.Tree, applicativeInstance: u.Tree, instanceType: u.Type, monadic: Boolean): Option[u.Tree] = {
    import u._

    /**
     * Lifts the following expression to an Applicative either by removing an extract function (it can be deeply nested)
     * or by simply adding a call to the pure function of the applicativeInstance if the expression contained no extract
     * functions.
     * @param expr Expression to be lifted by an AST transformation
     * @return New expression that has been lifted
     */
    def lift(expr: u.Tree, flatten: Boolean = false): (u.Tree, Int) = {
      def wrapInApply(expr: u.Tree, args: List[u.Tree]) = {
        val applyTerm = getApplyTerm(args.length, flatten)
        (q"$applicativeInstance.$applyTerm(..$args)($expr)", 1)
      }
      expr match {
        case fun: Apply if isExtractFunction(fun) =>
          val extracted = fun.args(0)
          if (hasExtracts(extracted) && !monadic) c.abort(c.enclosingPosition, "It is not possible to lift nested extracts in non monadic context")
          // directly nested extracts: extract(extract(a))
          if (isExtractFunction(extracted)) {
            val lifted = lift(extracted)._1
            (q"$applicativeInstance.join($lifted)",1)
          }
          else if (hasExtracts(extracted)) {
            lift(extracted, true)
          } else {
            (extracted, 1)
          }
        case _ if !hasExtracts(expr) => (q"$applicativeInstance.pure($expr)", 1)
        // I cannot get this to work for the life of me.
        // It's supposed to detect if there is only one extract a just map over it.
        // not need to know the exact structure, but it leads to widespread compiler crashing on macro expansion
        /*case _ if nbExtracts(expr) == 1 =>
          val (newExpr, replaced) = replaceExtractWithRefDebug(expr, insidePatternMatch = false)
          val List((name, arg)) = replaced
          //val lambda = createLambda(newExpr, names)
          //wrapWithApply(lambda, args)
          val liftedArg = lift(arg)._1
          val tpt = tq""
          val param = q"val $name: $tpt"
          wrapWithApply(q"{abbgg => $newExpr; ???}", List(arg))*/
        case app: Apply =>
          val (ref, args) = replaceExtractWithRefApply(app)
          wrapInApply(ref, args.map(lift(_)._1))
          // Not sure yet how to handle case with direct nested extracts
          // Currently will error because of check in first case
          /*else {
            val names: List[u.TermName] = List.fill(liftedArgs.size)(c.freshName()).map(TermName(_))
            val transformedArgs = liftedArgs.zip(names).map { case (arg, name) =>
              val ident = Ident(name)
              if (hasExtracts(arg)) ident
              else q"$applicativeInstance.pure($ident)"
            }
            val inner = createFunction(q"$applicativeInstance.$applyTerm(..$transformedArgs)($ref)", names)
            val reLiftedArgs = liftedArgs.map(lift(_))
            (q"$applicativeInstance.$applyTerm(..$reLiftedArgs)($inner)", 2)
          }*/
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
            val (newX1, argsWithWhatTheyReplace1) = replaceExtractWithRef(x1, insidePatternMatch = true)
            val (newX2, argsWithWhatTheyReplace2) =
              if (hasExtracts(x2)) {
                val paramName = TermName(c.freshName())
                (Ident(paramName), (List(paramName,x2)))
              }
              else (x2, (Nil))
            (cq"$newX1 => $newX2", argsWithWhatTheyReplace1 ++ argsWithWhatTheyReplace2)
          }.unzip
          val (names, args) = argsWithWhatTheyReplace.flatten.unzip
          val (allArgs, lhs, allNames) = if (hasExtracts(expr)) {
            val lhsName = TermName(c.freshName())
            (expr :: args, Ident(lhsName), lhsName :: names)
          } else (args, expr, names)
          val function = createFunction(q"$lhs match { case ..$tCases}", allNames)
          wrapInApply(function, allArgs.map(lift(_)._1))
        case ifExpr@If(expr, trueCase, falseCase) =>
          if (!monadic) {
            val (withExtractsRemoved, substitutions) = replaceExtractWithRefIf(ifExpr)
            val lambda = createFunction(withExtractsRemoved,substitutions.keys.toList)
            wrapInApply(lambda, substitutions.values.toList.map(lift(_)._1))
          }
          else {
            val List(exprT, trueCaseT, falseCaseT) =
              if (flatten) List(lift(expr)._1, trueCase, falseCase)
              else List(expr, trueCase, falseCase).map(lift(_)._1)
            (q"$applicativeInstance.bind($exprT)(if(_) $trueCaseT else $falseCaseT)", 1)
          }
        case Select(qual, name) =>
          val lifted = lift(qual)._1
          wrapInApply(q"_.${name.toTermName}", List(lifted))
        case Typed(expr, typeName) =>
          // TODO: This possibly not the most robust way of doing things, but it works for now
          val result = AppliedTypeTree(Ident(TypeName(instanceType.toString)),List(typeName))
          (Typed(lift(expr)._1, result),1)
        case _ => throw new AssertionError(s"An extract remains in this expression: $expr, but I don't know how to get rid of it, I am sorry...")
      }
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

    def replaceExtractWithRefApply(app: u.Apply): (u.Tree, List[u.Tree]) = {
      val namesWithReplaced = ListBuffer[(u.TermName, u.Tree)]()
      val newFun = if (hasExtracts(app.fun)) {
        val name = TermName(c.freshName())
        app.fun match {
          case Select(ref, methodName) =>
            namesWithReplaced += ((name, ref))
            Select(Ident(name), methodName)
          case innerApp: TypeApply =>
            // I am going to assume for now that a TypeApply only has a Select as a function
            val Select(ref, methodName) = innerApp.fun
            namesWithReplaced += ((name, ref))
            Select(Ident(name), methodName)
            // Here we are handling currying
          case innerApp: Apply =>
            namesWithReplaced += ((name, innerApp))
            Select(Ident(name), TermName("apply"))
        }
      } else app.fun
      val newArgs = app.args.map { arg =>
        if (hasExtracts(arg)) {
          val name = TermName(c.freshName())
          namesWithReplaced += ((name, arg))
          Ident(name)
        } else arg
      }
      if (namesWithReplaced.toList.size == app.args.size && app.fun.isInstanceOf[Ident]) (app.fun, app.args)
      else {
        val lhs = namesWithReplaced.toList.unzip._1.map(name => ValDef(Modifiers(Flag.PARAM | Flag.SYNTHETIC), name, TypeTree(), EmptyTree))
        val rhs = Apply(newFun, newArgs)
        (Function(lhs, rhs), namesWithReplaced.toList.unzip._2)
      }
    }

    def replaceExtractWithRefIf(ifElse: u.If): (u.Tree, Map[u.TermName,u.Tree]) = {
      val substitutions = collection.mutable.Map[u.TermName, u.Tree]()
      val List(newCondition, newThen, newElse) = List(ifElse.cond, ifElse.thenp, ifElse.elsep).map { expr =>
        if (hasExtracts(expr)){
          val name = TermName(c.freshName())
          substitutions += ((name, expr))
          Ident(name)
        } else expr
      }
      (If(newCondition, newThen, newElse), substitutions.toMap)
    }

    /**
     *
     * @param expr The expression from which to replace any extract with an identifier
     * @param insidePatternMatch Whether we are inside of a pattern match. If we are inside a pattern match, we need
     *                           to use a stable identifier in order for the transformed code to have the same meaning
     *                           as the original code because an identifier in a pattern match is being assigned too
     *                           if not indicated as a stable identifier that refers to some stable value outside
     *                           the pattern match
     * @return The transformed tree along with a list of new identifiers and the expressions they replace
     *         (including the original extract function)
     *         Pitfall: It would be tempting to remove the extract here but removing the extract should be left
     *         to more specialized code because there are few corner cases to consider.
     */
    def replaceExtractWithRef(expr: u.Tree, insidePatternMatch: Boolean): (u.Tree, (List[(u.TermName,u.Tree)])) = {
      val namesWithReplaced = ListBuffer[(u.TermName, u.Tree)]()
      object ReplaceExtract extends Transformer {
        override def transform(tree: u.Tree): u.Tree = tree match {
          case fun: Apply if isExtractFunction(fun) =>
            val name = TermName(c.freshName())
            namesWithReplaced += ((name, fun))
            if (insidePatternMatch) q"`$name`" else Ident(name)
          case _ => super.transform(tree)
        }
      }
      val result = ReplaceExtract.transform(expr)
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

    def nbExtracts(expr: u.Tree): Int = expr.filter(isExtractFunction).size

    def hasExtracts(expr: u.Tree): Boolean = expr.exists(isExtractFunction)

    def isExtractFunction(tree: u.Tree): Boolean = {
      val idiomBracketPath = "com.github.jedesah.IdiomBracket"
      val extractMethodNames = List(s"$idiomBracketPath.extract", s"$idiomBracketPath.auto.extract")
      tree match {
        case extract: Apply if extract.symbol != null && extractMethodNames.contains(extract.symbol.fullName) => true
        case q"com.github.jedesah.IdiomBracket.extract($_)" => true
        case _ => false
      }
    }

    def isDoubleExtract(expr: u.Tree) = if (isExtractFunction(expr)) isExtractFunction(expr.asInstanceOf[Apply].args(0)) else false

    if (ast.exists(isDoubleExtract)) c.abort(c.enclosingPosition, "It is not possible to lift directly nested extracts")
    val (result, transformArity) = lift(ast)
    if (transformArity == 0) None else Some(result)
  }
}