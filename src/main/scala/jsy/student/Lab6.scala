package jsy.student

import jsy.lab6.Lab6Like
import jsy.lab6.ast._
import jsy.util.DoWith

object Lab6 extends jsy.util.JsyApplication with Lab6Like {

  /*
   * CSCI 3155: Lab 6
   * Preston Dotsey
   *
   * Partner: Pankaj Behera
   * Collaborators: <N/A>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*** Exercises with Continuations ***/

   def foldLeftAndThen[A,B](t: Tree)(z: A)(f: (A,Int) => A)(sc: A => B): B = {
    def loop(acc: A, t: Tree)(sc: A => B): B = t match {
      case Empty => sc(acc)
      case Node(l, d, r) => loop(acc, l)((acc) => loop(f(acc, d), r)(sc))
    }
    loop(z, t)(sc)
  }

  def dfs[A](t: Tree)(f: Int => Boolean)(sc: List[Int] => A)(fc: () => A): A = {
    def loop(path: List[Int], t: Tree)(fc: () => A): A = t match {
      case Empty => fc()
      case Node(l, d, r) => f(d) match {
        case true  => sc(d::path)
        case false => loop(d::path, l)(()=> loop(d::path, r)(fc))
      }
    }
    loop(Nil, t)(fc)
  }

  /*** Regular Expression Parsing ***/

  /* We define a recursive decent parser for regular expressions in
   * REParser.
   * 
   * The REParserLike trait derives from Parsers in the Scala library to make
   * use of it's handing of input (Input) and parsing results (ParseResult).
   * 
   * The Parsers trait is actually a general purpose combinator parser library,
   * which we won't use directly.
   *
   * Grammar. You will want to write a BNF grammar here from your write-up
   * as the basis for your implementation.
   *
   *   re ::= union
   *
   *   union ::= intersect unions
   *   unions ::= epsilon | '|' intersect unions
   *
   *   intersect ::= concat {& concat}
   *   concat ::= not {not}
   *   not ::= ~ not | star
   *   star ::= atom {*|+|?}
   *   atom ::= ! | # | c | . | ( re 
   * 
   */
  object REParser extends REParserLike {
    /* The following items are the relevant pieces inherited from Parsers
     * 
     * type Input = Reader[Elem]
     * sealed abstract class ParseResult[+T] {
     *   val next: Input
     *   def map[U](f: T => U): ParseResult[U]
     * }
     * case class Success[+T](result: T, next: Input) extends ParseResult[T]
     * case class Failure(next: Input) extends ParseResult[Nothing]
     */

    def re(next: Input): ParseResult[RegExpr] = union(next)


    //union::=intersect{ '|' interesect}
    //a|c
    def union(next: Input): ParseResult[RegExpr] = intersect(next) match {
      case Success(r, next) => { //r = RSingle(a)
        def unions(acc: RegExpr, next: Input): ParseResult[RegExpr] =
          if (next.atEnd) Success(acc, next) //epsilon case: there is nothing after a
          else (next.first, next.rest) match { //if there is stuff after a, match the rest of the symbols
            case ('|', next) => intersect(next) match { //
              case Success(r, next) => unions(RUnion(acc, r), next)
              case _ => Failure("expected intersect", next)
            }
            case _ => Success(acc, next) // epsilon is when there are more chars in string
          }
        unions(r, next)
      }
      case _ => Failure("no intersect", next)
    }

    def intersect(next: Input): ParseResult[RegExpr] = {
      concat(next) match{
        case Success(r, next) => {
          def intersections(acc: RegExpr, next: Input): ParseResult[RegExpr] =
            if (next.atEnd) Success(acc, next)
            else (next.first, next.rest) match {
              case ('&', next) => concat(next) match {
                case Success(r, next) => intersections(RIntersect(acc,r), next)
                case _ => Failure("no concat", next)
              }
              case _ => Success(acc, next)
            }
          intersections(r, next)
        }
        //failure condition
        case _ => Failure("no concat", next)
      }
    }


    def concat(next: Input): ParseResult[RegExpr] = not(next) match {
      case Success(r, next) => {
        def concats(acc: RegExpr, next: Input): ParseResult[RegExpr] =
          if (next.atEnd) Success(acc, next)
          else not(next) match {
            case Success(r, next) => concats(RConcat(acc, r), next)
            case _ => Success(acc, next)
          }
        concats(r, next)
      }
      case fail => fail
    }

    def not(next: Input): ParseResult[RegExpr] = (next.first, next.rest) match {
      case ('~', rest) => not(rest) match{
        case Success(r, next) => Success(RNeg(r), next)
        case fail => fail
      }
      //if no "~", use star
      case _ => star(next) match {
        case Success(r, next) => Success(r, next)
        case fail => fail
      }
    }


    def star(next: Input): ParseResult[RegExpr] = atom(next) match {
      //take atom of next
      case Success(r, next) => {
        def stars(acc: RegExpr, next: Input): ParseResult[RegExpr] =
        //new function which grabs acc and next
          if (next.atEnd) Success(acc, next)
          //check to see if the next is the end
          else (next.first, next.rest) match {
            //if not, check for other symbols
            case ('+', next) => stars(RPlus(acc), next)
            case ('*', next) => stars(RStar(acc),next)
            case ('?', next) => stars(ROption(acc), next)
            //star modified version
            case _ => Success(acc, next)
            //if no symbols, success
          }
        stars(r, next)
        //execute stars on next 
      }
      case _ => Failure("atom not detected", next)
    }


    /* This set is useful to check if a Char is/is not a regular expression
       meta-language character.  Use delimiters.contains(c) for a Char c. */
    val delimiters = Set('|', '&', '~', '*', '+', '?', '!', '#', '.', '(', ')')

    def atom(next: Input): ParseResult[RegExpr] = {
      if (next.atEnd) Failure("empty, not able to match", next)
      else (next.first, next.rest) match {
        case ('!', next) => Success(RNoString, next)
        case ('#', next) => Success(REmptyString, next)
        case ('.', next) => Success(RAnyChar, next)
        case ('(', next) => re(next) match {
          case Success(reast, next) => (next.first, next.rest) match {
            case (')', next) => Success(reast, next)
            case _ => Failure("expected )", next)
          }
          case fail => fail
        }
        case (c, next) if !delimiters.contains(c) => Success(RSingle(c), next)
        case _ => Failure("need an atom", next)
      }
    }
  }


  /***  Regular Expression Matching ***/

  /** Tests whether a prefix of chars matches the regular expression re with a continuation for the suffix.
    *
    * @param re a regular expression
    * @param chars a sequence of characters
    * @param sc the success continuation
    * @return if there is a prefix match, then sc is called with the remainder of chars that has yet to be matched. That is, the success continuation sc captures “what to do next if a prefix of chars successfully matches re; if a failure to match is discovered, then false is returned directly.
    */

    // test: see if a prefix of chars matches the regular expression. if yes, the sc(success continuation) called with
    // remaining chars to be matched. If any early failure then return false.
   def test(re: RegExpr, chars: List[Char])(sc: List[Char] => Boolean): Boolean = (re, chars) match {
    /* Basic Operators */
    case (RNoString, _) => false ////No string should not match anything
    case (REmptyString, _) => sc(chars) //emptystring matches # in the string. Check each character for the #
    case (RSingle(_), Nil) => false //single should match any one character c. If no string was provided, return false
    case (RSingle(c1), c2 :: t) => if(c1==c2) sc(t) else false //single should match a specific character c1. If the string begins with c2, compare c1 and c2.
      //if c1 == c2, invoke the success continuation. Otherwise return false
    case (RConcat(re1, re2), _) => test(re1, chars)(charsp => test(re2, charsp)(sc))  //re1 and re2 should match, and they should match adjacent to each other
    case (RUnion(re1, re2), _) => test(re1, chars)(sc) || test(re2, chars)(sc) //either re1 or re2 should match the supplied chars
    case (RStar(re1), _) => sc(chars) || test(re1, chars)(sc)//invoke sc or test to see if re1 matches the supplied chars

    /* Extended Operators */
    case (RAnyChar, Nil) => false
    case (RAnyChar, _ :: t) => sc(t)
    case (RPlus(re1), _) => test(RConcat(re1, RStar(re1)), chars)(sc)
    case (ROption(re1), _) => sc(chars) || test(re1, chars)(sc)

    /***** Extra Credit Cases *****/
    case (RIntersect(re1, re2), _) => ???
    case (RNeg(re1), _) => ???
  }
  // retest: given a regular expr(re) and string s, return true if the string s belongs to the
  // language described by the regular expression re, otherwise returns false.
   def retest(re: RegExpr, s: String): Boolean = {
    def test(re: RegExpr, chars: List[Char], sc: List[Char] => Boolean): Boolean = (re, chars) match {
      /* Basic Operators */
      case (RNoString, _) => false
      case (REmptyString, _) => sc(chars)  //continue on with rest of string.
      case (RSingle(_), Nil) => false
      case (RSingle(c1), c2 :: t) => if (c1 == c2) sc(t) else false  //our prefix and head of list better match, then list should be empty
      case (RConcat(re1, re2), _) => test(re1, chars, { next => test(re2, next, sc) }) // test the first re, the continuation tests re2 (anonymous func).
      case (RUnion(re1, re2), _) => test(re1, chars, sc) || test(re2, chars, sc) // can do either one, "||".
      case (RStar(re1), _) => sc(chars) || test(re1, chars, { next => if (next.size < chars.size) test(RStar(re1), next, sc) else false })
      // check if list is empty. if false, then check similar to concat except we are looking for concatenations of re1.
      // both return false in order for match failure. zero or more concatenations of re1.
      /* Extended Operators */
      case (RAnyChar, Nil) => false
      case (RAnyChar, _ :: t) => sc(t) // similar to RSingle, but can be any character.
      case (RPlus(re1), _) => test(RConcat(re1, RStar(re1)), chars, sc) // one or more concatenations
      case (ROption(re1), _) => if (chars.isEmpty) true else test(re1, chars, sc) // 0 or one matches
      
      /***** Extra Credit Cases *****/
      case (RIntersect(re1, re2), _) => test(re1, chars, sc) && test(re2, chars, sc) 
      case (RNeg(re1), _) => test(re1, chars, sc) 
    } 
    test(re, s.toList, { chars => chars.isEmpty }) //TODO Conditions for success. There are no more chars left in list.
  }

  //If we have the left-recursive pair of productions-
  //A → Aα / β
  //(Left Recursive Grammar)

 //where β does not begin with an A.
 //Then, we can eliminate left recursion by replacing the pair of productions with-

    //A → βA’
    //A’ → αA’ / ∈

//(Right Recursive Grammar)

//Consider the following grammar and eliminate left recursion-

//E → E + E / E x E / a

//Solution-
 
//The grammar after eliminating left recursion is-
//E → aA
//A → +EA / xEA / ∈


  /*******************************/
  /*** JavaScripty Interpreter ***/
  /*******************************/

  /* This part is optional for fun and extra credit.
   *
   * If you want your own complete JavaScripty interpreter, you can copy your
   * Lab 5 interpreter here and extend it for the Lab 6 constructs.
   */

  /*** Type Inference ***/

  def typeof(env: TEnv, e: Expr): Typ = ???

  /*** Step ***/

  def substitute(e: Expr, v: Expr, x: String): Expr = ???
  def step(e: Expr): DoWith[Mem,Expr] = ???

  /*** Lower ***/

  def lower(e: Expr): Expr = e

}