package example
import better.files.Resource

object Hello extends App {
  def parseData(input: String): List[List[Char]] = {
    input.split('\n').toList.map(_.toCharArray.toList).toList
  }

  def check(input: List[Char], acc: List[Char]= Nil): Int = input match{
      case '(' :: xs => check(xs, '(' :: acc )
      case '[' :: xs => check(xs, '[' :: acc )
      case '<' :: xs => check(xs, '<' :: acc )
      case '{' :: xs => check(xs, '{' :: acc )
      case ')' :: xs => acc match {
        case '(' :: as => check(xs, as)
        case _ => 3
      }
      case ']' :: xs => acc match {
        case '[' :: as =>check(xs, as)
        case _ => 57
      }
      case '}' :: xs => acc match {
        case '{' :: as => check(xs, as)
        case _ => 1197
      }
      case '>' :: xs => acc match {
        case '<' :: as => check(xs, as)
        case _ => 25137
      }
    case _ :: xs => check(xs, acc)
    case Nil => 0
  }

  def unmatchedBrackets(input: List[Char], acc: List[Char]= Nil): Option[List[Char]] = input match{
      case '(' :: xs => unmatchedBrackets(xs, '(' :: acc )
      case '[' :: xs => unmatchedBrackets(xs, '[' :: acc )
      case '<' :: xs => unmatchedBrackets(xs, '<' :: acc )
      case '{' :: xs => unmatchedBrackets(xs, '{' :: acc )
      case ')' :: xs => acc match {
        case '(' :: as => unmatchedBrackets(xs, as)
        case _ => None
      }
      case ']' :: xs => acc match {
        case '[' :: as => unmatchedBrackets(xs, as)
        case _ => None
      }
      case '}' :: xs => acc match {
        case '{' :: as => unmatchedBrackets(xs, as)
        case _ => None
      }
      case '>' :: xs => acc match {
        case '<' :: as => unmatchedBrackets(xs, as)
        case _ => None
      }
    case _ :: xs => unmatchedBrackets(xs, acc)
    case Nil => Some(acc)
  }

  def score(brakets: List[Char]): Long = {
    brakets.foldLeft(0L) { (n, c) => 
      val score = c match {
        case '(' => 1
        case '[' => 2
        case '{' => 3
        case '<' => 4
        case _ => 0
      } 
      (n * 5) + score
    }
  }

  def middle(input: List[List[Char]]): Long ={
    val scores = input.flatMap(unmatchedBrackets(_)).map(score)
    scores.sorted.apply(scores.length/2)
  }

  def errorSum(input: List[List[Char]]): Int = {
    input.foldLeft(0)((a, b) => check(b) + a)
  }
  
  println(middle(parseData(Resource.getAsString("input.txt"))))
}
