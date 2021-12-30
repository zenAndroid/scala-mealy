println("Hi")

val x = -98

x - 44444444

val y = x - 8

val f = BigInt(1021543216543L)

println(s"abdolute value of x =  ${x.abs}!")

val nums = List("one", "two")

nums.map(_.capitalize)

case class Person(name: String)

val t = (11, "eleven", Person("Eleven"))

import scala.collection.mutable.ArrayBuffer

enum Topping:
  case foo, bar, qux

class Pizza(s: String):
  val topp = ArrayBuffer[Topping]()

val list: List[Any] = List(
  "a string",
  732, // an integer
  'c', // a character
  true, // a boolean value
  () => "an anonymous function returning a string"
)

list.foreach(element => println(element))

val longInt: Long = 987654321

val floatingNumber: Float = longInt.toFloat

println("kek")

val lost = (10 to 12).map(_ * 2)

val names = List("_olivia", "_walter", "_peter")

val capNames = for name <- names yield
  val foo = name.drop(1).capitalize
  foo

// A while loop
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")

m("fork")

val ms = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
  .withDefaultValue("kek")

ms("foo")

class Dog(br: String) {
  // Constructor code here
  private var breed = br

  // Define a method called bark, returning a String
  def bark = "Woof, woof!"

  // Values and methods are assumed public. "protected" and "private" keywords
  // are also available.
  private def sleep(hours: Int) =
    println(s"I'm sleeping for $hours hours")

  def getBreed = breed

  // Abstract methods are simply methods with no body. If we uncomment the
  // def line below, class Dog would need to be declared abstract like so:
  //   abstract class Dog(...) { ... }
  // def chaseAfter(what: String): String
}

val mydog = new Dog("greyhound")
println(mydog.getBreed) // => "greyhound"
println(mydog.bark) // => "Woof, woof!"

case class P(name: String, number: String)

val s = Set(1, 2, 3, 7)

for { n <- s } yield math.sqrt(n)

val nSquared2 = for { n <- s } yield math.sqrt(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10 } yield nSquared

def matchPerson(person: P) = person match
  // Then you specify the patterns:
  case P("George", number) => "We found George! His number is " + number
  case P("Kate", number)   => "We found Kate! Her number is " + number
  case P(name, number) => "We matched someone : " + name + ", phone : " + number

val pp = P("zenBoi", "suicide prevention hotline")

matchPerson(pp)

class Book(title: String, author: String, year: Int)

// By default they are vals, so immutable by default.

var p = Book("Mein Kampf", "Adolf Hitler", 1999)

import scala.math.*

case class Circle(radius: Double):
  def area: Double = Circle.calculateArea(radius)

object Circle:
  private def calculateArea(radius: Double): Double = Pi * pow(radius, 2.0)

val circle1 = Circle(5.0)
circle1.area

trait GreetingService:
  def translate(text: String): String
  def sayHello = translate("Hello")

trait TranslationService:
  def translate(text: String): String = "..."

trait ComposedService extends GreetingService, TranslationService

class Foo extends ComposedService

val ddp = Foo()

ddp.sayHello

class Foob:
  val foo: List[String] = List("Sadness", "and", "despair")

val df = new Foob()

// df.foo = df.foo :+ "cunt" ;; Compiler error: BASED

def add(a: Int, b: Int) = a + b

add(4, 6)

class Doggo(name: String = "woofer"):
  def speak() = println(
    name
  ) // better to put the parens in a arity-0 fn that is side-effectful

var bof = new Doggo

bof.speak()

m foreach print

m.foreach(print)

// Very nice !

def isEvenMethod(i: Int) = i % 2 == 0 // a method
val isEvenFunction = (i: Int) => i % 2 == 0 // a function

def sayHello(f: () => Unit): Unit = f()

def fuck(): Unit = print(5)

sayHello(fuck)

enum Planet(mass: Double, radius: Double):

  private final val G = 6.67300e-11
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) = otherMass * surfaceGravity

  case Mercury extends Planet(3.303e+23, 2.4397e6)
  case Venus extends Planet(4.869e+24, 6.0518e6)
  case Earth extends Planet(5.976e+24, 6.37814e6)

print(Planet.Earth.surfaceWeight(323))

class Logarithm(protected val underlying: Double):
  def toDouble: Double = math.exp(underlying)
  def +(that: Logarithm): Logarithm = Logarithm(this.toDouble + that.toDouble)
  def *(that: Logarithm): Logarithm = new Logarithm(
    this.underlying + that.underlying
  )
object Logarithm:
  def apply(d: Double): Logarithm = new Logarithm(math.log(d))

// l2 and l3 uses the *APPLY* method
val l2 = Logarithm(2.0) // new Logarithm(log(2)) ==> underlying = log(2)
val l3 = Logarithm(3.0) // new Logarithm(log(3)) ==> underlying = log(3)
/*
  l2.toDouble || math.exp(underlying) = math.exp(log(2)) = 2
  l3.toDouble || math.exp(underlying) = math.exp(log(3)) = 3
  (l2 * l3) = new Logarithm(log(2)+log(3))
  (l2 * l3).toDouble = math.exp(underlying) = math.exp(log2+log3) = explog2+explog3 = 2 * 3 = 6
  (l2 + l3) =
    1. l2.toDouble + l3.toDouble = 2 + 3 = 5
    2. + returns new Logarithm(log(5))
  (l2 + l3).toDouble = math..exp(log(5)) = 5
 */
println((l2 * l3).toDouble)
println((l2 + l3).toDouble)

case class ZipCodeApply(zip: Int, extension: Int = 0):
  override def toString =
    if extension != 0 then s"$zip-$extension" else zip.toString
object ZipCodeApply:
  def apply(zip: String, extension: String): ZipCodeApply =
    apply(zip.toInt, if extension.length == 0 then 0 else extension.toInt)
  def apply(zip: String): ZipCodeApply = apply(zip, "")

val testing = ZipCodeApply(123, 67)
val testingNew = new ZipCodeApply(123, 67)

// someone explain to this here idiot wtf is goin on pls

var foo : List[Int] = Nil

foo = List(1,2,3)
foo = foo.appended(35)
foo :+ 544
foo

val booleanArray : List[Boolean] = List(true,true,true,true,true)

booleanArray

booleanArray reduce(_ & _)

val foog : List[Int] = List(1,2,3,4,5,6,7,8,9,10)
foog.slice(1,foo.size)


var suka = 
    """
    """

try
    val b = BigInt("foo")
catch _ => print("Dank")

class DavidBanner

trait Angry:
  def beAngry() =
    println("You won like me ...")

trait Big:
  println("I big")

trait Green:
  println("m green")

// mix in the traits as DavidBanner
// is created
// val hulk = new DavidBanner with Big, Angry, Green