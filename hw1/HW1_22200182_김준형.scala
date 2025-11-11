//Problem 1:
//Solved by myself: N(I saw your lecture 24:22, I changed some characters)
//Time taken: about 15 min
//[Contract] dollarTowon: Int -> Int
//[Purpose] To convert dollars to won
//[Tests] 2: 2786

def dollarToWon(dollar: Int): Int = {
	dollar * 1393
}

//Problem 2:
//Solved by myself: N(I tried to use ? operator but It isn't exisit I saw https://kimilb412-2.blogspot.com/2014/03/scala-03.html this blog)
//also I asked Copilot to This code is correct 
//Time taken: around 15 min
//[Contract] maxInt: Int, Int, Int -> Int
//[Purpose] Find biggeset Int
//[Tests] (3, 5, 7)->7

def myMax(one: Int, two: Int): Int = {
	if (one > two) one else two
}

def maxOfThreeIntegers(one: Int, two: Int, three: Int): Int = {
	if (myMax(one, two) > myMax(two, three)) myMax(one, two) else myMax(two, three)
}


//Problem 3:
//Solved by myself: Y
//Time taken: 3 min
//[Contract] volume: Int, Int, Int -> Int
//[Purpose] to find coboid
//[Tests] (2, 3, 4)->24

def volumeCuboid(length: Int, breath: Int, height: Int): Int = {
	length * breath * height
}


//Problem 4:
//Solved by myself: Y
//Time taken: around 20
//[Contract] gcd: Int, Int -> Int
//[Purpose] find gcd
//[Tests] (12, 18)->6

def myMin(a: Int, b: Int): Int = {
	if (a < b) a else b
}

def isOne(in: Int): Int = {
	if (in == 1) 1 else 0
}

def myND(start: Int, now: Int): Int = {
	if ((isOne(now) == 0) && (start == now || (start % now) != 0)) myND(start, now - 1) else now
}

def findGCD(a: Int, b: Int, div: Int): Int = {
	if ((a % div) == 0) div else findGCD(a, b, myND(b, div)) 
}

def myGCD(a: Int, b: Int): Int = {
	findGCD(myMax(a, b), myMin(a, b), myMin(a, b))
}


//Problem 5:
//Solved by myself: Y
//Time taken around 3 min
//[Contract] combination: Int, Int -> Int
//[Purpose] find combination
//[Tests] (5, 2)->10

def myFac(in: Int): Int = {
	if (in > 1) in * myFac(in - 1) else 1
}

def combination(n: Int, k: Int): Int = {
	myFac(n) / (myFac(n-k) * myFac(k))
}


//Problem 6:
//Solved by myself: N (Lecture 2: 34:19 for defining types)
//Time taken around 3 hours (I ate dinner during this problem)
//[Contract] VehicleTax: Vehicle, Int, Int, Int -> Int / isVehicleSafe: Vehicle -> String
//[Purpose] find combination
//[Tests]

trait Vehicle
case class Bycicle(wheels: Int) extends Vehicle
case class Car(wheels: Int, windows: Int) extends Vehicle
case class Airplane(wheels: Int, windows: Int, engines: Int) extends Vehicle

def vehicleTax(ve: Vehicle, tph: Int, tpw: Int, tpe: Int): Int = ve match{
	case Bycicle(wheels) => wheels * tph
	case Car(wheels, windows) => wheels * tph + windows * tpw
	case Airplane(wheels, windows, engines) => wheels * tph + windows * tpw + engines * tpe
}

def isVehicleSafe (ve: Vehicle): String = ve match{
	case Bycicle(wheels) => if (wheels < 4) "safe" else "unsafe"
	case Car(wheels, windows) => if (wheels > 3 && windows > 2) "safe" else "unsafe"
	case Airplane(wheels, windows, engines) => if (wheels > 2 && windows > 10 && engines > 1) "safe" else "unsafe"
}

//Problem 7:
//Solved by myself: N (gemini "스칼라에서 'a', 'b', 'c'가 포함된 list를 주면 'Apple', 'Babo', 'Chocolate' 이런 단어로 나오는 함수를 만들고 싶어 이 단어리스트는 내가 만들고싶고")

/*
gemini's code
def transformList(chars: List[Char]): List[String] = {
  // 1. 문자와 단어를 짝지어주는 Map을 만듭니다.
  // 이 부분은 원하는 대로 자유롭게 수정할 수 있습니다.
  val wordMap = Map(
    'a' -> "Apple",
    'b' -> "Babo",
    'c' -> "Chocolate"
    // 필요하다면 다른 문자들도 추가할 수 있습니다.
    // 'd' -> "Donut",
    // 'e' -> "Elephant"
  )

  // 2. 입력받은 리스트(chars)의 각 문자를 map을 이용해 단어로 변환합니다.
  // wordMap.getOrElse(char, "알 수 없는 문자")는 Map에 해당 문자가 없을 경우
  // "알 수 없는 문자"라는 기본값을 반환하도록 하여 오류를 방지합니다.
  chars.map(char => wordMap.getOrElse(char, s"${char}: 정의되지 않은 문자"))
}
*/

//Time taken around 9 min
//[Contract] List[Char] -> List[String]
//[Purpose] give list of words which starts with alphabet in given list
//[Tests]

def nameAlphabet(arr: List[Char]): List[String] = {
	val wordMap = Map(
		'a' -> "alice",
		'c' -> "cherry",
		'j' -> "jc",
		'k' -> "kate"
	arr.map(char => wordMap.getOrElse(char, s"${char}: unamed"))
}

//Problem 8:
//Solved by myself: N
//Time taken around infinite min
//[Contract] List[Char] -> List[String]
//[Purpose] give list of words which starts with alphabet in given list
//[Tests]
