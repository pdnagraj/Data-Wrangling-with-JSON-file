import Wrangling._
import hw.json._

class TestSuite extends org.scalatest.FunSuite {

 test("Key"){
   val json1: Json = JsonDict(Map(JsonString("state") -> JsonString("MA")))
   assert(key(json1, "state") == Some(JsonString("MA")))
  } 

  test("fromstate"){
  val myJson: Json = JsonDict(Map(JsonString("state") -> JsonString("MA")))
  val moreJson: Json = JsonDict(Map(JsonString("state") -> JsonString("NJ"), JsonString("tate") -> JsonString("J") ))
  val jsonList: List[Json] = List(myJson, moreJson)

  assert(isFromState(myJson, "MA"))
  assert(fromState(jsonList, "NJ") == List(moreJson))
  }

test("rating less than"){
  val json1: Json = JsonDict(Map(JsonString("stars") -> JsonNumber(4.0) ))
  val json2: Json = JsonDict(Map(JsonString("stars") -> JsonNumber(5.7) ))
  val jsonList: List[Json] = List(json1, json2)

  assert(ratingLT(jsonList, 5.6) == List(json1))
  }

  test("rating Greater than"){
  val json1: Json = JsonDict(Map(JsonString("stars") -> JsonNumber(4.0) ))
  val json2: Json = JsonDict(Map(JsonString("stars") -> JsonNumber(5.7) ))
  val jsonList: List[Json] = List(json1, json2)

  assert(ratingGT(jsonList, 5.6) == List(json2))
  }

  test("categorize"){
  
  val json1: Json = JsonDict(Map(JsonString("categories") -> JsonArray(List(JsonString("Food")))))
  val jsonList: List[Json] = List(json1)

  assert(category(jsonList, "Food") == List(json1))
  }
}
