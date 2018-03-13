import hw.json._
import hw.wrangling.WranglingLike

object Wrangling extends WranglingLike {

val data: List[Json] = JsonHelper.fromFile("yelp.json")

def key(json: Json, key: String): Option[Json] = json match{
  case JsonDict(aMap) => aMap.get(JsonString(key))
  case _ => None
}

def isFromState(datum: Json, state: String): Boolean = datum match {
  case JsonDict(aMap) => aMap.get(JsonString("state")) match {
    case None => false
    case Some(stateName) => stateName match {
      case JsonString(x) => x == state
      case _ => false
    }
  }
  case _ => false
}

def fromState(data: List[Json], state: String): List[Json] = {
  data.filter(datum => isFromState(datum, state))
}


def ratingLTHelper(datum: Json, rating: Double): Boolean = datum match {
  case JsonDict(aMap) => aMap.get(JsonString("stars")) match {
    case None => false
    case Some(ratingScore) => ratingScore match{
      case JsonNumber(f) => (f <= rating)
      case _ => false
    }
  }
  case _=> false
}
  def ratingLT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => ratingLTHelper(datum, rating))

  }

 def ratingGTHelper(datum: Json, rating: Double): Boolean = datum match {
  case JsonDict(aMap) => aMap.get(JsonString("stars")) match {
    case None => false
    case Some(ratingScore) => ratingScore match{
      case JsonNumber(f) => (f >= rating)
      case _ => false
    }
  }
  case _=> false
}
  def ratingGT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => ratingGTHelper(datum, rating))
  }

  def containsCategory(alist: List[Json], cate: String): Boolean = alist match{
    case Nil => false
    case head :: tail =>{
       if(head.toString() == JsonString(cate).toString()) true
       else containsCategory(tail, cate)
    }
  } 


  def categoryHelper(datum: Json, cate: String): Boolean = datum match {
    case JsonDict(aMap) => aMap.get(JsonString("categories")) match{
      case None => false
      case Some(categoryThing) => categoryThing match {
        case JsonArray(x) => { 
        containsCategory(x, cate)}
        case _ => false
      }
    }
    case _=> false
  }


  def category(data: List[Json], cate: String): List[Json] = { 
    data.filter(datum => categoryHelper(datum, cate))
  }

 def groupByState(data: List[Json]): Map[String, List[Json]] =
  data.groupBy(datum => datum match {
    case JsonDict(aMap) => aMap.get(JsonString("state")) match {
      case Some(JsonString(stateCode)) => stateCode
      case _ => "no state"
    }
    case _ => "no state"
  })
  

  def getRating(head: Json): Double = { 
    head match{
      case JsonDict(aMap) => aMap.get(JsonString("stars")) match {
        case None => 0
        case Some(ratingScore) => ratingScore match{
          case JsonNumber(f) => f
          case _ => 0.0
      }
      case _ => 0.0
    }
  case _ => 0.0
  }
}

  def getHighestRating(data: List[Json], hNumber: Double): Double = {
    
    data match {
      case Nil => hNumber
      case head :: tail => {    if(getRating(head) > hNumber){ getHighestRating(tail, getRating(head))} 
                                else {getHighestRating(tail, hNumber)} }
    } 
  }

  def numberOfRatingsHelper(datum: Json, revCount: Double): Boolean = datum match {
  case JsonDict(aMap) => aMap.get(JsonString("review_count")) match {
    case None => false
    case Some(revScore) => revScore match{
      case JsonNumber(f) => (f >= revCount)
      case _ => false
    }
  }
  case _=> false
}
  def numberOfRatings(data: List[Json]): List[Json] = {
    data.filter(datum => numberOfRatingsHelper(datum, -1))
  }

  def bestPlace(data: List[Json]): Option[Json] = {
   ratingGT(data, getHighestRating(data, -1)) match {
      case Nil => None
      case head :: Nil => Some(head)
      case head :: tail => { numberOfRatings(head :: tail) match {
              case Nil => None
              case head :: Nil => Some(head)
              case head :: tail => Some(head)
          }

        }

      }


    }

  def hasAmbienceHelper(datum: Json, ambience: String): Boolean  =  datum match{
    case JsonDict(aMap) => aMap.get(JsonString("attributes")) match {
      case None => false
      case Some(JsonDict(attribu)) => attribu.get(JsonString("Ambience")) match {
          case None => false
            case Some(JsonDict(amb)) => amb.get(JsonString(ambience)) match{
              case None => false
              case Some(JsonBool(x)) => x
              case _=> false  
            } 
          case _ => false
        }
        case _ => false
      }
   case _ => false
  }


  def hasAmbience(data: List[Json], ambience: String): List[Json] = {
      data.filter(datum => hasAmbienceHelper(datum, ambience))
  }
  

}
