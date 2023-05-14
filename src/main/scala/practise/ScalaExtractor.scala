package practise

object EMail {
  def apply(user: String, domain: String) = 
    println("enter apply")
    user + "@" + domain
  def unapply(str: String): Option[(String, String)] = {
    println("enter EMail unapply")
    val parts = str split "@"
    if(parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

object Domain {
  def unapplySeq(whole: String): Option[Seq[String]] = 
    Some(whole.split("\\.").reverse.toSeq)
}

object ExpandedEMail {
  def unapplySeq(email: String): Option[(String, Seq[String])] @unchecked = {
    val parts = email split "@"
    if(parts.length == 2) 
      Some(parts(0), parts(1).split("\\.").reverse.toSeq)
    else None
  }
}

object ScalaExtractor {
  @main
  def email_test = 
    "fy@10086" match 
      case email @ EMail(user, domain) => println(s"$user in $domain $email")
      case _ => println("not match")
    "fy@cumt.com" match 
      case EMail(user, domain @ Domain(com, _*)) => println(s"$user - $domain - $com")
      case _ => println(s"not match") 
    val ExpandedEMail(name, topdom, subdom @ _*) = "fy@cumt.com" : @unchecked
    println(s"$name - $topdom - $subdom ")
}