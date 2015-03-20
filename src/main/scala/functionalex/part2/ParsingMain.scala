package functionalex.part2



object ParsingMain {

  def main(args: Array[String]): Unit = {
    run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
    run(or(string("abra"), string("cadabra")))("abra") == Right("cadabra")
  }

}
