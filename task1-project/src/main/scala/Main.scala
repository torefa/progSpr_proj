object Main extends App {

  def thread(body: => Unit): Thread = {
      val t = new Thread {
        override def run() = body
      }
      t.start
      t
    }

  // A few transaction examples using Threads

  val account1 = new Account(70000)
  val account2 = new Account(0)

  val first = thread {
    for (i <- 0 until 100) { Bank transaction (account1, account2, 30) }
  }

  val second = thread {
    for (i <- 0 until 100) { Bank transaction (account1, account2, 30) }
  }

  first.join(); second.join()

  val a1 = account1.getBalanceAmount
  println(s"Account 1 Balance: $a1")
  val a2 = account2.getBalanceAmount
  println(s"Account 2 Balance: $a2")
}
