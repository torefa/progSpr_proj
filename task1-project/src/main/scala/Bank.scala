object Bank {

  private var idCounter: Int = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = {

    from withdraw amount
    to deposit amount
  }

  def getUniqueId: Int = synchronized {
    idCounter += 1 //improved by synchronizing the method.
    idCounter
  }

}
