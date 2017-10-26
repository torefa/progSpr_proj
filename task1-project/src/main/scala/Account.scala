import exceptions._

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {

  private var balance = initialBalance
  
  def withdraw(amount: Double): Unit = synchronized {
    balance = balance - amount match {
      case b if b < 0.0 => throw new NoSufficientFundsException("Not enough money in account")
      case b if b > balance => throw new IllegalAmountException("Negative withdrawal amount")
      case b => b
    }
  }

  def deposit(amount: Double): Unit = synchronized {
    balance = balance + amount match {
      case b if b < balance => throw new IllegalAmountException("Negative withdrawal amount")
      case b => b
    }
  }

  def getBalanceAmount: Double = { balance }
}
