import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = synchronized {
    this.balance.amount = this.balance.amount - amount match {
      case b if b < 0.0 => throw new NoSufficientFundsException("Not enough money in account")
      case b if b > this.balance.amount => throw new IllegalAmountException("Negative withdrawal amount")
      case b => b
    }
  }

  def deposit(amount: Double): Unit = synchronized {
    this.balance.amount = this.balance.amount + amount match {
      case b if b < this.balance.amount => throw new IllegalAmountException("Negative withdrawal amount")
      case b => b
    }
  }

  def getBalanceAmount: Double = { this.balance.amount }

  def transferTo(account: Account, amount: Double) = {
    bank addTransactionToQueue (this, account, amount)
  }


}
