class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {

  private var balance = initialBalance
  
  def withdraw(amount: Double): Unit = {
    this.balance -= amount
  }
  def deposit(amount: Double): Unit = {
    this.balance += amount
  }
  def getBalanceAmount: Double = { this.balance }
}
