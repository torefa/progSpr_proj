import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    // Return whether all Transaction-objects in transactions are completed
    getTransactions.foldLeft(true)(_ && _.isCompleted)
  }

  def withdraw(amount: Double): Unit = {
    balance.amount = balance.amount - amount match {
      case b if b < 0.0 => throw new NoSufficientFundsException("Not enough money in account")
      case b if b > balance.amount => throw new IllegalAmountException("Negative withdrawal amount")
      case b => b
    }
  }

  def deposit(amount: Double): Unit = {
    balance.amount = balance.amount + amount match {
      case b if b < balance.amount => throw new IllegalAmountException("Negative withdrawal amount")
      case b => b
    }
  }

  def getBalanceAmount: Double = { balance.amount }

  def sendTransactionToBank(t: Transaction): Unit = {
    //Send a message containing t to the bank of this account
    BankManager.findBank(bankId) ! t
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {

    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

    if (reserveTransaction(t)) {
      try {
        withdraw(amount)
        sendTransactionToBank(t)

      } catch {
        case _: NoSufficientFundsException | _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }
    }
    t
  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    case TransactionRequestReceipt(to, transactionId, transaction) => {
      // Process receipt
      if (transactions.contains(transactionId)) {

        /*TODO: Her er det ikke vits å hente t fra mappen, fordi vi sender rundt referanser av Transaksjoner.
         men kan være greit å late det er pass by value, tror det er meningen,
         spesielt hvis man tenker på hvordan et bank-system med flere banker egentlig ville vært*/

        val t = transactions(transactionId) //transaction
        t.status = transaction.status
        t.receiptReceived = true

        /*If transaction fails, re-instate the amount that was withdrawn earlier.*/
        if (t.status == TransactionStatus.FAILED) {
          deposit(t.amount)
        }

      }
    }

    case BalanceRequest => sender ! getBalanceAmount // Should return current balance

    case t: Transaction => {
      // Handle incoming transaction
      
      try {
        deposit(t.amount)
        t.status = TransactionStatus.SUCCESS

      } catch {
        case _: IllegalAmountException =>
          t.status = TransactionStatus.FAILED
      }

      sender ! TransactionRequestReceipt(t.from, t.id, t)
    }

    case msg => {/*???*/}
  }


}
