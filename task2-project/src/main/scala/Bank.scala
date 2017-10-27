import scala.concurrent.forkjoin.ForkJoinPool
import concurrent._
import java.util.concurrent.atomic.AtomicInteger

class Bank(val allowedAttempts: Integer = 3) {

  private val uid = new AtomicInteger(0)
  private val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext = scala.concurrent.ExecutionContext.global

  //Start processing
  this processTransactions

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue push new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
  }

  def generateAccountId: Int = {
    uid incrementAndGet
  }

  private def processTransactions: Unit = {

    val process = Main.thread {

      while(true) {
        if (! transactionsQueue.isEmpty) {

          val t = transactionsQueue peek

          t synchronized {

            t.status match {
              case TransactionStatus.PENDING => executorContext.execute(t)
              case _ => processedTransactions.push(transactionsQueue.pop)
            }
          }
        }
        Thread.sleep(3);
      }
    }
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

}
