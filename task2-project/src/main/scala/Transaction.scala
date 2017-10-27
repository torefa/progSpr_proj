import exceptions._
import scala.collection.mutable.Queue

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  val queue = Queue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = synchronized {
    queue dequeue
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = synchronized {
    queue isEmpty
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = synchronized {
    queue enqueue t
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = synchronized {
    queue front
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = synchronized {
    queue iterator
  }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  var attempts = allowedAttemps

  override def run: Unit = {

    status synchronized {
      
      if (attempts <= 0) {
        status = TransactionStatus.FAILED
        return
      }

      def doTransaction() = {
        from withdraw amount
        to deposit amount
      }

      try { 

        if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
        } else to synchronized {
          from synchronized {
            doTransaction
          }
        }

        status = TransactionStatus.SUCCESS

      } catch {
        case e: Exception => {}
      }

      attempts -= 1
    }
    // Extend this method to satisfy new requirements.

  }
}
