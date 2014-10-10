package repo

import model.TicketStatus.{Open, Fixed}
import model.{TicketStatus, Bug, Issue, Ticket}

/**
 * PartialFunction
 * keys.max
 * sortBy
 * map.values
 * toSeq
 * collect
 *
 * @author kazuhiro
 * @since 2014/10/10
 */
object TicketRepo {
  private var map:Map[TicketId, Ticket] = Map(
    1L -> Issue(1, "MAKE MAIN PAGE", Fixed),
    2L -> Issue(2, "MAKE SUB PAGE"),
    3L -> Bug(3, "DIFFERENT ICON", "YOU SHOULD FIX"),
    4L -> Bug(4, "DIFFERENT NEXT PAGE", "YOU SHOULD FIX, TOO", Fixed)
  )

  def findAll():Seq[Ticket] = {
    map.values.toSeq.sortBy(_.id)
  }

  def findById(id:TicketId):Option[Ticket] = {
    map.get(id)
  }

  def createIssue(title:String):Issue = {
    val nextId:TicketId = map.keys.max + 1
    val issue:Issue = Issue(nextId, title)
    map = map + (nextId -> issue)
    issue
  }

  def createBug(title:String, description:String):Bug = {
    val nextId:TicketId = map.keys.max + 1
    val bug:Bug = Bug(nextId, title, description)
    map = map + (nextId -> bug)
    bug
  }

  def findIssuesByStatus(status:String):Option[Seq[Issue]] = {
    val st:Option[TicketStatus] = TicketStatus.of(status)
    st.map(s => {
      // PartialFunction
      // {case ...}はPartialFunctionのリテラル
      map.values.toSeq.collect({case x:Issue if x.status == s => x})
    })
  }

  def findByStatus(status:String):Option[Seq[Bug]] = {
    val st:Option[TicketStatus] = TicketStatus.of(status)
    st.map(s => {
      // PartialFunction
      // {case ...}はPartialFunctionのリテラル
      map.values.toSeq.collect({case x:Bug if x.status == s => x})
    })
  }

  def fix(id:TicketId):Boolean = {
    val ticket:Option[Ticket] = map.values.find( t => t.id == id && t.status == Open)
    ticket.exists(t => {
      map = map.updated(t.id, t match {
        case x:Issue => Issue(x.id, x.title, Fixed)
        case x:Bug => Bug(x.id, x.title, x.description, Fixed)
      })
      true
    })
  }

}
