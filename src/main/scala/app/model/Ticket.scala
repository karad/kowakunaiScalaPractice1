package model

/**
 *
 * @author kazuhiro
 * @since 2014/10/10
 */
sealed trait Ticket {
  val id:Long
  val title:String
  val status:TicketStatus
}

case class Issue(id:Long, title:String, status:TicketStatus = TicketStatus.Open) extends Ticket
case class Bug(id:Long, title:String, description:String, status:TicketStatus = TicketStatus.Open) extends Ticket