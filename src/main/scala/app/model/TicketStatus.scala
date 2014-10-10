package model

trait TicketStatus {}

/**
 *
 * @author kazuhiro
 * @since 2014/10/10
 */
object TicketStatus {
  // コンストラクターに引数を取れないので、フィールドは作られない
  case object Open extends TicketStatus
  case object Fixed extends TicketStatus

  def of(status:String):Option[TicketStatus] = {
    status.toLowerCase match {
      case "open" => Option(Open)
      case "fixed" => Option(Fixed)
      case _ => None
    }
  }
}
