package skinny.orm

import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import scalikejdbc._
import skinny.orm.feature._

case class Member(
    id: Long,
    name: Option[Name] = None,
    mentor: Option[Member] = None
)

object Member extends SkinnyCRUDMapper[Member] {
  override val tableName    = "members"
  override val defaultAlias = createAlias("m")
  val mentorAlias           = createAlias("mentor")

  val mentor =
    belongsToWithAlias[Member](Member -> Member.mentorAlias, (m, mentor) => m.copy(mentor = mentor)).byDefault
  val name =
    hasOne[Name](Name, (m, name) => m.copy(name = name))
      .includes[Name](
        (ms, ns) =>
          ms.map { m =>
            ns.find(n => m.name.exists(_.memberId == m.id)).map(v => m.copy(name = Some(v))).getOrElse(m)
        }
      )
      .byDefault

  override def extract(rs: WrappedResultSet, n: ResultName[Member]): Member = new Member(
    id = rs.long(n.id)
  )
}

case class Name(memberId: Long,
                member: Option[Member] = None)

object Name extends SkinnyCRUDMapper[Name] with TimestampsFeature[Name] with OptimisticLockWithTimestampFeature[Name] {

  override val tableName              = "names"

  override val useAutoIncrementPrimaryKey = false
  override val primaryKeyFieldName        = "memberId"

  override val defaultAlias = createAlias("nm")

  def extract(rs: WrappedResultSet, s: ResultName[Name]): Name = autoConstruct(rs, s, "member")
}
