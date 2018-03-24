package skinny.orm

import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import scalikejdbc._
import skinny.orm.feature._

case class Member(
    id: Long,
    name: Option[Name] = None,
    countryId: Long,
    mentorId: Option[Long],
    companyId: Option[Long],
    createdAt: DateTime,
    country: Country,
    company: Option[Company] = None,
    mentor: Option[Member] = None,
    mentorees: Seq[Member] = Nil,
    groups: Seq[Group] = Nil,
    skills: Seq[Skill] = Nil
)

object Member extends SkinnyCRUDMapper[Member] {
  override val tableName    = "members"
  override val defaultAlias = createAlias("m")
  val mentorAlias           = createAlias("mentor")
  val mentoreeAlias         = createAlias("mentoree")

  // if you use hasOne, joined entity should be Option[Entity]
  // this code should be here
  innerJoinWithDefaults(Country, (m, c) => sqls.eq(m.countryId, c.id)).byDefaultEvenIfAssociated

  // one-to-one
  val companyOpt = belongsTo[Company](Company, (m, c) => m.copy(company = c))
    .includes[Company](
      (ms, cs) =>
        ms.map { m =>
          cs.find(c => m.company.exists(_.id == c.id)).map(v => m.copy(company = Some(v))).getOrElse(m)
      }
    )
    .byDefault

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

  // groups
  hasManyThroughWithFk[Group](
    GroupMember,
    GroupMapper,
    "memberId",
    "groupId",
    (member, gs) => member.copy(groups = gs)
  ).byDefault
  // if GroupMapper is "Group", this code will work
  //hasManyThrough[Group](GroupMember, Group, (member, groups) => member.copy(groups = groups)).byDefault

  // skills
  val skillsSimpleRef = hasManyThrough[Skill](MemberSkill, Skill, (member, ss) => member.copy(skills = ss))
  // full definition example
  val skillsVerboseRef = hasManyThrough[MemberSkill, Skill](
    through = MemberSkill -> MemberSkill.createAlias("ms2"),
    throughOn = (m: Alias[Member], ms: Alias[MemberSkill]) => sqls.eq(m.id, ms.memberId),
    many = Skill -> Skill.createAlias("s2"),
    on = (ms: Alias[MemberSkill], s: Alias[Skill]) => sqls.eq(ms.skillId, s.id),
    merge = (member, ss) => member.copy(skills = ss)
  )

  // mentorees
  val mentorees = hasMany[Member](
    many = Member -> Member.mentoreeAlias,
    on = (m, mentorees) => sqls.eq(m.id, mentorees.mentorId),
    merge = (member, mentorees) => member.copy(mentorees = mentorees)
  ).includes[Member](
      (ms, mts) =>
        ms.map { m =>
          m.copy(mentorees = mts.filter(_.mentorId.exists(_ == m.id)))
      }
    )
    .byDefault

  override def extract(rs: WrappedResultSet, n: ResultName[Member]): Member = new Member(
    id = rs.long(n.id),
    countryId = rs.long(n.countryId),
    companyId = rs.longOpt(n.companyId),
    mentorId = rs.longOpt(n.mentorId),
    createdAt = rs.jodaDateTime(n.createdAt),
    country = Country(rs) // cannot use #autoConstruct in this case
  )
}

case class Name(memberId: Long,
                first: String,
                last: String,
                createdAt: DateTime,
                updatedAt: Option[DateTime] = None,
                member: Option[Member] = None)

object Name extends SkinnyCRUDMapper[Name] with TimestampsFeature[Name] with OptimisticLockWithTimestampFeature[Name] {

  override val tableName              = "names"
  override val lockTimestampFieldName = "updatedAt"

  override val useAutoIncrementPrimaryKey = false
  override val primaryKeyFieldName        = "memberId"

  override val defaultAlias = createAlias("nm")

  val member = belongsTo[Member](Member, (name, member) => name.copy(member = member)).byDefault

  def extract(rs: WrappedResultSet, s: ResultName[Name]): Name = autoConstruct(rs, s, "member")
}

case class Company(id: Option[Long] = None,
                   name: String,
                   countryId: Option[Long] = None,
                   country: Option[Country] = None,
                   members: Seq[Member] = Nil)

object Company extends SkinnyCRUDMapper[Company] with SoftDeleteWithBooleanFeature[Company] {
  override val tableName    = "companies"
  override val defaultAlias = createAlias("cmp")

  val countryOpt = belongsTo[Country](Country, (c, cnt) => c.copy(country = cnt)).byDefault

  val members = hasMany[Member](
    many = Member -> Member.defaultAlias,
    on = (c, ms) => sqls.eq(c.c("id"), ms.companyId),
    merge = (c, ms) => c.copy(members = ms)
  ).includes[Member](merge = (cs, ms) => cs.map(c => c.copy(members = ms.filter(_.companyId == c.id))))

  def extract(rs: WrappedResultSet, s: ResultName[Company]): Company = autoConstruct(rs, s, "country", "members")
}

case class Country(id: Long, name: String) extends SkinnyRecord[Country] {
  def skinnyCRUDMapper = Country
}

object Country extends SkinnyCRUDMapper[Country] {
  override val tableName                                             = "countries"
  override val defaultAlias                                          = createAlias("cnt")
  def extract(rs: WrappedResultSet, s: ResultName[Country]): Country = autoConstruct(rs, s)
}

case class Group(id: Long, name: String)

// using different name is ok though a little bit verbose, mapper must not be the companion.
object GroupMapper extends SkinnyCRUDMapper[Group] with SoftDeleteWithTimestampFeature[Group] {
  override val tableName                                         = "groups"
  override val defaultAlias                                      = createAlias("g")
  def extract(rs: WrappedResultSet, s: ResultName[Group]): Group = autoConstruct(rs, s)

  private[this] val logger = LoggerFactory.getLogger(classOf[Group])

  beforeCreate((session: DBSession, namedValues: Seq[(SQLSyntax, Any)]) => {
    logger.info(s"Before creation. params: ${namedValues}")
  })
  afterCreate((session: DBSession, namedValues: Seq[(SQLSyntax, Any)], generatedId: Option[Long]) => {
    logger.info(s"Created Group's id: ${generatedId}")
  })
}

case class GroupMember(groupId: Long, memberId: Long)

object GroupMember extends SkinnyJoinTable[GroupMember] {
  override val tableName    = "groups_members"
  override val defaultAlias = createAlias("gm")
}

case class Skill(id: Long, name: String, createdAt: DateTime, updatedAt: DateTime, lockVersion: Long)

object Skill
    extends SkinnyCRUDMapper[Skill]
    with TimestampsFeature[Skill]
    with OptimisticLockWithVersionFeature[Skill] {

  override val tableName                                                  = "skills"
  override val defaultAlias                                               = createAlias("s")
  override def extract(rs: WrappedResultSet, s: ResultName[Skill]): Skill = autoConstruct(rs, s)
}

case class MemberSkill(memberId: Long, skillId: Long)

object MemberSkill extends SkinnyJoinTable[MemberSkill] {
  override val tableName    = "members_skills"
  override val defaultAlias = createAlias("ms")
}
