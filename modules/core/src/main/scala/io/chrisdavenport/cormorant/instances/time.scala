package io.chrisdavenport.cormorant.instances

import cats.implicits._
import io.chrisdavenport.cormorant._
import java.time.{
  Duration,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
  Period,
  YearMonth,
  ZonedDateTime,
  ZoneId
}
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.{
  ISO_LOCAL_DATE,
  ISO_LOCAL_DATE_TIME,
  ISO_LOCAL_TIME,
  ISO_OFFSET_DATE_TIME,
  ISO_OFFSET_TIME,
  ISO_ZONED_DATE_TIME
}
import scala.util.Try

trait time {
  implicit final val instantGet: Get[Instant] = Get.tryOrMessage(
    field => Try(Instant.parse(field.x)),
    field => s"Failed to decode Instant: Received Field $field"
  )
  implicit final val instantPut: Put[Instant] = base.stringPut.contramap(_.toString)

  implicit final val zoneIdGet: Get[ZoneId] = Get.tryOrMessage(
    field => Try(ZoneId.of(field.x)),
    field => s"Failed to decode ZoneId: Received Field $field"
  )
  implicit final val zoneIdPut: Put[ZoneId] = base.stringPut.contramap(_.getId)

  final def getLocalDateTime(formatter: DateTimeFormatter): Get[LocalDateTime] =
    Get.tryOrMessage(
      field => Try(LocalDateTime.parse(field.x, formatter)),
      field => s"Failed to decode LocalDateTime: Received Field $field"
    )

  final def putLocalDateTime(formatter: DateTimeFormatter): Put[LocalDateTime] =
    base.stringPut.contramap(_.format(formatter))

  implicit final val localDateTimeGetDefault = getLocalDateTime(ISO_LOCAL_DATE_TIME)
  implicit final val localDateTimePutDefault = putLocalDateTime(ISO_LOCAL_DATE_TIME)

  final def getZonedDateTime(formatter: DateTimeFormatter): Get[ZonedDateTime] =
    Get.tryOrMessage(
      field => Try(ZonedDateTime.parse(field.x, formatter)),
      field => s"Failed to decode ZonedDateTime: Received Field $field"
    )
  final def putZonedDateTime(formatter: DateTimeFormatter): Put[ZonedDateTime] = 
    base.stringPut.contramap(_.format(formatter))

  implicit final val zonedDateTimeGetDefault = getZonedDateTime(ISO_ZONED_DATE_TIME)
  implicit final val zonedDateTimePutDefault = putZonedDateTime(ISO_ZONED_DATE_TIME)

  final def getOffsetDateTime(formatter: DateTimeFormatter): Get[OffsetDateTime] = 
    Get.tryOrMessage(
      field => Try(OffsetDateTime.parse(field.x, formatter)),
      field => s"Failed to decode OffsetDateTime: Received Field $field"
    )
  final def putOffsetDateTime(formatter: DateTimeFormatter): Put[OffsetDateTime] =
    base.stringPut.contramap(_.format(formatter))

  implicit final val offsetDateTimeGetDefault = getOffsetDateTime(ISO_OFFSET_DATE_TIME)
  implicit final val offsetDateTimePutDefault = putOffsetDateTime(ISO_OFFSET_DATE_TIME)

  final def getLocalDate(formatter: DateTimeFormatter): Get[LocalDate] = 
    Get.tryOrMessage(
      field => Try(LocalDate.parse(field.x, formatter)),
      field => s"Failed to decode LocalDate: Received Field $field"
    )
  final def putLocalDate(formatter: DateTimeFormatter): Put[LocalDate] =
    base.stringPut.contramap(_.format(formatter))

  implicit final val localDateGetDefault = getLocalDate(ISO_LOCAL_DATE)
  implicit final val localDatePutDefault = putLocalDate(ISO_LOCAL_DATE)

  final def getLocalTime(formatter: DateTimeFormatter): Get[LocalTime] = 
    Get.tryOrMessage(
      field => Try(LocalTime.parse(field.x, formatter)),
      field => s"Failed to decode LocalTime: Received Field $field"
    )
  final def putLocalTime(formatter: DateTimeFormatter): Put[LocalTime] =
    base.stringPut.contramap(_.format(formatter))

  implicit final val localTimeGetDefault = getLocalTime(ISO_LOCAL_TIME)
  implicit final val localTimePutDefault = putLocalTime(ISO_LOCAL_TIME)

  final def getOffsetTime(formatter: DateTimeFormatter): Get[OffsetTime] =
    Get.tryOrMessage(
      field => Try(OffsetTime.parse(field.x, formatter)),
      field => s"Failed to decode OffsetTime: Received Field $field"
    )
  final def putOffsetTime(formatter: DateTimeFormatter): Put[OffsetTime] =
    base.stringPut.contramap(_.format(formatter))

  implicit final val offsetTimeGetDefault = getOffsetTime(ISO_OFFSET_TIME)
  implicit final val offsetTimePutDefault = putOffsetTime(ISO_OFFSET_TIME)

  final def getYearMonth(formatter: DateTimeFormatter): Get[YearMonth] = 
    Get.tryOrMessage(
      field => Try(YearMonth.parse(field.x, formatter)),
      field => s"Failed to decode YearMonth: Received Field $field"
    )
  final def putYearMonth(formatter: DateTimeFormatter): Put[YearMonth] =
    base.stringPut.contramap(_.format(formatter))

  private final val yearMonthFormatter = DateTimeFormatter.ofPattern("yyyy-MM")
  implicit final val yearMonthGetDefault = getYearMonth(yearMonthFormatter)
  implicit final val yearMonthPutDefault = putYearMonth(yearMonthFormatter)

  implicit final val getPeriod: Get[Period] = Get.tryOrMessage(
    field => Try(Period.parse(field.x)),
    field => s"Failed to decode Period: Received Field $field"
  )
  implicit final val putPeriod: Put[Period] = 
    base.stringPut.contramap(_.toString)

  implicit final val durationGet: Get[Duration] = Get.tryOrMessage(
    field => Try(Duration.parse(field.x)),
    field => s"Failed to decode Duration: Received Field $field"
  )
  implicit final val durationPut: Put[Duration] =
    base.stringPut.contramap(_.toString)

}

object time extends time