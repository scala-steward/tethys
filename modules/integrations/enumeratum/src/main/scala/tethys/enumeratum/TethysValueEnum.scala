package tethys.enumeratum

import _root_.enumeratum.values._
import tethys.readers.KeyReader
import tethys.writers.KeyWriter
import tethys.{JsonReader, JsonWriter}

sealed trait TethysValueEnum[
    ValueType,
    EntryType <: ValueEnumEntry[ValueType]
] { this: ValueEnum[ValueType, EntryType] =>
  implicit def tethysReader: JsonReader[EntryType]
  implicit def tethysWriter: JsonWriter[EntryType]
}

trait StringTethysEnum[E <: StringEnumEntry]
    extends TethysValueEnum[String, E] { this: ValueEnum[String, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
  implicit val tethysKeyReader: KeyReader[E] =
    Enumeratum.keyReader(this)(_.withValueOpt)
  implicit val tethysKeyWriter: KeyWriter[E] = Enumeratum.keyWriter(_.value)
}

trait IntTethysEnum[E <: IntEnumEntry] extends TethysValueEnum[Int, E] {
  this: ValueEnum[Int, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
}

trait LongTethysEnum[E <: LongEnumEntry] extends TethysValueEnum[Long, E] {
  this: ValueEnum[Long, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
}

trait ShortTethysEnum[E <: ShortEnumEntry] extends TethysValueEnum[Short, E] {
  this: ValueEnum[Short, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
}
