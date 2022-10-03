package io.circe.derivation

import io.circe.examples.{Bar, Baz, Foo, Qux}
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import io.circe.testing.CodecTests

object StrictDecodingSuiteCodecs extends Serializable {
  implicit val decodeFoo: Decoder[Foo] = deriveDecoder(renaming.snakeCase, true, None, true)
  implicit val encodeFoo: Encoder.AsObject[Foo] = deriveEncoder(renaming.snakeCase, None)
  val codecForFoo: Codec.AsObject[Foo] = deriveCodec(renaming.snakeCase, true, None, true)

  implicit val decodeBar: Decoder[Bar] = deriveDecoder(renaming.snakeCase, true, None, true)
  implicit val encodeBar: Encoder.AsObject[Bar] = deriveEncoder(renaming.snakeCase, None)
  val codecForBar: Codec.AsObject[Bar] = deriveCodec(renaming.snakeCase, true, None, true)

  implicit val decodeBaz: Decoder[Baz] = deriveDecoder(renaming.snakeCase, true, None, true)
  implicit val encodeBaz: Encoder.AsObject[Baz] = deriveEncoder(renaming.snakeCase, None)
  val codecForBaz: Codec.AsObject[Baz] = deriveCodec(renaming.snakeCase, true, None, true)

  implicit def decodeQux[A: Decoder]: Decoder[Qux[A]] =
    deriveDecoder(renaming.replaceWith("aa" -> "1", "bb" -> "2"), true, None, true)
  implicit def encodeQux[A: Encoder]: Encoder.AsObject[Qux[A]] =
    deriveEncoder(renaming.replaceWith("aa" -> "1", "bb" -> "2"), None)
  def codecForQux[A: Decoder: Encoder]: Codec.AsObject[Qux[A]] = deriveCodec(
    renaming.replaceWith("aa" -> "1", "bb" -> "2"),
    true,
    None,
    true
  )
}

class StrictDecodingSuite extends CirceSuite {
  import StrictDecodingExample._
  import StrictDecodingSuiteCodecs._

  checkAll("Codec[Foo]", CodecTests[Foo].codec)
  checkAll("Codec[Foo] via Codec", CodecTests[Foo](codecForFoo, codecForFoo).codec)
  checkAll("Codec[Bar]", CodecTests[Bar].codec)
  checkAll("Codec[Bar] via Codec", CodecTests[Bar](codecForBar, codecForBar).codec)
  checkAll("Codec[Baz]", CodecTests[Baz].codec)
  checkAll("Codec[Baz] via Codec", CodecTests[Baz](codecForBaz, codecForBaz).codec)
  checkAll("Codec[Qux[Baz]]", CodecTests[Qux[Baz]].codec)
  checkAll("Codec[Qux[Baz]] via Codec", CodecTests[Qux[Baz]](codecForQux, codecForQux).codec)

  checkAll("Codec[User]", CodecTests[User].codec)
  checkAll("Codec[User] via Codec", CodecTests[User](User.codecForUser, User.codecForUser).codec)

  checkAll(
    "CodecAgreementWithCodec[Foo]",
    CodecAgreementTests[Foo](
      codecForFoo,
      codecForFoo,
      decodeFoo,
      encodeFoo
    ).codecAgreement
  )

  checkAll(
    "CodecAgreementWithCodec[Bar]",
    CodecAgreementTests[Bar](
      codecForBar,
      codecForBar,
      decodeBar,
      encodeBar
    ).codecAgreement
  )

  checkAll(
    "CodecAgreementWithCodec[Baz]",
    CodecAgreementTests[Baz](
      codecForBaz,
      codecForBaz,
      decodeBaz,
      encodeBaz
    ).codecAgreement
  )

  checkAll(
    "CodecAgreementWithCodec[Qux[Baz]]",
    CodecAgreementTests[Qux[Baz]](
      codecForQux,
      codecForQux,
      decodeQux,
      encodeQux
    ).codecAgreement
  )

  "deriveDecoder" should "return error when json has extra fields" in {
    val json = Json.obj(
      "first_name" -> Json.fromString("John"),
      "last_name" -> Json.fromString("Smith"),
      "role" -> Json.obj("TITLE" -> Json.fromString("Entrepreneur")),
      "address" -> Json.obj(
        "#" -> Json.fromInt(5),
        "street" -> Json.fromString("Elm Street"),
        "city" -> Json.fromString("Springfield"),
        "foo" -> Json.fromString("bar")
      )
    )

    json.as[User] match {
      case Left(DecodingFailure(str, ops)) =>
        assert(str === "Unexpected field: [foo]; valid fields: #, street, city")
      case a =>
        fail(s"Expected decoding failure, got $a")
    }
  }
}
