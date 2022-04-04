package com.mycode.implementations

import scala.annotation.tailrec

object CaesarCipher:
  
  // Original alphabet used to filter the original message and do permutations in it
  val alphabet: Vector[Char] =
    Vector(
      'A', 'B', 'C', 'D', 'E',
      'F', 'G', 'H', 'I', 'J',
      'K', 'L', 'M', 'N', 'O',
      'P', 'Q', 'R', 'S', 'T',
      'U', 'V', 'W', 'X', 'Y',
      'Z'
    )

  // Extension method for Char
  extension (ch: Char)

    // "Moves" by getting the index of the char in the alphabet added with the nth places.
    infix def move (places: Int, alp: Vector[Char] = alphabet): Char =
      val index = alp.indexOf(ch)

      if places <= 0 then alp((places + index+ alp.length) % alp.length)
      else alp((places + index) % alp.length)


  // Groups the message by filter all the characters outside alphabet
  def groupMessage(message: String, alp: Vector[Char] = alphabet): Vector[Char] =

    message
      .toUpperCase
      .toVector
      .filter(c => alp.contains(c))

  end groupMessage


  // Transforms a vector into a String
  def unGroupMessage(message: Vector[Char]): String =
    message.mkString


  // Encrypt function
  def encrypt(key: Int)(message: String): String =

    val cipherMessage = groupMessage(message) map (ch => ch move key)

    unGroupMessage(cipherMessage.grouped(5).toVector.flatMap(vec => s"${vec.mkString} "))

  end encrypt


  // Decrypt function
  def decrypt(key: Int)(secretMessage: String): String =

    val cipherMessage = groupMessage(secretMessage) map (ch => ch move (-1 * key))

    unGroupMessage(cipherMessage.grouped(5).toVector.flatMap(vec => s"${vec.mkString} "))

  end decrypt

end CaesarCipher

