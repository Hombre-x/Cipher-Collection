package com.mycode.main

import com.mycode.implementations._
import com.mycode.implementations.PlayfairCipher.createKey

object Main extends App:

  // Do whatever you what here!
  val message: String = "Hello Word! I'm using Scala 3 because Scala 2 sucks."

  val encryptedWithCaesar: String = CaesarCipher.encrypt(4)(message)
  val encryptedWithPlayfair: String = PlayfairCipher.encrypt(createKey("Martin Odersky"))(message)

  val decryptedWithCaesar: String = CaesarCipher.decrypt(4)(encryptedWithCaesar)
  val decryptedWithPlayfair: String = PlayfairCipher.decrypt(createKey("Martin Odersky"))(encryptedWithCaesar)

  println(/* Put here whatever you want from the values above */)

end Main

