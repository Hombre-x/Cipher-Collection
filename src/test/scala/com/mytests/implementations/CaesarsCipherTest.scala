package com.mytests.implementations

import com.mycode.implementations.CaesarCipher._

object CaesarsCipherTest extends App:

  def testMessage(k: Int, message: String): Unit =

    val encryptedMessage = encrypt(k)(message)

    println {
      s"""---------------------------------------
         |
         |The encrypted message: \"$message\" with $k permutation(s) is:
         |
         |$encryptedMessage
         |
         |""".stripMargin
    }

    val decryptedMessage = decrypt(k)(encryptedMessage)

    println {
      s"""The decrypted message becomes:
         |
         |$decryptedMessage
         |
         |---------------------------------------
         |""".stripMargin
    }

  end testMessage

  val test1: Unit = testMessage(1, "Hi there! This message ain't encrypted")
  val test2: Unit = testMessage(3, "Return to Rome")

end CaesarsCipherTest

