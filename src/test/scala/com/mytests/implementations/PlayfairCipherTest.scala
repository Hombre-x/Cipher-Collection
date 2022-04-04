package com.mytests.implementations

import com.mycode.implementations.PlayfairCipher._

object PlayfairCipherTest extends App:

  def testMessage(word: String, message: String): Unit = // WARNING: Impure method

    // Creating the key given a word:
    val key = createKey(s"$word")

    println {
      s"""---------------------------------------
         |
         |The key using the word \"$word\" is:
         |${key.getString}
         |
         |""".stripMargin
    }


    val encryptedMessage = encrypt(key)(message)

    println {
      s"""The encrypted message: \"$message\" is:
         |
         |$encryptedMessage
         |
         |""".stripMargin
    }

    val decryptedMessage = decrypt(key)(encryptedMessage)

    println {
      s"""The decrypted message becomes:
         |
         |$decryptedMessage
         |
         |---------------------------------------
         |""".stripMargin
    }

  end testMessage

  // Testing:

  val test1: Unit = testMessage("Yoan Pinzon", "This secret message is encrypted")
  val test2: Unit = testMessage("Satoshi Nakamoto", "I'm not Linus Torvalds ")

end PlayfairCipherTest

