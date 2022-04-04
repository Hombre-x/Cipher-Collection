package com.mycode.implementations

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

object PlayfairCipher:

  type Matrix[T] = Vector[Vector[T]]

  val alphabet: TreeSet[Char] =
    TreeSet(
      'A', 'B', 'C', 'D', 'E',
      'F', 'G', 'H', 'I', // 'J' -> Playfair combines the "I"s with the "J"s
      'K', 'L', 'M', 'N', 'O',
      'P', 'Q', 'R', 'S', 'T',
      'U', 'V', 'W', 'X', 'Y',
      'Z'
    )

  // Extension methods for the matrix
  extension [A](matrix: Matrix[A])

    def getRow(index: Int): Vector[A] =
      matrix(index)

    def getColumn(index: Int): Vector[A] =
      for
        vec <- matrix
      yield vec(index)

    def indexAt(element: A): (Int, Int) =

      @tailrec
      def searchVec(el: A, mtx: Matrix[A]): Vector[A] =

        if mtx.isEmpty then Vector.empty[A]
        else if mtx.head.indexOf(el) != -1 then mtx.head
        else searchVec(el, mtx.tail)

      end searchVec


      val vecInIndex = searchVec(element, matrix)

      (matrix.indexOf(vecInIndex), vecInIndex.indexOf(element))

    end indexAt

    def getString: String =

      @tailrec
      def vecToStr(vec: Vector[A], accum: String = ""): String =
        if vec.length == 1 then s"$accum, ${vec.head})"
        else vecToStr(vec.tail, s"$accum, ${vec.head}")

      @tailrec
      def mtxToStr(mtx: Vector[String], accum: String = ""): String =
        if mtx.length == 1 then s"$accum\n${mtx.head}"
        else mtxToStr(mtx.tail, s"$accum\n${mtx.head}")

      val matrixOfStrings: Vector[String] =
        for
          vec <- matrix
        yield vecToStr(vec.tail, s"(${vec.head}")

      mtxToStr(matrixOfStrings)

    end getString

  end extension

  // Extension methods for Integers
  extension (n: Int)

    infix def mod(m: Int): Int =

      @tailrec
      def tailMod(num: Int): Int =

        if num < 0 then tailMod(num + m)
        else num % m

      tailMod(n)

    end mod

  end extension

  // Function to create the Key
  def createKey(word: String): Matrix[Char] =

    val separatedWord = word.toUpperCase.toVector.filter(c => alphabet.contains(c))

    // This functions adds the padding to the original word
    def addPadding(letters: Vector[Char]): Vector[Char] =

      @tailrec
      def tailPadding(leftLetters: Vector[Char],
                      alphabet: TreeSet[Char],
                      usedLetters: Set[Char],
                      accum: Vector[Char]): Vector[Char] =

        if accum.length == 25 then
          accum

        else if leftLetters.isEmpty then
          tailPadding(leftLetters, alphabet.tail, usedLetters + alphabet.head, accum :+ alphabet.head)

        else if usedLetters(leftLetters.head) then
          tailPadding(leftLetters.tail, alphabet.tail, usedLetters + alphabet.head, accum :+ alphabet.head)

        else
          tailPadding(leftLetters.tail.filter(l => l != leftLetters.head),
                      alphabet - leftLetters.head,
                      usedLetters + leftLetters.head,
                      accum :+ leftLetters.head)

      end tailPadding

      tailPadding(separatedWord, alphabet, Set.empty[Char], Vector.empty[Char])

    end addPadding

    // Creates a 5 x 5 Matrix
    def createMatrix(paddedWord: Vector[Char]): Matrix[Char] =

      @tailrec
      def tailCreate(leftWord: Vector[Char], currentRow: Vector[Char], accum: Matrix[Char]): Matrix[Char] =

        if accum.length == 5 then accum
        else if currentRow.length == 5 then tailCreate(leftWord, Vector.empty[Char], accum :+ currentRow)
        else tailCreate(leftWord.tail, currentRow :+ leftWord.head, accum)

      tailCreate(paddedWord, Vector.empty[Char], Vector.empty)

    end createMatrix

    // Creating the key:
    val paddedWord = addPadding(separatedWord)
    createMatrix(paddedWord)

  end createKey

  // Function to group the message in pairs
  def groupMessage(message: String): Vector[(Char, Char)] =

    // Checks if two consecutive letters are the same, adds X between them and changes the "J"s with "I"s
    def addXtoMessage(message: Vector[Char]): Vector[Char] =

      @tailrec
      def tailAdd(leftLetters: Vector[Char], accum: Vector[Char]): Vector[Char] =

        leftLetters match
          case Vector() if accum.length % 2 == 0 =>
            accum

          case Vector() if accum.length % 2 != 0 =>
            accum :+ 'X'

          case char1 +: char2 +: tail if char1 == char2 =>
            tailAdd(leftLetters drop 1, accum :+ char1 :+ 'X')

          case Vector(c) =>
            tailAdd(leftLetters drop 2, accum :+ c)

          case _ =>
            tailAdd(leftLetters drop 2, accum :+ leftLetters.head :+ leftLetters.tail.head)

      tailAdd(message, Vector.empty[Char])

    end addXtoMessage

    // Creating the grouped messages
    for
      vec <- addXtoMessage(message.toUpperCase.replace('J', 'I').toVector.filter(c => alphabet.contains(c))).grouped(2).toVector
    yield (vec(0), vec(1))

  end groupMessage

  // Function to un-group the pairs in encrypted text
  def unGroupMessage(grouped: Vector[(Char, Char)]): String =

    @tailrec
    def tailUnGroup(leftPairs: Vector[(Char, Char)], accum: String): String =

      leftPairs match
        case Vector() => accum
        case (c1, c2) +: tail => tailUnGroup(tail, s"$accum $c1$c2")
        case _ => "Unmatched Pattern"

    tailUnGroup(grouped, "")

  end unGroupMessage

  // Encrypting function
  def encrypt(key: Matrix[Char])(message: String): String =

    // Grouping the message in pairs
    val groupedMsg: Vector[(Char, Char)] = groupMessage(message)

    // This set of functions takes a coordinate of a letter and returns the corresponding
    // character in the immediate desired direction.
    def getRight(coords: (Int, Int)): Char =
      key.getRow(coords._1)((coords._2 + 1) % 5)

    def getDown(coords: (Int, Int)): Char =
      key.getColumn(coords._2)((coords._1 + 1) % 5)

    def getIntersect(coords1: (Int, Int), coords2: (Int, Int)): (Char, Char) =
      (key(coords1._1)(coords2._2), key(coords2._1)(coords1._2))

    // This functions checks if the two chars are in the same row, column, or none of them and then
    // swaps the characters according to the rules of Playfair.
    def swapLetters(letterPair: (Char, Char)): (Char, Char) =

      val c1Index = key.indexAt(letterPair._1)
      val c2Index = key.indexAt(letterPair._2)

      // Same Row
      if c1Index._1 == c2Index._1 then
        (getRight(c1Index), getRight(c2Index))

      // Same Column
      else if c1Index._2 == c2Index._2 then
        (getDown(c1Index), getDown(c2Index))

      // Neither
      else
        getIntersect(c1Index, c2Index)

    end swapLetters

    // Return the encrypted message
    unGroupMessage(groupedMsg.map((tup: (Char, Char)) => swapLetters(tup)))

  end encrypt

  // Decrypting functions
  def decrypt(key: Matrix[Char])(secretMessage: String): String =

    // Grouping the secret message in pairs
    val groupedMsg = groupMessage(secretMessage)

    def getLeft(coords: (Int, Int)): Char =
      key.getRow(coords._1)((coords._2 - 1) mod 5)

    def getUp(coords: (Int, Int)): Char =
      key.getColumn(coords._2)((coords._1 - 1) mod 5)

    def getIntersect(coords1: (Int, Int), coords2: (Int, Int)): (Char, Char) =
      (key(coords1._1)(coords2._2), key(coords2._1)(coords1._2))


    def swapLetters(letterPair: (Char, Char)): (Char, Char) =

      val c1Index = key.indexAt(letterPair._1)
      val c2Index = key.indexAt(letterPair._2)

      // Same Row
      if c1Index._1 == c2Index._1 then
        (getLeft(c1Index), getLeft(c2Index))

      // Same Column
      else if c1Index._2 == c2Index._2 then
        (getUp(c1Index), getUp(c2Index))

      // Neither
      else
        getIntersect(c1Index, c2Index)

    end swapLetters

    // Return the decrypted message
    unGroupMessage(groupedMsg.map((tup: (Char, Char)) => swapLetters(tup)))

  end decrypt

end PlayfairCipher
