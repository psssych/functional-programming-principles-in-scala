import forcomp.Anagrams._

val s = List("Ala", "ma", "kota")
val a = List("Linux", "kurwa", "rulez")

//sentenceOccurrences(s)

def f(sentence: Sentence): List[Sentence] = {
  def iter(occ: Occurrences): List[Sentence] = {
    if (occ.isEmpty) List(Nil)
    else
      for {
        comb <- combinations(occ)
        word <- dictionaryByOccurrences.getOrElse(comb, Nil)
        sent <- iter(subtract(occ, wordOccurrences(word)))
      } yield word :: sent
  }
  iter(sentenceOccurrences(sentence))
}

f(a)

val occ = sentenceOccurrences(a)
val combs = combinations(occ)

val words = combs.map(comb => dictionaryByOccurrences.getOrElse(comb, Nil)).filter(x => x != List())

dictionaryByOccurrences