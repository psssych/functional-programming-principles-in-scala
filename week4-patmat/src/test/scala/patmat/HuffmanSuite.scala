package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times counter") {
    assert(times(List('a', 'b', 'a', 'c', 'b', 'a')) === List(('a', 3), ('b', 2), ('c', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode test text") {
    val testCode: CodeTree = Fork(Leaf('a', 4), Fork(Leaf('b', 1), Leaf('c', 1), List('b', 'c'), 2), List('a', 'b', 'c'), 6)
    val secret: List[Bit] = List(0, 0, 1, 1)
    assert(decode(testCode, secret) === List('a', 'a', 'c'))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert CodeTree to CodeTable") {
    val testCode: CodeTree = Fork(Leaf('a', 4), Fork(Leaf('b', 1), Leaf('c', 1), List('b', 'c'), 2), List('a', 'b', 'c'), 6)
    assert(convert(testCode) === List(('a', List(0)), ('b', List(1, 0)), ('c', List(1, 1))))
  }

}
