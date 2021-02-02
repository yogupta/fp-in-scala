package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(el) => el
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  // 3.27
  def depth(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => depth(left).max(depth(right)) + 1
    }
  }

  // 3.28
  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
    }
  }

  // 3.29
  def foldTree[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(foldTree(l)(f)(g), foldTree(r)(f)(g))
  }

}