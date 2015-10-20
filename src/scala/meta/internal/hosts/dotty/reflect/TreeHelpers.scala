package scala.meta.internal.hosts.dotty
package reflect

import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

trait TreeHelpers[A >: dtr.Untyped <: dty.Type] {
  self: ReflectToolkit[A] =>

  implicit class RichFoundationSymbol(sym: dsy.Symbol) {
    def displayName: String = sym.name.displayName
  }

  implicit class RichFoundationNameTree(tree: g.Tree) {
    // NOTE: scala.reflect's tree don't have parent links, so we have to approximate if we encounter an unattributed package object
    def displayName: String = {
      // def packageName(tree: g.NameTree): dco.Name = {
      //   if (tree.symbol != dsy.NoSymbol) tree.symbol.owner.name
      //   else tree.parent match {
      //     case _: dtr.PackageDef => dco.TermName(tree.parent.displayName)
      //     case _ => l.TermName("package")
      //   }
      // }
      tree match {
        //case tree: g.NameTree if tree.name == nme.PACKAGE => packageName(tree).displayName
        case tree: g.NameTree => tree.name.displayName
        case dtr.This(name) => name.displayName
        case dtr.Super(_, name) => name.displayName
        case _ => unreachable(debug(tree, tree.toString))
      }
    }
  }

  implicit class RichFoundationHelperName(name: dco.Names.Name) {
    def isAnonymous = {
      val isTermPlaceholder = name.isTermName && name.startsWith("$")
      val isTypePlaceholder = name.isTypeName && name.startsWith("$")
      val isAnonymous = name == nme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymous
    }
    def looksLikeInfix = {
      val hasSymbolicName = !name.decode.toString.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_')
      val idiomaticallyUsedAsInfix = name == nme.eq || name == nme.ne
      hasSymbolicName || idiomaticallyUsedAsInfix
    }
    def displayName = {
      // NOTE: "<empty>", the internal name for empty package, isn't a valid Scala identifier, so we hack around
      // NOTE(@smarter): Same for "<root>", I don't know why this isn't needed in scalahost
      if (name == nme.EMPTY_PACKAGE) "_empty_"
      else if (name == nme.ROOT) "_root_"
      else if (name.isAnonymous) "_"
      else name.decode.toString
    }
  }

  class Memento extends dut.Attachment.Container

  val OriginalAttachment = new dut.Attachment.Key[g.Tree]

  implicit class RichFoundationOriginalTree(tree: g.Tree) {
    def original: g.Tree = {
      def desugaringOriginal: Option[g.Tree] = tree.getAttachment(OriginalAttachment)
      def macroExpandee: Option[g.Tree] = {
        None
      }
      desugaringOriginal.orElse(macroExpandee).getOrElse(tree)
    }

    def forgetOriginal: (g.Tree, Memento) = {
      val memento = new Memento
      tree.allAttachments.foreach { case (k, v) =>
        memento.pushAttachment(k, v)
      }
      val tree1 = { tree.removeAttachment(OriginalAttachment); tree }
      //val tree2 = { tree1.attachments.remove[MacroExpansionAttachment]; tree1 }
      (tree1, memento)
    }

    def rememberOriginal(memento: Memento): g.Tree = {
      val tree05 = memento.getAttachment(OriginalAttachment).map(original => { tree.pushAttachment(OriginalAttachment, original); tree })
      val tree1 = tree05.getOrElse(tree)
      //val tree15 = memento.attachments.get[MacroExpansionAttachment].map(tree1.updateAttachment)
      //val tree2 = tree15.getOrElse(tree1)
      tree1
    }
  }
}
