package dotty.tools
package dotc
package core

import Types._
import Contexts._
import util.{SimpleIdentityMap, SimpleIdentitySet}
import reporting._
import config.Config
import collection.mutable
import java.lang.ref.WeakReference

import scala.annotation.internal.sharable
import scala.util.control.NonFatal

object TyperState {
  @sharable private var nextId: Int = 0

  // TODO: Replace this by an enum after the full bootstrap
  sealed trait Mode
  object Mode {
    case object Committable extends Mode
    case object Committed extends Mode
    case object Explore extends Mode
    case object Dirty extends Mode
    case object Test extends Mode
  }
}

class TyperState(previous: TyperState /* | Null */, private[this] var myMode: TyperState.Mode) {
  import TyperState._
  import TyperState.Mode._

  val id: Int = nextId
  nextId += 1

  def mode: Mode = myMode

  private def transitionModeTo(newMode: Mode)(implicit ctx: Context): Unit = {
    if (this.id == 29) {
      System.err.println(s"#T: $mode --> $newMode")
      Thread.dumpStack
    }
    (mode, newMode) match {
      case (Committable, n) if n != Dirty =>
      case (Explore, Committable) =>
      case (Explore, Dirty) =>
      case (Explore, Test) =>
      case (Dirty, Committable) =>
        // gc() // done upstream
      case (Dirty, Explore) =>
      case (Test, Committable) =>
      case (Test, Explore) =>
      case _ =>
        assert(mode == newMode && mode != Committed, s"Invalid transition from $mode to $newMode")
    }
    myMode = newMode
  }

  private[this] var myReporter =
    if (previous == null) new ConsoleReporter() else previous.reporter

  def reporter: Reporter = myReporter
  def setReporter(reporter: Reporter): this.type = { myReporter = reporter; this }

  private[this] var myConstraint: Constraint =
    if (previous == null) new OrderingConstraint(SimpleIdentityMap.Empty, SimpleIdentityMap.Empty, SimpleIdentityMap.Empty)
    else previous.constraint

  def constraint: Constraint = myConstraint
  protected def constraint_=(c: Constraint)(implicit ctx: Context): Unit = {
    if (Config.debugCheckConstraintsClosed && isGlobalRetainable) c.checkClosed()
    myConstraint = c
  }

  def unsafeSetConstraintTo(c: Constraint)(implicit ctx: Context): Unit = {
    constraint = c
  }

  /** Reset constraint to `c` and mark current constraint as retracted if it differs from `c` */
  def resetConstraintTo(c: Constraint): Unit = {
    assert(mode == Explore)
    if (c `ne` myConstraint) myConstraint.markRetracted()
    myConstraint = c
  }

  private val previousConstraint =
    if (previous == null) constraint else previous.constraint

  /** Record that `tvar` was instantiated to `tp`.
   *
   *  @pre `tvar` has not been instantiated yet.
   */
  private[core] def recordInstantiation(tvar: TypeVar, tp: Type)(implicit ctx: Context): Unit = {
    assert(!tvar.isInstantiated)
    if (ownedVarsContains(tvar))
      mode match {
        case Committable =>
          tvar.inst = tp
        case Explore =>
          transitionModeTo(Dirty)
        case _ =>
      }
  }

  def isRetainable: Boolean = mode != Test

  def isGlobalRetainable: Boolean =
    isRetainable && (previous == null || previous.isGlobalRetainable)

  private[this] var isShared = false

  /** Mark typer state as shared (typically because it is the typer state of
   *  the creation context of a source definition that potentially still needs
   *  to be completed). Members of shared typer states are never overwritten in `test`.
   */
  def markShared(): Unit = isShared = true

  /** A fresh typer state with the same constraint as this one. */
  def fresh(mode: Mode): TyperState = {
    val ts = new TyperState(this, mode).setReporter(new StoreReporter(reporter))
    ts
  }

  /** The set of uninstantiated type variables which have this state as their owning state */
  private[this] var myOwnedVars: TypeVars = SimpleIdentitySet.empty
  def ownedVars: TypeVars = myOwnedVars
  def ownedVars_=(vs: TypeVars): Unit = myOwnedVars = vs

  /** Equivalent to `ownedVars.contains(tvar)` but faster. */
  def ownedVarsContains(tvar: TypeVar)(implicit ctx: Context): Boolean =
    tvar.owningState != null && tvar.owningState.get == ctx.typerState

  /** The closest ancestor of this typer state (including possibly this typer state itself)
   *  which is not yet committed, or which does not have a parent.
   */
  def uncommittedAncestor: TyperState =
    if (mode == Committed) previous.uncommittedAncestor else this

  private[this] var testReporter: TestReporter = null

  def explore[T](op: (() => Unit) => T)(implicit ctx: Context): T = {
    val savedMode = mode
    val savedConstraint = constraint

    def rollbackConstraint() = resetConstraintTo(savedConstraint)

    if (mode != Dirty)
      transitionModeTo(Explore)

    try op(() => rollbackConstraint())
    catch {
      case NonFatal(ex) =>
        rollbackConstraint()
        myMode = savedMode
        throw ex
    }
    finally {
      assert(mode == Explore || mode == Dirty, s"$this: $mode")
      val needsGc = mode == Dirty && (savedConstraint ne constraint)
      savedMode match {
        case Committable =>
          if (needsGc)
            gc()
          transitionModeTo(savedMode)
        case Explore =>
          if (!needsGc)
            transitionModeTo(savedMode)
        case Dirty =>
          assert(mode == Dirty)
        case Test =>
          transitionModeTo(savedMode)
        case Committed =>
          assert(false, "unreachable")
      }
    }
  }

  /** Test using `op`. If current typerstate is shared, run `op` in a fresh disposable
   *  typerstate. If it is unshared, run `op` in current typerState, restoring typerState
   *  to previous state afterwards.
   */
  def test[T](op: Context => T)(implicit ctx: Context): T =
    if (isShared)
      op(ctx.fresh.setDisposableTyperState())
    else {
      val savedConstraint = constraint
      val savedReporter = myReporter
      val savedMode = myMode
      transitionModeTo(Test)
      myReporter = {
        if (testReporter == null || testReporter.inUse) {
          testReporter = new TestReporter(reporter)
        } else {
          testReporter.reset()
        }
        testReporter.inUse = true
        testReporter
      }
      try op(ctx)
      finally {
        testReporter.inUse = false
        resetConstraintTo(savedConstraint)
        myReporter = savedReporter
        transitionModeTo(savedMode)
      }
    }

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   *
   *  A note on merging: An interesting test case is isApplicableSafe.scala. It turns out that this
   *  requires a context merge using the new `&' operator. Sequence of actions:
   *  1) Typecheck argument in typerstate 1.
   *  2) Cache argument.
   *  3) Evolve same typer state (to typecheck other arguments, say)
   *     leading to a different constraint.
   *  4) Take typechecked argument in same state.
   *
   * It turns out that the merge is needed not just for
   * isApplicableSafe but also for (e.g. erased-lubs.scala) as well as
   * many parts of dotty itself.
   */
  def commit()(implicit ctx: Context): Unit = {
    val targetState = ctx.typerState
    if (this ne targetState) {
      transitionModeTo(Committed)
      targetState.constraint =
        if (targetState.constraint eq previousConstraint) constraint
        else targetState.constraint & (constraint, otherHasErrors = reporter.errorsReported)

      targetState.ownedVars ++= ownedVars

      constraint foreachTypeVar { tvar =>
        if (tvar.owningState.get eq this)
          tvar.owningState = new WeakReference(targetState)

        if ((tvar.owningState.get eq targetState)) {
          val inst = ctx.typeComparer.instType(tvar)
          if (inst.exists)
            targetState.recordInstantiation(tvar, inst)
        }
      }
      reporter.flush()
    }
  }

  /** Make type variable instances permanent by assigning to `inst` field if
    *  type variable instantiation cannot be retracted anymore. Then, remove
    *  no-longer needed constraint entries.
    */
  private def gc()(implicit ctx: Context): Unit = {
    val toCollect = new mutable.ListBuffer[TypeLambda]
    constraint foreachTypeVar { tvar =>
      if (!tvar.inst.exists) {
        val inst = ctx.typeComparer.instType(tvar)
        if (inst.exists && (tvar.owningState.get eq this)) {
          tvar.inst = inst
          val lam = tvar.origin.binder
          if (constraint.isRemovable(lam)) toCollect += lam
        }
      }
    }
    for (poly <- toCollect)
      constraint = constraint.remove(poly)
  }

  override def toString: String = s"TS[$id]"

  def stateChainStr: String = s"$this${if (previous == null) "" else previous.stateChainStr}"
}

/** Temporary, reusable reporter used in TyperState#test */
private class TestReporter(outer: Reporter) extends StoreReporter(outer) {
  /** Is this reporter currently used in a test? */
  var inUse: Boolean = false

  def reset(): Unit = {
    assert(!inUse, s"Cannot reset reporter currently in use: $this")
    infos = null
  }
}
