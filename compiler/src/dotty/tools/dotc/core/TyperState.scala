package dotty.tools
package dotc
package core

import Types._
import Contexts._
import util.{SimpleIdentityMap, SimpleIdentitySet}
import reporting._
import config.Config
import config.Printers.constr
import collection.mutable
import java.lang.ref.WeakReference
import util.Stats
import Decorators._

import scala.annotation.internal.sharable
import scala.util.control.NonFatal

object TyperState {
  @sharable private var nextId: Int = 0

  enum Mode {
    case Committable, Committed, Explore, Dirty, Test
  }
}

class TyperState(private val previous: TyperState /* | Null */, private[this] var myMode: TyperState.Mode) {
  import TyperState._
  import TyperState.Mode._

  Stats.record("typerState")

  val id: Int = TyperState.nextId
  TyperState.nextId += 1

  def mode: Mode = myMode

  private def transitionModeTo(newMode: Mode)(implicit ctx: Context): Unit = {
    // if (id == 149) {
    //   if (newMode == Committed)
    //     Thread.dumpStack
    // }
    // if (id == 2 || id == 1) {
    //   if (mode == Dirty) {
    //     println(s"$this: $mode -> $newMode")
    //     if (!constraint.isEmpty) println(constraint.show)
    //   }
    //   if (newMode != Dirty) {
    //     checkNoGc()
    //   }
    // }
    (mode, newMode) match {
      case (Committable, n) if n != Dirty =>
      case (Explore, Committable) =>
      case (Explore, Dirty) =>
      case (Explore, Test) =>
      case (Dirty, Committable) =>
        // gc() // done upstream
      case (Dirty, Explore) =>
      case (Dirty, Test) =>
      case (Test, Committable) =>
      case (Test, Explore) =>
      case (Test, Dirty) =>
      case (Committed, Explore) => // for error messages
      case (Explore, Committed) => // for error messages
      case _ =>
        assert(mode == newMode && mode != Committed, s"$this: Invalid transition from $mode to $newMode")
    }
    myMode = newMode
  }
    

  private var myReporter =
    if (previous == null) new ConsoleReporter() else previous.reporter

  def reporter: Reporter = myReporter

  def setReporter(reporter: Reporter): this.type = { myReporter = reporter; this }

  private var myConstraint: Constraint =
    if (previous == null) OrderingConstraint.empty
    else previous.constraint

  def constraint: Constraint = myConstraint
  protected def constraint_=(c: Constraint)(implicit ctx: Context): Unit = {
    // if (id == 615) {
    //   if (myConstraint ne c)
    //     println(s"$this ($mode) prev: ${myConstraint.show}\nnew: ${c.show}")
    // }
    // TODO: make this pass
    if (mode == Committed)
      assert(c == myConstraint, i"$this: before: $constraint\nafter:$c")
    if (Config.debugCheckConstraintsClosed && isGlobalCommittable) c.checkClosed()
    myConstraint = c
  }

  private[this] var inconsistent = false
  def unsafeSetConstraintTo(c: => Constraint)(implicit ctx: Context): Unit = {
    val savedInconsistent = inconsistent
    inconsistent = true
    try constraint = c
    finally {
      inconsistent = savedInconsistent
    }
  }

  /** Reset constraint to `c` and mark current constraint as retracted if it differs from `c` */
  def resetConstraintTo(c: Constraint): Unit = {
    assert(mode == Explore || mode == Dirty || mode == Test || (mode == Committed && c == myConstraint),
      s"Invalid mode: $this $mode")
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
    assert(!tvar.inst.exists, s"$this: tryInstantiate($tvar, $tp) but tvar.inst = ${tvar.inst}")
    if (ownedVarsContains(tvar))
      mode match {
        case Committable =>
          tvar.inst = tp
          // if (id == 615) {
          //   println(s"$this: Instantiating $tvar to $tp, c = ${constraint.show}")
          // }
        case Explore =>
          transitionModeTo(Dirty)
        case _ =>
      }
  }

  def isRetainable: Boolean = mode != Test

  // is isGlobalRetainable a better name ?
  def isGlobalCommittable: Boolean =
    isRetainable && (previous == null || previous.isGlobalCommittable)

  private var isShared = false

  /** Mark typer state as shared (typically because it is the typer state of
   *  the creation context of a source definition that potentially still needs
   *  to be completed). Members of shared typer states are never overwritten in `test`.
   */
  def markShared(): Unit = isShared = true

  def fresh(mode: Mode): TyperState = {
    assert(this.mode != Committed, s"$this: calling fresh on already committed constraint $constraint")
    val ts = new TyperState(this, mode).setReporter(new StoreReporter(reporter))
    ts
  }

  /** The uninstantiated variables */
  def uninstVars: collection.Seq[TypeVar] = constraint.uninstVars

  /** The set of uninstantiated type variables which have this state as their owning state */
  private var myOwnedVars: TypeVars = SimpleIdentitySet.empty
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

  private var testReporter: TestReporter = null

  def explore[T](op: (() => Unit) => T)(implicit ctx: Context): T = {
    val savedMode = mode
    val savedConstraint = constraint

    def rollbackConstraint() = resetConstraintTo(savedConstraint)

    // if (id == 149) {
    //   println(i"BEFORE $this ($mode): ${constraint}")
    // }

    // XX: In a Test, we should not transition, otherwise isRetainable changes value
    if (mode != Dirty && mode != Test && mode != Committed) {
      checkInvariants()
      transitionModeTo(Explore)
    }


    val ret =
      try op(() => rollbackConstraint())
      catch {
        case NonFatal(ex) =>
          rollbackConstraint()
          myMode = savedMode
          throw ex
      }

    // XX: add no need gc sanity check

    assert(mode == Explore || mode == Dirty || mode == Test || mode == Committed, s"$this: $mode")
    val needsGc = mode == Dirty && (savedConstraint ne constraint)
    // if (id == 149) {
    //   println(i"AFTER $this ($mode): ${constraint}")
    // }
    savedMode match {
      case Committable =>
        if (needsGc)
          gc()
        else
          checkNoGc()
        transitionModeTo(savedMode)
      case Explore =>
        if (!needsGc) {
          checkNoGc()
          transitionModeTo(savedMode)
        }
        // gc()
        // transitionModeTo(savedMode)
      case Dirty =>
        assert(mode == Dirty)
      case Test =>
        assert(mode == Test)
      case Committed =>
        assert(mode == Committed)
        checkNoGc()
    }
    checkInvariants()
    ret
  }  

  /** Test using `op`. If current typerstate is shared, run `op` in a fresh exploration
   *  typerstate. If it is unshared, run `op` in current typerState, restoring typerState
   *  to previous state afterwards.
   */
  def test[T](op: (given Context) => T)(implicit ctx: Context): T =
    if (isShared)
      op(given ctx.fresh.setDisposableTyperState())
    else {
      val savedConstraint = constraint
      val savedReporter = myReporter
      val savedMode = myMode
      transitionModeTo(Test)
      myReporter = {
        if (testReporter == null || testReporter.inUse)
          testReporter = new TestReporter(reporter)
        else
          testReporter.reset()
        testReporter.inUse = true
        testReporter
      }
      try op(given ctx)
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
    Stats.record("typerState.commit")
    val targetState = ctx.typerState
    if (constraint ne targetState.constraint)
      constr.println(i"committing $this to $targetState, fromConstr = $constraint, toConstr = ${targetState.constraint}")

    if (this ne targetState) {
      // println(i"$this COMMIT START: $constraint")
      transitionModeTo(Committed)
      if (targetState.constraint eq previousConstraint) targetState.constraint = constraint
      else targetState.mergeConstraintWith(this)

      targetState.ownedVars ++= ownedVars

      var needsGc = false
      constraint foreachTypeVar { tvar =>
        if (tvar.owningState.get eq this)
          tvar.owningState = new WeakReference(targetState)

        // XX: instType might use wrong constraint ?
        if ((tvar.owningState.get eq targetState) && ctx.typeComparer.instType(tvar).exists)
          needsGc = true
      }
      if (needsGc) {
        if (targetState.mode == Committable)
          targetState.gc()
        else if (targetState.mode == Explore) {
          targetState.transitionModeTo(Dirty)
        }
      } else if (targetState.mode == Committable)
        targetState.checkNoGc()

      // println(i"$this COMMIT PREFLUSH: $constraint")
      reporter.flush()      
    }
  }

  def mergeConstraintWith(that: TyperState)(implicit ctx: Context): Unit =
    constraint = constraint & (that.constraint, otherHasErrors = that.reporter.errorsReported)

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
    checkInvariants()
  }

  private def checkNoGc()(implicit ctx: Context): Unit = {
    constraint foreachTypeVar { tvar =>
      if (!tvar.inst.exists) {
        val inst = ctx.typeComparer.instType(tvar)
        assert(!(inst.exists && (tvar.owningState.get eq this)),
          i"$this ($mode): No gc set to be run in $constraint")
        // if (inst.exists) {
        //   println(s"$this: not instantiating $tvar owned by ${tvar.owningState.get}")
        // }
      }
    }
    checkInvariants()
  }

  private def checkInvariants()(implicit ctx: Context): Unit = {
    // if Committed, we might still end up doing explorations etc and have tvars which are instantiated
    // proper fix: make frozen really frozen, ensure Commmitted only happen when frozen.
    if (inconsistent || mode == Committed)
      return
    constraint foreachTypeVarComplete { tvar =>
      if (tvar.inst.exists) {
        val lam = tvar.origin.binder
        assert(!constraint.isRemovable(lam),
          s"$this ($mode): ${tvar}#${tvar.hashCode} removable but still in ${constraint.show}")
      }
    }
  }

  override def toString: String = {
    def ids(state: TyperState): List[String] =
      s"${state.id}${if (state.isRetainable) "" else "X"}" ::
        (if (state.previous == null) Nil else ids(state.previous))
    s"TS[${ids(this).mkString(", ")}]"
  }

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
