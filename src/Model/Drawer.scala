package Model

trait Drawer {
  def apply(state:State,controller: Controller): Unit
}
