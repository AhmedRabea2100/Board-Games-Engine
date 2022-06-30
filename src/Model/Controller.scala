package Model

trait Controller {
  def apply(state:State,input:String): State
}
