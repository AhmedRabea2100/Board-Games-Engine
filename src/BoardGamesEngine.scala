import Model._
case class BoardGamesEngine(draw:Drawer,control:Controller){
  var state: State = null;
  state = control(state,"");
  draw(state,control)


}
