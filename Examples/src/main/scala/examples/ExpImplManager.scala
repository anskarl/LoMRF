package examples

import lomrf.logic.dynamic.DynamicAtomBuilder
import lomrf.util.ImplFinder

object ExpImplManager {
  def main(args: Array[String]){
    val finder = new ImplFinder(Set(classOf[DynamicAtomBuilder]))
    finder.searchPath("./LoMRF/target/classes")
    //finder.loadDir("./LoMRF/target/dist")
    val result = finder.result
    result.foreach(println(_))

  }
}
