package lomrf.util

package object collection {

  type PartitionFetcher[Key,Collection,Value] = (Key, Collection) => Value

}
