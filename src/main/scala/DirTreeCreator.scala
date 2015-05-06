
import java.io.File
import java.io.FileFilter

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object DirTreeCreator {

    var hierarchyStack = Stack.empty[String]

    implicit def toFileFilter(func: (File) => Boolean) = new FileFilter {
        def accept(file: File): Boolean = func(file)
    }

    def dirTreeFileFilter(file: File): Boolean = file match {
        case f if(f.isDirectory) => f match {
            case d if(d.getName == ".svn")    => false
            case d if(d.getName == ".git")    => false
            case d if(d.getName == "target")  => false
            case d if(d.getName == "project") => false
            case d if(d.getName == "classes") => false
            case _ => true
        }
        case _ => true
    }

    trait FileHolder
    object Sentinel extends FileHolder
    case class FileWrap(file: File) extends FileHolder

    def listFiles(dir: File): List[FileHolder] =
        dir.listFiles(dirTreeFileFilter _).map(FileWrap(_)).toList :+ Sentinel

    def vertexName(file: File, hierarchy: String) = hierarchy+ file.getName

    /* Use view bounds */
    def init[B <% Seq[_]](l: B): B = if(l.nonEmpty) l.init.asInstanceOf[B] else l

    @tailrec
    def downDirectoryTree(files: List[FileHolder], hierarchy: List[String], outBuffer: ListBuffer[String]) {
        files match {
            case Nil => /* Nothing to do */
            case FileWrap(f) :: (rofs @ List(Sentinel, _*)) => {
                outBuffer += vertexName(f, hierarchy.mkString + "└")
                f match {
                    case d if(d.isDirectory) => downDirectoryTree(listFiles(d) ::: rofs, hierarchy :+ "　", outBuffer)
                    case _ => downDirectoryTree(rofs, hierarchy, outBuffer)
                }
            }
            case FileWrap(f) :: rofs => {
                outBuffer += vertexName(f, hierarchy.mkString + "├")
                f match {
                    case d if(d.isDirectory) => downDirectoryTree(listFiles(d) ::: rofs, hierarchy :+ "│", outBuffer)
                    case _ => downDirectoryTree(rofs, hierarchy, outBuffer)
                }
            }
            case Sentinel :: rofs => downDirectoryTree(rofs, init(hierarchy), outBuffer)
            case fs => throw new IllegalStateException(fs.toString)
        }
    }
//    def downDirectoryTree(dir: File, hierarchy: List[String], retBuilder: StringBuilder) {
//        dir.listFiles(dirTreeFileFilter _) match {
//            case Array() => /* Finish */
//            case files => files.foreach { f =>
//                retBuilder ++= (hierarchy.mkString + 
//                                "├" +
//                                f.getName +
//                                f"%n")
//                if(f.isDirectory) {
//                     downDirectoryTree(f, "│" :: hierarchy, retBuilder)
//                }
//            }
//        }
//    }

    def main(args: Array[String]) {

        val root = new File(args(0))

        val treeBuffer = ListBuffer(f"${root.getName}")
        downDirectoryTree(listFiles(root), Nil, treeBuffer)
        println(treeBuffer.mkString(f"%n"))
    }
}

