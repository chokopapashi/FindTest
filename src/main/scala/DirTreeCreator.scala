
import java.io.File
import java.io.FileFilter

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.language.postfixOps

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

    def fileType(file: File): String = {
        import scala.sys.process._
        (s"c:/cygwin/bin/file.exe -b ${file.getAbsolutePath}" !!).trim.split(",")(0)
    }

    val MAX_LINE_COUNT = 20

    def getCharBytesLength(c: Char) = c.toString.getBytes("Shift_JIS").length
    def getStringBytesLength(s: String) = s.getBytes("Shift_JIS").length

    def splitFileName(file: File, hierarchy: List[String]) = {
        @tailrec
        def split(sl: List[Char], zb: Int, zmax: Int, dl: List[StringBuilder]):List[StringBuilder] = sl match {
            case Nil => dl
            case clh :: clt =>
                if(zmax < (zb + getCharBytesLength(clh)))
                    split(clt, 0, zmax, StringBuilder.newBuilder.append(clh) :: dl)
                else {
                    dl.head += clh
                    split(clt, zb + getCharBytesLength(clh), zmax, dl)
                }
        }

        /* +2 = branch size(wide char size). */
        split(file.getName.toList, 0, MAX_LINE_COUNT-(getStringBytesLength(hierarchy.mkString)+ 2), List(StringBuilder.newBuilder)) reverse
    }

//    def fileInfo(file: File, hierarchy: String, branch: String) = hierarchy + branch + file.getName + " : (" + fileType(file) + ")"
    def putBuffer(outBuffer: ListBuffer[String], file: File, hierarchy: List[String], branch: String, nextBranch: String) {
        var first = true
        for(lsb <- splitFileName(file, hierarchy)) {
            val b = if(first) {
                        first = false
                        branch
                    } else
                        nextBranch

            outBuffer += hierarchy.mkString + b + lsb.mkString
        }
    }

    /* Use view bounds */
    def init[B <% Seq[_]](l: B): B = if(l.nonEmpty) l.init.asInstanceOf[B] else l

    @tailrec
    def downDirectoryTree(files: List[FileHolder], hierarchy: List[String], outBuffer: ListBuffer[String]) {
        files match {
            case Nil => /* Nothing to do */
            case FileWrap(f) :: (rofs @ List(Sentinel, _*)) => {
                putBuffer(outBuffer, f, hierarchy, "└", "　")
                f match {
                    case d if(d.isDirectory) => downDirectoryTree(listFiles(d) ::: rofs, hierarchy :+ "　", outBuffer)
                    case _ => downDirectoryTree(rofs, hierarchy, outBuffer)
                }
            }
            case FileWrap(f) :: rofs => {
                putBuffer(outBuffer, f, hierarchy, "├", "│")
                f match {
                    case d if(d.isDirectory) => downDirectoryTree(listFiles(d) ::: rofs, hierarchy :+ "│", outBuffer)
                    case _ => downDirectoryTree(rofs, hierarchy, outBuffer)
                }
            }
            case Sentinel :: rofs => downDirectoryTree(rofs, init(hierarchy), outBuffer)
            case fs => throw new IllegalStateException(fs.toString)
        }
    }

    def main(args: Array[String]) {

        if(args.length == 0)
            sys.exit(1)

        val root = new File(args(0))
        if(!root.exists)
            sys.exit(2)

        val treeBuffer = ListBuffer(f"${root.getName}")
        downDirectoryTree(listFiles(root), Nil, treeBuffer)
        println(treeBuffer.mkString(f"%n"))
    }
}

