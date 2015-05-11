
import java.io.File
import java.io.FileFilter
import java.nio.file.Files
import java.nio.file.FileSystems
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.attribute.DosFileAttributes
import java.nio.file.attribute.FileTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.math

object DirTreeCreator {

    implicit def toFileFilter(func: (File) => Boolean) = new FileFilter {
        def accept(file: File): Boolean = func(file)
    }

    val MAX_LINE_BYTE_WIDTH = 70

    def fileAttributes_fileType(file: File): String = {
        import scala.sys.process._
        (s"c:/cygwin/bin/file.exe -b ${file.getAbsolutePath}" !!).trim.split(",")(0)
    }

    def fileAttributes_dateTime(file: File, supportedAttributes: mutable.Set[String]): String = {
        if(supportedAttributes.contains("basic")) {
            val sb = StringBuilder.newBuilder

            val attrb = Files.readAttributes(file.toPath, classOf[BasicFileAttributes]) 
            def fileTime2ZoneDateTime(fileTime: FileTime) = fileTime.toInstant.atZone(ZoneId.systemDefault)
            val dtf = DateTimeFormatter.ofPattern("yy/MM/dd-HH:mm:ss")

            sb ++= fileTime2ZoneDateTime(attrb.creationTime).format(dtf) + " "
            sb ++= fileTime2ZoneDateTime(attrb.lastModifiedTime).format(dtf) + " "
            sb ++= fileTime2ZoneDateTime(attrb.lastAccessTime).format(dtf) + " "

            sb.mkString
        } else
            "--/--/-----:--:-- --/--/-----:--:-- --/--/-----:--:--"
    }

    def fileAttributes_dos(file: File, supportedAttributes: mutable.Set[String]): String = {
        if(supportedAttributes.contains("dos")) {
            val sb = StringBuilder.newBuilder

            val attrd = Files.readAttributes(file.toPath, classOf[DosFileAttributes]) 
            if(attrd.isDirectory) {
                sb ++= "----"
            } else {
                sb += (if(attrd.isArchive())  'A' else '-')
                sb += (if(attrd.isHidden())   'H' else '-')
                sb += (if(attrd.isReadOnly()) 'R' else '-')
                sb += (if(attrd.isSystem())   'S' else '-')
            }

            sb.mkString
        } else
            "----"
    }

    def fileAttributes_size(file: File, supportedAttributes: mutable.Set[String]): String = {
        if(supportedAttributes.contains("basic")) {
            val sb = StringBuilder.newBuilder

            val GIGA = 1073741824L
            val MEGA = 1048576L
            val KIRO = 1024L
            val attrb = Files.readAttributes(file.toPath, classOf[BasicFileAttributes]) 
            if(attrb.isDirectory)
                sb ++= "   <DIR>"
            else {
                def floatPart(size: Long, base: Long) = math.rint(((size%base).toDouble/base.toDouble)*1000) match {
                    case f if(1000 <= f) => 999
                    case f => f.toInt
                }

                if(GIGA < attrb.size)
                    sb ++= f"${attrb.size/GIGA}%3d.${floatPart(attrb.size,GIGA)}%03dG"
                else if(MEGA < attrb.size)
                    sb ++= f"${attrb.size/MEGA}%3d.${floatPart(attrb.size,MEGA)}%03dM"
                else if(KIRO < attrb.size)
                    sb ++= f"${attrb.size/KIRO}%3d.${floatPart(attrb.size,KIRO)}%03dK"
                else
                    sb ++= f"${attrb.size}%8d"
            }

            sb.mkString
        } else
            "       -"
    }

    def fileAttributes(file: File): String = {
        val sb = StringBuilder.newBuilder
        val supportedAttributes = FileSystems.getDefault.supportedFileAttributeViews

        sb ++= fileAttributes_size(file, supportedAttributes)
        sb ++= " "
        sb ++= fileAttributes_dos(file, supportedAttributes)
        sb ++= " "
        sb ++= fileAttributes_dateTime(file, supportedAttributes)
        sb ++= " "
//        sb ++= fileAttributes_fileType(file)

        sb.mkString
    }

    def getCharBytes(c: Char) = c.toString.getBytes("Shift_JIS")

//    def getCharBytesLength(c: Char) = c.toString.getBytes("Shift_JIS").length
//    def getCharsBytesLength(ca: Array[Char]) = ca.foldLeft(0)((z,c) => z + c.toString.getBytes("Shift_JIS").length)
//    def getStringBytesLength(s: String) = s.getBytes("Shift_JIS").length
//    def newBytes() = Array.fill(MAX_LINE_BYTE_WIDTH)(0x20.toByte)

    def alignWidth(src: mutable.ListBuffer[Byte], pw: Int): String = {
        val cw = pw + src.size
        if(cw < MAX_LINE_BYTE_WIDTH)
            src ++= List.fill(MAX_LINE_BYTE_WIDTH - cw)(0x20.toByte)
        new String(src.toArray, "Shift_JIS")
    }

    def splitFileName(file: File, hierarchy: List[String]) = {
        val pw = hierarchy.mkString.getBytes.length + 2    /* +2 = branch size(wide char size). */

        @tailrec
        def split(src: List[Char], tmp: mutable.ListBuffer[Byte], dst: List[String]):List[String] = src match {
            case Nil => if(tmp.isEmpty) dst
                        else alignWidth(tmp, pw) :: dst
            case srch :: srct => {
                val bytes = getCharBytes(srch)
                if(MAX_LINE_BYTE_WIDTH < (pw + tmp.size + bytes.length)) {
                    split(srct, mutable.ListBuffer(bytes: _*), alignWidth(tmp, pw) :: dst)
                } else {
                    split(srct, (tmp ++= bytes), dst)
                }
            }
        }

        split(file.getName.toList, mutable.ListBuffer.empty[Byte], Nil).reverse
    }

    def setupFileInfo(dsts: mutable.ListBuffer[String], file: File, hierarchy: List[String], branch: String, nextBranch: String) {
        var first = true
        for(fn <- splitFileName(file, hierarchy)) {
            if(first) {
                first = false
                dsts += hierarchy.mkString + branch + fn + " " + fileAttributes(file)
            } else
                dsts += hierarchy.mkString + nextBranch + fn
        }
    }

    def setupRootInfo(dsts: mutable.ListBuffer[String], file: File) {
        var first = true
        for(fn <- splitFileName(file, Nil)) {
            if(first) {
                first = false
                dsts += fn + "   " + fileAttributes(file)
            } else
                dsts += fn
        }
    }

    val ignoreSet = Set(
        ('D,".svn"),
        ('D,".git"),
        ('D,"target"),
        ('D,"project"),
        ('D,"classes")
    )

    def dirTreeFileFilter(file: File): Boolean = ! ignoreSet.exists{
        case ('D, n) => (file.isDirectory) && (file.getName == n)
        case _ => false
    }

    trait FileHolder
    object Sentinel extends FileHolder
    case class FileWrap(file: File) extends FileHolder

    def listFiles(dir: File): List[FileHolder] =
        dir.listFiles(dirTreeFileFilter _).sortWith((f1,f2) =>
            if((f1.isDirectory) && (f2.isDirectory)) f1.getName < f2.getName
            else if(f1.isDirectory) true
            else if(f2.isDirectory) false
            else f1.getName < f2.getName
        ).map(FileWrap(_)).toList :+ Sentinel
//        dir.listFiles(dirTreeFileFilter _).map(FileWrap(_)).toList :+ Sentinel

    /* Use view bounds */
    def init[B <% Seq[_]](l: B): B = if(l.nonEmpty) l.init.asInstanceOf[B] else l

    @tailrec
    def downDirectoryTree(files: List[FileHolder], hierarchy: List[String], dsts: mutable.ListBuffer[String]) {
        files match {
            case Nil => /* Nothing to do */
            case FileWrap(f) :: (rofs @ List(Sentinel, _*)) => {
                setupFileInfo(dsts, f, hierarchy, "└", "　")
                f match {
                    case d if(d.isDirectory) => downDirectoryTree(listFiles(d) ::: rofs, hierarchy :+ "　", dsts)
                    case _ => downDirectoryTree(rofs, hierarchy, dsts)
                }
            }
            case FileWrap(f) :: rofs => {
                setupFileInfo(dsts, f, hierarchy, "├", "│")
                f match {
                    case d if(d.isDirectory) => downDirectoryTree(listFiles(d) ::: rofs, hierarchy :+ "│", dsts)
                    case _ => downDirectoryTree(rofs, hierarchy, dsts)
                }
            }
            case Sentinel :: rofs => downDirectoryTree(rofs, init(hierarchy), dsts)
            case fs => throw new IllegalStateException(fs.toString)
        }
    }

    def main(args: Array[String]) {

        if(args.length == 0)
            sys.exit(1)

        val root = new File(args(0))
        if(!root.exists)
            sys.exit(2)

        println(s"""[root = ${root.getAbsolutePath} , Creation DateTime = ${ZonedDateTime.now.format(DateTimeFormatter.ofPattern("yyyy/MM/dd-HH:mm:ss"))}]""")
        println()
        println("File/Folder                                                            Size     Attr Creation          LastModified      LastAccess       ")
        println("----------------------------------------------------------------------+--------+----+-----------------+-----------------+-----------------")

        val treeBuffer = mutable.ListBuffer.empty[String]
        setupRootInfo(treeBuffer,root)
        downDirectoryTree(listFiles(root), Nil, treeBuffer)
        println(treeBuffer.mkString(f"%n"))
    }
}

