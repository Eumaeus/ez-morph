import scala.io.Source
import java.io._
import edu.holycross.shot.greek._
import edu.furman.classics.poslib._
import scala.util.control.Exception.catching

val defaultFile:String = "documents/unit4_ucode.txt"
val defaultBetaFile:String = "documents/unit4.txt"



/* Error reporting in the Console */
def printError(e:String):Unit = {
	println("*************************")
    println("ERROR!")
	println(e)
	println("*************************")
}

/* Useful Values */

val sigmaTerminators:Vector[String] = Vector(",",".",":", ";", "'", "—", " ", "\t")
val punctuationMatcher = "[.,:;]".r

def depunctuate(s:String):String = {
	val dp:String = punctuationMatcher.replaceAllIn(s,"")
	val fixedGraves:String = "\\\\".r.replaceAllIn(dp,"/")
	fixedGraves
}

def ucodePlus(s:LiteraryGreekString):String = {
	val uc1:String = s.ucode.replaceAll(":","·")
	val uc2:String = {
		if (uc1.last == 'σ') {
			s"ς${uc1.reverse.tail}".reverse
		} else { uc1 }
	}
	val matcher = "σ.".r	
	val uc3 = {
		matcher.replaceAllIn(uc2, m => {
			val secondChar:String = m.group(0).tail
			if (sigmaTerminators.contains(secondChar)) { s"ς${secondChar}"}
			else { s"σ${secondChar}"}
		})
	}

	uc3

}

def greek(beta:String = "e)lu/qhn lo/gos.") {
	val testGreekString:LiteraryGreekString = LiteraryGreekString(beta)
	val ascii = testGreekString.ascii
	val ucode = ucodePlus(testGreekString)
	println("\n-----------------------------")
	println("Literary Greek")
	println("-----------------------------")
	println(s"${ascii} \n\n${ucode}\n")
	println("-----------------------------\n")
}

def betaFile(ucfile:String = defaultFile, betafile:String = defaultBetaFile):Unit = {
	val ucData:Vector[String] = Source.fromFile(ucfile).getLines.toVector
	val pw = new PrintWriter(new File(betafile))
	pw.write( "" )
	for (l <- ucData) {
		if (l.size > 0) {
			val lgs:LiteraryGreekString = LiteraryGreekString(l.toString)
			val bstring:String = lgs.ascii
			pw.write(s"${bstring}\n")	
		} else {
			pw.write(s"\n")
		}
	}
	pw.close

}






