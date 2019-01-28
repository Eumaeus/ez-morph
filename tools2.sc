import scala.io.Source
import java.io._
import edu.holycross.shot.greek._
import edu.furman.classics.poslib._
import scala.util.control.Exception.catching
import  java.util.Calendar

val defaultLexFile:String = "../lexdata/lexicon.cex"
//val defaultLexFile:String = "/Users/cblackwell/Desktop/Keians/lexdata/lexicon.cex"
val defaultFormsFile:String = "../lexdata/forms.cex"
//val defaultFormsFile:String = "/Users/cblackwell/Desktop/Keians/lexdata/forms.cex"
val parts:GreekPartsOfSpeech = GreekPartsOfSpeech()



/* USER PARAMETERS */
val distanceThreshold:Int = 45 // lower = fewer suggested matches. From 1 to 100.

/* --------------- */


def handling[Ex <: Throwable, T](exType: Class[Ex])(block: => T): Either[Ex, T] =
  catching(exType).either(block).asInstanceOf[Either[Ex, T]]

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

def betaCode(filePath:String = "documents/"):Unit = {
	import scala.sys.process._

	val outputMdFile:String = s"${filePath}betaCodeGuide.md"
	val outputDocFile:String = s"${filePath}betaCodeGuide.docx"
	val pw = new PrintWriter(new File(outputMdFile))

	val ucodeGreekStr:String = "αβγδεζηθικλμνξοπρσςτυφχψωἀἁάὰᾶᾳᾄϊΑΒΓἈἌ.;:'"

	
		val line1:String = s"""
# Beta Code Guide 

| Unicode | BetaCode |
|---------|----------|
	"""
	pw.write(line1)
	println(line1)

	for (c <- ucodeGreekStr) yield {
		val lgs:LiteraryGreekString = LiteraryGreekString(c.toString)
		val line2:String = s"| ${lgs.ucode} | ${lgs.ascii} |"
		println(line2)
		pw.append(line2)
		pw.append("\n")
	}

	pw.close

	val pandocIt:String = s"pandoc -o ${outputDocFile} ${outputMdFile}"
	println(s"Running '${pandocIt}'")
	pandocIt ! 
	
}

/* --------------- */

case class LexEntry(id:String, lemmaString:String, pos:String, entry:String, notes:String){
	val lemma:LiteraryGreekString = LiteraryGreekString(s"${lemmaString}. ") // throw in a space to help with sigmas
	override def toString:String = {
		val reportNotes:String = {
			notes.size match {
				case n if (n < 1) => ""
				case _ => notes
			}
		}
		val ts:String = s"${id}: ${ucodePlus(lemma)} (${lemma.ascii}); ${pos}; ${entry}. Notes: ${reportNotes}"
		ts
	}
	def markdown:String = {
		val ts:String = s"${id}: ${ucodePlus(lemma)}; ${pos}; *${entry}*."
		ts
	}

	def html:String = {
		s"""<span class="lexEntry">${id} <span class="lemma">${ucodePlus(lemma)}</span> <span class="posTag">${pos}</span> <span class="entry">${entry}</span></span>"""
	}
}

case class FormEntry(form:String, lex:LexEntry, postag:String) {
		val greekForm:LiteraryGreekString = LiteraryGreekString(form)
		val pos:GreekPartOfSpeech = parts.posFromTag(postag)

		override def toString:String = {
			val entryString:String = lex.toString
			val ts:String = s"${ucodePlus(greekForm)} (${greekForm.ascii}) = ${pos}.\nFrom: ${entryString}"
			ts
		}

		def markdown:String = {
			val entryString:String = lex.toString
			val ts:String = s"**${ucodePlus(greekForm)}**: ${pos}, from ${ucodePlus(lex.lemma)}, *${lex.entry}*. "
			ts
		}

		def html:String = {
			val entryString:String = lex.html
			val ts:String = s"""<span class="formEntry"><span class="form">${ucodePlus(greekForm)}</span> <span class="pos">${pos}</span> from <br/>  ${entryString}</span>"""
			ts
		}
}

// for holding looked up stuff
var lookupRes:scala.collection.mutable.Map[String,(Vector[FormEntry], Vector[LexEntry])] = scala.collection.mutable.Map()

// Init variabls to hold data
var lexEntriesOption:Option[Vector[LexEntry]] = None
var formEntriesOption:Option[Vector[FormEntry]] = None

def doValidate(lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile):Boolean = {
	try {
		val lexRawData:String = {
			val lexData:String = Source.fromFile(lexFile).getLines.mkString("\n")
			println(s"\nFound ${lexData.lines.size} lines of lexical data.")
			lexData
		}
		println(s"Got raw lexicon data.")

		val formsRawData:String = {
				val formsData:String = Source.fromFile(formsFile).getLines.mkString("\n")
				println(s"Found ${formsData.lines.size} lines of forms data.")
				formsData
		}
		println(s"Got raw forms data.")

		// Validate and build a vector of lexEntries
		val lexVec:Vector[String] = lexRawData.lines.toVector.filter(_.size > 1)
		println(s"Filtered out empty lexicon lines.")

		val headerLine:String = lexVec.head
		if (headerLine != "id#lemma#partOfSpeech#entry#notes") {
			throw new Exception(s"Invalid header in file ${lexFile}:\n\t${headerLine}\nshould be\n\tid#lemma#partOfSpeech#entry#notes")
		}
		val lexEntryStrings:Vector[String] = lexVec.tail

		// Get a vector of lexical entries
		println("Creating lexicon entries: \n")
		val lexEntries:Vector[LexEntry] = {
			lexEntryStrings.map(les => {
				// Test for fields		
				val splitLine = les.split("#")
				if (splitLine.size < 4){
					throw new Exception(s"""This line in file ${lexFile} has too few components\n\n\t"${les}"\n""")
				}
				if (splitLine.size > 5){
					throw new Exception(s"""This line in file ${lexFile} has too many components\n\n\t"${les}" """)
				}
				// if we got here, we have a good number of fields
				// so make a LexEntry
				val newId:String = splitLine(0)
				val newLemma:String = splitLine(1)
				val newPos:String = splitLine(2)
				val newEntry:String = splitLine(3)
				val newNotes:String = {
					splitLine.size match {
						case 4 => ""
						case 5 => splitLine(4)
						case _ => {
							throw new Exception(s"""Error (wrong number of components) in line: "${les}" of file ${lexFile}.""")
						}
					}
				}
				val newLexEntry:LexEntry = LexEntry(newId, newLemma, newPos, newEntry, newNotes)
				// check for invalid characters
				if (newLexEntry.lemma.ucode.contains("#")) {
					throw new Exception(s""" "${newLexEntry.lemma.ucode} (${newLexEntry.lemma.ascii})" n file ${lexFile} contains an invalid character.""")
				}
				//println(s"\n----------")
				print(s"${newLexEntry.id} ")
				newLexEntry
			})
		}
		println("\nAll lexicon entries created.\n")
		// Confirm that there are no duplicate IDs 
		val testVec = lexEntries.groupBy(_.id).toVector
		val filteredVec = testVec.filter(o => o._2.size > 1)
		if (filteredVec.size > 0){
			val errorStr1:String = s"The following Lexical Entries in file ${lexFile} have duplicate IDs:"
			//for (dup <- filteredVec){ println(s"${dup}")}
			val errorStr2:String = filteredVec.mkString("\n")
			throw new Exception(s"${errorStr1}\n${errorStr2})")
		}
		println(s"Confirmed that there are no duplicat lexicon IDs.")

		// Validate and build a vector of forms 
		val formsLinesVec:Vector[String] = formsRawData.lines.toVector
		val formsVec:Vector[String] = formsLinesVec.filter(_.size > 0)
		println("Filtered out blank forms lines.")

		val formsHeaderLine:String = formsVec.head
		if (formsHeaderLine != "lexId#form#postag") {
			throw new Exception(s"Invalid header in file ${formsFile}:\n\t${formsHeaderLine}\nshould be\n\tlexId#form#postag")
		}
		val formsEntryStrings:Vector[String] = formsVec.tail


		// Get a vector of forms
		println("\nCreating forms…\n")
		val allForms:Vector[FormEntry] = {
			formsEntryStrings.map(les => {
				// Test for fields		
				val splitLine = les.split("#")
				if (splitLine.size < 3){
					throw new Exception(s"This line in file ${formsFile} has too few components\n\n\t${les}")
				}
				if (splitLine.size > 3){
					throw new Exception(s"This line in file ${formsFile} has too many components\n\n\t${les}")
				}
				// if we got here, we have a good number of fields
				// so make a LexEntry
				val newForm:String = splitLine(1)
				val newLexIdString:String = splitLine(0)
				val newDesc:String = splitLine(2)

				// test for valid lexEntry
				val matchLexEntry = lexEntries.filter(_.id == newLexIdString)
				if (matchLexEntry.size < 1){
					throw new Exception(s"""The form "${les}" (in file ${formsFile}) claims to be linked to a lexical entry with ID=${newLexIdString}, but no LexEntry in file ${lexFile} has that ID.""")
				} 
				/* Lets test the POS tag on its own */
				val posEither: Either[edu.furman.classics.poslib.PosException, GreekPartOfSpeech] = handling(classOf[edu.furman.classics.poslib.PosException])(parts.posFromTag(newDesc))
				posEither match {
					case Left(e) => {
						throw new Exception(s"""\n\n There is something wrong with the part-of-speech tag "${newDesc}" on the line "${les}" in file ${formsFile}. \n\n More details (the mistaken character will be after "tag ->": \n\n ${e}""")
					}
					case Right(_) => {
						// do nothing 
					}
				}

				val newFormEntry:FormEntry = FormEntry(newForm, matchLexEntry(0), newDesc)
				if (newFormEntry.greekForm.ucode.contains("#")) {
					throw new Exception(s""" "${newFormEntry.greekForm.ucode} (${newFormEntry.greekForm.ascii})" in file ${formsFile} contains an invalid character.""")
				}
				print(". ")
				newFormEntry
			})
		}
		println("\nForms created.")
		// Confirm that there are no duplicate IDs 
		val testFormsVec = allForms.groupBy(identity).toVector
		val filteredFormsVec = testFormsVec.filter(o => o._2.size > 1)
		if (filteredFormsVec.size > 0){
			val errStr1:String = (s"The following Form Entries in file ${formsFile} are duplicates:")
			//for (dup <- filteredFormsVec){ println(s"${dup}\n")}
			val errStr2:String = filteredFormsVec.mkString("\n")
			throw new Exception(s"${errStr1}\n${errStr2}")
		}
		println(s"Files are valid!")	
		true

	} catch {
		case e:Exception => {
			printError(e.toString)
			false
		}
	}

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

def getLexEntries(lexFile:String = defaultLexFile):Vector[LexEntry] = {
	// Validate and build a vector of lexEntries
	val lexRawData:String = {
		val lexData:String = Source.fromFile(lexFile).getLines.filter(_.size > 1).mkString("\n")
		lexData
	}	

	val lexVec:Vector[String] = lexRawData.lines.toVector

	val headerLine:String = lexVec.head
	val lexEntryStrings:Vector[String] = lexVec.tail

	// Get a vector of lexical entries
	val lexEntries:Vector[LexEntry] = {
		lexEntryStrings.map(les => {
			val splitLine = les.split("#")
			val newId:String = splitLine(0)
			val newLemma:String = splitLine(1)
			val newPos:String = splitLine(2)
			val newEntry:String = splitLine(3)
			val newNotes:String = {
				splitLine.size match {
					case 5 => splitLine(4)
					case _ => ""
				}
			}
			val newLexEntry:LexEntry = LexEntry(newId, newLemma, newPos, newEntry, newNotes)
			//println(s"\n----------")
			//println(s"${newLexEntry}")
			newLexEntry
		})
	}
	lexEntries
}

def getFormEntries(formsFile:String = defaultFormsFile, lexEntries:Vector[LexEntry]):Vector[FormEntry] = {
	val formsRawData:String = {
		val formsData:String = Source.fromFile(formsFile).getLines.mkString("\n")
		formsData
	}
	// Validate and build a vector of forms 
	val formsLinesVec:Vector[String] = formsRawData.lines.toVector
	val formsVec:Vector[String] = formsLinesVec.filter(_.size > 0)
	val formsEntryStrings:Vector[String] = formsVec.tail

		// Get a vector of forms
		val allForms:Vector[FormEntry] = {
			formsEntryStrings.map(les => {
				// Test for fields		
				val splitLine = les.split("#")
				// if we got here, we have a good number of fields
				// so make a LexEntry
				val newForm:String = splitLine(1)
				val newLexIdString:String = splitLine(0)
				val newDesc:String = splitLine(2)

				// test for valid lexEntry
				val matchLexEntry = lexEntries.filter(_.id == newLexIdString)
				val newFormEntry:FormEntry = FormEntry(newForm, matchLexEntry(0), newDesc)

				newFormEntry
			})
		}
		allForms
}

// calculate the Levenshtein distance for two strings
def distance(s1: String, s2: String): Int = {
	val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }

	@inline
	def minimum(i: Int*): Int = i.min

	for {j <- dist.indices.tail
		i <- dist(0).indices.tail} dist(j)(i) =
			if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
			else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)
		dist(s2.length)(s1.length)
}

// get a Levenshtein distance for two strings, weighted based on the length of the test string
def weightedDistance(s1:String, s2:String):Int = {
	val targetSize:Double = s1.size.toDouble
	val dist:Double = distance(s1, s2).toDouble
	val weightedDist:Double = dist/targetSize * 100
	weightedDist.toInt
}

def printDistance(s1: String, s2: String) {
	println("Standard: %s -> %s : %d".format(s1, s2, distance(s1, s2)))
	println("Weighted: %s -> %s : %d".format(s1, s2, weightedDistance(s1, s2)))
}


/* Load up our data! */

def loadData(lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile) {
	if ( doValidate(lexFile, formsFile) ) {
		val lexEntries:Vector[LexEntry] = getLexEntries(lexFile)
		lexEntriesOption = Some(lexEntries)
		val formEntries:Vector[FormEntry] = getFormEntries(formsFile, lexEntries)
		formEntriesOption = Some(formEntries)
	} else {
		lexEntriesOption = None
		formEntriesOption = None
	}
}

def refresh(lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile) {
	println("\n----------------\nClearing old data…")
	lexEntriesOption = None
	formEntriesOption = None
	println("\nValidating new data…")
	loadData(lexFile, formsFile)
	if ((lexEntriesOption != None) && (formEntriesOption != None)) {
		val numLex:Int = lexEntriesOption.get.size
		val numForms:Int = formEntriesOption.get.size
		println(s"Success! \nLoaded ${numLex} lexical entries and ${numForms} forms.")
		println("----------------\n")
	} else {
		println("Error: Data not loaded!")
	}
}

def validate(lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile) {
	refresh(lexFile, formsFile)
}








