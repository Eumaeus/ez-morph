import scala.io.Source
import java.io._
import edu.holycross.shot.greek._
import edu.furman.classics.poslib._
import scala.util.control.Exception.catching

val defaultLexFile:String = "../lexdata/lexicon.cex"
val defaultFormsFile:String = "../lexdata/forms.cex"
val parts:GreekPartsOfSpeech = GreekPartsOfSpeech()


/* USER PARAMETERS */
val distanceThreshold:Int = 55 // lower = fewer suggested matches. From 1 to 100.

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
}

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

		val formsRawData:String = {
				val formsData:String = Source.fromFile(formsFile).getLines.mkString("\n")
				println(s"Found ${formsData.lines.size} lines of forms data.")
				formsData
		}

		// Validate and build a vector of lexEntries
		val lexVec:Vector[String] = lexRawData.lines.toVector.filter(_.size > 1)

		val headerLine:String = lexVec.head
		if (headerLine != "id#lemma#partOfSpeech#entry#notes") {
			throw new Exception(s"Invalid header in file ${lexFile}:\n\t${headerLine}\nshould be\n\tid#lemma#partOfSpeech#entry#notes")
		}
		val lexEntryStrings:Vector[String] = lexVec.tail

		// Get a vector of lexical entries
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
				//println(s"${newLexEntry}")
				newLexEntry
			})
		}
		// Confirm that there are no duplicate IDs 
		val testVec = lexEntries.groupBy(_.id).toVector
		val filteredVec = testVec.filter(o => o._2.size > 1)
		if (filteredVec.size > 0){
			val errorStr1:String = s"The following Lexical Entries in file ${lexFile} have duplicate IDs:"
			//for (dup <- filteredVec){ println(s"${dup}")}
			val errorStr2:String = filteredVec.mkString("\n")
			throw new Exception(s"${errorStr1}\n${errorStr2})")
		}

		// Validate and build a vector of forms 
		val formsLinesVec:Vector[String] = formsRawData.lines.toVector
		val formsVec:Vector[String] = formsLinesVec.filter(_.size > 0)

		val formsHeaderLine:String = formsVec.head
		if (formsHeaderLine != "lexId#form#postag") {
			throw new Exception(s"Invalid header in file ${formsFile}:\n\t${formsHeaderLine}\nshould be\n\tlexId#form#postag")
		}
		val formsEntryStrings:Vector[String] = formsVec.tail


		// Get a vector of forms
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
				newFormEntry
			})
		}
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


def performLookup(s1:String, dt:Int = distanceThreshold):(Vector[FormEntry], Vector[LexEntry]) = {
	if ((lexEntriesOption != None) && (formEntriesOption != None)) {
		val perfectMatches:Vector[FormEntry] = {
			 val greekS1:LiteraryGreekString = LiteraryGreekString(s1)
			//formEntries.filter(_.form == s1)
			formEntriesOption.get.filter( f => {
				LiteraryGreekString(f.form).ascii == greekS1.ascii
			})
		}	
		val partialMatches:Vector[LexEntry] = {
			if (perfectMatches.size > 0) { Vector() }
			else {
				val greekS1:LiteraryGreekString = LiteraryGreekString(s1)
				val lexMatches:Vector[LexEntry] = {
					lexEntriesOption.get.filter(f => {
						weightedDistance(greekS1.ascii, f.lemmaString) <= dt
					})
				}
				val formMatches:Vector[FormEntry] = {
					formEntriesOption.get.filter(f => {
						weightedDistance(greekS1.ascii, f.form) <= dt
					})
				}
				val lexesForForms:Vector[LexEntry] = {
					formMatches.map(f => {
						f.lex
					})
				}
				val finalMatches:Vector[LexEntry] = (lexMatches ++ lexesForForms).distinct
				finalMatches
			}		
		}
		val returnMap:(Vector[FormEntry], Vector[LexEntry]) = (perfectMatches, partialMatches)
		returnMap
	} else {
		println("""No data. Either the lexical entries, or the form entries are empty, or both. You might try loading data with "validate()" """)
		val returnMap:(Vector[FormEntry], Vector[LexEntry]) = (Vector(), Vector())
		returnMap
	}
}

def returnLookup(s1:String, dt:Int = distanceThreshold, markdown:Boolean = false):String = {
	val forms:(Vector[FormEntry], Vector[LexEntry]) = performLookup(s1, dt )
	// report
	if (forms._1.size > 0){
		val headerVec:Vector[String] = Vector(
			s"""\nMatches ${forms._1.size}:"""
		)
		val formsVec:Vector[String] = {
			for (m <- forms._1) yield {
				markdown match {
					case true => m.markdown
					case false => m.toString
				}
			}
		}
		val returnVec:Vector[String] = headerVec ++ formsVec
		returnVec.mkString(" ")
	} else {
		val headerVec:Vector[String] = Vector(
			if (markdown) {
				s"❌ No documented form matches."
			} else {
				s"No documented form matches."
			}
		)

		val formsVec:Vector[String] = {
			if (forms._2.size > 0 ){
				val suggVec:Vector[String] = {
					for (m <- forms._2) yield {
						markdown match {
							case true => m.markdown
							case false => s"${m}"
						}
					}
				}
				Vector("Suggestions:") ++ suggVec
			} else {
				Vector(s"No suggestions.")
			}
		}
		val returnVec:Vector[String] = headerVec ++ formsVec
		returnVec.mkString(" ")
	}
}

def lookup(s1:String, dt:Int = distanceThreshold, markdown:Boolean = false):Unit = {
	val lookupStr:String = returnLookup(s1, dt, markdown)	
	println(lookupStr)
}

def doAnalyze(s1:String, dt:Int = distanceThreshold, markdown:Boolean = false):String = {
	val depunctuated:String = depunctuate(s1) 
	val tokens:Vector[String] = depunctuated.split(" ").toVector
	val testGreekString:LiteraryGreekString = LiteraryGreekString(s1)

	val headerStr:Vector[String] = {
		markdown match {
			case true => {
				Vector(s"\n1. ${ucodePlus(testGreekString)}")
			}			
			case false => {
				Vector(s"\n${ucodePlus(testGreekString)}")
			}
		}
	}

	val analysisStr:Vector[String] = {
		for ((t,i) <- tokens.zipWithIndex) yield {
			markdown match {
				case true => {
					s"\n    ${i+1}. **${ucodePlus(LiteraryGreekString(t))}**. ${returnLookup(t, markdown = true)}\n"
				}
				case false => {
					s"\n${i+1}. ${ucodePlus(LiteraryGreekString(t))}\n${returnLookup(t)}"
				}
			}
		}
	}
	val responseVec:Vector[String] = headerStr ++ analysisStr
	responseVec.mkString("\n")
}

def analyze(s1:String, dt:Int = distanceThreshold):Unit = {
	if ( (lexEntriesOption == None) || (formEntriesOption == None) ) {
		if (lexEntriesOption == None) {
			val error:String = s"""There are no lexical entries loaded. Lexical entries are expected to be in ${defaultLexFile}. You might try loading data with "validate()". """
			printError(error)
		}
		if (formEntriesOption == None) {
			val error:String = s"""There are no morphological forms loaded. Forms are expected to be in ${defaultFormsFile}. You might try loading data with "validate()". """
			printError(error)
		}
	} else {
		val analysis:String = doAnalyze(s1, dt)
		println(analysis)
	}
}

def analyzeFile(name:String = "exercises", filePath:String = "documents/", lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile, logOnly:Boolean = false):Unit = {
	try {
		println(s"\nUpdating and validating data, from ${lexFile} and ${formsFile}")
		validate(lexFile, formsFile)

		if ( (lexEntriesOption == None) || (formEntriesOption == None) ) {
			if (lexEntriesOption == None) {
				val error:String = s"""There are no lexical entries loaded. Lexical entries are expected to be in ${defaultLexFile}. You might try loading data with "validate()". """
				printError(error)
			}
			if (formEntriesOption == None) {
				val error:String = s"""There are no morphological forms loaded. Forms are expected to be in ${defaultFormsFile}. You might try loading data with "validate()". """
				printError(error)
			}
		} else {
			import scala.sys.process._
			val inputPath:String = s"${filePath}${name}.txt"	
			val outputMdFile:String = s"${filePath}${name}.md"
			val outputDocFile:String = s"${filePath}${name}.docx"
			val exData:Vector[String] = Source.fromFile(inputPath).getLines.filter(_.size > 0).toVector

			val anaVec:Vector[String] = exData.map(l => doAnalyze(l, markdown = true))

			if (logOnly){ 
				println( anaVec.mkString("\n") )
			} else {

				val pw = new PrintWriter(new File(outputMdFile))
				pw.write( anaVec.mkString("\n") )
				pw.close

				val pandocIt:String = s"pandoc -o ${outputDocFile} ${outputMdFile}"
				println(s"\n Success! The results will be in:\n\n ${outputMdFile} \n\n and \n\n ${outputDocFile}.\n")
				pandocIt ! 
			}
		}
	} catch {
		case e:Exception => printError(s"${e}")
	}
}

val tipsString:String = """

Things you can do:

1. Validate Lexical and Morphological data:

scala> validate()
	or
scala> refresh()

2. Print pretty Greek:

scala> greek("a)/qnrwpos")

3. Generate a guide to Beta Code

scala> betaCode()

4. Lookup forms in your data:

scala> lookup("lu/w")

5. Analyze a sentence of Greek:

scala> analyze("to\\n a)/nqrwpon lu/w.")

	(n.b. You have to double any beta-code grave accents, because '\' has a special meaning in Scala.)

6. Analyze a file consisting of Greek sentences:

scala> analyzeFile()

	This assumes the file of sentrences is "documents/exercises.txt"; output will be a markdown file and a .docx file in 'documents/'

scala> analyzeFile("unit1")

	This will anayze the file "documents/unit1.txt".

7. Get details on what forms you have seen. 

    E.g. find all genitive forms you have recorded:

scala> val justGenitives:Vector[FormEntry] = formEntries.filter(f => { f.pos.grammaticalcase match { case Some(gc) => gc.short == "gen"; case _ => false } })
scala> for (f <- justGenitives) { println(f) }

    E.g. find all forms of verbs you have recorded:

scala> val justVerbs:Vector[FormEntry] = formEntries.filter(f => { f.pos.pos match { case Some(p) => p.short == "verb"; case _ => false } })
scala> for (f <- justVerbs) { println(f) }

(You can see these tips again by typing "tips".)

╔═════════════════════════════╗
║ Ready to Work!              ║
║ Load data with "validate()" ║
╚═════════════════════════════╝

"""

def tips:Unit = {
	for (l <- tipsString.lines) { println(l) }	
}

tips





