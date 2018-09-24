import scala.io.Source
import java.io._
import edu.holycross.shot.greek._
import edu.furman.classics.poslib._

val defaultLexFile:String = "../lexdata/lexicon.cex"
val defaultFormsFile:String = "../lexdata/forms.cex"
val parts:GreekPartsOfSpeech = GreekPartsOfSpeech()

/* USER PARAMETERS */
val distanceThreshold:Int = 55 // lower = fewer suggested matches. From 1 to 100.

/* --------------- */

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
		val ts:String = s"**${ucodePlus(greekForm)}**, from ${ucodePlus(lex.lemma)}, *${lex.entry}*): ${pos}. "
		ts
	}
}

def quikValidate(lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile):Boolean =  {
	var isValid:Boolean = true
	val lexRawData:String = {
		try {
			val lexData:String = Source.fromFile(lexFile).getLines.mkString("\n")
			lexData
		} catch {
			case e:Exception => {
				println(s"${e}")
				sys.exit(0)
			}
		}
	}
	val formsRawData:String = {
		try {
			val formsData:String = Source.fromFile(formsFile).getLines.mkString("\n")
			formsData
		} catch {
			case e:Exception => {
				println(s"${e}")
				sys.exit(0)
			}
		}
	}
	
	// Validate and build a vector of lexEntries
	val lexVec:Vector[String] = lexRawData.lines.toVector

	val headerLine:String = lexVec.head
	if (headerLine == "id#lemma#partOfSpeech#entry#notes") {
		//println(s"Header on lexical entries is valid.")
	} else {
		println(s"Invalid header:\n\t${headerLine}\nshould be\n\tid#lemma#partOfSpeech#entry#notes")
		isValid = false
	}
	val lexEntryStrings:Vector[String] = lexVec.tail


	// Get a vector of lexical entries
	val lexEntries:Vector[LexEntry] = {
		lexEntryStrings.map(les => {
			// Test for fields		
			val splitLine = les.split("#")
			if (splitLine.size < 4){
				println(s"This line has too few components\n\n\t${les}")
				isValid = false
			}
			if (splitLine.size > 5){
				println(s"This line has too many components\n\n\t${les}")
				isValid = false
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
						println(s"Error (wrong number of components) in line: ${les}")
						isValid = false
						""
					}
				}
			}
			val newLexEntry:LexEntry = LexEntry(newId, newLemma, newPos, newEntry, newNotes)
			// check for invalid characters
			if (newLexEntry.lemma.ucode.contains("#")) {
				println("----- ERROR! ------")
				println(s"${newLexEntry.lemma.ucode} (${newLexEntry.lemma.ascii}) contains an invalid character.")
				isValid = false
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
		println(s"The following Lexical Entries have duplicate IDs:")
		for (dup <- filteredVec){ println(s"${dup}")}
		isValid = false
	}

	// Validate and build a vector of forms 
	val formsLinesVec:Vector[String] = formsRawData.lines.toVector
	val formsVec:Vector[String] = formsLinesVec.filter(_.size > 0)

	val formsHeaderLine:String = formsVec.head
	if (formsHeaderLine == "lexId#form#postag") {
		println(s"Header on forms is valid.")
	} else {
		println(s"Invalid header:\n\t${formsHeaderLine}\nshould be\n\tlexId#form#postag")
		isValid = false
	}
	val formsEntryStrings:Vector[String] = formsVec.tail


	// Get a vector of forms
	val allForms:Vector[FormEntry] = {
		formsEntryStrings.map(les => {
			// Test for fields		
			val splitLine = les.split("#")
			if (splitLine.size < 3){
				println(s"This line has too few components\n\n\t${les}")
				isValid = false
			}
			if (splitLine.size > 3){
				println(s"This line has too many components\n\n\t${les}")
				isValid = false
			}
			// if we got here, we have a good number of fields
			// so make a LexEntry
			val newForm:String = splitLine(1)
			val newLexIdString:String = splitLine(0)
			val newDesc:String = splitLine(2)

			// test for valid lexEntry
			val matchLexEntry = lexEntries.filter(_.id == newLexIdString)
			if (matchLexEntry.size < 1){
				println(s"No LexEntry matches id ${newLexIdString} from: '${les}'.")
				//sys.exit(0)
			} 
			val newFormEntry:FormEntry = FormEntry(newForm, matchLexEntry(0), newDesc)

			if (newFormEntry.greekForm.ucode.contains("#")) {
				println("----- ERROR! ------")
				println(s"${newFormEntry.greekForm.ucode} (${newFormEntry.greekForm.ascii}) contains an invalid character.")
				isValid = false
			}
			newFormEntry
		})
	}
	// Confirm that there are no duplicate IDs 
	val testFormsVec = allForms.groupBy(identity).toVector
	val filteredFormsVec = testFormsVec.filter(o => o._2.size > 1)
	if (filteredFormsVec.size > 0){
		println(s"The following Form Entries are duplicates:")
		for (dup <- filteredFormsVec){ println(s"${dup}\n")}
		isValid = false
	}

	isValid match {
		case true => println(s"Files are valid!")
		case false => println(s"INVALID. See error messages above.")
	}
	isValid

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

def getLexEntries(lexFile:String = defaultLexFile):Option[Vector[LexEntry]] = {
	if (quikValidate()) { 
		// Validate and build a vector of lexEntries
		val lexRawData:String = {
			val lexData:String = Source.fromFile(lexFile).getLines.mkString("\n")
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

		Some(lexEntries)

	} else {
		None
	}
}

def getFormEntries(formsFile:String = defaultFormsFile, lexEntries:Vector[LexEntry]):Option[Vector[FormEntry]] = {
	if (validate()) {
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
		Some(allForms)
	} else {
		None
	}
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

val lexEntriesOption:Option[Vector[LexEntry]] = getLexEntries()
if (lexEntriesOption == None) { 
	println(s"No valid Lexical Entries found.")
	sys.exit(0)
} 
// If we got here, we've got lexEntries
val lexEntries:Vector[LexEntry] = lexEntriesOption.get

val formEntriesOption:Option[Vector[FormEntry]] = getFormEntries(lexEntries = lexEntries)
if (formEntriesOption == None) {
	println(s"No valid Forms found.")
	sys.exit(0)
}

// If we got here, we've got Form entries
val formEntries:Vector[FormEntry] = formEntriesOption.get

def performLookup(s1:String, dt:Int = distanceThreshold):(Vector[FormEntry], Vector[LexEntry]) = {
	val perfectMatches:Vector[FormEntry] = {
		 val greekS1:LiteraryGreekString = LiteraryGreekString(s1)
		//formEntries.filter(_.form == s1)
		formEntries.filter( f => {
			LiteraryGreekString(f.form).ascii == greekS1.ascii
		})
	}	
	val partialMatches:Vector[LexEntry] = {
		if (perfectMatches.size > 0) { Vector() }
		else {
			val greekS1:LiteraryGreekString = LiteraryGreekString(s1)
			val lexMatches:Vector[LexEntry] = {
				lexEntries.filter(f => {
					weightedDistance(greekS1.ascii, f.lemmaString) <= dt
				})
			}
			val formMatches:Vector[FormEntry] = {
				formEntries.filter(f => {
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
			s"No documented form matches."
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
					s"\n    ${i+1}. ${ucodePlus(LiteraryGreekString(t))}. ${returnLookup(t, markdown = true)}\n"
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
	val analysis:String = doAnalyze(s1, dt)
	println(analysis)
}

def analyzeFile(name:String = "exercises", filePath:String = "documents/"):Unit = {
	import scala.sys.process._
	val inputPath:String = s"${filePath}${name}.txt"	
	val outputMdFile:String = s"${filePath}${name}.md"
	val outputDocFile:String = s"${filePath}${name}.docx"
	val exData:Vector[String] = Source.fromFile(inputPath).getLines.filter(_.size > 0).toVector

	val anaVec:Vector[String] = exData.map(l => doAnalyze(l, markdown = true))
	println( anaVec.mkString("\n") )

	val pw = new PrintWriter(new File(outputMdFile))
	pw.write( anaVec.mkString("\n") )
	pw.close

	val pandocIt:String = s"pandoc -o ${outputDocFile} ${outputMdFile}"
	println(s"Running '${pandocIt}'")
	pandocIt ! 
}

val tipsString:String = """

Things you can do:

1. Print pretty Greek:

scala> greek("a)/qnrwpos")

2. Generate a guide to Beta Code

scala> betaCode()

3. Lookup forms in your data:

scala> lookup("lu/w")

4. Analyze a sentence of Greek:

scala> analyze("to\\n a)/nqrwpon lu/w.")

	(n.b. You have to double any beta-code grave accents, because '\' has a special meaning in Scala.)

5. Analyze a file consisting of Greek sentences:

scala> analyzeFile()

	This assumes the file of sentrences is "documents/exercises.txt"; output will be a markdown file and a .docx file in 'documents/'

scala> analyzeFile("unit1")

	This will anayze the file "documents/unit1.txt".

6. Get details on what forms you have seen. 

    E.g. find all genitive forms you have recorded:

scala> val justGenitives:Vector[FormEntry] = formEntries.filter(f => { f.pos.grammaticalcase match { case Some(gc) => gc.short == "gen"; case _ => false } })
scala> for (f <- justGenitives) { println(f) }

    E.g. find all forms of verbs you have recorded:

scala> val justVerbs:Vector[FormEntry] = formEntries.filter(f => { f.pos.pos match { case Some(p) => p.short == "verb"; case _ => false } })
scala> for (f <- justVerbs) { println(f) }

(You can see these tips again by typing "tips".)

"""

def tips:Unit = {
	for (l <- tipsString.lines) { println(l) }	
}


println()
println()
println("************************")
println(s"* Ready to work!      *")
println("************************")

tips





