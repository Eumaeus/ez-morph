import scala.io.Source
import edu.holycross.shot.greek._
import edu.furman.classics.poslib._

val defaultLexFile:String = "../lexdata/lexicon.cex"
val defaultFormsFile:String = "../lexdata/forms.cex"

val parts:GreekPartsOfSpeech = GreekPartsOfSpeech()


case class LexEntry(id:String, lemmaString:String, pos:String, entry:String, notes:String){
	val lemma:LiteraryGreekString = LiteraryGreekString(s"${lemmaString}. ") // throw in a space to help with sigmas


	override def toString:String = {
		val reportNotes:String = {
			notes.size match {
				case n if (n < 1) => "No notes."
				case _ => notes
			}
		}
		val ts:String = s"${id}: ${lemma.ucode} (${lemma.ascii}). ${pos}. ${entry}. Notes: ${reportNotes}"
		ts
	}
}

case class FormEntry(form:String, lex:LexEntry, postag:String) {
	val greekForm:LiteraryGreekString = LiteraryGreekString(form)
	if (postag.size != 9){
		throw new Exception(s"${postag} is not a valid part of speech tag.")
	}
	val pos:GreekPartOfSpeech = parts.posFromTag(postag)
	override def toString:String = {
		val entryString:String = lex.toString
		val ts:String = s"${greekForm.ucode} (${greekForm.ascii}) = ${pos}.\nFrom: ${entryString}"
		ts
	}
}

def validate(lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile) {
	var isValid:Boolean = true
	val lexRawData:String = {
		try {
			val lexData:String = Source.fromFile(lexFile).getLines.mkString("\n")
			println(s"\nFound lexicon data: ${lexData.lines.size} lines.")
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
			println(s"\nFound forms data: ${formsData.lines.size} lines.")
			formsData
		} catch {
			case e:Exception => {
				println(s"${e}")
				sys.exit(0)
			}
		}
	}
	println("Files exist.")
	
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
			val newForm:String = splitLine(0)
			val newLexIdString:String = splitLine(1)
			val newDesc:String = splitLine(2)

			// test for valid lexEntry
			val matchLexEntry = lexEntries.filter(_.id == newLexIdString)
			if (matchLexEntry.size < 1){
				println(s"No LexEntry matches id ${newLexIdString} from: '${les}'.")
				//sys.exit(0)
			} 
			val newFormEntry:FormEntry = FormEntry(newForm, matchLexEntry(0), newDesc)
			if (newFormEntry.greekForm.ucode.contains("#")) {
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

}







