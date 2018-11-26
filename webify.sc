import scala.io.Source
import java.io._
import edu.holycross.shot.greek._
import edu.furman.classics.poslib._
import scala.util.control.Exception.catching
import  java.util.Calendar

:load tools2.sc
:load html_boilerplate.sc

def htmlAnalyze(bc:String, uc:String, ti:Int, si:Int, dt:Int = distanceThreshold):String = {

	/* Get perfect matches, if any */

	val perfectMatches:Vector[FormEntry] = {
		 val greekS1:LiteraryGreekString = LiteraryGreekString(bc)
		formEntriesOption.get.filter( f => {
			LiteraryGreekString(f.form).ascii == greekS1.ascii
		})
	}

	/* If (and only if) there are no perfect matches, find suggestions */

	val partialMatches:Vector[LexEntry] = {
		if (perfectMatches.size > 0) { Vector() }
		else {
			val greekS1:LiteraryGreekString = LiteraryGreekString(bc)
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

	/* if there are perfect matches, assemble their parts of speech */

	val poss:Option[Vector[GreekPartOfSpeech]] = {
		perfectMatches.size match {
			case n if (n < 1) => {
				None
			}
			case _ => {
				val posVec:Vector[GreekPartOfSpeech] = perfectMatches.map( pm => {
					val parts:GreekPartsOfSpeech = GreekPartsOfSpeech()
					val pos:GreekPartOfSpeech = parts.posFromTag(pm.postag)
					pos
				}).toVector
				Some(posVec)
			}
		}
	}

	/* Is it a verb? */
	val isVerb:Boolean = {
		val returnVal:Boolean = { 
			perfectMatches.size match {
				case n if (n < 1) => {
					false	
				}
				case _ => {
					poss match {
						case Some(vpos) => {
							val possibleCases:Vector[GreekPartOfSpeech] = vpos.filter(p => {
								p.pos match {
									case Some(gotIt) => {
										gotIt.short == "verb"
									}
									case None => false
								}

							})	
							possibleCases.size > 0
						}
						case None => false
					}
				}
			}
		}
		returnVal
	}


	/* If a noun, adjective, article, or pronoun, find what cases are represented */
	val hasCase:List[String] = List("art","noun","adj","pron")
	val cases:Option[Vector[String]] = {
		perfectMatches.size match {
			case n if (n < 1) => {
				None
			}
			case _ => {
				poss match {
					case Some(vpos) => {
						val possibleCases:Vector[GreekPartOfSpeech] = vpos.filter(p => {
							p.pos match {
								case Some(gotIt) => {
									hasCase.contains(gotIt.short)
								}
								case None => false
							}

						})	
						if (possibleCases.size > 0){
							val caseList:Vector[String] = possibleCases.filter( pc => {
								pc.grammaticalcase != None
							}).map( pc => {
								pc.grammaticalcase.get.short
							}).distinct
							Some(caseList)
						} else {
							None
						}

					}
					case None => None
				}
			}
		}
	}


	val idString:String = s"s${si}t${ti}"
	val verbClass:String = if (isVerb) "verb" else ""
	val caseClass:String = {
		cases match {
			case Some(c) => c.mkString(" ")
			case None => ""
		}
	}
	val undoneClass:String = {
		if (partialMatches.size > 0) " unanalyzed " else ""
	}
	val allClasses:String = s"tokenSpan unhovered ${verbClass} ${caseClass} ${undoneClass}"
	val opener:String = s"""<span id="tokenSpan_${idString}" class="${allClasses}" >""" 
	//val tokenValue:String = s"${ucodePlus(uc)}"
	val tokenValue:String = s"${ucodePlus(LiteraryGreekString(uc))}"
	val htmlElementOpen:String = opener
	val htmlElementClose:String = "</span>"
	val formsList:String = {
		if (perfectMatches.size > 0) {
			s"""<ul class="formsList hidden" id="formsList_${idString}">"""  + perfectMatches.map(pm => {
				s"<li>${pm.html}</li>"
			}).mkString("\n") + "</ul>"
		} else { "" }
	}
	val suggestionsList:String = {
		if (partialMatches.size > 0) {
			s"""<ul class="suggestionsList hidden" id="suggestionsList_${idString}">"""  + partialMatches.map(pm => {
				s"<li>${pm.html}</li>"
			}).mkString("\n") + "</ul>"
		} else { "" }
	}
	val allHtml:String = htmlElementOpen + tokenValue + formsList + suggestionsList + htmlElementClose
	allHtml
}

def doAnalyze(s1:String, si:Int, dt:Int = distanceThreshold, markdown:Boolean = false):String = {
	val depunctuated:String = depunctuate(s1) 
	val tokens:Vector[String] = depunctuated.split(" ").toVector
	val testGreekString:LiteraryGreekString = LiteraryGreekString(s1)

	if (markdown) {
		val uCodeTokens:Vector[String] = testGreekString.ucode.split(" ").toVector
		val zippedTokens:Vector[((String, String), Int)] = tokens.zip(uCodeTokens).zipWithIndex
		//println(s"working with ${zippedTokens}\n")
		val htmlString:String = zippedTokens.map(zt => {
			println(s"\ttoken ${zt._2}/${zippedTokens.size}")
			s"${htmlAnalyze(zt._1._1, zt._1._2, zt._2, si)}"
		}).mkString("\n")
		htmlString
	} else {
		""	
	}


}



def analyzeFile(name:String = "exercises", filePath:String = "documents/", lexFile:String = defaultLexFile, formsFile:String = defaultFormsFile, logOnly:Boolean = false):Unit = {
	try {
		lookupRes = scala.collection.mutable.Map() 
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
			val outputHtmlFile:String = s"${filePath}${name}.html"
			val exData:Vector[String] = Source.fromFile(inputPath).getLines.filter(_.size > 0).toVector

			/* Experiment with efficiency */
			val allWords:Vector[String] = depunctuate(exData.mkString(" ")).split(" ").filter(_.size > 0).toVector
			println(s"There are ${allWords.size} words.")
			println(s"There are ${allWords.distinct.size} distinct words.")



			val anaVec:Vector[String] = exData.zipWithIndex.map(zwi => {
				//val then = Calendar.getInstance()
				val start = System.currentTimeMillis
				val l:String = zwi._1
				val i:Int = zwi._2
				println(s"Sentence ${zwi._2}/${exData.size}")
				val analysisString:String = doAnalyze(l, i, markdown = true)
				val totalTime = System.currentTimeMillis - start

			    //println("%1d".format(totalTime))
				//val now = Calendar.getInstance()
				//val elapsed = now.get(Calendar.SECOND) - then.get(Calendar.SECOND)
				//println(s"Sentence analyzed in: ${elapsed}")
				val idString:String = s"s${zwi._2}"
				val analysis:String = s"""<li class="greekSentence" id="${idString}">${analysisString}</li>"""
				analysis
			})

			if (logOnly){ 
				println( anaVec.mkString("\n") )
			} else {
				val pw = new PrintWriter(new File(outputHtmlFile))
				pw.write(htmlHeader)
				pw.append( anaVec.mkString("\n\n") )
				pw.append( htmlFooter )
				pw.close
				println(s"\n Success! The results will be in:\n\n ${outputHtmlFile}.\n")
			}
		}
	} catch {
		case e:Exception => printError(s"${e}")
	}
}






