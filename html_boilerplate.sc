val htmlHeader:String = """

<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<meta http-equiv="X-UA-Compatible" content="ie=edge">
	<title>EZ-Morph Greek Reading Report</title>
	<link rel="stylesheet" title="styles-g" href="styles-g.css">
	<link rel="stylesheet alternat" title="styles-blank" href="styles-blank.css">
</head>

<body>

<script>
	function switch_style ( css_title )
	{
	  var i, link_tag ;
	  for (i = 0, link_tag = document.getElementsByTagName("link") ;
	    i < link_tag.length ; i++ ) {
		    if ((link_tag[i].rel.indexOf( "stylesheet" ) != -1) &&
			    link_tag[i].title) {
			      link_tag[i].disabled = true ;
			      if (link_tag[i].title == css_title) {
				        link_tag[i].disabled = false ;
			    }
		    }
		}
	}

</script>

<h1> </h1>

<form id="styleSwitcher">
<input type="submit" onclick="switch_style('styles-g');return false;" name="styles-g" value="Color-code" id="styles-g">
<input type="submit" onclick="switch_style('styles-blank');return false;" name="styles-blank" value="Plain" id="styles-black">
</form>


<ol class="analyzedSentences">

"""

val htmlFooter:String = """

</ol>
</body>
</html>
"""

