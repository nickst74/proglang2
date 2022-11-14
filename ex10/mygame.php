<?php
session_start();

if(!isset($_SESSION['counter'])) {
	$_SESSION['counter'] = 0;
}

if(!isset($_SESSION['start_time'])) {
	$_SESSION['start_time'] = new DateTime();
}

$questions = array("···· · ·–·· ·–·· –––  ·–– ––– ·–· ·–·· –··",
                    "··· – ·– –·–· –·–  ––– ···– · ·–· ··–· ·–·· ––– ·––  – –––  – ···· ·  ·–· · ··· –·–· ··– ·",
					"·–– ···· ·– –  –·· ––– · ···  – ···· ·  ··–· ––– –··–  ··· ·– –·––",
                    "·–– ···· · –·  ·– ––  ··  ––·– ··– · ··· – ·· ––– –·  –– ·– ·–· –·–  – ···· ·  ·––· ·–· · ··· · –· –  ––– ··–·  –·–· ––– ··– ·–· ··· ·  ··· – ––– ·––·  ··· ––– ·–· ·–· –·––  –·–· ––– –– –– ·–  – ·– ·–· –·· ·· ···  ··–· ··– –· –·–· – ·· ––– –· ·– ·–·· ·· – –·––  ·· ···  ··· – ·· ·–·· ·–··  ··– –· –·· · ·–· ––· ––– ·· –· ––·  –·· · ···– · ·–·· ––– ·––· · –– · –· –",
					"··– ·––·  ··– ·––·  –·· ––– ·–– –·  –·· ––– ·–– –·  ·–·· · ··–· –  ·–· ·· ––· ···· –  ·–·· · ··–· –  ·–· ·· ––· ···· –  ··· – ––– ·––·  –·–· ···· · ·– –  –– ––– –·· ·  ··– –· ·–·· ––– –·–· –·– · –··  –·–· ––– –– –– ·–  ··– –· ·–·· ·· –– ·· – · –··  ··–· ·–· · ·  ––· ––– ––– ––· ·–·· ·  ··· · ·– ·–· –·–· ···· · ···",
					"– ···· ·  ·– –· ··· ·–– · ·–·  – –––  ·–·· ·· ··–· ·  – ···· ·  ··– –· ·· ···– · ·–· ··· ·  ·– –· –··  · ···– · ·–· –·–– – ···· ·· –· ––·  ·––· ·–·· ··– ···  – ···· ·  –· ··– –– –··· · ·–·  ––– ··–·  ···· ––– ·–· –· ···  ––– –·  ·–  ··– –· ·· –·–· ––– ·–· –·  ·· ···  ····– ···––",
					"– ···· · ··· ·  ––· ––– ––– ––· ·–·· ·  · ·– ··· – · ·–·  · ––· ––· ···  ·– ·–· ·  – –––  –·· –·–– ·  ··–· ––– ·–·",
					"–– –·––  ––· ··– ·· ·–·· – –·––  ·––· ·–·· · ·– ··· ··– ·–· ·  ·· ···  ·–– ·– – –·–· ···· ·· –· ––·  ··–· ·– ·· ·–·· ·– ·–· –– –·––",
					"·–· ··– –· –· ·· –· ––·  ––– ··– –  ––– ··–·  ·· –·· · ·– ···  –·–· ––– –– –– ·–  ···· ––– ·––  ·– –··· ––– ··– –  ··· ––– –– ·  ·––· ··· –·–– –·–· ···· ––– ·–·· ––– ––· –·––  – ···· · ––– ·–· ·· · ···",
					"– ···· ·  ·––· ·–· ·· –· –·–· ·· ·––· ·–·· ·  ––– ··–·  –·–· ––– ––· –· ·· – ·· ···– ·  –·–· ––– –· ··· ·· ··· – · –· –·–· –·––  ··· ··– ––· ––· · ··· – ···  – ···· ·– –  ·–– ·  ···· ·– ···– ·  ·– –·  ·· –· –· · ·–·  –·· ·–· ·· ···– ·  – –––  ···· ––– ·–·· –··  ·– ·–·· ·–··  ––– ··– ·–·  ·– – – ·· – ··– –·· · ···  ·– –· –··  –··· · ···· ·– ···– ·· ––– ·–·  ·· –·  ···· ·– ·–· –– ––– –· –·––  ·– –· –··  ·– ···– ––– ·· –··  –·· ·· ··· ··· ––– –· ·– –· –·–· ·",
					"·–  ··· ·· –– ·––· ·–·· ·  · –··– ·– –– ·––· ·–·· ·  ––– ··–·  –·–· ––– ––· –· ·· – ·· ···– ·  –·· ·· ··· ··· ––– –· ·– –· –·–· ·  ·· ···  – ···· ·  –··· · ···· ·– ···– ·· ––– ··– ·–·  ––– ··–·  ··· –– ––– –·–· –·– ·· –· ––·  –··· ··– –  – ···· ·  ·– – – ·· – ··– –·· ·  – ···· ·– –  ··· –– ––– –·–· –·– ·· –· ––·  ·–·· · ·– –·· ···  – –––  –·–· ·– –· –·–· · ·–·",
					"··· ··– –·–· ····  –·–· ––– –· – ·–· ·– –·· ·· –·–· – ·· ––– –· ···  –·–· ·– –·  ·–·· · ·– –··  – –––  ··–· · · ·–·· ·· –· ––· ···  ––– ··–·  –·· ·· ··· –·–· ––– –– ··–· ––– ·–· –  – ···· ·– –  – ···· ·  ·· –· –·· ·· ···– ·· –·· ··– ·– ·–··  – · –· –·· ···  – –––  –– ·· –· ·· –– ·· ––·· ·  –··· –·––  ·– –·· –·· ·· –· ––·  –·–· ––– –– –– ·–  –– ––– –·· ·· ··–· –·–– ·· –· ––·  ––– ·–·  ·–· · –·· ··– –·–· ·· –· ––·  – ···· ·  ·· –– ·––· ––– ·–· – ·– –· –·–· ·  ––– ··–·  – ···· · ··· ·  –··· · ·–·· ·· · ··–· ···",
					"– ···· ·– –  ·· ···  ·– ·–·· ·–··  ··–· ––– ·–·· –·– ···",
					"·– –· –··  –· ––– ·––  ·–– ·  ·–· · – ··– ·–· –·  – –––  ––– ··– ·–·  ··· ·–– · · –  –·–· ––– –– ·––· ·· ·–·· · ·–·  ·––· ·–· ––– ·––– · –·–· –");

$answers = array("hello world", 
                	"stack overflow to the rescue",
					"what does the fox say",
                	"when am i question mark the present of course stop sorry comma tardis functionality is still undergoing developement",
					"up up down down left right left right stop cheat mode unlocked comma unlimited free google searches",
					"the answer to life the universe and everything plus the number of horns on a unicorn is 43",
					"these google easter eggs are to dye for",
					"my guilty pleasure is watching failarmy",
					"running out of ideas comma how about some psychology theories",
					"the principle of cognitive consistency suggests that we have an inner drive to hold all our attitudes and behavior in harmony and avoid dissonance",
					"a simple example of cognitive dissonance is the behaviour of smocking but the attitude that smocking leads to cancer",
					"such contradictions can lead to feelings of discomfort that the individual tends to minimize by adding comma modifying or reducing the importance of these beliefs",
					"that is all folks",
					"and now we return to our sweet compiler project");

?>


<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Yet another simple code game!</title>
<style type="text/css">

body,td,th {
	font-family: Verdana, Arial, Helvetica, sans-serif;
	font-size: x-large;
	color: #CCCCCC;
}

body {
  	background-color: #333399;
}

.title {
	font-family: "Courier New", Courier, monospace;
	font-weight: bold;
	font-size: 48px;
	color: #00FF66;
}

.question {color: #FFCC33}
.number {color: #FFFF33}
.md5sum {color: #FFCCFF}
.emph {color: #99ee99}
.alert {color: #ee77aa}

.right {
	color: #33FF66;
	font-weight: bold;
}
.wrong {
	color: #FF3366;
	font-weight: bold;
}

a:link {
  	color: #CCFFFF;
}

a:visited {
  	color: #CCFFFF;
}

input {
	background-color: #eeee66;
	color: #333399;
}

code {
	font-family: monospace;
	display: block;
	background-color: #66eeee;
	color: #993333;
	border: 1px solid black;
	padding: 8px;
	width: 95%;
	margin-bottom: 2em;
}

textarea.wide {
	font-family: monospace;
	font-size: x-large;
	color: #333333;
	border: 1px solid black;
	padding: 8px;
	width: 95%;
}
</style>
</head>
<body>
    <h1>Yet another simple code game!</h1>
    <p><span class="question">Question <?php echo $_SESSION['counter']+1; ?></span>:</p>
    <code><?php echo $questions[$_SESSION['counter']] ?></code>
    
    <?php
		if(!isset($_POST['submit'])) {
			echo "<span class=\"question\">Make it quick, the clock is ticking...</span>";
			echo "<p></p>";
			echo "<form action=\"/mygame.php\" id=\"f\" name=\"f\" method=\"post\">
					<textarea class=\"wide\" name=\"answer\" id=\"answer\"></textarea>
					<br>
					<input type=\"submit\" name=\"submit\" id=\"submit\" value=\"Submit!\">
					<input type=\"reset\" value=\"Reset\">
				</form>";
		} else {
			if(trim(strtolower($_POST['answer'])) == $answers[$_SESSION['counter']]) {
				echo "<p class=\"right\">Right!  :-)</p>";
				echo "<hr>";
				if($_SESSION['counter'] < count($questions)-1) {
					echo "<form action=\"/mygame.php\" id=\"r\" name=\"r\" method=\"post\">
							<input type=\"hidden\" id=\"continue\" name=\"continue\" value=\"continue\">
							<input type=\"submit\" name=\"again\" id=\"again\" value=\"Continue!\">
							</form>";
					$_SESSION['counter']++;
				} else {
					$end_time = new DateTime();
					$elapsed = $end_time->getTimestamp() - $_SESSION['start_time']->getTimestamp();
					echo "<p>It took you " . $elapsed . " seconds.</p>";
					echo "<form action=\"/mygame.php\" id=\"r\" name=\"r\" method=\"post\">
							<input type=\"hidden\" id=\"reset\" name=\"reset\" value=\"reset\">
							<input type=\"submit\" name=\"again\" id=\"again\" value=\"Play again!\">
							</form>";
					unset($_SESSION['counter']);
					unset($_SESSION['start_time']);
				}
			} else {
				echo "<p class=\"wrong\">Wrong!  :-(</p>";
				echo "<hr>";
				echo "<form action=\"/mygame.php\" id=\"r\" name=\"r\" method=\"post\">
						<input type=\"hidden\" id=\"continue\" name=\"continue\" value=\"continue\">
						<input type=\"submit\" name=\"again\" id=\"again\" value=\"Continue!\">
						</form>";
			}
			unset($_POST['submit']);
		}
    ?>
</body>
</html>