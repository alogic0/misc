#!/bin/bash


function slide-show-1 () {
cat << END
<!DOCTYPE html>
<html>
<head>
<style>
* {box-sizing:border-box}
body {font-family: Verdana,sans-serif;margin:0}
img {
    max-width: 100%;
    max-height: 100%;
}
#navbar {
  overflow: hidden;
}

#navbar a {
  float: left;
  display: block;
  color: white;
  background-color: #4CAF50;
  text-align: center;
  padding: 5px 5px;
  text-decoration: none;
  font-size: 22px;
}

#navbar a:hover {
  cursor: default;
  background-color: rgba(0,0,0,0.8);
  color: black;
}

#navbar a.next {
  float: right;
}

.content {
  padding: 16px;
}

.sticky {
  position: fixed;
  bottom: 80px;
  width: 100%
}


/* Caption text */
.text {
  color: grey;
  font-size: 15px;
  padding: 8px 12px;
  position: absolute;
  bottom: 8px;
  width: 100%;
  text-align: center;
}

/* Number text (1/3 etc) */
.numbertext {
  color: grey;
  font-size: 12px;
  padding: 8px 12px;
  position: absolute;
  top: 0;
}

</style>
</head>
<body>

<div id="navbar" class="sticky">
  <a onclick="plusSlides(-1)">&#10094;</a>
  <a class="next" onclick="plusSlides(1)">&#10095;</a>
</div>

<div class="content">
  <div class="numbertext"></div>
  <a href="" id="photoA" target="_blank">
    <img src="" id="photoImg">
  </a>
  <div class="text"><i id="demo"></i></div>
</div>
<script>
END
}

function slide-show-2 () {
cat << END
var slideIndex = 1;
showSlides(slideIndex);

function plusSlides(n) {
  showSlides(slideIndex += n);
}

function currentSlide(n) {
  showSlides(slideIndex = n);
}

function showSlides(n) {
  if (n > slides.length) {slideIndex = 1}    
  if (n < 1) {slideIndex = slides.length}
  var imgPath = escape(slides[slideIndex-1]);
  document.getElementById("photoA").href = imgPath;
  document.getElementById("photoImg").src = imgPath;
}

currentSlide(1);
</script>

</body>
</html>
END
}

slide-show-1;
echo -n 'var slides = '
find "$@" \( -iname '*.jpg' -o -iname '*.png' -o -iname '*.jpeg' \) -printf '%p\n' \
  | sort \
  | while read i;
      do echo -n \"$i\"\,;
      done \
  | sed 's/^/\[/;s/\,$/\]/'
echo
slide-show-2;
