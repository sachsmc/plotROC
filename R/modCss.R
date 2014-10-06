#' Add custom font size, more customizations to come
#' 
#' @keywords Internal


modCss <- function(font.size){

cssstring <- '
<style type = "text/css">

.tess {
fill: blue;
stroke: blue;
stroke-width: 0px;
opacity: 0;
}

.line {
fill: none;
stroke: steelblue;
stroke-width: 1.5px;
}

.dot {
fill: none;
stroke-width: 0px;
opacity: 0;
fill-opacity: 1;
}

.vors :hover circle {
fill: red;
opacity: 1;
}

.vors :hover text {

opacity: 1;

}   

.hidetext {

font-size: %s;
stroke-opacity: 0;
fill-opacity: 1;
fill: black;
opacity: 0;
position: relative;

}
</style>
'

sprintf(cssstring, font.size)

}
