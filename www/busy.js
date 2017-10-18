setInterval(function(){
	

  if ($('html').attr('class')=='shiny-busy') {
    $('div.busy').show()
	$('div.notbusy').hide()
	
	
  } else {
    $('div.busy').hide()
	$('div.notbusy').show()
  }
	
},100)


