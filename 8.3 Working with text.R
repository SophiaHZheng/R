my.text<-"Over the last decade, bluetongue virus have spread northwards from the mediterranean area. Initially this was ascribed to climate changes, but it has since been realized that a major contributing factor has been new transmitting vectors, culicoides obsoletus and culicoides pulicaris, which have the ability to aquire and transmit the disease. Recently, schmallenberg virus has emerged in northern europe, transmitted by biting midges as well."
need_modified <- c('bluetongue','culicoides','europe','mediterranean','northern','schmallenberg')
modify <- c('Bluetongue','Culicoides','Europe','Mediterranean','Northern','Schmallenberg')

my.new.text <- my.text
for (i in 1:length(need_modified)){
  my.new.text <- gsub(need_modified[i], modify[i], my.new.text, ignore.case = FALSE)
}

my.new.text

#If don't construct a new text before working on that in the loop, 
#everytime in the loop you are always working on the original text!
#So the result will be only substituting the last element
