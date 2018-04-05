fileToSample<-function(filename,perc=0.1){
        set.seed(2)
        #Load libraries
        library(readr)        
        
        #Calculate the total size of the file
        total_rows<-as.numeric(system(paste0("wc -l<",filename),intern = TRUE))
        #Calculate the chunk size required
        chunk_size<-max(floor(total_rows*perc/10),1)
        #Create a vector to hold the data
        data_read<-c()
        #create a probability vector
        prob<-rbinom(ceiling(total_rows/chunk_size),1,perc)
        index<-which(prob==1)
        #Run a loop to read a sample of the file.
        for(i in index){
                data_read<-c(data_read,read_lines(filename,n_max = chunk_size,skip=chunk_size*(i-1)))
        }
        
        #Clean the dataset
        data_read<-gsub("[0-9[:punct:]]","",data_read)#Remove numbers and punctiaons
        data_read<-gsub(paste(prof_data$V1,collapse="|"),"",data_read)#remove profanity words
        data_read<-gsub("[^a-zA-Z0-9[:punct:][:space:]]","",data_read)#Remove foreign words
        data_read<-gsub("[[:space:]]{2,}"," ",data_read)#Remove extra whitespaces
        
        #Return the final dataset
        return(data_read)
}