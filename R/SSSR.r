#!/usr/bin/Rscript;

#NOTE:
#RSQLite package must be pre-installed before installing SSSR package
#or calling SSSR.r as source file.

#Public Variables
#db_Connection<-NULL;
#assign("db_Connection", NULL, envir = globalenv() )
#db_Driver<-NULL;
assign("db_Driver", NULL, envir = globalenv() )
#defaultSessId<-"";
assign("defaultSessId", "", envir = globalenv() )
#documentStarted<-FALSE;
assign("documentStarted", FALSE, envir = globalenv() )
#std_in<-"";
assign("std_in", "", envir = globalenv() )

#This function initializes SQLite driver,
#creates a connection link to database under /tmp directory and
#creates tables for session and application data
#Maximum number of concurrent connections is 128
dbutil.openConnection<-function(){
	if (!exists("db_Connection", envir=globalenv())){
		require("RSQLite",quietly=TRUE);
		driver<-sqliteInitDriver(max.con=128);
		assign("db_Driver", driver, envir = globalenv())
		platform<-.Platform; sepr<-platform$file.sep;
		conn<-sqliteNewConnection(drv=driver,dbname=paste(dirname(tempdir()),sepr,"rsessions_1_0_3",sep=""));
		assign("db_Connection", conn, envir = globalenv())
		res<-sqliteQuickSQL (conn,"create table if not exists rsessions (id integer primary key asc, sessionid varchar(20), parameter varchar(255), value clob)");
		res<-sqliteQuickSQL (conn,"create table if not exists rapplication (id integer primary key asc, parameter varchar(255), value clob)");
	}
}

#R's own serialize() and unserialize() functions work with arrays but
#we cannot read and write them from/to database tables as they are. dbutil.serialize() function
#serializes given object and returns a character array.
dbutil.serialize<-function(obj){
	serializedObject<-rawToChar(serialize(obj,NULL,ascii=TRUE));
	return(serializedObject);
}

#This function returns an R object for a given serialized R object.
dbutil.unSerialize<-function(serializedObject){
	obj<-unserialize(charToRaw(serializedObject));
	return(obj);
}

#Determines the content type of an R page. Default value is plain html
response.setContentType<-function(contentType="text/html"){
	response.print(paste("Content-type: ",contentType,"\n",sep=""));
}

#response.print function takes unknown amount of parameters and sends them to clients 
#depending on their types or classes
response.print<-function(...){
	argList<-list(...);
	for (myArg in argList){
		if (typeof(myArg)=="list" || class(myArg)=="matrix"){
			print(myArg);
		}else{		
			cat(myArg);
		}
	}	
}

#A wrapper function for response.print
response.println<-function(...){
	response.print(...);
	response.print("\n");
}

#Sends redirecting directive to client.
#Must be called before response.startDocument()
response.sendRedirect<-function(aUrl){
	response.print(paste("Refresh: 0; url=",aUrl,"\n",sep=""));
}


#Extracts value of a given parameter from a Query String
request.getParameter<-function(aQueryString,paramName,delimiter="&"){
	retValue<-"";
	allVars<-unlist(strsplit(aQueryString,delimiter));
	for (myVars in allVars){
		myParamStr<-unlist(strsplit(myVars,"="));
		if (strwrap(myParamStr[1])==paramName){
			retValue<-myParamStr[2];
			break;
		}
	}
	return(retValue);
}

#Extracts values of a given parameter from a web form defined with GET method
request.getGETParameter<-function(paramName){
	retValue<-"";
	queryString<-Sys.getenv("QUERY_STRING");
	retValue<-request.getParameter(queryString,paramName);
	return(URLdecode(retValue));
}

#Extracts values of a given parameter from a web form defined with POST method
#All values in STDIN device are saved in std_in variable.
request.getPOSTParameter<-function(paramName){
	retValue<-"";
	streamLen<-Sys.getenv("CONTENT_LENGTH");
	if (streamLen<1) return("");
	if (nchar( get("std_in", envir=globalenv()))<1){
		myFile<-file("stdin");
		content<-readChar(con=myFile, nchars=streamLen);
		close(myFile);
		assign("std_in", content, envir = globalenv())
	}
	retValue<-request.getParameter( get("std_in", envir=globalenv()), paramName);
	return(URLdecode(retValue));
}

#Extracts value of a given cookie variable
request.getCookie<-function(paramName){
	retValue<-"";
	cookiesText<-Sys.getenv("HTTP_COOKIE");
	retValue<-request.getParameter(cookiesText,paramName,delimiter=";");
	return(retValue);
}

#Sends user friendly error messages
response.sendError<-function(errStr){
	response.println("<table bgcolor=RED>","<tr>","<td>",errStr,"</td>","</tr>","</table>");
}

#Creates or changes cookies values 
response.setCookie<-function(cookieName,cookieValue){
	if (exists("documentStarted", envir = globalenv())) {
		response.sendError("You can not set values of cookies after headers sent");
	}
	response.print(paste("Set-Cookie: ",cookieName,"=",cookieValue,";\n",sep=""));
}

#Sends \n character to finalize document's header part
response.startDocument<-function(){
	response.print("\n");
	assign("documentStarted", TRUE, envir = globalenv())
}

#Creates a random string using A-Z letters length of 20.
#This random string determines the client for session functions.
#Function searches session table for preventing the twice usage of same session id.
session.getRandomSessionID<-function(){
	aPass<-""
	for (i in 1:20){
		myUTF<-intToUtf8(as.integer(runif(1,65,86)))
		aPass<-paste(aPass,myUTF,sep="")
	}
	conn<-get("db_Connection", envir = globalenv())
	res<-sqliteQuickSQL (conn ,paste("select * from rsessions where sessionid='",aPass,"'",sep=""))
	if (dim(res)[1]>0){
		aPass<-session.getRandomSessionID()
	}
	return(aPass);
}

#This function opens database connection, if it is closed.
#Catches session id, if it is defined; 
#otherwise generates new random session id and registers it.
session.start<-function(){
	dbutil.openConnection();
	sid<-request.getCookie("RSESSID");
	assign("defaultSessId", sid, envir = globalenv())
	if (nchar(get("defaultSessId", envir = globalenv()))<1) {
		sid<-session.getRandomSessionID()
		assign ("defaultSessId", sid, envir = globalenv())
		response.setCookie("RSESSID",sid)
	}
}

#Deletes all of the session data for current client.
session.delete<-function(){
	sessid<-session.getSessionId();
	sql<-paste("delete from rsessions where sessionid='",sessid,"'",sep="");
	res<-sqliteQuickSQL(get("db_Connection", envir=globalenv()) ,sql);
}

#Deletes given session value
session.deleteParameter<-function(paramName){
        sessid<-session.getSessionId();
        res<-sqliteQuickSQL ( get("db_Connection", envir=globalenv()) , paste("delete from rsessions where sessionid='",sessid,"' and parameter='",paramName,"'",sep=""));
}

#Updates and creates a session parameter.
session.setParameter<-function(paramName,paramValue){
	sessid<-session.getSessionId();
	oldValue<-session.getParameter(paramName);
	myVal<-dbutil.serialize(paramValue);
	if (oldValue==""){
		sql<-paste("insert into rsessions (sessionid, parameter, value) values ('",sessid,"','",paramName,"','",myVal,"')",sep="");
	}else{
		sql<-paste("update rsessions set value='",myVal,"' where sessionid='",sessid,"' and parameter='",paramName,"'",sep="");
	}
	res<-sqliteQuickSQL (get("db_Connection", envir=globalenv()), sql);
}

#Returns value of a given session variable
session.getParameter<-function(paramName){
	sessid<-session.getSessionId();
	res<-sqliteQuickSQL (get("db_Connection", envir=globalenv()), paste("select value from rsessions where sessionid='",sessid,"' and parameter='",paramName,"'",sep=""));
	if (is.na(res[1,1])){
		return("");
	}else{
		return(dbutil.unSerialize(res[1,1]));
	}
}

#Returns a list of all session parameters for current client.
session.getAllParameters<-function(){
	dbutil.openConnection();
	sessid<-session.getSessionId();
	sql<-paste("select * from rsessions where sessionid='",sessid,"'",sep="");
	res<-sqliteQuickSQL(get("db_Connection", envir=globalenv()),sql);
	return(res);
}

#Returns session id of current client.
session.getSessionId<-function(){
	sessid<-request.getCookie("RSESSID");
	if (nchar(sessid)<1) sessid<-get("defaultSessId", envir=globalenv());
	return(sessid);
}

#Application parameters are visible for all online clients.
#This function creates and changes value of a public variable.
application.setParameter<-function(paramName, paramValue){
	dbutil.openConnection();
	oldValue<-application.getParameter(paramName);
	myVal<-dbutil.serialize(paramValue);
	if (oldValue==""){
		sql<-paste("insert into rapplication (parameter,value) values ('",paramName,"','",myVal,"')",sep="");
	}else{
		sql<-paste("update rapplication set value='",myVal,"' where parameter='",paramName,"'",sep="");
	}
	res<-sqliteQuickSQL(get("db_Connection", envir=globalenv()),sql);
}

#Returns value of a public variable.
application.getParameter<-function(paramName){
	dbutil.openConnection();
	sql<-paste("select value from rapplication where parameter='",paramName,"'",sep="");
	res<-sqliteQuickSQL(get("db_Connection", envir=globalenv()),sql);
	if (is.na(res[1,1])){
		return("");
	}else{
		return(dbutil.unSerialize(res[1,1]));
	}
}

#Returns a list of all public variables.
application.getAllParameters<-function(){
	dbutil.openConnection();
	sql<-"select * from rapplication";
	res<-sqliteQuickSQL(get("db_Connection", envir=globalenv()),sql);
	return(res);
}

#This method delete all of the public variables.
#When web server restarted, this function can be called manually 
#to restart application.
application.delete<-function(){
	dbutil.openConnection();
	sql<-"delete from rapplication";
	res<-sqliteQuickSQL(get("db_Connection", envir=globalenv()),sql);
}









