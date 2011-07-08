# ----- Opani -----

OPANI_URL <- 'https://opani.com'
OPANI_HOME <- NULL
OPANI_TOKEN <- NULL
opani.local <- NULL
o.figure.name <- NULL
opani.username <- ''
opani.cachepath <- NULL
values <- NULL
if (!exists('opani.appuser')) opani.appuser <- NULL
if (!exists('opani.appname')) opani.appname <- NULL
if (!exists('odir')) odir <- NULL
if (!exists('uid')) uid <- NULL
if (!exists('tid')) tid <- NULL
if (!exists('wid')) wid <- NULL

.First.lib <- function(libname, pkgname) {
    OPANI_HOME    <<- file.path(Sys.getenv('HOME'), '.opani')
    OPANI_TOKEN   <<- file.path(OPANI_HOME, 'token')
    o.figure.name <<- paste("fig",toString(floor(stats::runif(1, 0, 1e+09))),'png',sep='.')
}

cachepath.init <- function() {
    if(exists('odir') && !is.null(odir)) {
        opani.cachepath <<- paste(odir,'cache',opani.appuser,opani.appname,sep='/')
    } else {
        opani.cachepath <<- paste(getwd(),'cache','me','opani',sep='/')
    }
}

login <- function(token) {
    uid <<- token
    opani.local <<- TRUE
    if (!file.exists(OPANI_HOME)) dir.create(OPANI_HOME, recursive=TRUE)
    writeLines(c(token), OPANI_TOKEN)
}

get.username <- function() {
    if (nchar(opani.username) == 0) {
        opani.username <<- get.opani('/username/')
    }
    opani.username
}

is.opani <- function() {
    exists('uid') && !is.null(uid)
}

is.worker <- function() {
    is.null(opani.local)
}

login_required <- function() {
    if (!logged_in()) {
        if (file.exists(OPANI_TOKEN)) { 
            login(readLines(OPANI_TOKEN, warn=FALSE))
        } else {
            if (interactive()) {
                login(readline("Please enter your login token (https://opani.com/account/): "))
            } else {
                stop('Please login(TOKEN) to Opani first. Find your TOKEN at https://opani.com/account/')
            }
        }
    }
}

get.opani <- function(path, check=TRUE, extra.args='') {
    login_required()
    result <- system(paste('curl --silent -f -H "Authorization: Basic ', uid, '" ', extra.args, ' "', paste(OPANI_URL, path, sep="/"), '"', sep=""), intern=TRUE)
    if (!length(result) && (check)) {
        stop(paste("Could not get", path))
    }
    result
}
put.opani <- function(path, filename) {
    system(paste('curl --silent -X PUT -H "Authorization: Basic ', uid, '" -H "Content-Type: text/plain" --data-binary  "@', filename, '" "', paste(OPANI_URL, path, sep="/"), '"', sep=""), intern=TRUE)
}
logged_in <- is.opani


# ----- Map-Reduce -----

emit <- function(value) {
    if (is.worker()) {
        result <- system(paste('curl --silent -H "Authorization: Basic ', uid, '" -F value="', value, '" "', OPANI_URL, '/task/', tid, '/add/"', sep=""), intern=TRUE)
        if (length(result) > 0) stop(result)
    } else {
        values <<- c(values, value)
    }
}

get.values <- function(app=NULL) {
    login_required()
    app <- get.app(app)
    strsplit(get.opani(paste(app, '/keys/?keep=yes', sep="")), '\n', fixed=TRUE)
}

# ----- Caching -----

cache.exists <- function(filename) {
  if (is.null(opani.cachepath)) cachepath.init()
  if( file.exists(paste(opani.cachepath,filename,sep='/')) ) {
    return(TRUE)
  } else {
    if( ! file.exists(opani.cachepath) ) {
      dir.create(opani.cachepath, recursive = TRUE)
    }
    return(FALSE)
  }
}

cache <- function(filename, content.fun) {
    if (is.null(opani.cachepath)) cachepath.init()
    # 1. try memcache first
    #content <- redisGet(filename)
    #if(!is.null(content)) {
    #  #print(paste('redis',filename))
    #  return(content)
    #} else {
    if (TRUE) {
      # 2. try file cache second
      cachepath.filename <- paste(opani.cachepath,filename,sep='/')
      if(cache.exists(filename)) {
        # print('file')
        load(file=cachepath.filename)
        #try(redisSet(filename, content))
        return(content)
      } else {
        # 3. save to file + memcache
        content <- content.fun()
        try(save(content, file=cachepath.filename))
        #try(redisSet(filename, content))
        return(content)
      }
   }
}

# ----- App -----

get.app <- function(app=NULL) {
    if(is.null(app)) {
        app <- paste(get.username(), basename(getwd()), sep="/")
        #if (!file.exists('./run.r')) {
        #    stop(paste('No Opani app found. No run.r in ', getwd(), sep=""))
        #}
    } else {
        if (length(unlist(strsplit(app,'/'))) != 2) {
            app <- paste(get.username(), app, sep="/")
        }
    }
    app
}

load.app <- function(app=NULL) {
    login_required()
    app <- get.app(app)
    params <- unlist(strsplit(app, "/"))
    username <- params[1]
    appname <- params[2]
    dirname <- appname
    new <- FALSE
    if (basename(getwd()) == dirname) {
        cat("You're already in the app directory. Use load.files()")
    } else {
        if (!file.exists(dirname)) {
            new <- TRUE
            cat('Creating directory', dirname, '\n')
            dir.create(dirname, showWarnings=FALSE)
        }
        setwd(dirname)
        cat('Changed working directory to', dirname, '\n')
    }
    #if (!file.exists('run.r')) {
    #    writeLines(c(""), 'run.r')
    #}
    if (new) load.files()
    cat("next: run.app.local(c(1,2,3,4))\n")
}


run.app <- function(keys=NULL, app=NULL, cluster=NULL) {
    login_required()
    app <- get.app(app)
    params <- unlist(strsplit(app, "/"))
    username <- params[1]
    appname <- params[2]


    if (is.null(cluster)) {
        url <- paste(OPANI_URL, 'cluster/submit', sep="/")
    } else {
        url <- paste(OPANI_URL, 'cluster', cluster, 'submit', sep="/")
    }

    if (length(keys) == 0) {
        keys <- ''
    } else {
        keys <- paste(keys, collapse="\n") 
    }
    jobid <- system(paste('curl --silent -f -H "Authorization: Basic ', uid, '" -F"name=R_client" -F"username=', username, '" -F"app=', appname, '" -F"keys=', keys, '" -F"engine=r" "', url, '"', sep=""), intern=TRUE)
    cat('View the output at ', paste(OPANI_URL, app, sep='/'), '\n')
    jobid
}

run.app.local <- function(keys=NULL) {
    show <- function(p=NULL) { if( ! is.null(p) ) print(p) }
    o.image <- show
    show.image <- show
    wid <<- 0
    values <<- NULL
    wd <- getwd()
    if( file.exists('install.r') ) source('install.r', local=TRUE)
    if( file.exists('init.r') )    source('init.r', local=TRUE)

    dir <- paste(wd, 'data', sep='/')
    if( ! file.exists(dir) )       dir.create(dir)
    
    setwd(dir)
    for (key in keys) {
        cat('\n####', key, '- run.r', '####\n')
        try(source('../run.r', local=TRUE))
    }
    setwd(wd)
    
    if (file.exists('reduce.r')) {
      cat('\n#### (reduce) ####\n')
      try(source('reduce.r', local=TRUE))
    }
    cat('next: save.files(); run.app(c(1,2,3,4,...))\n')
}

stop.app <- function(jobid=NULL, app=NULL)
{
    login_required()
    kill.all  <- TRUE
    app       <- get.app(app)
    params    <- unlist(strsplit(app, "/"))
    username  <- params[1]
    appname   <- params[2]
    parameter <- ''
    if (kill.all) parameter <- '?killall=yes'
    if (is.null(jobid)) {
        get.opani(paste(username,'/',appname,'/kill/',parameter,sep=''), check=FALSE)
    } else {
        get.opani(paste(username,'/',appname,'/',jobid,'/kill/',parameter,sep=''), check=FALSE)
    }
}

create.app <- function(app=NULL, question, private=NULL) {
    login_required()
    app <- get.app(app)
    params <- unlist(strsplit(app, "/"))
    username <- params[1]
    name <- params[2]
    if (is.null(private)) { private <- '' } else { private <- '-Fprivate=on' }
    tryCatch({
        get.opani('/r/new/', check=TRUE, extra.args=paste(private, ' -Fname="', name, '" -Fshort_description="', question, '"', sep=""))
        print('App created.') 
    }, 
        error = function () {stop('App exists.')}
    )
}

# ----- Results -----

get.result <- function(jobid=NULL, app=NULL, format='text') {
    login_required()
    app <- get.app(app)
    if (!is.null(jobid)) {
        url <- paste(app, jobid, paste('joblog/?format=', format, sep=""), sep='/')
    } else {
        url <- paste(app, paste('joblog/?format=', format, sep=""), sep='/')
    }
    get.opani(url)
}

# ----- Files -----

get.filenames <- function(app=NULL) {
    login_required()
    app <- get.app(app)
    get.opani(paste(app, 'file/list/', sep='/'))
}

get.file <- function(filename, app=NULL) {
    login_required()
    app <- get.app(app)
    get.opani(paste(paste(app, 'file/raw/?filename=', sep='/'), filename, sep=""))
}
load.file <- function(filename=NULL, app=NULL) {
    login_required()
    app <- get.app(app)
    if (!is.null(filename)) {
        filenames <- c(filename)
    } else {
        filenames <- get.filenames(app)
    }
    for (filename in filenames) {
        cat('Loading', filename, '\n')
        fdir <- dirname(filename)
        if (!file.exists(fdir)) {
            dir.create(fdir, showWarnings=FALSE, recursive=TRUE)
        }
        if (!file.exists(filename) || readline(prompt=paste(filename, 'exists, overwite? (y/N) ')) == "y") {
            writeLines(get.file(filename, app), filename)
        }
    }
}
load.files <- function(app=NULL) {load.file(NULL, app)}

save.file <- function(filename=NULL, app=NULL) {
    login_required()
    if (is.null(filename)) {
        files <- list.files('.')
        files <- append(files[grep(glob2rx("*.r"), files)], files[grep("README", files)])
        files <- append(files, get.filenames())
        sapply(unique(files), function(f) { save.file(filename=f) })
    } else {
        app <- get.app(app)
        cat('Saving',filename,'->',app,'\n')
        put.opani(paste(paste(app, 'file/add/?filename=', sep='/'), filename, sep=""), filename)
    }
    cat('')
}
save.files <- function(app=NULL) {save.file(NULL, app)}

# ----- Amazon S3 -----

put.s3.file <- function(s3url, filename) {
    system(paste('curl --silent --retry 5 -X PUT -H "Content-Type: " --data-binary "@', filename, '" "', s3url, '"', sep=""), intern=TRUE)
}

get.s3.file <- function(s3url, filename) {
    system(paste('curl --silent --retry 5 "', s3url, '" >"', filename, '"', sep=""), intern=TRUE)
}

load.s3 <- function(filename) {
    login_required()
    s3url <- get.opani(paste('/s3sign/?method=GET&key=', filename, sep=""))
    get.s3.file(s3url, filename)
}

save.s3 <- function(filename) { 
    login_required()
    s3url <- get.opani(paste('/s3sign/?method=PUT&key=', filename, sep=""))
    put.s3.file(s3url, filename)
    s3url <- get.opani(paste('/s3sign/?method=GET&key=', filename, sep=""))
    cat('<strong>Download:</strong> <a href="', s3url, '">', filename, '</a>')
}

save.s3.video <- function(filename) {
    login_required()
    s3url <- get.opani(paste('/s3sign/?method=PUT&key=', filename, sep=""))
    put.s3.file(s3url, filename)
    s3url <- get.opani(paste('/s3sign/?method=GET&key=', filename, sep=""))
    cat('<video width="640" height="360" controls autoplay><source src="', s3url, '" type="video/mp4" /><object width="640" height="360" type="application/x-shockwave-flash" data="http://opani-public.s3.amazonaws.com/player.swf"><param name="movie" value="http://opani-public.s3.amazonaws.com/player.swf" /> <param name="flashvars" value="controlbar=over&amp;file=', s3url, '" /></object></video>')
}

# ----- Rackspace CloudFiles -----

get.cloudfile <- function(filename='') {
    login_required()
    rstokens <- get.opani('/rackspace-token/')
    if(length(rstokens) > 0) {
        rstokens   <- strsplit(rstokens, '\n')
        rstoken    <- rstokens[[1]]
        storageurl <- rstokens[[2]]
        container  <- rstokens[[3]]
        url        <- paste(storageurl, container, filename, sep="/")
        cmd <- paste('curl --silent --retry 5 -H "Expect:" -H "X-Auth-Token: ', rstoken, '" "', url, '"', sep='')
        system(cmd, intern=TRUE)
    }
}

load.cloudfile <- function(filename) {
  login_required()
  if (is.null(opani.cachepath)) cachepath.init()
  if( ! cache.exists(filename) ) {
    rstokens <- get.opani('/rackspace-token/')
    if(length(rstokens) > 0) {
        rstokens   <- strsplit(rstokens, '\n')
        rstoken    <- rstokens[[1]]
        storageurl <- rstokens[[2]]
        container  <- rstokens[[3]]
        url        <- paste(storageurl, container, filename, sep="/")
        cmd <- paste('curl --silent --retry 5 -H "Expect:" -H "X-Auth-Token: ', rstoken, '" "', url, '"', sep='')
        if(length(filename)) {
            cmd <- paste(cmd, ' -o "', filename, '"', sep='')
        }
        cat(paste('Downloading cloud file: ',filename,'...\n',sep=''))
        
        wd <- getwd()
        setwd(opani.cachepath)
        system(cmd, intern=TRUE)
        setwd(wd)
    }
  }
}

save.cloudfile <- function(filename) {
   login_required()
    rstokens <- get.opani('/rackspace-token/')
    if(length(rstokens) > 0) {
        rstokens   <- strsplit(rstokens, '\n')
        rstoken    <- rstokens[[1]]
        storageurl <- rstokens[[2]]
        container  <- rstokens[[3]]
        url        <- paste(storageurl, container, filename, sep="/")
        cmd <- paste('curl -f --silent --retry 5 -D - -X PUT -T "', filename, '" "X-Auth-Token: ', rstoken, '" "', url, '"', sep='')
        system(cmd, intern=TRUE)

    cat('<strong>Download:</strong> <a href="', url, '">', filename, '</a>')
    }
}
