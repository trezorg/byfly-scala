coursera-scala-dl
==================

Scala script that helps to know Byfly balance. Just another :)

Installation
------------

    $ git clone https://github.com/trezorg/byfly-scala.git
    $ cd byfly-scala
    $ sbt run
    $ sbt assembly

It is also possible to make a self-executable Linux script by

    $ sbt deploy

It will create the executable file "byfly" in the project directory


Usage
-------

It is possible to set your login and password in a configuration file.
The configuration file can be set by the script parameter *--filename*.
It is also possible just to put the file into the user's home directory or into
a current directory. In this case the file should have the name byfly.conf.

File structure is pretty simple:

    login = some_username
    password = some_password

You can also set those parameters and other from the command line.
Check for details

    java -jar /path/to/cousera.jar --help

or

    $sbt
    >run --help


or

    $./byfly --help
