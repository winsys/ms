# MySQL-Front Dump 2.5
#
# Host: localhost   Database: MS
# --------------------------------------------------------
# Server version 4.0.3-beta-nt


#
# Table structure for table 'AddTitles'
#

DROP TABLE IF EXISTS AddTitles;
CREATE TABLE AddTitles (
  PYEAR smallint(6) NOT NULL default '0',
  PMONTH tinyint(4) NOT NULL default '0',
  PDAY tinyint(4) NOT NULL default '0',
  TITLE varchar(70) NOT NULL default '',
  AUTHOR varchar(70) NOT NULL default '',
  ETITLE varchar(70) NOT NULL default ''
) TYPE=MyISAM;



#
# Table structure for table 'Data'
#

DROP TABLE IF EXISTS Data;
CREATE TABLE Data (
  ID int(11) NOT NULL auto_increment,
  PYEAR smallint(6) NOT NULL default '0',
  PMONTH tinyint(4) NOT NULL default '0',
  PDAY tinyint(4) NOT NULL default '0',
  PTIME tinyint(4) default NULL,
  ISBIBLE tinyint(4) NOT NULL default '0',
  ISSPECIAL tinyint(4) NOT NULL default '0',
  HASBOOKMARK tinyint(4) NOT NULL default '0',
  TITLEID smallint(6) NOT NULL default '0',
  CNUM smallint(6) default NULL,
  CHAPTER text,
  PRIMARY KEY  (ID)
) TYPE=MyISAM;



#
# Table structure for table 'Notes'
#

DROP TABLE IF EXISTS Notes;
CREATE TABLE Notes (
  ID int(11) NOT NULL auto_increment,
  TITLEID smallint(6) NOT NULL default '0',
  CNUM smallint(6) default NULL,
  COMMENT varchar(100) default NULL,
  PRIMARY KEY  (ID)
) TYPE=MyISAM;



#
# Table structure for table 'Titles'
#

DROP TABLE IF EXISTS Titles;
CREATE TABLE Titles (
  ID int(11) NOT NULL auto_increment,
  PYEAR smallint(6) NOT NULL default '0',
  PMONTH tinyint(4) NOT NULL default '0',
  PDAY tinyint(4) NOT NULL default '0',
  PTIME tinyint(4) default NULL,
  PLENGTH smallint(6) NOT NULL default '0',
  ISBIBLE tinyint(4) NOT NULL default '0',
  ISSPECIAL tinyint(4) NOT NULL default '0',
  HASBOOKMARK tinyint(4) NOT NULL default '0',
  TITLE varchar(70) binary NOT NULL default '',
  PLACE varchar(70) binary NOT NULL default '',
  ETITLE varchar(70) NOT NULL default '',
  PRIMARY KEY  (ID)
) TYPE=MyISAM;

