#!/usr/bin/perl -w
#
# File: popclean.pl
# Author: Jan Klaverstijn <jan@klaverstijn.nl>
# Date: 19JUL2001
#
# Purpose:
# Clear messages on an ISP's POP3 server that:
#        1. Are older than a specified number of hours.
#        2. Have been previously downloaded by fetchmail.
#
# File Name:      $Workfile:   popclean.pl $
# CVS File:       $Source: /var/cvsmaster/popclean/popclean.pl,v $
# Author:         $Author: jan $
# Date:           $Date: 2004/08/07 22:09:27 $
# Revision:       $Revision: 1.28 $
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#
# Comments:
# Multiple POP3 accounts can be handled. Usernames and passwords are
# obtained from the fetchmail configuration.
# Messages are checked against the fetmailids file.
# This is to avoid old messages that have not been downloaded from being
# deleted.

# Packages
use Mail::POP3Client;
use Mail::Header;
use strict;
use Time::localtime;
use HTTP::Date;
use Sys::Syslog qw(:DEFAULT setlogsock);  # default set, plus setlogsock
use Getopt::Long;
use Pod::Usage;

use Mail::IMAPClient;

# Globals
my (@idlines,
    $pop, $imap,
    $username, $mailserver, $password,
    %usertable,
    $from, $subject, $date, $uidl, $size,
    $age, $delflag, $rdate
   );
my $HOME = $ENV{'HOME'};
my $idfilename;
my $opt_idfilename;
my $rc_idfilename;
my $rcfilename = $HOME . "/.fetchmailrc";
my $maxage = 48;
my $maxsize = 0;
my $opt_timeout;
my $opt_port;
my $user;
my $pwd;
my $server;
my $debug;
my $version;
my $help;
my $usage;
my $verbose;
my $usesyslog;
my $test;
my $force;
my $skipuidl;
my $opt_ssl;
my $opt_amode = 'BEST';
my $total_delcount = 0;
my $total_kept = 0;
my $size_kept = 0;
my $size_deleted = 0;
my $maxattempts = 3;
my $port;
my $protocol='pop3';
my $timeout;
my $delspam = 0;
my $spamtag;
my ($too_old, $too_large, $label_age, $label_size, $isspam, $label_subject);

my $VERSION = sprintf "%d.%02d", q$Revision: 1.28 $ =~ m/ (\d+) \. (\d+) /xg;

my @OLD_ARGV = @ARGV;    # Getopt::Long tends to clear @ARGV
Getopt::Long::Configure ("bundling");
GetOptions(
   'maxage|a=i'      => \$maxage,
   'limit|l=i'       => \$maxsize,
   'help|h'          => \$help,
   'usage|?'         => \$usage,
   'version|V'       => \$version,
   'debug|d'         => \$debug,
   'fetchmailrc|f=s' => \$rcfilename,
   'verbose|v'       => \$verbose,
   'syslog|S'        => \$usesyslog,
   'timeout|T=i'     => \$opt_timeout,
   'test-mode|test|t'=> \$test,
   'skipuidl|k'      => \$skipuidl,
   'uidlfile|i=s'    => \$opt_idfilename,
   'force|F'         => \$force,
   'user|u=s'        => \$user,
   'password|p=s'    => \$pwd,
   'port|P=i'        => \$opt_port,
   'ssl'             => \$opt_ssl,
   'authmode|A=s'    => \$opt_amode,
   'attempts=i'      => \$maxattempts,
   'protocol=s'      => \$protocol,
   'server|s=s'      => \$server,
   'spamtag|g=s'       => \$spamtag
) or pod2usage(-verbose => 0, -message => "Invalid option. Use popclean --help for more information", -exitval => 64);
@ARGV = @OLD_ARGV;

if ($usage) {
   pod2usage(-verbose => 0, -message => "Use popclean --help for more information", -exitval => 64);
}

if ($help) {
   pod2usage(-verbose => 2, -message => " ", -exitval => 64);
};

# Switches
$debug = ($debug ? 1 : 0);
$verbose = ($verbose ? 1 : 0);
$usesyslog = ($usesyslog ? 1 : 0);
$test = ($test ? 1 : 0);
$force = ($force ? 1 : 0);
$skipuidl = ($skipuidl ? 1 : 0);
$opt_ssl = ($opt_ssl ? 1 : 0);
$delspam = ($spamtag ? 1 : 0);

$verbose=1 if $debug;

if ($version) {
   &printversion;
   exit 0;
}

if ($usesyslog) {
   openlog('popclean', 'cons,pid', 'mail');
   setlogsock('unix');
}

&logmsg('info', "Popclean version $VERSION started.");
if ($maxage > 0) {
   &logmsg('info', "Messages older than $maxage hours will be deleted.");
}

if ($maxsize > 0) {
   &logmsg('info', "Messages larger than $maxsize KB will be deleted.");
   $maxsize = $maxsize * 1024;  # bytes -> KB
}

if ($delspam) {
   &logmsg('info', "Spam will be deleted (tag=" . $spamtag . ").");
   $spamtag = quotemeta($spamtag);
}
else {
   if ($maxage + $maxsize == 0) {
      &logmsg('info', "Useless combination of --limit=0 and --maxage=0. Quiting.");
      exit 1;
   }
}

# Read fetchmail file with UIDL's.
if (! $skipuidl) {
   if (defined $opt_idfilename) {
      $idfilename=$opt_idfilename;
   }
   elsif (defined $rc_idfilename) {
      $idfilename = $rc_idfilename;
   }
   else {
      $idfilename = "$HOME/.fetchids";
   }

   &ReadIdFile;
}
else {
   &logmsg ('info', 'Not checking for .fetchids');
}

if ($user) {
   if (! $server) {
      &logmsg('crit', 'Missing server name.');
      exit 1;
   }
   if (! $pwd) {
      $pwd = &promptUser("Enter password for $user");
   }

   $timeout = ($opt_timeout ? $opt_timeout : 60);
   if ($opt_ssl) {
      $port = ($opt_port ? $opt_port : 995);
   }
   else {
      $port = ($opt_port ? $opt_port : 110);
   }

   &Pop3Maint ($server, $user, $pwd, $protocol);
   &Finish;
   exit 0;

}

# Traversing the configuration tree
my @rv = &parse_config_file($rcfilename);
foreach my $p (@rv) {

   if ($p->{proto} !~ /pop/i) {
      # Skip non-POP servers for now
      &logmsg('info', 'Skipping server ' . $p->{poll} . ': unsupported protocol ' . $p->{proto});
      next;   # Jump to next server.
   }

   if (defined $opt_timeout) {
      $timeout=$opt_timeout; # Take timeout value from command line
   }
   elsif (defined $p->{timeout}) {
      $port = $p->{timeout}; # Take timeout value from fetchmailrc if present
   }
   else {
      $timeout=60; # default
   }

   if (defined $opt_port) {
      $port = $opt_port; # Take port number from command line
   }
   elsif (defined $p->{port}) {
      $port = $p->{port}; # Take port number from fetchmailrc if present
   }
   else {
      $port=110; # default
   }

   foreach my $u (@{$p->{'users'}}) {
      # Now do it
      &Pop3Maint ($p->{poll}, $u->{user}, $u->{pass}, $p->{proto});
   }
}

&Finish;

#
# End of main program
#

                        ###############
                        # SUBROUTINES #
                        ###############

sub printversion {
   # Print the revision number. This is based upon the automatic CVS revision tag.
   &logmsg ("info", "This is popclean version $VERSION\n");
}

sub Finish {
   $size_kept=$size_kept/1024;
   $size_deleted=$size_deleted/1024;
   &logmsg ('info', "Popclean finished. $total_delcount deleted (" .
                     sprintf("%.0f", $size_deleted) . " KB), $total_kept kept (" .
		     sprintf("%.0f", $size_kept) . " KB)");
   if ($usesyslog) {
      closelog();
   }
}

sub Pop3Maint {
   ($mailserver, $username, $password, $protocol) = @_;

   my($time, $search, $index,
      $delcount, $kept, $i,
      );

   my($now) = ctime();
   my($tnow) = str2time($now);

   &logmsg ('info', "Processing account $username at $mailserver with protocol $protocol");
   if ($debug) {
      # Only echo password if debugging.
      &logmsg('info', "Signing on $username with password $password to server $mailserver:$port");
   }
   elsif ($verbose) {
      &logmsg('info', "Signing on $username to server $mailserver:$port");
   }

   # my($pop) = Mail::POP3Client->new($username, $password, $mailserver);
   #* constructor
   #* new Mail::POP3Client( USER => user,
   #*                       PASSWORD => pass,
   #*                       HOST => host,
   #*                       AUTH_MODE => [BEST|APOP|CRAM-MD5|PASS],
   #*                       TIMEOUT => 30,
   #*                       LOCALADDR => 'xxx.xxx.xxx.xxx[:xx]',
   #*                       DEBUG => 1 );

   if ($protocol eq "imap") {
      &Connect2IMAP($maxattempts) or return;
      exit 1;
   }
   else {
      &Connect2POP($maxattempts) or return;
      &logmsg ('info', "Using POP3Client.pm version ". $pop->Version());
   $delcount=0;
   # For every message parse the headers.
   for ($i = 1; $i <= $pop->Count; $i++) {
      ($index, $uidl) = split ' ', $pop->Uidl($i);  # Get UIDL
      ($index, $size) = split('\s+', $pop->List($i)); # Get message size
      my @hdarray=$pop->Head($i);
      my $header = new Mail::Header \@hdarray;
      # The headers still contain the newline at the end. Chomp them.
      chomp ($from=$header->get("From"));
      chomp ($subject=($header->get("Subject")?$header->get("Subject"):'<no subject>'));  # Subject could be empty/undefined.

      # Determine if we should delete is as spam.
      $isspam = 0;
      $label_subject="";
      if ($delspam) {
         if ($subject =~ /$spamtag/) {
            $isspam = 1;
            $label_subject=' (!)';
         }
      }

      # Take date from Date header. It's not our first choice,
      # but it may be needed for later fallback.
      chomp ($date=$header->get("Date"));
      # Sometimes the date has PM instead of a timezone
      # like  Mon, 25 Jun 2001 05:56:26 PM
      $date =~ s/PM//;      # Chop off PM

      # Take date from first Received header. According to
      # RFC822 the reveived date is always after the last semi-colon
      my $rcount = $header->count("Received", 0);
      if ($rcount > 0) {
	 $_ = $header->get("Received", 0); # Take the lates one.
	 /.*;/;
	 $rdate = $';  # The date is everything after the last semi-colon in the string.
	 chomp $rdate;
      }
      else {
         $rdate="";
      }

      # Determine the date we will use to calculate age.
      if ($rdate gt "") {
         $date = $rdate;
      }
      else {
         &logmsg ('warning', "Problem with date in Received header. Reverting to Date header ($date)");
      }
      # Strip off trailing timezone, eg. (GMT). Often this contains invalid information 
      # like a DST suffix that makes HTTP:Date choke.
      $_=$date;
      $date=$1 if /\s(.+?)(?:\([^)]+\))?$/;
      $date =~ s/^ |\t|\n//;  # Trim leading whitespace.
      $time = str2time($date);

      if (!defined($time)) {
         &logmsg ('warning', "str2time: cannot handle \"$date\"");
         if ($force) {
            # Force deletion of messages with weird dates.
            $time=0;
            &logmsg ('warning', "Deletion forced");
         }
         else {
            $time=999999999999;
         }
      }
      # Calculate age of message in hours.
      $age = ($tnow - $time)/3600;
      $delflag="N";
      $search = $username . "@" . $mailserver . " " . $uidl;

      if ($maxage == 0 or $age <= $maxage) {
         $too_old = 0;
	 $label_age=' ';
      }
      else {
         $too_old = 1;
	 $label_age=' (!)';
      }

      if ($maxsize == 0 or $size <= $maxsize) {
         $too_large = 0;
	 $label_size=' '; 
      }
      else {
         $too_large = 1;
	 $label_size=' (!)'; 
      } 

      &logmsg('info', "    Id: $search") if $verbose;
      &logmsg('info', "    Age: " . sprintf("%.1f", $age) . " hours" . $label_age)  if $verbose;
      &logmsg('info', "    Size: " . sprintf("%.0f", $size) . " bytes"  . $label_size) if $verbose;
      &logmsg('info', "    Subject: $subject" . $label_subject) if $verbose;
      if ($too_old || $too_large || $isspam) {
         # Check with fetchids file if message has already been seen.
         # Compose fetchids record: "user@server uidl"
         if ($skipuidl or &SearchIdFile($search) > 0) {
            # We can delete the message.
            $delflag="Y";
            $delcount++;
            $pop->Delete($index) if !$test;
            &logmsg('info', "Deleted message from $from ($date)") if $verbose;
            $size_deleted+=$size;
            }
      }
      if ($delflag eq "N") {
         &logmsg('info', "Kept message from $from ($date)") if $verbose;
         $size_kept+=$size;
      }
   } # End of loop over all messages

   $kept=$pop->Count-$delcount;
   $total_kept+=$kept;
   $total_delcount+=$delcount;
   &logmsg ('info', "${username} at $mailserver: $delcount deleted, $kept kept");

   # Closing the connection also commits the deletions.
   $pop->Close();
}

sub parse_config_file {
   #
   # parse_config_file(file, [&global])
   # Parses a fetchmail config file into a list of hashes, each representing
   # one mail server to poll
   # This code is based upon the code for the fetchmail Webmin module
   #

   my $lnum = 0;
   my ($line, @rv, @toks);

   # Tokenize the file
   if (open(FILE, $_[0])) {
      logmsg('info', "reading configuration info from $rcfilename") if $verbose;
      while($line = <FILE>) {
      $line =~ s/\r|\n//g;
         $line =~ s/^\s*#.*$//;
         while($line =~ /^[\s:;,]*"([^"]*)"(.*)$/ ||
               $line =~ /^[\s:;,]*'([^"]*)'(.*)$/ ||
               $line =~ /^[\s:;,]*([^\s:;,]+)(.*)$/) {
            push(@toks, [ $1, $lnum ]);
            $line = $2;
         }
         $lnum++;
      }
      close(FILE);
   }
   else {
      &logmsg ('crit', "Error opening fetchmail configuration file $rcfilename");
      exit 1;
   }

   # Split into poll sections
   @toks = grep { $_->[0] !~ /^(and|with|has|wants|options|here)$/i } @toks;
   my ($poll, $user, $i);
   for($i=0; $i<@toks; $i++) {
      my $t = $toks[$i];
      
      # Global options
      if ($t->[0] eq 'idfile') {
         $rc_idfilename = $toks[++$i]->[0];
      }
      # Server options
      elsif ($t->[0] eq 'poll' || $t->[0] eq 'server' ||
         $t->[0] eq 'skip' || $t->[0] eq 'defaults') {
         # Start of a new poll
         $poll = { 'line' => $t->[1],
              'file' => $_[0],
              'index' => scalar(@rv),
              'skip' => ($t->[0] eq 'skip'),
              'defaults' => ($t->[0] eq 'defaults') };
         $poll->{'poll'} = $toks[++$i]->[0] if (!$poll->{'defaults'});
         undef($user);
         push(@rv, $poll);
      }
      elsif ($t->[0] eq 'proto' || $t->[0] eq 'protocol') {
         $poll->{'proto'} = $toks[++$i]->[0];
      }
      elsif ($t->[0] eq 'port') {
         $poll->{'port'} = $toks[++$i]->[0];
      }
      elsif ($t->[0] eq 'timeout') {
         $poll->{'timeout'} = $toks[++$i]->[0];
      }
      elsif ($t->[0] eq 'interface') {
         $poll->{'interface'} = $toks[++$i]->[0];
      }
      elsif ($t->[0] eq 'idfile') {
         $poll->{'idfile'} = $toks[++$i]->[0];
      }

      # User options
      elsif ($t->[0] eq 'user' || $t->[0] eq 'username') {
         $user = { 'user' => $toks[++$i]->[0] };
         push(@{$poll->{'users'}}, $user);
      }
      elsif ($t->[0] eq 'pass' || $t->[0] eq 'password') {
         $user->{'pass'} = $toks[++$i]->[0];
      }
      elsif ($t->[0] eq 'ssl') {
         $user->{'ssl'} = $toks[++$i]->[0];
      }

      else {
         # Found an irrelevant option!
      }

      if ($poll) {
         if ($i<@toks) {
            $poll->{'eline'} = $toks[$i]->[1];
         }
         else {
            $poll->{'eline'} = $toks[$#toks]->[1];
         }
      }
   }

   return @rv;
} # End of sub parse_config_file()

sub SearchIdFile {
# Search the fetchids file for the UIDL
   my $search = shift;
   # Counter
   my($lineNo) = 0;
   my($line) = "";

   # Indicator
   my($found) = 0;

   foreach $line (@idlines) {
      # increment line counter
      $lineNo++;
      # does this line contain the UIDL?
      my $safesearch = quotemeta($search);  # UIDL may contain special chars.
      if ($line =~ /$safesearch/) {
      $found = $lineNo;
      last;
     }
   }
   return $found;
}

sub Connect2IMAP {
   # Connect to the IMAP server. If needed, retry a specified number
   # of times.
   my $folder = "Antivirus";
   my $attempts = shift;
   my $account = "$username\@$mailserver:$port";
   &logmsg ("info", "Connecting to IMAP using $account");
   while ($attempts--) {
      $imap = Mail::IMAPClient->new(
                      Server   => $mailserver,
                      User     => $username,
                      Password => $password,     
                      Timeout  => $timeout,
                      Debug    => $debug
      )       or die $@;
      # Process success here ...
      $imap->select($folder);
      
      my $right_now = time;
      #subtract the age difference in seconds
      my $cutoff_date = $right_now - ($maxage * 3600);
      #convert to RFC2060 format
      my $Rfc2060_date = $imap->Rfc2060_date($cutoff_date);
      &logmsg ("info", "Cut-off date: $Rfc2060_date");
      # Get number of messages in folder.
      my $msgcount = $imap->message_count($folder);
      &logmsg('info', 'There is a total of ' . sprintf("%.0f", $msgcount) . ' messages in folder ' . $folder . '.');
      #fetch all messages before that time
      my @message_list = $imap->search("before",$Rfc2060_date) or
                   warn "No messages found before $Rfc2060_date.\n";
      #how many messages are in the list
      my $number_of_old_messages = @message_list;
      &logmsg('info', 'There is a total of ' . sprintf("%.0f", $number_of_old_messages) . ' messages too old.');
      #pack this list to the trash
      my $counter;
      for($counter = 0; $counter < $number_of_old_messages;$counter++) {
         my $msg_id = $message_list[$counter];
         my $ref = $imap->parse_headers($msg_id,"Reply-to","From", "Subject", "Received", "To");
         if ($verbose) {
            &logmsg ("info", 'ID:       ' . "$msg_id");
            &logmsg ('info', 'From:     ' . $ref->{'From'}[0]);
            &logmsg ('info', 'Subject:  ' . $ref->{'Subject'}[0]);
            &logmsg ('info', 'Received: ' . $ref->{'Received'}[0]);
         }
      }
      return 1;  # Success
   }
   continue {
      if ($attempts > 0) {
         &logmsg('info', "Pausing between retries for $account") if $verbose;
         sleep (1);
      }
   }
   &logmsg('crit', "Giving up on $account after $maxattempts connection attempts.");
   return 0;  # Failed to connect. We shoud actually consider to give up on this server alltogether.
}

sub Connect2POP {
   # Connect to the POP server. If needed, retry a specified number
   # of times.
   my $attempts = shift;
   my $account = "$username\@$mailserver:$port";
   while ($attempts--) {
      if ($opt_ssl) {
         # This looks like it could be more optimal,
         # but USESSL is supported in POP3Client starting at 2.18.
         # This way non-ssl users can work with pre-2.18 versions.
         $pop = new Mail::POP3Client(USER     => $username,
                                     PASSWORD => $password,
                                     HOST     => $mailserver,
                                     TIMEOUT  => $timeout,
                                     PORT     => $port,
                                     USESSL   => $opt_ssl,
                                     DEBUG    => $debug);
      }
      else {
         $pop = new Mail::POP3Client(USER     => $username,
                                     PASSWORD => $password,
                                     HOST     => $mailserver,
                                     TIMEOUT  => $timeout,
                                     PORT     => $port,
                                     AUTH_MODE => $opt_amode,
                                     DEBUG    => $debug);
      }
      if ($pop->State() eq 'DEAD') {
         # State = DEAD means dead session; this could be retried.
         &logmsg ('crit', "Problem connecting to POP server $mailserver:$port. " .
                   "Server message: " . $pop->Message());
         next;  # Retry
      }
      elsif ($pop->State() eq 'AUTHORIZATION') {
         # State = AUTHORIZATION means no use retrying.
         &logmsg ('crit', "Authorization failed for $account. " .
   	       "Server message: " . $pop->Message());
         return 0;  # Failed
      }
      else {
         return 1;  # Success
      }
   }
   continue {
      if ($attempts > 0) {
         &logmsg('info', "Pausing between retries for $account") if $verbose;
         sleep (1);
      }
   }
   &logmsg('crit', "Giving up on $account after $maxattempts connection attempts.");
   return 0;  # Failed to connect. We shoud actually consider to give up on this server alltogether.
}


sub ReadIdFile {
# Read all lines from the file.
   if (open(FILE, $idfilename)) {
      &logmsg('info', "Reading UIDL info from $idfilename") if $verbose;
      @idlines = <FILE>;
      close(FILE);
   }
   else {
      &logmsg ('crit', "Error opening fetchids file $idfilename");
      exit 1;
   }
}

sub logmsg {
   my ($prio, $msg) = @_;
   if ($usesyslog) {
      syslog ($prio, $msg);
   }
   else {
      print STDERR "$msg\n";
   }
}

sub promptUser {

   #-------------------------------------------------------------------#
   #  two possible input arguments - $promptString, and $defaultValue  #
   #  make the input arguments local variables.                        #
   #-------------------------------------------------------------------#

   my ($promptString, $defaultValue) = @_;

   #-------------------------------------------------------------------#
   #  if there is a default value, use the first print statement; if   #
   #  no default is provided, print the second string.                 #
   #-------------------------------------------------------------------#

   if ($defaultValue) {
      print $promptString, "[", $defaultValue, "]: ";
   }
   else {
      print $promptString, ": ";
   }

   $| = 1;               # force a flush after our print
   $_ = <STDIN>;         # get the input from STDIN (presumably the keyboard)


   #------------------------------------------------------------------#
   # remove the newline character from the end of the input the user  #
   # gave us.                                                         #
   #------------------------------------------------------------------#

   chomp;

   #-----------------------------------------------------------------#
   #  if we had a $default value, and the user gave us input, then   #
   #  return the input; if we had a default, and they gave us no     #
   #  no input, return the $defaultValue.                            #
   #                                                                 #
   #  if we did not have a default value, then just return whatever  #
   #  the user gave us.  if they just hit the  key,           #
   #  the calling routine will have to deal with that.               #
   #-----------------------------------------------------------------#

   if ($defaultValue) {
      return $_ ? $_ : $defaultValue;    # return $_ if it has a value
   }
   else {
      return $_;
   }
}

# ----- POD Usage starts here ----------------------------------------

=head1 NAME

popclean - script to purge messages at a POP3 server older than a certain age, larger than a given size or tagged by a spamfilter.

=head1 SYNOPSIS

B<popclean> [options]


Options:

 -a, --maxage                      Set maximum age in hours before a message is purged
 -l, --limit                       Messages larger than this limit in KB will be deleted
 -g, --spamtag                     Messages with this text in the subject will be considered tagged as spam and will be deleted
 -f, --fetchmailrc=file            Specify an alternate name for the fetchmail run control file
 -i, --uidlfile=file               Specify an alternate name for the .fetchids file
 -k, --skipuidl                    Bypass verification of .fetchids file
 -u, --user                        Specify username
 -p, --password                    Specify password
 -s, --server                      Specify POP3 server name or IP address
 -t, --test-mode, --test           Operate as normal but do not actually purge messages
 -S, --syslog                      Write messages to syslog daemon
 -T, --timeout                     Set timeout in seconds for connecting to the POP server
 --attempts                        Number of attempts before giving up on connecting to a server
 -P, --port                        The POP server port number
 -A, --authmode                    Authentication method. Use BEST (default), APOP, CRAM-MD5 or PASS.
 -v, --verbose                     Write details about deleted messages
 -d, --debug                       Write detailed information while processing messages
 -F, --force                       Purge messages even with unrecognisable date
 -V, --version                     Print version
 -?, --usage                       Print usage message
 -h, --help                        Print documentation

=head1 OPTIONS

=over 4

=item B<-A>, B<--maxage> N

Any message older than N hours will be deleted. If --maxage=0 is specified
the age will be ignored. This is useful when deletion based upon size 
is intended. Default: 48

=item B<-l>, B<--limit> N

Any message larger than N Kbytes will be deleted. If limit=0 is 
specified the size will be ignored. This is the default behaviour.
If --limit and --maxage are combined they are OR'ed. This means a message 
needs to exceed either --limit or --maxage to be deleted.

=item B<-g>, B<--spamtag> <tag>

Specifies a string that, if present in the subject line, causes the message
to be deleted from the server. Spamfilters often add a special tag to the
subject of a message to indicate it is considered spam. If --spamtag and 
--maxage are combined they are OR'ed. This means a message needs to exceed
either --maxage or be considered spam to be deleted.

=item B<-V>, B<--version>

Displays the version information for your copy of popclean.
No further actions are performed.

=item B<-u>, B<--user> <username>

The B<-u> parameter will cause popclean to process a specific user
account.  This requires the server name or address to be specified
as well.

=item B<-s>, B<--server> <servername or address>

Specifies the host name or dotted IP address of the POP3 server.
This option is required if B<-u> is given.

=item B<-p>, B<--password> <password>

Specifies the cleartext password belonging to the user account
as specified by B<-u> and B<-s>.  If no password is specified but
the B<-u> and B<-s> options are present, the user is prompted to
specify one.

=item B<-P>, B<--port> <portnum>

Specifies the POP server port (default = 110). This value overrides
the value specified in the fetchmailrc file.

=item B<-A>, B<--authmode> <mode>

Authentication mode used to login to the POP server. If the mode is set
to BEST, and the server appears to support APOP, it will try APOP, if
that fails, then it will try SASL CRAM-MD5 if the server appears to
support it, and finally PASS. If the mode is set to APOP, and the server
appears to support APOP, it will use APOP or it will fail to log in.
Likewise, for CRAM-MD5, no PASS-fallback is made. Otherwise password is
sent in clear text.

=item B<-T>, B<--timeout> <seconds>

The timeout option (default = 60 seconds) allows you to set a
server-nonresponse timeout in seconds. After the timeout expires
the connection is considered dead. This value overrides the value
specified in the fetchmailrc file.

=item B<--attempts> <number>

Number of attempts (default=3) to connect to a server. Sometimes the
first attempt to connect to a server fails for unknown reasons, usually
network related. This option allows the control of the number of
retries. Authorization failures are never retried.

=item B<-f>, B<--fetchmailrc> <filename>

Specify the location of the fetchmail configuration file> By
default popclean will look for the file B<$HOME/.fetchmailrc>.
Many options and directives are ignored. The following options are 
actually used: server, username, password, port, timeout.
When account information is explicitly specified through the
--user option this file is ignored. See the README file for details.

=item B<-i>, B<--uidlfile> <filename>

Specify an alternate name for the .fetchids file used by fetchmail to
save POP3 UIDs.  By default the file B<$HOME/.fetchids> is read to
validate POP3 UIDs to make shure that fetchmail has fetched a message
before deciding to delete it. This prevents deletion of messages when
fetchmail has not run for a period longer than MAXAGE. This value
overrides the value specified in the fetchmailrc file.

=item B<-k>, B<--skipuidl>

Bypass validation of UIDs against the .fetchids file. Beware of the
danger of unwanted deletion if fetchmail has not run in a while. This
option is useful if fetchmail is not around (in this case also use
--user, --password and --server) or is not using the UIDL option for
some reason.

=item B<-t>, B<--test-mode>, B<--test>

Popclean will work normally but will not actually delete any messages.
This is especially useful for testing purposes.

=item B<-S>, B<--syslog>

This option allows you to redirect status and error messages emitted
to the syslog(3) system daemon if available. Messages are logged with
an id of popclean, the facility LOG_MAIL, and priorities LOG_ERR,
LOG_ALERT or LOG_INFO. This option is intended for logging status
and error messages which indicate the results while cleaning a POP
account. Error messages for command line options and parsing the
configuration file are still written to stderr.

=item B<-v>, B<--verbose>

Verbose mode. Information about processed messages is echoed to
the output.

=item B<-d>, B<--debug>

Debug mode. Implies verbose mode. Echoes extra diagnostic information
about all messages. In particular the conversation with the POP3
server echoed. Warning: This can cause lots of output when many
messages and/or many accounts are processed.

=item B<-F>, B<--force>

Forced deletion of messages. Sometimes the age of a message cannot
be determined due to malformed date headers. With this option these
messages are always deleted. Beware: this can lead to deletion of
recent messages.

=back

=head1 DESCRIPTION

Fetchmail downloads ("fetches") messages from a POP3 mailserver and
forwards them your local (client) machine's delivery system using SMTP.
It can either leave a copy of these messages on that server or delete
them entirely. Many users of fetchmail have requested the possibility to
postpone that deletion to a later date. Every single user has different
reasons for that request. As per the FAQ (see
http://catb.org/~esr/fetchmail/fetchmail-FAQ.html), item G5, fetchmail
is not meant to implement policy; hence the second-most-requested
feature for fetchmail is denied. Popclean comes to the rescue.

Popclean is a perl script that deletes messages from a POP3 server a
certain amount of time after they have been received by that server. It
does so by looking at the latest "Received:" header. It was designed to
work in conjunction with fetchmail. By default, account information is
obtained from the fetchmail configuration. Popclean will also look at
the .fetchids file to determine if a message was received by the local
host before deleting it from the POP server. This helps in avoiding
deletion of unread messages.

Additionally, popclean can delete messages that exceed a certain size
or contain a tag in the subject line that was inserted by a spamfilter.

Provided the correct set of
arguments was specified no Fetchmail information is needed. Many options
control the behaviour of popclean. Some of them actually make the
presence of fetchmail optional.

=head1 ENVIRONMENT

No environment variables, aside from those used by perl, are required
to be set.

=head1 SEE ALSO

fetchmail(1)

=head1 AUTHOR

Jan Klaverstijn E<lt>jan@klaverstijn.nlE<gt>

=head1 PREREQUISITES

=over 4

=item Mail::POP3Client

=item Mail::Header

=item HTTP::Date

=item Sys::Syslog

=back
