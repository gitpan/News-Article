# -*- Perl -*-
###########################################################################
# Written and maintained by Andrew Gierth <andrew@erlenstar.demon.co.uk>
# Thanks to Russ Allbery <rra@stanford.edu> for comment and significant
# contributions.
#
# Copyright 1997 Andrew Gierth. Redistribution terms at end of file.
#
# $Id: Article.pm 1.13 1997/12/27 23:19:07 andrew Exp $
#
# TODO:
#   - better way of handling the system-dependent configuration
#   - reformat for 80 columns :-)
#
###########################################################################
#
# Envelope, n. The coffin of a document; the scabbard of a bill; the husk
#              of a remittance; the bed-gown of a love-letter.
#                                                         -- Ambrose Bierce
#

=head1 NAME

News::Article - Object for handling Usenet articles in mail or news form.

=head1 SYNOPSIS

  use News::Article;

See below for functions available.

=head1 DESCRIPTION

An object for representing a Usenet article (or a mail
message). Primarily written for use with mail2news and/or moderation
programs.

=head1 USAGE

  use News::Article;

Article exports nothing.

A new Article object must be created with the I<new> method.

=cut

package News::Article;

use strict;
use English;

use FileHandle ();
use Net::Domain qw(hostfqdn);
use Net::NNTP ();
use IPC::Open3 qw(open3);
use PGP::Sign qw(pgp_sign pgp_verify pgp_error);

use vars qw($VERSION @SENDMAIL %SPECIAL %UNIQUE);
use subs qw(canonical fix_envelope source_init);

($VERSION = (split (' ', q$Revision: 1.13 $ ))[1]) =~ s/\.(\d)$/.0$1/;

###########################################################################
# System-dependent configuration
#
# How to mail an article. The code assumes that this is a
# sendmail-workalike; i.e. can accept envelope recipients as arguments
# or -t to parse the headers for recipients. Also uses -f to set the
# envelope sender (this may cause problems on pre-V8 sendmails if
# used by an untrusted user).

@SENDMAIL = ((grep { -x $_ } 
	      qw(/usr/sbin/sendmail /usr/lib/sendmail /bin/false))[0],
	     qw(-oi -oem));

# End of system-dependent configuration
###########################################################################
# Constant data
#
# Words to treat specially when canonifying header names

%SPECIAL = ('-'    => '-',    '_'    => '_',
	    'id'   => 'ID',   'pgp'  => 'PGP',  'nntp' => 'NNTP', 
	    'uidl' => 'UIDL', 'mime' => 'MIME', 'smtp' => 'SMTP');

# RFC1036 (and news generally) is much less tolerant of multiple
# fields than RFC822. 822 allows for multiple message-ids, which is
# arguably seriously broken, so we ignore that. We list here only the
# most significant news fields; handling the rest sensibly is up to
# the caller.

%UNIQUE = map { $_ => 1 } qw(date followup-to from message-id newsgroups 
			     path reply-to subject sender);

# Description of internal storage:
#
# $self->{Headers}
#
#   A hash of header names to values. The value stored 
#   is always a reference to an array of values. The value stored
#   always includes embedded newlines and whitespace, but not the
#   header name or leading whitespace after the colon. There is no
#   trailing newline on the value.
#
# $self->{RawHeaders}
#
#   Array of headers as read from external source. One header per
#   element, with embedded newlines preserved (but trailing ones
#   removed).
#
# $self->{Envelope}
#
#   Envelope From address. Set from a Unix-style "From " header on
#   read. When sending mail, the value here is used (unless undefined)
#   as the envelope sender.
#
# $self->{Body}
#
#   Array of text lines forming the body. Never contains embedded
#   newlines.
#

###########################################################################
# CONSTRUCTION
###########################################################################

=head2 Article Methods

=over 4

=item new ()

=item new ( SOURCE [,MAXSIZE [,MAXHEADS]] )

Use this to create a new Article object. Makes an empty article if no
parameters are specified, otherwise reads in an article from C<SOURCE>
as for C<read>.

=cut

sub new
{
    my $class = shift;
    my $self = { 
	Headers    => {},
	RawHeaders => [],
	Envelope   => undef,
	Sendmail   => [ @SENDMAIL ],
	Body       => [],
    };
    bless $self,$class;

    if (@_) 
    {
	return undef unless defined ($_[0]);
	$self->read(@_) or return undef; 
    }

    $self;
}

#--------------------------------------------------------------------------

=item clone ()

Create a new Article as an exact clone of the current one.
Returns a ref to the new object.

=cut

sub clone
{
    my $src = shift;
    my $class = ref($src);
    my $headers = {};
    my $obj = {
	Headers    => $headers,
	RawHeaders => [ @{$src->{RawHeaders}} ],
	Envelope   => $src->{Envelope},
	Sendmail   => [ @{$src->{Sendmail}} ],
	Body       => [ @{$src->{Body}} ],
    };

    for (keys %{$src->{Headers}})
    {
	$headers->{$_} = [ @{$src->{Headers}{$_}} ];
    }

    return bless $obj,$class;
}

###########################################################################
# HEADER MANIPULATION
###########################################################################

=item envelope ( [SENDER] )

If C<SENDER> is specified, sets the envelope sender to the specified
value (which will then subsequently be used if the article is mailed).
Returns the (new or current) envelope sender in any case.

=cut

sub envelope
{
    my $self = shift;
    return $self->{Envelope} = shift if (@_);
    $self->{Envelope};
}

#--------------------------------------------------------------------------

=item rawheaders ()

Returns a list (or a reference to an array if in scalar context) of
the original header lines of the article, as read from the input
source. Terminating newlines are not included. (Continued headers are
returned as single strings with embedded newlines.)

=cut

sub rawheaders
{
    my $self = shift;
    wantarray ? @{$self->{RawHeaders}} : $self->{RawHeaders};
}

#--------------------------------------------------------------------------

=item header_names ()

Returns a list of the names of all headers currently present
in the article.

=cut

sub header_names
{
    my $self = shift;
    keys %{$self->{Headers}};
}

#--------------------------------------------------------------------------

=item headers ([FIRST [,NEXT [,LAST]]])

Returns a list of all header strings with no terminating
newlines. Continued headers will have embedded newlines.

FIRST, NEXT and LAST are optional references to arrays of header
names. The order of the returned headers is as follows:

 - headers specified by FIRST (one value only per name)
 - headers in the order originally read in (if any)
 - headers specified by NEXT (one value only per name)
 - any remaining headers not named in LAST, sorted by name
 - headers named in LAST (all values)

=cut

sub headers
{
    my $self = shift;
    my $hdrs = $self->{Headers};
    my @preseq = map { canonical $_ } @{shift || $self->{HdrsFirst} || []};
    my @addseq = map { canonical $_ } @{shift || $self->{HdrsEnd} || []};
    my @postseq = map { canonical $_ } @{shift || $self->{HdrsLast} || []};
    my %postseq = map { ($_,1) } @postseq;
    my %tmph = map { my $h = canonical($_);
		     ($h, [ map { $h.": ".$_ } @{$hdrs->{$_}} ])
		     } keys %$hdrs;
    my @seq = map { /^([\w-]+):\s/ ? (canonical $1) : "X-Broken-Header" } @{$self->{RawHeaders}};
    @seq = grep { !$postseq{$_} } @seq;
    my $v;

    ((map { $v = $tmph{$_}; $v && @$v ? pop(@$v) : (); } @preseq),
     (map { $v = $tmph{$_}; $v && @$v ? pop(@$v) : (); } @seq),
     (map { $v = $tmph{$_}; $v && @$v ? pop(@$v) : (); } @addseq),
     (map { $v = $tmph{$_}; $v && @$v ? (@{$tmph{$_}}) : () }
          sort grep { !$postseq{$_} } keys %tmph),
     (map { $v = $tmph{$_}; $v && @$v ? (@{$tmph{$_}}) : () } @postseq));
}

#sub headers
#{
#    my $headers = $_[0]{Headers};
#    map {
#	my $header = canonical($_);
#	map { $header.": ".$_ } @{$headers->{$_}};
#    } keys %$headers;
#}

=item headers_first (HDR...)

Set default ordering for headers().

=cut

sub headers_first
{
    shift->{HdrsFirst} = [ @_ ];
}

=item headers_next (HDR...)

Set default ordering for headers().

=cut

sub headers_next
{
    shift->{HdrsEnd} = [ @_ ];
}

=item headers_last (HDR...)

Set default ordering for headers().

=cut

sub headers_last
{
    shift->{HdrsLast} = [ @_ ];
}

#--------------------------------------------------------------------------

=item set_headers ( NAME, VALUE [, NAME, VALUE [...]] )

For each header name supplied, replace any current occurences of the
header with the specified value(s). Each value may be a single scalar,
or a reference to an array of values. Returns undef without completing
the assignments if any attempt is made to supply multiple values for a
unique header. Undef or empty values cause the header to be deleted.
(If an array is supplied, it is not copied.)

=cut

sub set_headers
{
    my $self = shift;

    while (@_)
    {
	my $name = lc shift;
	my $val = shift;

	delete $self->{Headers}{$name} and next
	    if !defined($val) || (ref($val) && @$val < 1);

	$val = [ $val ] unless ref($val);

	return undef if $UNIQUE{$name} && @$val > 1;

	$self->{Headers}{$name} = $val;
    }

    1;
}

#--------------------------------------------------------------------------

=item add_headers ( NAME, VALUE [, NAME, VALUE [...]] )

Add new header values without affecting existing ones. Each value is
either a single scalar, or a reference to an array of values. Returns
undef without completing if any attempt is made to supply duplicate
values for a unique header. (If an array reference is supplied, the
array is copied.)

=cut

sub add_headers
{
    my $self = shift;

    while (@_)
    {
	my $name = lc shift;
	my $val = shift;
	next unless defined($val);

	$val = [ $val ] unless ref($val);
	my $curval = \@{$self->{Headers}{$name}};     # magic

	return undef if ($UNIQUE{$name} && (@$val + @$curval > 1));

	push @$curval,@$val;
    }
}

# explanation of 'magic': $curval gets a reference to an array which
# is also referred to by $self->{Headers}{$name} - *even* if there was
# no previous value for $self->{Headers}{$name} (if necessary, a new
# anon array springs into existence)

#--------------------------------------------------------------------------

=item drop_headers ( NAME [, NAME [...]] )

Delete all values of the specified header(s).

=cut

sub drop_headers
{
    my $self = shift;
    for (@_)
    {
	delete $self->{Headers}{lc $_};
    }
}

#--------------------------------------------------------------------------

=item header ( NAME )

Returns a list of values for the specified header. Returns a null list
if the header does not exist. In scalar context, returns the first
value found or undef.

=cut

sub header
{
    my $self = shift;
    my $name = lc shift;
    my $val = $self->{Headers}{$name};

    return defined($val) ? @$val : () if wantarray;
    return $val->[0];
}

#--------------------------------------------------------------------------

=item rename_header ( SRC, DEST [, ACTION] )

Transform the name of a header without touching the value. Fails
if the source header does not exist. Returns undef on failure,
true on success.

Optional ACTION (may be "drop", "clobber", "add", or "fail"
(default)), specifies what to do if both source and destination exist:

  ACTION     PREVIOUS DEST
  drop       unchanged      (SRC dropped)
  clobber    dropped        (SRC replaces DEST)
  add        preserved      (SRC added to DEST)
  fail       unchanged      (operation fails)

=cut

sub rename_header
{
    my $self = shift;
    my $oldname = lc shift;
    my $newname = lc shift;
    my $action = shift || 'fail';

    return undef unless exists($self->{Headers}{$oldname});

    if (exists($self->{Headers}{$newname}))
    {
	return undef if $action eq 'fail';
    }
    else
    {
	$action = 'clobber';
    }

    my $oldval = delete $self->{Headers}{$oldname};

    if    ($action eq 'clobber') { $self->{Headers}{$newname} = $oldval; }
    elsif ($action eq 'add')     { $self->add_headers($newname, $oldval); }

    1;
}

###########################################################################
# ARTICLE BODY
###########################################################################

=item body ()

Return the body of the article as a list of lines (no newlines),
or a reference to an array in scalar context (the array may be
modified in this case).

=cut

sub body
{
    wantarray ? @{$_[0]->{Body}} : $_[0]->{Body};
}

#--------------------------------------------------------------------------

=item lines ()

Returns the number of lines in the article body.

=cut

sub lines
{
    my $self = shift;
    scalar(@{$self->{Body}});
}

#--------------------------------------------------------------------------

=item bytes ()

Returns the total size of the article body, not counting newlines.

=cut

sub bytes
{
    my $self = shift;
    my $total = 0;
    for (@{$self->{Body}})
    {
	$total += length($_);
    }
    $total;
}

#--------------------------------------------------------------------------

=item set_body ( BODY )

Replace the current article body with the specified text.
Expects a list, each item of which is either one line,
or multiple lines separated by newlines (but with no final
newline).

=cut

sub set_body
{
    my $self = shift;
    $self->{Body} = [];
    $self->add_body(@_);
}

#--------------------------------------------------------------------------

=item add_body ( BODY )

Append the specified text to the current article body.
Expects a list, each item of which is either one line,
or multiple lines separated by newlines (but with no final
newline), or a reference to an array of lines.

=cut

sub add_body
{
    my $self = shift;
    my $body = $self->{Body};
    local($_);

    for (@_)
    {
	if (ref($_))
	{
	    $self->add_body(@$_);
	}
	else 
	{
	    my @lines = split(/\n/);
	    push @$body,@lines ? @lines : "";
	}
    }
}

###########################################################################
# INPUT FUNCTIONS
###########################################################################

=item read_headers ( SOURCE, MAXSIZE )

Read article headers (terminated by an empty line) from the specified
source (see C<read> for defintion of allowed sources).

Gives up (returning undef) if more than MAXSIZE bytes are read. Returns
the amount read.

=cut

sub read_headers
{
    my ($self, $source, $maxsz) = @_;
    my $last = undef;
    my $first = 1;
    my $hhead = {};
    my $name;
    my $val;
    my $size = 0;

    # Nuke the body and hashed headers - always.

    $self->{Body} = [];
    $self->{Headers} = $hhead;

    # If we have read some raw headers already, append a marker.  This
    # is partly to cope with C-news/ANU-news moderator mail, where the
    # news article is encapsulated in a mail message rather than
    # simply mailed, but we don't want to lose the mail path.

    my $head = $self->{RawHeaders};
    push @$head,"X-More-Headers: ----" if @$head;

    # Set up the data source.

    $source = source_init($source);
    return undef unless defined($source);

    local($_);

    my $line;

    while (defined($line = &$source()))
    {
	# size limit

	return undef if ($size += length($line)) > $maxsz;

	chomp $line;
	last if $line eq '';

	for (split(/\n/,$line))
	{
	    # lines of whitespace only are allowed in continuations - but
	    # we drop them as they serve no useful purpose 
	    next if /^\s*$/;

	    # Envelope From (unix-style). Must be the first line, and we trim
	    # off the timestamp if present

	    $self->{Envelope} = fix_envelope($POSTMATCH), next 
		if !$last && /^From /;

	    # Ignore bogus extra >From lines (procmail has a bad habit of adding
	    # these, unpredictably, unless you recompile it to trust everybody)
	    next if /^>From /;

	    # continuation line? If so, append to most recent data
	    if (/^\s/)
	    {
		if (ref($last))
		{
		    $head->[$#$head] .= "\n".$_;
		    $last->[$#$last] .= "\n".$_;
		}
		next;
	    }

	    # Extract header name and value. If the name looks
	    # unreasonable, hack around it to make the problem easily
	    # visible. We are deliberately over-strict in the allowed
	    # format of names (the RFCs allow any printable ASCII char
	    # other than whitespace or ':' in header names, but in
	    # practice only alphanumerics, '-' and (rarely) '_' are
	    # found). We lose any superfluous whitespace after the ':'
	    # here (only likely to be noticable for Subject lines).
	    
	    if (/^([\w-]+):\s+(.*)$/)
	    {
		$val = $2;
		$name = lc $1;
	    }
	    else
	    {
		$val = $_;
		$name = "x-broken-header";
	    }
	    
	    # Tack raw header onto array of raw headers
	    
	    push @$head,$_;
	    
	    # Add header to hash. Roughly equivalent to add_header, but
	    # handles duplicate unique headers silently

	    $last = \@{$hhead->{$name}};

	    if ($UNIQUE{$name})
	    {
		@$last = ( $val );
	    }
	    else
	    {
		push @$last,$val;
	    }
	}
    }

    $size;
}   

#--------------------------------------------------------------------------

=item read_body ( SOURCE, MAXSIZE )

Read an article body from the specified source (see C<read>). Stops at
end of file; fails (returning undef) if MAXSIZE is reached prior to
that point.  Returns the number of bytes read (may be 0 if the body is
null).

Useless trailing blank lines are removed.

=cut
	
sub read_body
{
    my ($self, $source, $maxsize) = @_;
    my $size = 0;

    # Set up the data source.

    $source = source_init($source);
    return undef unless defined($source);

    local($_);

    my $body = $self->{Body} = [];
    my $line;

    while (defined($line = &$source()))
    {
	return undef if ($size += length($line)) > $maxsize;
	chomp $line;
	push @$body,"" unless $line;

	for (split(/\n/,$line))
	{
	    push @$body,$_;
	}
    }

    # remove excess trailing blank lines

    while (@$body && $body->[$#$body] =~ /^\s*$/) { pop @$body; }

    # return the article size
    $size;
}

#--------------------------------------------------------------------------

=item read ( SOURCE [,MAXSIZE [,MAXHEADS]] )

Reads in an article from C<SOURCE>.

C<SOURCE> may be any of the following:

- a CODE ref, which is called to return lines or chunks of data

- an ARRAY ref, assumed to contain a list of lines with optional
line terminators

- a SCALAR ref, assumed to contain text with embedded newlines

- a scalar, assumed to be a filename, which is opened and read

- anything else is assumed to be a glob, reference to a glob,
or reference to a filehandle, and is read from accordingly

When reading in articles, C<MAXHEADS> is the maximum header size to
read (default 8k), and C<MAXSIZE> is the maximum article body size
(default 256k). If C<MAXSIZE> is explicitly specified as 0, then no
attempt at reading the body is made. Returns the total number of bytes
read, or undef if either limit is reached or no headers were found.

=cut

sub read
{
    my ($self, $source, $maxsize, $maxhead) = @_;
    my $hsize = 0;
    my $bsize = 0;

    $maxhead = 8192 unless $maxhead;
    $maxsize = 262144 unless defined($maxsize);

    # Set up the data source.

    $source = source_init($source);
    return undef unless defined($source);

    $hsize = $self->read_headers($source,$maxhead)
	or return undef;

    if ($maxsize)
    {
	$bsize = $self->read_body($source,$maxsize);
	return undef unless defined($bsize);
    }

    $hsize + $bsize;
}

###########################################################################
# OUTPUT FUNCTIONS
###########################################################################

=item write ( FILE )

Write the entire article to the specified filehandle reference.

=cut

sub write
{
    my ($self, $fh) = @_;
    print $fh join("\n", $self->headers(), "", @{$self->{Body}}, "");
}

#--------------------------------------------------------------------------

=item write_original ( FILE )

Write the original headers followed by the article body to the
specified filehandle reference.

=cut

sub write_original
{
    my ($self, $fh) = @_;
    print $fh join("\n", @{$self->{RawHeaders}}, "", @{$self->{Body}}, "");
}

###########################################################################
# MAIL FUNCTIONS
###########################################################################

=item sendmail ( [COMMAND] )

Get or set the command and options that will be used to mail the
article. Defaults to a system dependent value such as
  /usr/sbin/sendmail -oi -oem

=cut

sub sendmail
{
    my $self = shift;
    $self->{Sendmail} = [ @_ ] if (@_);
    @{$self->{Sendmail}};
}

#--------------------------------------------------------------------------

=item mail ( [RECIPIENTS...] )

Mails the article to the specified list of recipients, or to the
addressed recipients in the header (To, Cc, Bcc) if none are supplied.
Attempts to set the envelope sender to the stored envelope sender, if
set, so unset that before mailing if you do not want this behavior.

=cut

sub mail 
{
    my ($self, @recipients) = @_;
    my @command = @{$self->{Sendmail}};
    push @command,'-f',$self->{Envelope} if (defined($self->{Envelope}));
    push @command, @recipients ? @recipients : '-t';

    my $sendmail = new FileHandle;
    my $errors = new FileHandle;

    eval { open3 ($sendmail, $errors, $errors, @command) };
    if ($@) { return undef }

    local $SIG{PIPE} = 'IGNORE';

    $self->write($sendmail);
    close $sendmail;

    # Check the return status of sendmail to see if we were successful.
    $? == 0;
}

###########################################################################
# NEWS FUNCTIONS
###########################################################################

=item post ( [CONN] )

Post the article. Avoids inews due to undesirable header munging and
unwarranted complaints to stderr. Takes an optional parameter which is
a Net::NNTP reference.  If supplied, posts the article to it;
otherwise opens a new reader connection and posts to that.

Throws an exception containing the error message on failure.

=cut

sub post
{
    my $self = shift;
    my $server = shift;

    if (!$server)
    {
	$server = new Net::NNTP;
	die "Unable to connect to server" unless $server;
	$server->reader();
    }

    $server->post(join("\n", $self->headers(), "", @{$self->{Body}}))
	or die $server->code().' '.($server->message())[-1];

    1;
}

=item ihave ( [CONN] )

Inject the article. Takes an optional parameter which is a Net::NNTP
reference.  If supplied, posts the article to it; otherwise opens a
new transport connection and posts to that. All required headers must
already be present, including Path and Message-ID.

Throws an exception containing the error message on failure.

=cut

sub ihave
{
    my $self = shift;
    my $server = shift;

    my $msgid = $self->header('message-id');
    die "Article contains no message-id" unless $msgid;

    if (!$server)
    {
	$server = new Net::NNTP;
	die "Unable to connect to server" unless $server;
    }

    $server->ihave($msgid, join("\n", $self->headers(), "", @{$self->{Body}}))
	or die $server->code().' '.($server->message())[-1];

    1;
}

#--------------------------------------------------------------------------

=item add_message_id ( [PREFIX [, DOMAIN] ] )

If the current article lacks a message-id, then create one.

=cut

sub add_message_id
{
    my $self = shift;
    return undef if $self->{Headers}{'message-id'};

    my $prefix = shift || '';
    my $domain = shift || hostfqdn() || 'broken-configuration';
    my ($sec,$min,$hr,$mday,$mon,$year) = gmtime(time);
    ++$mon;
    $self->set_headers('message-id', 
		       sprintf('<%s%04d%02d%02d%02d%02d%02d$%04x@%s>',
			       $prefix, 
			       $year+1900, $mon, $mday, $hr, $min, $sec,
			       0xFFFF & (rand(32768) ^ $$), $domain));
}

#--------------------------------------------------------------------------

=item add_date ()

If the current article lacks a date, then add one (in local time).

=cut

sub add_date
{
    my $self = shift;
    return undef if $self->{Headers}{'date'};

    my $now = time;
    my ($sec,$min,$hr,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    my ($gsec,$gmin,$ghr,$gmday) = gmtime(time);

    use integer;
    $gmday = $mday + ($mday <=> $gmday) if (abs($mday-$gmday) > 1);
    my $tzdiff = 24*60*($mday-$gmday) + 60*($hr-$ghr) + ($min-$gmin);
    my $tz = sprintf("%+04.4d", $tzdiff + ($tzdiff/60*40));

    $mon = (qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))[$mon];
    $wday = (qw(Sun Mon Tue Wed Thu Fri Sat Sun))[$wday];
    $year += 1900;
    $self->set_headers('date',
		       sprintf("%s, %02d %s %d %02d:%02d:%02d $tz",
			       $wday,$mday,$mon,$year,$hr,$min,$sec,$tz));
}
   

###########################################################################
# AUTHENTICATION FUNCTIONS
###########################################################################

=item sign_pgpmoose ( GROUP, PASSPHRASE [, KEYID] )

Signs the article according to the PGPMoose spec.  We require that pgp be
on the path to do this.  Takes a "group" which can be either a newsgroup
or an address, a PGP password, and an optional key id and returns a null
list on success, the PGP error output as a list on failure.

If the key id is omitted, we will assume that if the group is an e-mail
address, the key id is that address surrounded by <>, and otherwise the
key id will be the group with a space on either side.  This is so that one
can help PGP distinguish between the keys for (say) mod.config and
mod.config.status.  The PGP key id should be something like:

  Moderator of group.name <request-address@some.host>

The article to be signed must already have all of the headers needed by
PGPMoose (Newsgroups, From, Subject) or this will fail. Message-ID is
added if necessary.

=cut

sub sign_pgpmoose 
{
    my ($self, $group, $passphrase, $keyid) = @_;

    # If we don't have a key id, try to generate one from the group.
    # Surround it by angle brackets if it's an e-mail address or by spaces
    # if it's a group.

    $keyid = ($group =~ /\@/) ? "<$group>" : " $group "
	unless (defined $keyid);

    # Check to make sure we have the required headers.
    for (qw(newsgroups from subject)) 
    {
        return ("Required header $_ missing")
	    unless $self->{Headers}{$_};
    }

    $self->add_message_id();

    # Now, put together all of the stuff we need to sign.  First, we need a
    # list of newsgroups, sorted.

    my $headers = $self->header('newsgroups');
    $headers =~ s/\s//g;
    $headers = join("\n", (sort split(/,+/, $headers)), '');

    # Next we need an array of headers: From, Subject, and Message-ID in
    # that order, killing initial and final whitespace and any spaces after
    # colons.
    for (qw(from subject message-id))
    {
        my $value = $self->header($_);
        $value =~ s/\n.*//;
        $value =~ s/^ +//;
        $value =~ s/\s+$//;
        $value =~ s/: +/:/g;
        $headers .= $value . "\n";
    }

    # Finally, we need to give it the body of the article, making the
    # following transformations:
    #
    #  - Lines consisting solely of spaces are deleted.
    #  - A leading "--" is replaced by "- --"
    #  - A leading "from" (case-insensitive) has > prepended.
    #  - A leading "subject" (case-insensitive) has > prepended.
    #  - A leading single "." is changed to "..".
    #  - All trailing whitespace on a line is removed.
    #
    # The easy way to do this is to define an anonymous sub that sends back
    # a line at a time.  That way, we don't end up wasting memory by storing
    # two copies of the article body (which could potentially be long).
    my $body;
    {
        my $line = 0;
        $body = sub {
            my $text;
            do 
	    {
                $text = $self->{Body}[$line++];
                return undef unless defined $text;
            } while ($text =~ /^ *$/);
            $text =~ s/^--/- --/;
            $text =~ s/^(from|subject)/>$1/i;
            $text =~ s/^\.($|[^.])/..$1/;
            $text =~ s/\s+$//;
            $text . "\n";
        }
    }

    # Now, actually calculate the signature and add it to the headers.
    my $signature = pgp_sign ($keyid, $passphrase, $headers, $body);
    return pgp_error
	unless defined($signature);

    $signature =~ s/\n(.)/\n\t$1/g;
    $self->add_headers('x-auth', "PGPMoose V1.1 PGP $group\n\t$signature");
    return ();
}

#--------------------------------------------------------------------------

=item verify_pgpmoose ( GROUP )

Verifies an article signature according to the PGPMoose spec.  We
require that pgp be on the path to do this.  Takes a "group" which can
be either a newsgroup or an address, and an optional key id.

Looks for a X-Auth header matching the specified group or address, and
if found, checks the validity of the signature. If successful, returns
the signer identity (from the PGP output), otherwise returns false.

=cut

sub verify_pgpmoose 
{
    my ($self, $group, $keyid) = @_;

    my $sig = (grep(/^ PGPMoose \s+ V\d\.\d \s+ PGP \s+ \Q$group\E \n /isx,
		      $self->header('x-auth')))[0];

    return undef unless $sig;

    $sig =~ s/[^\n]*\n//;
    $sig =~ s/\t//g;

    # Now, put together all of the stuff we need to sign.  First, we need a
    # list of newsgroups, sorted.

    my $headers = $self->header('newsgroups');
    $headers =~ s/\s//g;
    $headers = join ("\n", (sort split (/,+/, $headers)), '');

    # Next we need an array of headers: From, Subject, and Message-ID in
    # that order, killing initial and final whitespace and any spaces after
    # colons.
    for (qw(from subject message-id)) 
    {
        my $value = $self->header($_);
        $value =~ s/\n.*//;
        $value =~ s/^ +//;
        $value =~ s/\s+$//;
        $value =~ s/: +/:/g;
        $headers .= $value . "\n";
    }

    # Finally, we need to give it the body of the article, making the
    # following transformations:
    #
    #  - Lines consisting solely of spaces are deleted.
    #  - A leading "--" is replaced by "- --"
    #  - A leading "from" (case-insensitive) has > prepended.
    #  - A leading "subject" (case-insensitive) has > prepended.
    #  - A leading single "." is changed to "..".
    #  - All trailing whitespace on a line is removed.
    #
    # The easy way to do this is to define an anonymous sub that sends back
    # a line at a time.  That way, we don't end up wasting memory by storing
    # two copies of the article body (which could potentially be long).
    my $body;
    {
        my $line = 0;
        $body = sub {
            my $text;
            do 
	    {
                $text = $self->{Body}[$line++];
                return undef unless defined $text;
            } while ($text =~ /^ *$/);
            $text =~ s/^--/- --/;
            $text =~ s/^(from|subject)/>$1/i;
            $text =~ s/^\.($|[^.])/..$1/;
            $text =~ s/\s+$//;
            $text . "\n";
        }
    }

    pgp_verify ($sig, undef, $headers, $body);
}

#--------------------------------------------------------------------------

=item sign_control ( KEYID, PASSPHRASE [, HEADER [...] ] )

Signs the article in the manner used for control messages.  This is
derived from signcontrol, written by David Lawrence, but with fewer sanity
checks since we assume people know what they're doing.  Caveat programmer.

We take a key id, a PGP password, and an optional list of extra
headers to add to the signature.  By default, Subject, Control,
Message-ID, Date, From, and Sender are signed. Any signed header that
isn't present in the article will be signed with an empty value. Date
and Message-ID are automatically added if needed.

=cut

sub sign_control 
{
    my ($self, $keyid, $passphrase, @extra) = @_;
    my @headers = qw(subject control message-id date from sender);
    push @headers, map {lc $_} @extra;

    # Check to make sure we have the required headers.
    for (qw(subject control from))
    {
        return ("Required header $_ missing")
	    unless $self->{Headers}{$_};
    }

    $self->add_message_id('cmsg-');
    $self->add_date();

    # We have to sign the list of headers and each header on a seperate
    # line.  Note that the verification code doesn't support continuation
    # headers, so be careful not to use them when calling this method.

    my $signheads = join (',', map { canonical $_ } @headers);
    my @sign;
    push (@sign, 'X-Signed-Headers: ' . $signheads . "\n");
    for (@headers) 
    {
        push (@sign, (canonical $_).": ".($self->header($_) || '')."\n");
    }

    # Now send everything to PGP to sign.  We have to add a new line to the
    # end of every line of the body, since we're storing it without them.
    # Make sure we munge for attached signatures, since pgpverify tests with
    # an attached signature.
    local $PGP::Sign::MUNGE = 1;
    my $body;
    {
        my $line = 0;
        $body = sub {
            my $text = $self->{Body}[$line++];
            defined $text ? $text . "\n" : undef;
        }
    }
    my ($signature, $version) =
        pgp_sign ($keyid, $passphrase, \@sign, "\n", $body);
    return pgp_error
	unless defined($signature);

    # Add tabs after the newlines and add the signature to the headers.
    $signature =~ s/\n(.)/\n\t$1/g;
    $self->add_headers('x-pgp-sig', "$version $signheads\n\t$signature");
    return ();
}

###########################################################################
# INTERNAL FUNCTIONS
###########################################################################

# Convert a header name to canonical capitalisation. We keep the header
# names in lowercase internally to simplify, but prefer to emit standard-
# looking forms on output.

sub canonical
{
    my $name = lc shift;
    join('',map { ($SPECIAL{$_} || ucfirst $_); } split(/([_-])/,$name));
}

# Fix up an envelope sender taken from a Unix-style "From" line.

# This isn't guaranteed to work due to variations in From line
# format. An explicit decision has been made to trust the
# header format *rather than* the sanity of the envelope
# address, because we have no control over the latter, whereas
# the former is generated by local software and therefore
# should be fixable if it is too insane.

# Theory:
#   If there's a timestamp (check for MMM DDD NN HH:MM) then remove
#   it and everything following it. Otherwise remove any trailing
#   text resembling 'remote from ...'.
#   Then remove trailing spaces from the result and return it.

sub fix_envelope
{
    my $from = shift;

    $from =~ s/\s \w\w\w \s \w\w\w \s [\d\s]\d \s \d\d:\d\d(:\d\d)? \s .*? $//x
	or $from =~ s/\s remote \s from \s .* $//x;

    $from =~ s/\s+$//;

    return $from;
}

# Initialise a data source; returns a CODE ref with which to
# read from that source.
#
# Allowed sources are:
#  GLOBs or unknown refs are assumed to be filehandles or equivalent.
#  ARRAY refs (treated as a list of lines)
#  SCALAR refs (treated as text)
#  SCALARs (treated as filenames)
#  CODE refs are left unchanged

sub source_init
{
    my $source = shift;

    if (ref(\$source) ne 'GLOB')
    {
	return $source if (ref($source) eq 'CODE');
    
	if (ref($source) eq 'ARRAY')
	{
	    my $index = 0;
	    return sub { $source->[$index++] };
	}

	if (ref($source) eq 'SCALAR')
	{
	    my $pos = 0;
	    return sub { return undef unless $pos < length($$source);
			 my $tmp = $pos;
			 $pos = 1 + index($$source,"\n",$tmp);
			 if ($pos <= $tmp)
			 {
			     $pos = 1 + length($$source);
			     return substr($$source,$tmp);
			 }
			 else
			 {
			     ++$pos;
			     return substr($$source,$tmp,($pos - $tmp));
			 }
		     };
	}
    
	if (!ref($source))
	{
	    $source = new FileHandle("<$source");
	    return undef unless $source;
	}
    }

    return sub { scalar(<$source>) };
}

###########################################################################
# THE END
###########################################################################

1;

__END__

###########################################################################
#
# $Log: Article.pm $
# Revision 1.13  1997/12/27 23:19:07  andrew
# Missing 'x' flag on extended regexp in fix_envelope
#
# Revision 1.12  1997/12/13 13:00:52  andrew
# Changed add_date to use local time and add timezone offset
#
# Revision 1.11  1997/12/12 11:42:34  andrew
# corrections to header ordering code
#
# Revision 1.10  1997/12/12 11:09:30  andrew
# Added header ordering stuff
#
# Revision 1.9  1997/12/10 19:20:54  andrew
# added ihave
#
# Revision 1.8  1997/11/08 17:51:45  andrew
# Typos, and handling of error return in post().
#
# Revision 1.7  1997/10/22 20:59:27  andrew
# Clean up distribution terms for release
#
# Revision 1.6  1997/10/22 19:54:41  andrew
# Fixed old typo in RCS revision keyword
#
# Revision 1.5  1997/08/31 01:35:25  andrew
# Added obligatory quotation :-)
#
# Revision 1.4  1997/08/29 03:31:07  andrew
# Fix typo in previous mod
#
# Revision 1.3  1997/08/29 00:34:28  andrew
# Update for latest PGP::Sign (v0.08).
# Add reference handling to add_body().
# Allow -f '' in mail().
#
# Revision 1.2  1997/07/30 12:13:11  andrew
# cleanup (no changes)
#
# Revision 1.1  1997/07/29 15:20:40  andrew
# Initial revision
#
#
#
###########################################################################

=head1 AUTHOR

Written by Andrew Gierth <andrew@erlenstar.demon.co.uk>

Thanks to Russ Allbery <rra@stanford.edu> for comments and
suggestions.

=head1 COPYRIGHT

Copyright 1997 Andrew Gierth <andrew@erlenstar.demon.co.uk>

This code may be used and/or distributed under the same terms as Perl
itself.

=cut

###########################################################################
