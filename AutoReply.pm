# -*- Perl -*-
###########################################################################
# Written and maintained by Andrew Gierth <andrew@erlenstar.demon.co.uk>
#
# Copyright 1997 Andrew Gierth. Redistribution terms at end of file.
#
# $Id: AutoReply.pm 1.5 1997/10/22 21:00:10 andrew Exp $
#
###########################################################################
#
# Address, n. 1. A formal discourse, usually delivered to a person who has
#             something by a person who wants something that he has.
#             2. The place at which one recieves the delicate attentions
#             of creditors.
#                                                        -- Ambrose Bierce
#

=head1 NAME

News::AutoReply - derivative of News::Article for generating autoreplies

=head1 DESCRIPTION

Like News::Article, but must be given a reference to another article
at creation time - initialises To, In-Reply-To, References etc.
correctly as an automatic reply.

=head1 USAGE

  use News::AutoReply;

Exports nothing.

=cut

package News::AutoReply;

use News::Article;
use strict;
use English;
use vars qw(@ISA);

@ISA = qw(News::Article);

=head1 Constructor

=over 4

=item new ( ORIGINAL )

Construct an autoreply to a message, assuming that the Reply-To (if
present, otherwise the From) header of C<ORIGINAL> is valid.

Returns a new Article object with no body or envelope sender, but with
suitable headers.

If an environment variable LOOP is defined, it is used as the contents
of an X-Loop header added to the reply (this is useful when using this
code in progs launched from a procmail recipe). Always preserves X-Loop
headers in the original.

The reference-folding code could probably be improved.

=cut

sub new
{
    my $class = shift;
    my $src = shift;

    my $self = $class->SUPER::new(@_);
    return undef unless $self;

    $self->reply_init($src);
}

#--------------------------------------------------------------------------
# private. Factored out of new() so that FormReply etc. can inherit
# this.

sub reply_init
{
    my $self = shift;
    my $src = shift;

    my $to = $src->header('reply-to') || $src->header('from');
    return undef unless $to;

    $self->add_headers(to => $to);
    $self->set_headers("x-loop" => [ $src->header("x-loop") ]);
    $self->add_headers("x-loop" => $ENV{LOOP}) if defined($ENV{LOOP});

    if (!defined($self->header("subject")))
    {
	my $subj = $src->header("subject") || "(no subject)";
	$subj =~ s/^(\s*[Rr][Ee]:\s+)?/Re: /;
	$self->set_headers(subject => $subj);
    }
    
    my $srcid = $src->header("message-id");
    $self->set_headers("in-reply-to" => $srcid) if $srcid;

    my $refs = $src->header("references");
    if ($refs)
    {
	$refs =~ tr/ \t\n/ /s;
	$refs .= " ".$srcid if $srcid;
	my $newrefs = "";
	my $line = 5;
	while ($refs)
	{
	    if ($refs =~ /([^\s]*)\s+/)
	    {
		if (($line + length($1)) < 70 || !$newrefs || !$line)
		{
		    $newrefs .= " ".$1;
		    $line += 1 + length($1);
		}
		else
		{
		    $newrefs .= "\n\t".$1;
		    $line = 1 + length($1);
		}
	    }
	    $refs = $POSTMATCH;
	}
	$self->set_headers(references => $newrefs);
    }
    elsif ($srcid)
    {
	$self->set_headers(references => $srcid);
    }

    return $self;
}

1;

__END__

###########################################################################
#
# $Log: AutoReply.pm $
# Revision 1.5  1997/10/22 21:00:10  andrew
# Cleanup terms for public release
#
# Revision 1.4  1997/08/31 02:10:46  andrew
# Added an ObQuote.
#
# Revision 1.3  1997/08/29 00:36:29  andrew
# No longer overrides Subject: if set in the source headers.
#
#
###########################################################################

=head1 AUTHOR

Andrew Gierth <andrew@erlenstar.demon.co.uk>

=head1 SOURCE

Contact the author.

=head1 COPYRIGHT

Copyright 1997 Andrew Gierth <andrew@erlenstar.demon.co.uk>

This code may be used and/or distributed under the same terms as Perl
itself.

=cut

###########################################################################
