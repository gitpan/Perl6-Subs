# $Id: Subs.pm,v 1.5 2005/03/24 23:12:29 chip Exp $

package Perl6::Subs;

=head1 NAME

Perl6::Subs - Define your subroutines in the Perl 6 style

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';

use strict;
use warnings;
use Text::Balanced qw( extract_codeblock );

#
# Modules required for use by generated code
#

##use Params::Validate ();
use Scalar::Util ();
use Carp ();


#----------------------------------------------------------------
# Debugging
#

our $DEBUG ||= $ENV{PERL6_SUBS_DEBUG};
$::RD_TRACE ||= $ENV{PERL6_SUBS_RD_TRACE};  # FIXME
$::RD_HINT  ||= $ENV{PERL6_SUBS_RD_HINT};   # FIXME


#----------------------------------------------------------------
# Misc patterns
#

our $word_rx = qr{ ( [[:alpha:]_] \w* (?: :: [[:alpha:]_] \w* )* ) \b }x;

sub _inside {
    my $x = shift;
    if (defined $x) { $x =~ s/^[\({]//; $x =~ s/[\)}]\z// }
    $x;
}


#################################################################
# Perl6::Subs::Type
#

{
    package Perl6::Subs::Type;

    use Class::Struct
      __PACKAGE__, {
		    name    => '$',  # opt
		    base    => '$',  # opt
		    mustdef => '$',  # opt
		    qual    => '$',  # opt - built-in       - use _VAL_
		    where   => '$',  # opt - user-specified - use $_
		    valid   => '$',  # opt - P::V spec
		   };

    my %BASIC_TYPE;
    {
	my %t = (
		 # catch-all
		 Any	=> {},

		 # basic boxed types
		 Str	=> {},
		 Num	=> { qual => q{ Scalar::Util::looks_like_number(_VAL_) } },
		 Int	=> { base => 'Num',
			     qual => q{ int(_VAL_) == _VAL_ } },
		 Ref	=> { qual => q{ ref(_VAL_) } },
		 Bool	=> {},

		 # basic unboxed types: don't allow undefs
		 str	=> { base => 'Str',  mustdef => 1 },
		 num	=> { base => 'Num',  mustdef => 1 },
		 int	=> { base => 'Int',  mustdef => 1 },
		 ref	=> { base => 'Ref',  mustdef => 1 },
		 bool	=> { base => 'Bool', mustdef => 1 },

		 # aggregates
		 Array	=> { qual => q{ Scalar::Util::reftype(_VAL_) eq 'ARRAY' } },
		 Hash	=> { qual => q{ Scalar::Util::reftype(_VAL_) eq 'HASH'  } },

		 # runnables
		 Code	=> { qual => q{ Scalar::Util::reftype(_VAL_) eq 'CODE'  } },

		 # globs - not really Perl 6
		 Glob	=> { qual => q{ Scalar::Util::reftype(\_VAL_) eq 'GLOB' } },
		 GlobRef=> { qual => q{ Scalar::Util::reftype(_VAL_)  eq 'GLOB' } },
		);

	for (keys %t) {
	    my %init = %{ $t{$_} };
	    delete $init{base};
	    $BASIC_TYPE{$_} = Perl6::Subs::Type->new( name => $_, %init );
	}

	for (keys %t) {
	    if (my $b = $t{$_}{base})
	      { $BASIC_TYPE{$_}->base( $BASIC_TYPE{$b} ) }
	}
    };

    #
    # alternate constructor
    #

    sub build {
	my ($class, %args) = @_;
	my ($name, $where, $valid) = @args{qw( name where valid )};

	my $type = ( $BASIC_TYPE{$name || 'Any'}
		     || Perl6::Subs::Type->new( name => $name,
						qual => qq{ UNIVERSAL::isa(_VAL_, ${name}::) } ) );

	$type = Perl6::Subs::Type->new( base  => $type,
					where => $where,
					valid => $valid )
	      if $where || $valid;

	$type;
    }
}


#################################################################
# Common functions for Class::Struct classes.
# (Must do it this way because ISA and Class::Struct don't mix.)
#

my $CS_has_trait =
  sub {
      my ($self, $trait) = @_;
      exists $self->traits->{$trait};
  };


#################################################################
# Perl6::Subs::Param
#

{
    package Perl6::Subs::Param;

    use Class::Struct
      __PACKAGE__, {
		    name      => '$',	# formal name (no package)
		    traits    => '%',	# traits
		    type      => '$',	# type object
		   };

    sub has_trait;
    *has_trait = $CS_has_trait;
}


#################################################################
# Perl6::Subs::Sub
#

{
    package Perl6::Subs::Sub;

    use Parse::RecDescent;

    use Class::Struct
      __PACKAGE__, {
		    name      => '$',	# declared name (may not be full name)
		    traits    => '%',	# traits
		    p_inv     => '$',	# invocant (iff $self->has_trait('method'))
		    p_pos     => '@',	# positional parameters (may be empty)
		    p_pos_req => '$',	# number of p_pos that are required
		    p_named   => '%',	# named parameters (may be empty)
		    p_slurpy  => '$',	# slurpy parameter (optional)
		   };

    sub has_trait;
    *has_trait = $CS_has_trait;

    my $Parser = Parse::RecDescent->new(q{

	{
	    our $pcomma = qr{ , | (?= \s* \) | \s* \z ) }x;   # comma _or_ end of prototype
	    our $word   = $Perl6::Subs::word_rx;
	    our $s_var  = qr{ \$   $word }x;
	    our $a_var  = qr{ [@%] $word }x;

	    our $Any_Type = Perl6::Subs::Type->build( name => 'Any' );
	}

	sub	: m{ sub \b | method \b }x
		  ( /$word/ )(?)
		  proto(?)
		  traits
		  m{ (?= \\\{ | ; | \z ) }x
		    {
			my $sub = Perl6::Subs::Sub->new(
							name   => $item[2][0],
							traits => $item{traits},
							@{ $item{'proto(?)'}[0] },
						       );

			#
			# C<method foo> means C<sub foo :method>
			#

			$sub->traits('method', undef)
			  if $item[1] eq 'method';

			#
			# If sub is a method, then it must have an invocant
			# If sub is not a method, then it must not have one.
			#
			# FIXME: '$self' is not necessarily a universal default.
			#

			if ($sub->has_trait('method')) {
			    $sub->p_inv( Perl6::Subs::Param->new( name => '$self',
								  type => $Any_Type ) )
			      unless $sub->p_inv;
			}
			else {
			    die "Invocant is only permitted for methods; use 'method' keyword or trait"
			      if $sub->p_inv;
			}

			# return Sub object

			$sub;
		    }

	proto	: { $thisparser->{local}{seen} = {} }	# FIXME: Use This
		  '('
		    inv(?)
		    pos(s?)
		    (
			opt(s)		    { [ opt    => $item[1] ] }
		      | named(s) slurpy(?)  { [ named  => $item[1], slurpy => $item[2][0] ] }
		      | slurpy		    { [ slurpy => $item[1] ] }
		      |			    { [] }
		    )
		  ')'
		    {
			my %p = (
				 inv => $item{'inv(?)'}[0],
				 pos => $item{'pos(s?)'},
				 @{ $item[5] },
				);

			if (@{ $p{named} || [] } && ($p{slurpy} && $p{slurpy}->name !~ /^%/))
			  { die "Slurpy parameter must be hash after named parameters (e.g. \"*%foo\")" }

			[
			  p_inv     => $p{inv},
			  p_pos     => [
					@{ $p{pos}       },
					@{ $p{opt} || [] },
				       ],
			  p_pos_req => scalar @{ $p{pos} },
			  p_named   => { map { $_->name => $_ } @{ $p{named} || [] } },
			  p_slurpy  => $p{slurpy},
			]
		    }

	inv	: param[ qr//,   $s_var, qr/:/   ]
	pos	: param[ qr//,   $s_var, $pcomma ]
	opt	: param[ qr/\?/, $s_var, $pcomma ]
	named	: param[ qr/\+/, $s_var, $pcomma ]
	slurpy	: param[ qr/\*/, $a_var, $pcomma ]

	param	: typeq /$arg[0]/ /$arg[1]/ traits             /$arg[2]/
			{ Perl6::Subs::Param->new( name   => $item[3],
						   type   => $item{typeq},
						   traits => $item{traits} ) }

		|       /$arg[0]/ /$arg[1]/ traits /of\b/ type /$arg[2]/
			{ Perl6::Subs::Param->new( name   => $item[2],
						   type   => $item{type},
						   traits => $item{traits} ) }

	var	: /$arg[0]/

	type	: /$word/ where(?)
		    { Perl6::Subs::Type->build( name => $item[1],
						@{ $item[2][0] || [] } ) }

	typeq	: type
		| { Perl6::Subs::Type->build( name => 'Any' ) }

	where	: m{ where \b | valid \b }x <perl_codeblock>
		    {
		      my $code = $item[2];

		      #
		      # KLUDGE ALERT
		      # Assume they need a 'do' block if there's a ';' or 'my' in there.
		      # Otherwise just give them parentheses.
		      #
		      for ($code) {
			if (/;/ || m{ \b my \b }x)
			  { $_ = "do $_" }
			else
			  { s#^{#(#; s#}\z#)# }
		      }

		      [ $item[1], $code ]
		    }

	# 'traits' returns a hash ref to ( name => value ) trait pairs
	# 'trait' returns an array ref to a [ name, value ] trait pair

	traits	: trait(s?)	{ +{ map { @$_ } @{ $item[1] } } }

	trait	: m{ \: | is \b }x /$word/ ( <perl_codeblock ()> )(?)
		    { [ $item[2], Perl6::Subs::_inside($item[3][0]) ] }
		| /returns\b/ <perl_codeblock ()>
		    { [ $item[1], Perl6::Subs::_inside($item[2])    ] }

    });


    sub new_from_decl {
	my ($class, $text) = @_;

	my $orig_len = length($text);

	my $sub = $Parser->sub(\$text)
	  or return;

	wantarray
	  ? ($sub,
	     substr($_[1], 0, length($_[1]) - length($text)),
	     $text)
	  : $sub;
    }


    sub init_code {
	my ($self) = @_;

	my $code = '';

	my @pos     = @{ $self->p_pos     };
	my $pos_req =    $self->p_pos_req;
	my %named   = %{ $self->p_named   };
	my $slurpy  =    $self->p_slurpy;

	#
	# For parameter passing, invocant is just positional,
	#  so include invocant in @pos and $pos_req.
	# (Hm, I wonder whether I should just fix p_pos.)
	#

	if (my $inv = $self->p_inv) {
	    unshift @pos, $inv;
	    ++$pos_req;
	}

	#
	# Enforce minimum and maximum parameter counts (if any).
	#

	if (!%named && !$slurpy && $pos_req == @pos) {
	    # min/max are the same
	    $code .= qq{    Carp::croak "Wrong number of parameters (expected $pos_req)" if \@_ != $pos_req;\n};
	}
	else {
	    # minimum and/or maximum differ

	    $code .= qq{    Carp::croak "Too few parameters (min $pos_req)" if \@_ < $pos_req;\n};
	    unless (%named or $slurpy) {
		my $max = @pos;
		$code .= qq{    Carp::croak "Too many parameters (max $max)" if \@_ > $max;\n};
	    }
	}

	#
	# Grab positional, slurpy, and named all at once.
	#

	# named param variable should be an array if we're using Params::Validate

	{
	    my $named_var = $slurpy ? $slurpy->name : '%_perl6_named_params';
	    die "BUG: named slurp not a hash" if %named && $named_var !~ /^%/;
	    $named_var =~ s/^[\@%]// or die;

	    my @vars = map { $_ ? $_->name : () } @pos, $slurpy;
	    push @vars, "\%$named_var" if %named && !$slurpy;

	    if (!@vars) {
		# do nothing
	    }
	    elsif ($vars[-1] eq '@_') {
		pop @vars;
		if (!@vars) {
		    # do nothing
		}
		elsif (@vars <= 3) {
		    $code .= qq{    my $_ = shift;\n} for @vars;
		}
		else {
		    local $" = ', ';
		    $code .= qq{    my ( @vars ) = splice \@_, 0, @{[ scalar @vars ]};\n};
		}
	    }
	    else {
		local $" = ', ';
		$code .= qq{    my ( @vars ) = \@_;\n};
	    }

	    #
	    # If there are named parameters, go get 'em.
	    #

	    if (%named) {
		my @vars = sort keys %named;
		my @keys = map { /^\$(\w+)\z/ or die "BUG"; $1 } @vars;

		if (my @req_vars = grep { $named{$_}->has_trait('required') } @vars) {
		    my @req_keys = map { /^\$(\w+)\z/ } @req_vars;

		    my $croak = q{Carp::croak "Missing required named parameter '$_'"};
		    $code .= qq{    exists \$$named_var\E{\$_} or $croak for qw( @req_keys );\n};
		}

		## FIXME: insert Params::Validate call here
		$code .=
		  @vars == 1
		    ? qq{    my $vars[0] = delete \$$named_var\E{'$keys[0]'};\n}
		    : qq{    my ( @{[ join ', ', @vars ]} ) = delete \@$named_var\E{qw( @keys )};\n};

		# leftover named params are a bug unless the last param is *%slurpy
		$code .= qq{    Carp::croak "Unknown named parameter(s): \@{[ sort keys \%$named_var ]}" if \%$named_var;\n}
		  unless $slurpy;
	    }
	}

	#
	# Check validity of non-slurpy parameters
	#

	for my $param (@pos,
		       map { $named{$_} } sort keys %named)
	{
	    my ($name, $type) = ($param->name, $param->type);

	    #use Data::Dumper;
	    #print Dumper $param->type;

	    my %ck;
	    for (my $t = $type; $t; $t = $t->base) {
		$ck{mustdef} ||= $t->mustdef;
		for my $attr (qw( qual where )) {
		    for ($t->$attr) {
			unshift @{ $ck{$attr} }, $_
			  if $_;
		    }
		}
	    }

	    if ($ck{qual} || $ck{mustdef}) {
		my $qual = join( '&&',
				 $ck{mustdef} ? 'defined(_VAL_)' : (),
				 @{ $ck{qual} } );
		if (!$ck{mustdef})
		  { $qual = qq{ !defined(_VAL_) || ($qual) } }

		for ($qual) {
		    s{ \b _VAL_ \b }{$name}xg;
		    s/^\s+//;
		    s/\s+$//;
		}

		my $wanted = $type->name;
		if (!$wanted && ($type->where || $type->valid) && $type->base)
		  { $wanted = $type->base->name }
		$wanted = $wanted ? (($wanted =~ /^[aeiuo]/i ? "an" : "a") . " $wanted") : "valid";

		$code .= qq{    Carp::croak 'Parameter $name is not $wanted' unless $qual;\n};
	    }

	    if ($ck{where}) {
		my $where = join ' && ', @{ $ck{where} };
		$code .= qq{    $where or Carp::croak 'Parameter $name failed where{} test' for $name;\n};
	    }
	}

	#
	# Let caller decide what to do with the code.
	#

	$code;
    }
}


#----------------------------------------------------------------
# Now filter using the above code
#

use Filter::Simple;

FILTER_ONLY
  code =>
    sub {
	# FIXME: Use This
	my $ph = $Filter::Simple::placeholder;

	my $code = $_;
      SUB:
	while ($code =~ m{ \b
			   (
			     ( sub | ( method ) ) \b	# $3 is 'method' foo
			     (?: \s+ ( $word_rx ) )?
			     \s*
			   )
			   (?(3)  (?= \( | \{ | : | is \b | returns \b )
				| (?= \( [^\)]* \w ) )
			 }xog)
	{
	    my ($name) = $3;

	    my $proto_pos = pos($code);
	    my $sub_pos = $proto_pos - length($1);

	    print "Perl6::Subs - trying sub decl at {{" . substr($code, $sub_pos, 40) . "}}\n"
	      if $DEBUG;

	    my ($sub, $subtext) = Perl6::Subs::Sub->new_from_decl(substr($code, $sub_pos));

	    unless ($sub) {
		warn "Can't parse sub declaration for &$name\n" if $name;
		pos($code) = $proto_pos;
		next SUB;
	    }

	    #
	    # Replace the Perl 6 declaration with an equivalent Perl 5.
	    #

	    my $p5 = 'sub';
	    $p5 .= ' ' . $sub->name if defined $sub->name;

	    $p5 .= "\n" x ($subtext =~ tr/\n//);

	    while (my ($k, $v) = each %{ $sub->traits }) {
		# some traits are P6-specific
		next if $k eq 'returns';

		$p5 .= " :$k";
		$p5 .= "($v)" if defined $v;
	    }

	    $p5 .= ' ';

	    substr($code, $sub_pos, length($subtext)) = $p5;
	    pos($code) = $sub_pos + length($p5);

	    #
	    # If this is a sub definition, insert the initialization code.
	    #

	    if ($code =~ m{ \G \s* \{ }xgc) {

		my $init = $sub->init_code;

		if ($DEBUG)
		    { $init = "\n" . $init  }  # for indentation
		else
		    { $init =~ s/\n[ ]*/ /g }  # for line numbers

		my $init_pos = pos($code);
		substr($code, $init_pos, 0) = $init;
		pos($code) = $init_pos + length($init);
	    }

	    print "Perl6::Subs - resuming search for sub decls at {{" . substr($code, pos($code), 40) . "}}\n"
	      if $DEBUG;
	}

	$_ = $code;
    },
  all =>
    sub {
	$DEBUG or return;
	my $sep = "=" x 72 . "\n";
	print $sep, $_, $sep;
    };

1;

__END__

=head1 SYNOPSIS

  use Perl6::Subs;

  sub get (Array $x) { pop @$x }

  method foo { $self->bar }        # default invocant: '$self'

=head1 DESCRIPTION

Perl6::Subs lets you write subroutine definitions in Perl6 syntax,
with some limitations.  Given a subroutine defined with a Perl 6
prototype, Perl6::Subs generates Perl 5 code for that subroutine that
will, at runtime: declare the formal parameters, assign them their
values, and validate their contents.

Perl6::Subs supports parameters of five categories:

=over

=item *

invocant (e.g. C<$self>) -- marked by a trailing colon rather than the usual comma

=item *

mandatory positional -- without decoration

=item *

optional positional -- with a "?" before the variable name

=item *

optional named -- with a "+" before the variable name

=item *

required named -- with a "+" prefix and an C<is required> trait

=item *

"slurpy" -- i.e. an array or hash that consumes the rest of the actual
parameters -- with a "*" before the variable name

=back

Note that any given subroutine can either optional positional parameters
I<or> named parameters I<or> a slurpy parameter.  As a special exception,
named parameters may be followed by a slurpy hash; the hash will contain
all the key/value pairs other than the ones used for named parameters.
Other combinations are not legal due to limitations of Perl 5 parameter
conventions.

=head1 SUPPORTED TYPES

Perl6::Subs knows how to validate the following Perl 6 I<fundamental types>:

=over

=item B<str>:

Any defined scalar.  (Note: There is no test for not being a
reference; given Perl 5's overloading facility, a reference may
actually have a useful string value if you go ahead and use it.)

=item B<num>:

A defined scalar that passes &Scalar::Util::looks_like_number.

=item B<int>:

A B<num> with no fractional part.

=item B<ref>:

Any reference value.

=item B<bool>:

Any defined scalar.

=back

Perl6::Subs knows how to validate the following Perl 6 C<object types>
(but note that while Perl 6 considers all these to be objects, Perl 5
generally does not):

=over

=item B<Any>:

Any value, including undef.

=item B<Str>:

Any scalar value, including undef.
(For Perl6::Subs, this is a synonym for B<Any>.)

=item B<Num>:

A B<num>, or undef.

=item B<Int>:

An B<int>, or undef.

=item B<Ref>:

A reference value, or undef.

=item B<Array>:

An array reference, or undef.

=item B<Hash>:

A hash reference, or undef.

=item B<Code>:

A code (subroutine) reference, or undef.

=item B<Glob>:

A glob value (e.g. C<*STDOUT>), or undef.

=item B<GlobRef>:

A glob reference (e.g. C<\*STDOUT>), or undef.

=back

Any other bareword used as a type is assumed to name a user-defined class.
A parameter so typed must satisfy UNIVERSAL::isa() of that class, or be
undef.

=head1 CAVEATS

=over

=item Avoid variable named after Perl quoting operators.

Do not use parameter names that turn into Perl quoting operators when
their sigils are stripped: "$y", "$m", "@tr", "@q", etc.  If you do
so, Filter::Simple will be fooled into thinking large parts of your
program are quoted strings, and large parts of your code may go
unfiltered.

=item No Perl 5 prototypes.

The Perl 5 code produced by this source filter I<never> includes Perl 5
prototypes for the functions with Perl 6 prototypes.  This is a design
decision driven mostly by its intended usage: creating object-oriented
Perl code.  Method calls in Perl 5 always ignore prototypes.  And if you
don't know what a Perl5 prototype is, exactly, defining it can be a tricky
thing.

I suspect that if we ever add this feature, it will be as an additional
function attribute:

    sub foo (Int $x) is perl5_prototype($) { ... }

=back

=head1 TODO

=over

=item *

Some missing Perl 6 features that would be helpful in Perl 5 code:

=over

=item *

Junction types (e.g. C<Foo | Bar>)

=item *

Default parameter values (e.g. C<Int +$pos = 0>)

=item *

Parameter traits and the features they enable (e.g. C<Int +$pos is
required>.  (Perl6::Subs doe parse parameter traits, but it then
proceeds to ignore them.)

=item *

Perl 6 prototypes on sub I<declarations> (as opposed to definitions).

=item *

Interspersed comments in sub definitions.  If you include any comments
from the "sub" keyword to the open brace of the sub body, bad things will
happen.  This bug should be easy to fix by using the "placeholder" regular
expression provided so kindly by Damian.

=back

=item *

The default invocant, currently C<$self>, should be user-changable.  But
I'm not sure the best user interface.  This would work:

  use Perl6::Subs invocant => '$this';

And yet, would you really want to have to put that at the top of every
module?  You'd probably just start using $self.  For a large project,
something like this would be better:

  # MyProject.pm
  package MyProject;
  use Perl6::Subs::Defaults invocant => '$this';

=item *

There's a hook for support of Params::Validate: C<valid {}>, which works
syntatically just like C<where {}>.  As of this writing, it does nothing.

=back

=head1 BUGS

This module is a source filter.  Source filters always break.  For
example, the breakage caused by parameter names that turn into Perl
quoting operators when their sigils are stripped may never be fixed.

Please report any other bugs or feature requests to
C<bug-perl6-subs@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Perl6-Subs>.
I will be notified, and then you'll automatically be notified of
progress on your bug as I make changes.

=head1 AUTHOR

Chip Salzenberg, C<< <chip@pobox.com> >>

=head1 ACKNOWLEDGEMENTS

Thanks to Heath Market Science <hmsonline.com> for funding creation of
this module.  Thanks also to Larry, Damian, Allison, et al for Perl 6
subroutine syntax, and to Damian for Filter::Simple and Parse::RecDescent.

=head1 COPYRIGHT & LICENSE

Copyright 2005 Chip Salzenberg and Health Market Science.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at
your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
