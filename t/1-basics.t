#!/usr/bin/perl -w
# $Id: 1-basics.t,v 1.5 2005/03/28 18:43:26 chip Exp $

use Test::More tests => 17;

use strict;
use warnings;
use blib;

use Perl6::Subs;

pass("used");

#----------------------------------------------------------------

sub foo ( $a of Int where { my $x = $_; $x > 0 },
	  Int $b,
	  Int ?$c )
  returns(List)
{
    ($a, $b, $c)
}

ok   eq_array([foo(1,2)], [1, 2, undef]);

ok   +( eval { foo(qw(x y z)) }, $@ );
like $@, qr/\$a is not an Int/;

ok   +( eval { foo(qw(0 1 2)) }, $@ );
like $@, qr/\$a failed where/;

#----------------------------------------------------------------

sub Foo::thing ( $me: +$a is required, +$b, *%more ) : method, returns(Any) {
    $a || $b || $more{c};
}

is +Foo->thing( a => 1         ), 1, "thing(a=>1)";
is +Foo->thing( a => 0, b => 2 ), 2, "thing(b=>2)";
is +Foo->thing( a => 3, c => 4 ), 3, "thing(a=>3,c=>4)";

ok   +( eval { +Foo->thing( b => 5, c => 6 ) }, $@ );
like $@, qr/required named parameter 'a'/;

#----------------------------------------------------------------

sub match ( Rule $r, *@_ ) { grep /$r/, @_ }

ok   eq_array([match qr/\d/, qw(1 a 2 b 3)], [qw(1 2 3)]);
ok   +( eval { match 0 }, $@ );
like $@, qr/\$r is not a Rule/;

#----------------------------------------------------------------

{ package Thing;
  method new ( Str $class: ) { bless {}, $class }
  method cliche { "It's clobberin' time!\n" } }

sub embarrass (Thing $t) { $t->cliche }

like( embarrass(Thing->new), qr/clobber/i, "user type" );

ok   +( eval { embarrass('thing') }, $@ );
like $@, qr/\$t is not a Thing/;
