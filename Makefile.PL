use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Perl6::Subs',
    AUTHOR              => 'Chip Salzenberg <chip@pobox.com>',
    VERSION_FROM        => 'lib/Perl6/Subs.pm',
    ABSTRACT_FROM       => 'lib/Perl6/Subs.pm',
    PL_FILES            => {},
    PREREQ_PM => {
	'Filter::Simple'    => '0.70',
	'Parse::RecDescent' => '1.91',
	'Text::Balanced'    => '1.90',
        'Test::More'        => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Perl6-Subs-*' },
);
