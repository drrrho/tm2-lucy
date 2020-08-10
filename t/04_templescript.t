use strict;
use warnings;

use Test::More;
use Test::Exception;
use TM2::TS::Test;

use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

my $warn = shift @ARGV;
unless ($warn) {
    close STDERR;
    open (STDERR, ">/dev/null");
    select (STDERR); $| = 1;
}

use constant DONE => 1;

use TM2;
use Log::Log4perl::Level;
$TM2::log->level($warn ? $DEBUG : $ERROR); # one of DEBUG, INFO, WARN, ERROR, FATAL

use TM2::TempleScript;
use TM2::TempleScript::Parser;
$TM2::TempleScript::Parser::UR_PATH = '/usr/share/templescript/ontologies/';

sub _parse {
    my $t = shift;
    use TM2::Materialized::TempleScript;
    my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::TriggerAble')
	;

    $tm->deserialize ($t);
    return $tm;
}

sub _mk_ctx {
    my $stm = shift;
    return [ { '$_'  => $stm, '$__' => $stm } ];
}

#-- TESTS ----------------------------------------------------------

if (DONE) {
    my $AGENDA = q{basic operations: };

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my $tm = _parse (qq{
%include file:ontologies/lucy.atm

aaa isa person
! AAA is nice guy

bbb isa person
! BBB is also a nice man

ccc isa person
! CCC is not such a nice person

ddd isa person
! DDD is definitely an unnice person

#--

isa storage @ lang:perl :
   class       : fulltext-query
   ts:software : urn:x-perl:TM2::ObjectAble::StringsViaLucy

fulltext-query
  ~ urn:x-mime:text/query
lucy:root         : $root
lucy:auto-create  : 1
lucy:unique-tid   : 1 # at every STORE we look whether that tid is there and is to be deleted

}.q{
? isa ts:converter
    ts:mimetype @ ts:input  : "text/plain"
    ts:mimetype @ ts:output : "text/query"
return """
   return TM2::Literal->new ($_[0]->[0], 'urn:x-mime:text/query');
""" ^^ lang:perl !

});

    use TM2::TempleScript::Stacked;
    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    my $tss;
#--
    use TM2::ObjectAble::StringsViaLucy;
    $tss = TM2::TempleScript::return ($ctx, q{ ( person >> instances ) | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
    is ((scalar @$tss), 4, $AGENDA.'indexing person names');
#--
    $tss = TM2::TempleScript::return ($ctx, q{ ( person >> instances ) | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
    diag "filled index again to test uniqueness";
#--
    $tss = TM2::TempleScript::return ($ctx, q{ aaa ~[text/query]~> });  # forward lookup
    is_singleton ($tss, TM2::Literal->new ('AAA is nice guy', 'urn:x-mime:text/query'), 'lookup by key, 2nd indexing');
#--

    is_deeply (TM2::TempleScript::return ($ctx, q{ ( "AAA" <~[text/query]~ ) }),
               [ [ 'tm:aaa' ] ], $AGENDA.'AAA results');

    ok (eq_set ([ 'tm:aaa', 'tm:bbb', 'tm:ccc' ],
                [ map { $_->[0] } @{ TM2::TempleScript::return ($ctx, q{ ( "nice" <~[text/query]~ ) }) } ]), $AGENDA.'nice result');
    ok (eq_set ([ 'tm:ddd' ],
                [ map { $_->[0] } @{ TM2::TempleScript::return ($ctx, q{ ( "unnice" <~[text/query]~ ) }) } ]), $AGENDA.'unnice result');

    #warn Dumper $tss;
#warn Dumper $tss; exit;
#    $tss = TM2::TempleScript::return ($ctx, q{ $_ ~[text/query]~> });  # lookup
#warn Dumper $tss; exit;
}

if (DONE) {
    my $AGENDA = q{timed operations: };

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my $tm = _parse (qq{

%include file:ontologies/lucy.atm

aaa
! AAA is nice guy

bbb
! BBB is also a nice man

ccc
! CCC is not such a nice person

ddd
! DDD is definitely an unnice person

#--

isa storage @ lang:perl :
   class       : fulltext-query
   ts:software : urn:x-perl:TM2::ObjectAble::StringsViaLucy

fulltext-query
  ~ urn:x-mime:text/query
lucy:root        : $root
lucy:auto-create : 1
lucy:unique-tid  : 1 # at every STORE we look whether that tid is there and is to be deleted
lucy:expiry      : "3.1 sec"

}.q{
? isa ts:converter
    ts:mimetype @ ts:input  : "text/plain"
    ts:mimetype @ ts:output : "text/query"
return """
   return TM2::Literal->new ($_[0]->[0], 'urn:x-mime:text/query');
""" ^^ lang:perl !

});

    use TM2::TempleScript::Stacked;
    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    my $tss;
#--
    use TM2::ObjectAble::StringsViaLucy;
    $tss = TM2::TempleScript::return ($ctx, q{ ( aaa, bbb ) | zigzag | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
    is ((scalar @$tss), 2, $AGENDA.'indexing person1 names');
    sleep 2;
    $tss = TM2::TempleScript::return ($ctx, q{ ( ccc ) | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
    sleep 1;
    $tss = TM2::TempleScript::return ($ctx, q{ ( ddd ) | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
#--
    sleep 1;
    $tss = TM2::TempleScript::return ($ctx, q{ ( aaa, bbb, ccc, ddd ) | zigzag | ( $0  ~[text/query]~> ) });  # forward lookup
    ok (eq_set ([ map { $_->[0]->[0] } @$tss ],
		[
		    'CCC is not such a nice person',
		    'DDD is definitely an unnice person'
		]), $AGENDA.'expired result');
#warn Dumper $tss
}

done_testing;

__END__


    is_deeply ($tss, [ [ 'tm:aaa' ], [ 'tm:bbb' ] ], 'reverse');
    $tss = TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" <~[xxx/yyy]~ ) });
    is_deeply ($tss, [ [ 'tm:aaa' ], [ 'tm:bbb' ] ], 'reverse');
#--
    throws_ok {
        TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" << zoomer aaa/bbb) });
    } qr/cannot find/, 'unknown implicit conversion';
#--
    $tss= TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" << zoomer xxx/zzz) });
    ok (! @$tss, 'unwilling to reverse xxx/yyy');
#--
    $tss = TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" << zoomer ) });
    ok (! @$tss, 'unwilling to reverse */*');
    $tss = TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" <~~ ) });
    ok (! @$tss, 'unwilling to reverse */*');
#    warn Dumper $tss;
    use TM2::ObjectAble::StringViaPlucene;

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my %hash1;
    tie %hash1, 'TM2::ObjectAble::StringViaPlucene', root => $root;
    $tm->storage ('xxx/*' => \%hash1);

    my %hash2;
    tie %hash2, 'InvertAbleHash';
    $tm->storage ('xxx/yyy' => \%hash2);

    $tm->objectify ('xxx/zzz',       'tm:bbb', new TM2::Literal ('hell-o-world')); # goes into xxx/*
    $tm->objectify ('xxx/yyy',       'tm:aaa', new TM2::Literal ('hell-o-world')); # goes into xxx/yyy/, reversible
    $tm->objectify ('zzz/xxx',       'tm:aaa', new TM2::Literal ('hell-o-world')); # goes into */*

if (DONE) {
    my $AGENDA = q{object atomification: };
    {
	package PERSON;
	use Data::Dumper;
	use Moose;
	has 'age'  => (is => 'rw', 'isa' => 'Num');
	has 'name' => (is => 'rw', 'isa' => 'Str');
	has 'url'  => (is => 'rw', 'isa' => 'Str');
	around 'BUILDARGS' => sub {
	    my $orig = shift;
	    my $class = shift;
#warn "params ".Dumper \@_;
	    return $class->$orig (name => $_[0]->[0], age => $_[1]->[0], url => $_[2]->[0]);
	};
	1;
    }
    my $tm = _parse (q{

person isa class
 ~ urn:x-whatever:person

isa implementation @ lang:perl :
  class       : person
  ts:software : urn:x-perl:PERSON

aaa isa person
! AAA
age : 23
homepage : http://aaa.com/

bbb isa person
! BBB
age : 24
homepage : http://bbb.com/

});

    my $ctx = [ { '$_' => $tm, '$__' => $tm } ];
#--
    my $tss;

    if (DONE) {
	my $SUBAGENDA = $AGENDA.q{without package: };
	$tss = TM2::TempleScript::return ($ctx, q{
               ( person >> instances ) .. ( ./name, ./age, ./homepage ) 
    });
#	warn Dumper $tss; exit;

	ok ((scalar @$tss) == 2, $SUBAGENDA.'implicit atomification: exactly two tuples');
	map { is ( ref ($_), 'TM2::Literal', $SUBAGENDA.'implicit atomification: all literals '.$_->[0]) } @{ $tss->[0] };
	map { is ( ref ($_), 'TM2::Literal', $SUBAGENDA.'implicit atomification: all literals '.$_->[0]) } @{ $tss->[1] };
    }

    if (DONE) {
	my $SUBAGENDA = $AGENDA.q{with Perl package: };
	$tss = TM2::TempleScript::return ($ctx, q{
               ( person >> instances) .. ( ./name, ./age, ./homepage ) .. ( atomify person )
        });

	ok ((scalar @$tss) == 2, $SUBAGENDA.'explicit atomification: exactly two tuples');
	map { ok (scalar @$_ == 1, $SUBAGENDA.'explicit atomification: each tuple has length 1')  } @$tss;
	ok (eq_set ([ map { $_->[0]->name } @$tss],
		    [ 'AAA', 'BBB' ]), $SUBAGENDA.'explicit atomification: object names');
	ok (eq_set ([ map {$_->[0]->age } @$tss],
		    [ 23, 24 ]), $SUBAGENDA.'explicit atomification: object ages');
	ok (eq_set ([ map {$_->[0]->url } @$tss],
		    [ 'http://aaa.com/', 'http://bbb.com/' ]), $SUBAGENDA.'explicit atomification: object ages');
    }
    if (DONE) {
	my $SUBAGENDA = $AGENDA.q{with Perl package (via indicator): };
	$tss = TM2::TempleScript::return ($ctx, q{
`               ( person >> instances) .. ( ./name, ./age, ./homepage ) .. ( atomify urn:x-whatever:person )
        });

	ok ((scalar @$tss) == 2, $SUBAGENDA.'explicit atomification: exactly two tuples');
	map { ok (scalar @$_ == 1, $SUBAGENDA.'explicit atomification: each tuple has length 1')  } @$tss;
	ok (eq_set ([ map { $_->[0]->name } @$tss],
		    [ 'AAA', 'BBB' ]), $SUBAGENDA.'explicit atomification: object names');
	ok (eq_set ([ map {$_->[0]->age } @$tss],
		    [ 23, 24 ]), $SUBAGENDA.'explicit atomification: object ages');
	ok (eq_set ([ map {$_->[0]->url } @$tss],
		    [ 'http://aaa.com/', 'http://bbb.com/' ]), $SUBAGENDA.'explicit atomification: object ages');
    }
}

if (DONE) {
    my $AGENDA = q{crunch, as Perl list: };
    my $tm = _parse (q{

aaa isa person
! AAA
age : 23
homepage : http://aaa.com/

bbb isa person
! BBB
age : 24
homepage : http://bbb.com/

});

    my $ctx = [ { '$_' => $tm, '$__' => $tm } ];
    my $tss = TM2::TempleScript::return ($ctx, q{
             ( ( person >> instances) .. ( ./name, ./age, ./homepage ) crunch )
    });
#	warn Dumper $tss; exit;
    ok ((scalar @$tss) == 2, $AGENDA.'exactly two tuples');
    map { ok (scalar @$_ == 1, $AGENDA.'each tuple has length 1')  } @$tss;
    ok (eq_set ([ map {$_->[0]->[0]->[0] } @$tss],
		[ 'AAA', 'BBB' ]), $AGENDA.'object values');
}

done_testing;

__END__
