use strict;
use Data::Dumper;
use Test::More;
use Test::Exception;

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

use TM2;
use Log::Log4perl::Level;
$TM2::log->level($warn ? $DEBUG : $ERROR); # one of DEBUG, INFO, WARN, ERROR, FATAL

use constant DONE => 1;

use TM2::Literal;

#-- tests begin here ----------------------------

use_ok ('TM2::ObjectAble::StringsViaLucy');

if (DONE) {
    my $AGENDA = q{basic operations (no expiry): };

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my %hash;
    tie %hash, 'TM2::ObjectAble::StringsViaLucy', root => $root, create => 1;
    my $o = tied %hash;
    like ("$o", qr{TM2::ObjectAble::StringsViaLucy}, $AGENDA.'tiedness');

#--
    foreach (qw(aaa bbb ccc)) {
        ok (! exists $hash{$_}, $AGENDA."$_ does not exist");
        $hash{$_} = TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query');
        ok (  exists $hash{$_}, $AGENDA."$_ does now exist");
    }
    foreach (qw(aaa bbb ccc)) {
	is_deeply ($hash{$_}, TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query'), $AGENDA.'existing key');
    }
    is_deeply ($hash{'ddd'}, undef,                                                                               $AGENDA.'still non-existing key');
#--
    @hash{qw(ddd eee)} = ( TM2::Literal->new ('this is a text ddd', 'urn:x-mime:text/query'),
			   TM2::Literal->new ('this is a text eee', 'urn:x-mime:text/query'));
    foreach (qw(aaa bbb ccc ddd eee)) {
	is_deeply ($hash{$_}, TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query'), $AGENDA.'existing key');
    }
#--
    ok ($o->can('SLICEABLE'), $AGENDA.'sliceable');
    $o->STORE_SLICE ({ 'ddd' => TM2::Literal->new ('this is a text2 ddd'),
		       'eee' => TM2::Literal->new ('this is a text2 eee') });
    foreach (qw(aaa bbb ccc)) {
	is_deeply ($hash{$_}, TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query'), $AGENDA.'existing key');
    }
    foreach (qw(ddd eee)) {
	is_deeply ($hash{$_}, TM2::Literal->new ("this is a text2 $_", 'urn:x-mime:text/query'), $AGENDA.'existing, modified key');
    }

#-- delete
    delete $hash{'aaa'};
    is_deeply ($hash{'aaa'}, undef,                                                                                 'deleted key');
    foreach (qw(bbb ccc)) {
	is_deeply ($hash{$_}, TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query'), $AGENDA.'existing key');
    }
    foreach (qw(ddd eee)) {
	is_deeply ($hash{$_}, TM2::Literal->new ("this is a text2 $_", 'urn:x-mime:text/query'), $AGENDA.'existing key');
    }
#--
    $o->DELETE_LIST (qw(ccc eee));
    is_deeply ($hash{'ccc'}, undef,                                                                                 'deleted key');
    is_deeply ($hash{'eee'}, undef,                                                                                 'deleted key');
    is_deeply ($hash{bbb}, TM2::Literal->new ("this is a text bbb",  'urn:x-mime:text/query'), $AGENDA.'existing key');
    is_deeply ($hash{ddd}, TM2::Literal->new ("this is a text2 ddd", 'urn:x-mime:text/query'), $AGENDA.'existing key');


#-- iteration
    my @tids;
    while (my ($tid, $value) = each %hash) {
	push @tids, $tid;
    }
    ok (eq_set (\@tids, [ qw(bbb ddd) ]), 'iteration');
#-- inverse FETCH
    ok (eq_set ([ $o->HCTEF (TM2::Literal->new ('this', 'urn:x-mime:text/query')) ],
                [ qw(bbb ddd) ]), 'all hits');

    foreach (qw(bbb ddd)) {
        ok (eq_set ([ $o->HCTEF (TM2::Literal->new ($_, 'urn:x-mime:text/query')) ],
                    [ $_ ]), 'one hit');
    }

#-- continuing
    foreach (qw(ddd eee)) {
        $hash{$_} = TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query');
    }
    ok (eq_set ([ $o->HCTEF (TM2::Literal->new ('this', 'urn:x-mime:text/query')) ],
                [ qw(bbb ddd eee) ]), 'all hits');
    foreach (qw(bbb eee)) {
        ok (eq_set ([ $o->HCTEF (TM2::Literal->new ($_, 'urn:x-mime:text/query')) ],
                    [ $_ ]), 'one hit');
    }
    foreach (qw(ddd)) {
        ok (eq_set ([ $o->HCTEF (TM2::Literal->new ($_, 'urn:x-mime:text/query')) ],
                    [ $_]), 'still one hit');
    }
}

if (DONE) {
    my $AGENDA = q{timed expiry: };

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my %hash;
    tie %hash, 'TM2::ObjectAble::StringsViaLucy', root => $root, create => 1, expiry => 3;
    my $o = tied %hash;
    is ($o->{expiry}, 3, $AGENDA.'trivial option');

#--
    $hash{'aaa'} = TM2::Literal->new ('aaa', 'urn:x-mime:text/query');
    ok (exists $hash{'aaa'}, $AGENDA.'immediate recovery');
    is_deeply ($hash{'aaa'}, TM2::Literal->new ('aaa', 'urn:x-mime:text/query'), $AGENDA.'immediate recovery');
    sleep 1;
    ok (exists $hash{'aaa'}, $AGENDA.'immediate recovery');
    is_deeply ($hash{'aaa'}, TM2::Literal->new ('aaa', 'urn:x-mime:text/query'), $AGENDA.'immediate recovery');
    sleep 3;
    ok (! exists $hash{'aaa'}, $AGENDA.'after expiry');
    is ($hash{'aaa'}, undef, $AGENDA.'undef after expiry');
#-- overwrite
    $hash{'aaa'} = TM2::Literal->new ('aaa', 'urn:x-mime:text/query');
    ok (exists $hash{'aaa'}, $AGENDA.'immediate recovery');
    is_deeply ($hash{'aaa'}, TM2::Literal->new ('aaa', 'urn:x-mime:text/query'), $AGENDA.'immediate recovery');
    sleep 2;
    $hash{'aaa'} = TM2::Literal->new ('bbb', 'urn:x-mime:text/query');
    ok (exists $hash{'aaa'}, $AGENDA.'immediate recovery, after update');
    is_deeply ($hash{'aaa'}, TM2::Literal->new ('bbb', 'urn:x-mime:text/query'), $AGENDA.'immediate recovery, after update');
    sleep 2;
    ok (exists $hash{'aaa'}, $AGENDA.'immediate recovery, before timeout');
    is_deeply ($hash{'aaa'}, TM2::Literal->new ('bbb', 'urn:x-mime:text/query'), $AGENDA.'before timeout');
    sleep 2;
    ok (! exists $hash{'aaa'}, $AGENDA.'after expiry');
    is ($hash{'aaa'}, undef, $AGENDA.'undef after expiry');
#-- slices
    $o->STORE_SLICE ({ 'ddd' => TM2::Literal->new ('ddd'),
		       'eee' => TM2::Literal->new ('eee') });
    is_deeply ($hash{$_}, TM2::Literal->new ("$_", 'urn:x-mime:text/query'), $AGENDA.'slice before expiry') for qw(ddd eee);
    sleep 4;
    ok (! exists $hash{$_}, $AGENDA.'slice after expiry') for qw(ddd eee);
#-- iteration
    $hash{'aaa'} = TM2::Literal->new ('aaa', 'urn:x-mime:text/query');
    sleep 1;
    $hash{'bbb'} = TM2::Literal->new ('bbb', 'urn:x-mime:text/query');
    sleep 2;
    $hash{'ccc'} = TM2::Literal->new ('ccc', 'urn:x-mime:text/query');
    $hash{'ddd'} = TM2::Literal->new ('ddd', 'urn:x-mime:text/query');
    $hash{'eee'} = TM2::Literal->new ('eee', 'urn:x-mime:text/query');
    sleep 2;
    diag ("aaa, bbb expired");
    my (@tids, @values);
    while (my ($tid, $value) = each %hash) {
	push @tids, $tid;
	push @values, $value->[0];
    }
    ok (eq_set (\@tids, [ qw(ccc ddd eee) ]), $AGENDA.'iteration');
    ok (eq_set (\@values, [ qw(ccc ddd eee) ]), $AGENDA.'iteration');
}

if (DONE) { # tm identifiers

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my %hash;
    tie %hash, 'TM2::ObjectAble::StringsViaLucy', root => $root, create => 1;
    my $o = tied %hash;

    is ($hash{'tm:whatever'}, undef, 'index does not exist yet');
#--
    foreach (qw(aaa-xxx bbb-34 ccc_d)) {
        $hash{"tm:$_"} = TM2::Literal->new ("this is a text $_", 'urn:x-mime:text/query');
    }

    is_deeply ($hash{'tm:aaa-xxx'}, TM2::Literal->new ('this is a text aaa-xxx', 'urn:x-mime:text/query') , 'existing key');
    is_deeply ($hash{'tm:ddd'},     undef,                                                                  'non-existing key');

    ok (eq_set ([ $o->HCTEF (TM2::Literal->new ('this', 'urn:x-mime:text/query')) ],
                [ qw(tm:aaa-xxx tm:bbb-34 tm:ccc_d) ]), 'all hits');

    foreach (qw(aaa-xxx bbb-34 ccc_d)) {
        ok (eq_set ([ $o->HCTEF (TM2::Literal->new ($_, 'urn:x-mime:text/query')) ],
                    [ "tm:$_" ]), 'one hit');
    }
}

done_testing;

__END__
