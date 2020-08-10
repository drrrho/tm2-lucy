use strict;
use warnings;
use utf8;

#use Devel::NYTProf;
#DB::disable_profile();

use Test::More;
use Test::Exception;
use Test::Moose;
use Data::Dumper;
use HTTP::Status qw(:constants);
use JSON;

#use Carp;
#$SIG{ __DIE__ } = sub { return; Carp::confess( @_ ) };

use HTTP::Request::Common (); # qw/GET POST PUT DELETE/;

my $warn = shift @ARGV;

unless ($warn) {
    ok (1, 'we do not perform any tests');
    done_testing;
    exit;
}

unless ($warn) {
    close STDERR;
    open (STDERR, ">/dev/null");
    select (STDERR); $| = 1;
}

use constant DONE    => 1;
use constant STRESS  => 1;
use constant PROFILE => 0;

use TM2; # to see the $log
use Log::Log4perl::Level;
$TM2::log->level($warn ? $DEBUG : $ERROR); # one of DEBUG, INFO, WARN, ERROR, FATAL

use TM2::Literal;
use TM2::TempleScript;
use TM2::TS::Test;

#--

unshift  @TM2::TempleScript::Parser::TS_PATHS, '../templescript/ontologies/';
$TM2::TempleScript::Parser::UR_PATH = '../templescript/ontologies/';

use TM2::Materialized::TempleScript;
my $env = TM2::Materialized::TempleScript->new (
              file    => $TM2::TempleScript::Parser::UR_PATH . 'env.ts',             # then the general processing map (aka environment)
              baseuri => 'ts:')
        ->extend ('TM2::ObjectAble')
        ->extend ('TM2::ImplementAble')
        ->extend ('TM2::TriggerAble')
        ->sync_in;

sub _mk_ctx {
    my $stm = shift;
    return [ { '$_'  => $stm, '$__' => $stm } ];
}

#-- tests -------------------------------------------------------------------------------------

use_ok ('TM2::RESTfulNG');

if (STRESS) {
    my $AGENDA = q{indexing (via single hash assignment) and serving: }; diag $AGENDA;

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    use TM2::Materialized::TempleScript;
    my $tm = new TM2::Materialized::TempleScript (baseuri => 'tm:',
						  inline  => q{

%include file:web.ts

%include file:ontologies/lucy.atm

%include file:restdaemon.ts

resty isa rest:daemon
   = http://127.0.0.1:8888/
! TestServerToken

#--

isa storage @ lang:perl :
   class       : fulltext-query
   ts:software : urn:x-perl:TM2::ObjectAble::StringsViaLucy

}.qq{
fulltext-query
  ~ urn:x-mime:text/query
lucy:root          : $root
lucy:auto-create   : 1
lucy:unique-tid    : 1 # at every STORE we look whether that tid is there and is to be deleted
lucy:auto-optimize : 1 # at every STORE(_SLICE) we optimize

}.q{
§ isa ts:converter
    ts:mimetype @ ts:input  : "text/plain"
    ts:mimetype @ ts:output : "text/query"
return """
   return TM2::Literal->new ($_[0]->[0], 'urn:x-mime:text/query');
""" ^^ lang:perl !

§ isa ts:converter
    ts:mimetype @ ts:input  : "http/uri"
    ts:mimetype @ ts:output : "application/json"
return """
    my $http = TM2::TempleScript::PE::lookup_var ($cs, '$http');
    my $resp = $http->do_request(
           request => HTTP::Request::Common::GET ($_[0], 'Accept' => 'application/json'),
           )->get;

#    my $loop = TM2::TempleScript::PE::lookup_var ($cs, '$loop');
#    use Data::Dumper;
#    warn "after get ".(scalar $loop->notifiers); # .Dumper [ map { "$_" } $loop->notifiers ];
    return TM2::Literal->new ($resp->content, 'urn:x-mime:application/json');
#    return TM2::Literal->new ('[[whatever]]', 'urn:x-mime:application/json');
""" ^^ lang:perl !

generate-data isa ts:function
return """
    # my @c = ('a' .. 'b');
    my @c = ('a' .. 'z', 'A' .. 'O');

    my %h;
    foreach my $c1 (@c) {
        foreach my $c2 (@c) {
            foreach my $c3 (@c) {
                $h{'tm:'.$c1.$c2.$c3} = TM2::Literal->new (uc ("$c1$c2$c3")." is a ".(rand > 0.5 ? 'un' : '')."nice person", 'urn:x-mime:text/query');
            }
        }
    }
    use Test::More;
    $TM2::log->debug ("generated: ".scalar keys %h);
    return bless \%h, 'templescript/hash;mime=text/query';
""" ^^ lang:perl !

mappy isa topic-map
holds { }

#--

q1 isa rest:queryable
       isa web:resource
~ urn:x-regurl:search\?q=(?<q>.+)
return
  -{
    mappy ~[templescript/map]~> => $_
    |><|
    ( $user:q  <~[text/query]~ ??? "no result")
   }-

query-stream isa ts:stream
return
    <+ every 2 sec +>
# |  ("AAA", "nice", "unnice", "rumsti") | zigzag
  |  ("AAA")
  | ( "{ resty = }search?q={$0}" ^^ xsd:anyURI )
  | ( ( $0 ~[ application/json ]~> ) !! ( "cannot connect anymore" ) )
  |->> io:write2log

indexing-stream isa ts:stream
return
  -{ count | (1, re-index-full ~[templescript/query]~>) |->> ts:fusion (ts:forks) => $f
     ||><||
        <+ every 20 secs +>
     |  @ $f
     |->> io:write2log
   }-

re-index-full isa ts:function
return
   -{
      mappy ~[templescript/map]~> => $_
      |><|
                                                        ("=========================== indexing ...")        |->> io:write2log
      |->> generate-data
      |->> ts:hash2block
      | ( $1  ==> <~[text/query]~ $0 )
      | count |                                         ("=========================== indexing { $0 } done")|->> io:write2log
    }-| ->> io:write2log

%cancel

# indexing-stream isa ts:streamx
# return
#     <++ every 10 secs ++>
#  |-{
#       mappy ~[templescript/map]~> => $_
#     |><|
# # | ->> re-index-add-all
# # | ->> re-index-non-existing
# #    ->> re-index-full @ ts:forks
#     ->> re-index-xxx # @ ts:forks
#    }-|->> io:write2log

# indexing-stream isa ts:stream
# return
#    <+ every 15 secs +>
# |  "===========> indexing outside" |->> io:write2log
# | @ $f
# |  "===========> indexing done outside" |->> io:write2log

# indexing-stream isa ts:stream
# return
#    <+ every 15 secs +>
# |  "===========> indexing outside" |->> io:write2log
# |-{ 
#     |><|
#     <<- now | @ $f }-
# |  "===========> indexing done outside" |->> io:write2log

#--

re-index-add-hash isa ts:function
return
     ( $0  ++> <~~ $_ )

re-index-non-existing isa ts:function
return
      ->> ts:hash2block
# |_10000_
     | ( $1  ||> <~[text/query]~ $0 )


%log all consumed

})
        ->extend ('TM2::ObjectAble')
        ->extend ('TM2::TriggerAble')
        ->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->extend ('TM2::CacheAble::mime')
	->sync_in
	;
    require_ok ('TM2::ObjectAble::StringsViaLucy');

    use TM2::TempleScript::Stacked;
    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    use IO::Async::Loop;
    my $loop = IO::Async::Loop->new;

    use Net::Async::HTTP;
    my $http = Net::Async::HTTP->new();
    $loop->add( $http );

#    my (undef, $f_object) = %{ $tm->object ('templescript/query', 'tm:re-index-full') };
#    use TM2::TS::Stream::forking::factory;
#    my $f = TM2::TS::Stream::forking::factory->new ([1], $f_object->{'templescript/query'}, loop => $loop, ctx => $ctx );

    $ctx = [ { '$loop' => $loop, '$http' => $http, 
#	       '$f' => $f 
             }, @$ctx ];

    diag "start timing ...";
    my $ts;
    {
	(my $ss, $ts) = $tm->execute ($ctx);
	$loop->watch_time( after => 145, code => sub { diag "stopping map"     if $warn; push @$ss, bless [], 'ts:collapse'; } );
#	$loop->watch_time( after => 45, code => sub { diag "stopping process" if $warn; $f->DEMOLISH } );
    }
    $loop->watch_time( after => 147, code => sub { diag "stopping loop" if $warn; $loop->stop; } );

#    $TM2::log->level($ERROR);
    my $error;
#     use Benchmark qw(:all) ;
#     timethis (5000, sub {
#         use HTTP::Request::Common;
#         my $resp = $http->do_request(
#             request => HTTP::Request::Common::GET ('http://localhost:8888/search?q=nice', 'Accept' => 'application/json'),
#             )->get;
# 	print $resp->code         == HTTP_OK ? '.' : '-';
# 	# $error = 1 unless $resp->code         == HTTP_OK;
# 	# $error = 1 unless $resp->content      eq 'xxx';
# 	# $error = 1 unless $resp->content_type eq 'text/plain';
# warn Dumper $resp;
# 	      });
#    ok (! defined $error, $AGENDA.'results');
    $loop->run;
}

done_testing;

__END__

#== special to dynamic pages ==================================================================

if (DONE) {
    my $AGENDA = q{DP, structural ng, daemon -> stream: };

    my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						   inline => '

%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

aaa isa web:resource
    isa rest:proxyable
= http://aaa/

resty isa rest:daemon
   = http://localhost:8888/
! TestServerToken

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::Executable')
	->extend ('TM2::ImplementAble')
	->sync_in
	;

    does_ok ($tm, 'TM2::RESTfulNG::DynamicPages');

    my @streams = $tm->instances  ('ts:stream');
    is ((scalar @streams), 1, $AGENDA.'one generated stream');
    my (undef, $vv) = %{ $tm->object (undef, $streams[0]) };
    my ($mime, $v) = %$vv;
    isa_ok ($v,    'PE',             $AGENDA.'stream PE');
    is ($mime, 'templescript/query', $AGENDA.'MIME for generated stream');
}

if (DONE) {
    my $AGENDA = q{DP, routes, structural: };

    my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						   inline => '
%include file:ontologies/web.atm

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss)

index1 isa web:resource
       isa rest:indexable

index2 isa web:resource
       isa rest:indexable
~ urn:x-regurl:whole-map/(?<INDEX>\d+)

index3

something isa web:resource

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::Executable')
	->extend ('TM2::ImplementAble')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss = [];
    unshift @$ctx, { '$tss' => $tss } ;
    my $cpr = $tm->main_stream ($ctx);
#warn $cpr->toString;
    isa_ok ($cpr, 'PEprj', $AGENDA.'top-level');

#--
    @$tss = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    is_singleton ($tss, undef, $AGENDA.'singleton route');
    my $routes = $tss->[0]->[0];
    ok (eq_set ([ map { "$_" } map { $_->[0] } @$routes ],
		[ '(?^:whole-map/(?<INDEX>\\d+))', '(?^:index1)' ]), $AGENDA.'index uris');
    map { is ($_->[1], 'rest:indexable', $AGENDA.'type') } @$routes;
    map { is ($_->[2], 'rest:non-negotiable', $AGENDA.'nego') } @$routes;
    map { is ($_->[4], undef, $AGENDA.'code') } @$routes;
#warn Dumper $routes; exit;

#--
    my ($params, $re, $route, $nego, $tid, $code) = TM2::RESTfulNG::DynamicPages::route ($routes, 'index1');
    is ($route, 'rest:indexable', $AGENDA.'route computed');
    is ($tid,   'tm:index1',      $AGENDA.'tid computed');
    isa_ok ($params, 'HASH');

    throws_ok {
	TM2::RESTfulNG::DynamicPages::route ($routes, 'index2');
    } qr/no route/, $AGENDA.'no route';
#--
    ($params, $re, $route, $nego, $tid, $code) = TM2::RESTfulNG::DynamicPages::route ($routes, 'whole-map/3');
    is ($route, 'rest:indexable', $AGENDA.'route computed');
    is ($tid,   'tm:index2',      $AGENDA.'tid computed');
    isa_ok ($params, 'HASH');
}

if (DONE) {
    my $AGENDA = q{DP, structural nng, feasibility: };

    my $tm = TM2::Materialized::TempleScript->new  (baseuri => 'tm:',
						    inline => '

%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

index1

response-ng isa ts:function
return
   (      ->> rest:dp-route
      | ( ->> handle-index )                       if [ $1 = rest:indexable ]
        ( ->> ts:panic ( "you shalt not pass" ) )  otherwise
      | ( $0  ~[ http/response ]~ )
   ) !! ( $!! ~[ http/response ]~ )

handle-index isa ts:function
return 
    ( $user:xxx )

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    my $tss;
    my $resp;
#--
    $tss = TM2::TempleScript::return ([ {
	'$req'         => ( HTTP::Request::Common::GET 'index1?xxx=44', 'Accept' => 'text/plain' ),
	'$rest:routes' => [ [ qr/index1/, 'rest:indexable', 'rest:non-negotiable', 'tm:index1'  ] ]
					},
					@$ctx ],
				      q{ ( $req ) |->> response-ng    });
	$resp = $tss->[0]->[0];
#warn Dumper $resp; exit;
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'http resp worked');
	is ($resp->content, '44', $AGENDA.'http content');
	is ($resp->content_type, 'application/json', $AGENDA.'mime');
#--
    $tss = TM2::TempleScript::return ([ {
	'$req'         => ( HTTP::Request::Common::GET 'index2?xxx=44', 'Accept' => 'text/plain' ),
	'$rest:routes' => [ [ qr/index1/, 'rest:indexable', 'rest:non-negotiable', 'tm:index1' ] ]
					},
					@$ctx ],
				      q{ ( $req ) |->> response-ng    });
	$resp = $tss->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_NOT_FOUND, $AGENDA.'code');
	like ($resp->message, qr/no route/, $AGENDA.'message');
}

if (DONE) {
    my $AGENDA = q{indexable (staging): };

    my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						   inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response
             |->> ts:tap ($tss:result)

index1 isa rest:indexable
       isa web:resource
       isa rest:negotiable

index2 isa web:resource
       isa rest:indexable
~ urn:x-regurl:whole-map/(?<INDEX>\d+)

index3

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
		     '$ts:converters' => {},
    } ;

    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];
#warn "routes".Dumper $routes; exit;

    { #-- index call
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	my $tss = TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'         => ( HTTP::Request::Common::GET 'index1', 'Accept' => 'text/x-astma' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
#warn Dumper $tss, $tss2; exit;
	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
#warn Dumper $resp;
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	like   ($resp->content, qr/version/, $AGENDA.'serialized map');
	is     ($resp->content_type, 'text/x-astma', $AGENDA.'result mime');
    }

    { #-- index call, mime problem
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	my $tss = TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'         => ( HTTP::Request::Common::GET 'index1', 'Accept' => 'text/xxxx-astma' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

#warn Dumper $tss, $tss2; exit;
	is ((scalar @$tss2), 1, $AGENDA.'single result');
#	is_deeply ( $tss2->[0]->[1] , TM2::Literal->new (44) , $AGENDA.'connection run thru');
	my $resp = $tss2->[0]->[0];
#warn Dumper $resp;
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_INTERNAL_SERVER_ERROR, $AGENDA.'code');
	like ($resp->message, qr/unhandled/, $AGENDA.'message');
    }
    { #-- index call, differening pattern
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	my $tss = TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'         => ( HTTP::Request::Common::GET 'whole-map/23', 'Accept' => 'text/x-astma' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

#warn Dumper $tss, $tss2; exit;
	is ((scalar @$tss2), 1, $AGENDA.'single result');
#	is_deeply ( $tss2->[0]->[1] , TM2::Literal->new (44) , $AGENDA.'connection run thru');
	my $resp = $tss2->[0]->[0];
#warn Dumper $resp;
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	like   ($resp->content, qr/version/, $AGENDA.'serialized map');
	is     ($resp->content_type, 'text/x-astma', $AGENDA.'result mime');
    }
    { #-- index call, mime problem
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	my $tss = TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'         => ( HTTP::Request::Common::GET 'index3', 'Accept' => 'text/x-astma' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

#warn Dumper $tss, $tss2; exit;
	is ((scalar @$tss2), 1, $AGENDA.'single result');
#	is_deeply ( $tss2->[0]->[1] , TM2::Literal->new (44) , $AGENDA.'connection run thru');
	my $resp = $tss2->[0]->[0];
#warn Dumper $resp;
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_NOT_FOUND, $AGENDA.'code');
	like ($resp->message, qr/no route/, $AGENDA.'message');
    }
}

for (1..DONE) {
    my $AGENDA = q{responsible (staging): };

    my $tm = TM2::Materialized::TempleScript->new (inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response |->> ts:tap ($tss:result)

rrr isa web:resource
    isa rest:responsible

sss isa web:resource
    isa rest:responsible

ttt isa web:resource
    isa rest:responsible
    isa rest:negotiable

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
    } ;

    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];

    my $content = "xxxx";
    $tm->objectify ('http/response', 'tm://nirvana/rrr', HTTP::Response->new (HTTP_OK, "",
									      [ "Content-Length" => length ($content),
										"Content-Type"   => 'text/plain'],
									      $content));
    my $tcpr = $tm->code_object ($tm->tids('t'));
    for (my $i = 0; $i < 5; $i++) { # correctness
	my $ctx2 = [ {
	    '$req'        => ( HTTP::Request::Common::GET 'rrr', 'Accept' => 'text/plain' ),
	    '$rest:routes' => $routes }, @$ctx ];
	@$tss2 = ();
	my $tss = TM2::TempleScript::PE::eval_cpr ($ctx2, $tcpr);
#warn Dumper $tss, $tss2;
#
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'http resp worked');
	is ($resp->content, 'xxxx', $AGENDA.'http content');
	is ($resp->content_type, 'text/plain', $AGENDA.'mime');
    }
    { # correctness
	my $ctx2 = [ {
	    '$req'        => ( HTTP::Request::Common::GET 'sss', 'Accept' => 'text/plain' ),
	    '$rest:routes' => $routes }, @$ctx ];
	@$tss2 = ();
	my $tss = TM2::TempleScript::PE::eval_cpr ($ctx2, $tcpr);
#warn Dumper $tss, $tss2;
	my $resp = $tss2->[0]->[0];
	is ($resp->code, HTTP_NO_CONTENT, $AGENDA.'http resp worked');
	like ($resp->message, qr/no content/, $AGENDA.'no content');
    }
    { #- mime type does not matter
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'rrr', 'Accept' => 'application/json' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
#warn Dumper $resp;
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'http resp worked');
	is ($resp->content, 'xxxx', $AGENDA.'http content');
	is ($resp->content_type, 'text/plain', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
    {
	local $TODO = "content negotiation with responsible resources";
	ok(0);
    }
    if (STRESS) {
	my $ctx2 = [ {
	    '$req'        => ( HTTP::Request::Common::GET 'rrr', 'Accept' => 'text/plain' ),
	    '$rest:routes' => $routes }, @$ctx ];
	my $ss = TM2::TempleScript::PE::cpr2stream ({}, {}, $ctx2, $tcpr->decanonicalize ($ctx2));
        my $ts = TM2::TS::Stream::last3 ($ss);
	use Benchmark qw(:all) ;
	timethis (1000, sub {
	    push @$ss, [ undef ];
	}, "run compiled");
	is ((scalar @$ts), 1000, "compiled run, result length");
    }

    if (STRESS) {
	my $ctx2 = [ {
	    '$req'        => ( HTTP::Request::Common::GET 'rrr', 'Accept' => 'text/plain' ),
	    '$rest:routes' => $routes }, @$ctx ];
	use Benchmark qw(:all) ;
	@$tss2 = ();
	timethis (1000, sub {
	    my $tss = TM2::TempleScript::PE::eval_cpr ($ctx2, $tcpr);
	}, "build/run/teardown");
	is ((scalar @$tss2), 1000, "compile/run/teardown run, result length");
    }

}

if (DONE) {
    my $AGENDA = q{proxyable (staging): };

    my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						   inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response |->> ts:tap ($tss:result)

proxy1 isa web:resource
       isa rest:proxyable
       isa rest:negotiable
= http://whatever

proxy2 isa web:resource
       isa rest:proxyable

proxy3 isa web:resource
       isa rest:proxyable
= http://nothing

proxy4 isa web:resource
       isa rest:deep-proxyable
= file://whatever/
~ urn:x-regurl:site/(?<path>.+)

proxy5 isa web:site
       isa rest:proxyable
       isa rest:negotiable
= file://whatever/
~ urn:x-regurl:site2/(?<path>.+)

proxy6 isa web:resource
       isa rest:proxyable
= file://whatever/

xxx1 isa ts:converter
ts:mimetype @ ts:input  : "http/uri"
ts:mimetype @ ts:output : "http/response"
returns """
    my $uri = $_[0];
#warn "local resolve $uri";
    my ($content, $mime);
    if ($uri eq q|http://nothing|) {
       return []; # die "no content at $uri";
    } elsif ($uri =~ qr{file://whatever/(.+)}) {
       ($content, $mime) = ($1, q|image/png|);
    } else {
       ($content, $mime) = (q|xxxx|, q|image/png|);
    }
  use HTTP::Status qw(:constants);
  use HTTP::Response;
  return HTTP::Response->new (HTTP_OK, "",
     	    			[ "Content-Length" => length ($content),
 				  "Content-Type"   => $mime ],
				$content);
""" ^^ lang:perl !

xxx2 isa ts:converter
ts:mimetype @ ts:input  : "http/uri"
ts:mimetype @ ts:output : "*/*"
returns """
    my $uri = $_[0];
#warn "local resolve $uri";
    my ($content, $mime);
    if ($uri eq q|http://nothing|) {
       return []; # die "no content at $uri";
    } elsif ($uri =~ qr{file://whatever/(.+)}) {
       ($content, $mime) = ($1, q|image/png|);
    } else {
       ($content, $mime) = (q|xxxx|, q|image/png|);
    }
  return TM2::Literal->new ($content, "urn:x-mime:$mime");
""" ^^ lang:perl !

yyy isa ts:converter
  ts:mimetype @ ts:input  : "image/*"
  ts:mimetype @ ts:output : "http/response"
return """
  my $content = $_[0];
  my $mime    = $_[1];
# warn "to http response $content with mime $mime";
# $content = $content->[0];
# warn "content $content mime >>$mime<<";
  use HTTP::Status qw(:constants);
  use HTTP::Response;
  return HTTP::Response->new (HTTP_OK, "",
     	    			[ "Content-Length" => length ($content->[0]),
 				  "Content-Type"   => $mime ],
				$content->[0]);
""" ^^ lang:perl !

zzz isa ts:converter
ts:mimetype @ ts:input  : "image/png"
ts:mimetype @ ts:output : "image/jpeg"
returns """
    return TM2::Literal->new (q|yyyy|, q|urn:x-mime:image/jpeg|);
""" ^^ lang:perl !

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
		     '$ts:converters' => {},
		     '$mime:pipes' => {},
    } ;

    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];


    if (STRESS) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	my $ctx2 = [ {
	    '$req'        => ( HTTP::Request::Common::GET 'site/longer/path', 'Accept' => 'image/png' ),
	    '$rest:routes' => $routes }, @$ctx ];
	my ($ss, $ts) = TM2::TempleScript::PE::pe2pipe ($ctx2, $tcpr);
	@$ts = ();
	my $N = 5000;
	diag "stress starts ...";
	use Benchmark qw(:all) ;
	
	timethis ($N, sub {
	    push @$ss, [ undef ];
	}, "run compiled");
#warn Dumper $ts;
	if (0) { # just double checking
	    is ((scalar @$ts), $N, "compiled run, result length");
	    foreach (@$ts) {
		my $resp = $_->[0];
		isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
		is ($resp->code, HTTP_OK, $AGENDA.'proxy does exist, there is address URL');
		is ($resp->content, 'longer/path', $AGENDA.'proxy result');
		is ($resp->content_type, 'image/png', $AGENDA.'mime');
	    }
	}
	exit;
    }
    if (PROFILE) {
	diag "profiling ....";
	my $tcpr = $tm->code_object ($tm->tids('t'));
#warn $tcpr->toString; exit;
	use IO::Async::Loop;
	my $loop = IO::Async::Loop->new;

	my $stats = {};
	my $ctx2 = [ {
	    '$req'        => ( HTTP::Request::Common::GET 'site/longer/path', 'Accept' => 'image/png' ),
	    '$rest:routes' => $routes,
	    '$loop' => $loop,
	    '$stats:counters' => $stats,
		     }, @$ctx ];

	{
	    my ($ss, $ts) = TM2::TempleScript::PE::eval_cpr_async ($ctx2, $tcpr);
#warn "final $ts";
	    @$ts = ();
	    my $N = 1000;
#	    require_ok ('Devel::NYTProf');
#	    DB::enable_profile();
	    foreach (1..$N) {
		push @$ss, [ undef ];
	    }
#	    DB::disable_profile();
#warn Dumper $ts;
	    if (0) { # just double checking
		is ((scalar @$ts), $N, "compiled run, result length");
		foreach (@$ts) {
		    my $resp = $_->[0];
		    isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
		    is ($resp->code, HTTP_OK, $AGENDA.'proxy does exist, there is address URL');
		    is ($resp->content, 'longer/path', $AGENDA.'proxy result');
		    is ($resp->content_type, 'image/png', $AGENDA.'mime');
		}
	    }
#	    warn Dumper $stats;
	    if (1) {
		use Image::Magick;
		my $output = Image::Magick->new(magick=>'png');
		use TM2::TS::Profiler;
		my $r;
		$r = $output->BlobToImage( TM2::TS::Profiler::profile2png ($stats) );    $TM2::log->warn ($r) if $r;
		$r = $output->Write('/tmp/xxx.png');                                     $TM2::log->warn ($r) if $r;
#		$r = $output->Display();                                                 $TM2::log->warn ($r) if $r;
	    }
#warn Dumper $ls;
	}
	diag "profiling done";
	exit;
    }

#-- proxy call
    for (1..5) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy1', 'Accept' => 'image/png' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'proxy does exist, there is address URL');
	is ($resp->content, 'xxxx', $AGENDA.'proxy result');
	is ($resp->content_type, 'image/png', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
#-- proxy call, negotiation
    for (1..5) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy1', 'Accept' => 'image/jpeg' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'proxy does exist, there is address URL');
	is ($resp->content, 'yyyy', $AGENDA.'proxy result');
	is ($resp->content_type, 'image/jpeg', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
#-- proxy call, broken negotiation
    for (1..5) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy1', 'Accept' => 'image/xxx' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_INTERNAL_SERVER_ERROR, $AGENDA.'code');
	like ($resp->message, qr/no.+converter/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
#-- proxy without address
    for (1..5)  {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy2', 'Accept' => 'image/png' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_INTERNAL_SERVER_ERROR, $AGENDA.'code');
	like ($resp->message, qr/no subject/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
#-- proxy call, no content
    for (1..5)  {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy3', 'Accept' => 'image/png' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_INTERNAL_SERVER_ERROR, $AGENDA.'code');
	like ($resp->message, qr/no.+ content/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
#-- proxy call, no nego
    for (1..5)  {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy6', 'Accept' => 'image/jpeg' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->content, 'xxxx', $AGENDA.'proxy result');
	is ($resp->content_type, 'image/png', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
#-- deep proxy call, non-neg
    for (1..5) {
#warn Dumper $routes;
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'site/longer/path', 'Accept' => 'image/png' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result, deep');
	is ($resp->code, HTTP_OK, $AGENDA.'proxy does exist, there is address URL');
	is ($resp->content, 'longer/path', $AGENDA.'proxy result');
#warn Dumper $tss2; exit;
    }
    for (1..5) { #-- deep proxy call, with convenience, nego
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'site2/longer/path2', 'Accept' => 'image/jpeg' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result, deep');
	is ($resp->code, HTTP_OK, $AGENDA.'proxy does exist, there is address URL');
	is ($resp->content, 'yyyy', $AGENDA.'proxy result');
#warn Dumper $tss2; exit;
    }
}

if (DONE) {
    my $AGENDA = q{drillable (staging): };

    my $tm = TM2::Materialized::TempleScript->new (baseur => 1,
						   inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response |->> ts:tap ($tss:result)

drill1 isa rest:drillable
       isa web:resource

drill2 isa rest:drillable
       isa web:resource
       isa rest:negotiable
holds
    43
holds
    "44" ^^ xsd:string

xxx isa ts:converter
ts:mimetype @ ts:input  : "text/plain"
ts:mimetype @ ts:output : "image/jpeg"
returns """
    return TM2::Literal->new (q|yyyy|, q|urn:x-mime:image/jpeg|);
""" ^^ lang:perl !

yyy isa ts:converter
  ts:mimetype @ ts:input  : "image/*"
  ts:mimetype @ ts:output : "http/response"
return """
  my $content = $_[0];
  my $mime    = $_[1];
# warn "to http response $content with mime $mime";
# $content = $content->[0];
# warn "content $content mime >>$mime<<";
  use HTTP::Status qw(:constants);
  use HTTP::Response;
  return HTTP::Response->new (HTTP_OK, "",
     	    			[ "Content-Length" => length ($content->[0]),
 				  "Content-Type"   => $mime ],
				$content->[0]);
""" ^^ lang:perl !

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
    } ;

    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];

    $tm->objectify ('templescript/literal', 'tm://nirvana/drill1', TM2::Literal->new ("xxxxx"));
    $tm->objectify ('templescript/literal', 'tm://nirvana/drill1', TM2::Literal->new (43));

#-- drill call
    if (1) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'drill1', 'Accept' => 'templescript/literal' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'drill worked');
	is ($resp->content, '43', $AGENDA.'drill result');
	is ($resp->content_type, 'application/json', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
    if (1) { # drill call with mime negotiation
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'drill2', 'Accept' => 'image/jpeg' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'drill worked');
	is ($resp->content, 'yyyy', $AGENDA.'drill result');
	is ($resp->content_type, 'image/jpeg', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
#-- drill call (intrinsic)
    if (1) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'drill2', 'Accept' => 'templescript/literal' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'drill worked');
	is ($resp->content, '43', $AGENDA.'drill result');
	is ($resp->content_type, 'application/json', $AGENDA.'mime');
#warn Dumper $tss2; exit;
#--
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'drill2', 'Accept' => 'text/plain' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	$resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is ($resp->code, HTTP_OK, $AGENDA.'drill worked');
	is ($resp->content, '44', $AGENDA.'drill result');
	is ($resp->content_type, 'text/plain', $AGENDA.'mime');
#warn Dumper $tss2; exit;
    }
}

if (DONE) {
    my $AGENDA = q{queryable (staging): };

    my $tm = new TM2::Materialized::TempleScript (baseuri => 'tm:',
						  inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response |->> ts:tap ($tss:result)

query1 isa rest:queryable
       isa web:resource
~ urn:x-regurl:add/(?<a>\d+)/(?<b>\d+)
return
    ( 7 )

query2 isa rest:singletonic
       isa web:resource
return
    ( $http:xxx )

query3 isa rest:singletonic
       isa web:resource
return
    ( $user:xxx + 1 )

query3a isa rest:singletonic
        isa web:resource
        isa rest:negotiable
return
    ( $user:xxx + 1 )

query4 isa rest:singletonic
       isa web:resource
return
    ( """
use HTTP::Status qw(:constants);
die (TM2::Web::Exception->new (HTTP_NOT_IMPLEMENTED, "lfkfsdfsd"));
""" ^^ lang:perl ! )

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
    } ;


    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];

#-- simple query call
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'add/3/4', 'Accept' => 'application/json' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');

	isa_ok ($tss2->[0]->[0], 'HTTP::Response', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content, '[[7]]', $AGENDA.'result');
	like ($tss2->[0]->[0]->content_type, qr/application/, $AGENDA.'result mime');
#warn Dumper $tss2; exit;
    }
#-- simple query call with ok parameter, no negotiation
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'query3?xxx=23', 'Accept' => 'application/json' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');
	isa_ok ($tss2->[0]->[0], 'HTTP::Response', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content, '24', $AGENDA.'result');
	like ($tss2->[0]->[0]->content_type, qr/application/, $AGENDA.'result mime');
#warn Dumper $tss2; exit;
    }
#-- simple query call with ok parameter, negotiation ignored
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'query3?xxx=24', 'Accept' => 'text/html' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');
	isa_ok ($tss2->[0]->[0], 'HTTP::Response', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content, '25', $AGENDA.'result');
	like ($tss2->[0]->[0]->content_type, qr/application/, $AGENDA.'result mime, ignored');
#warn Dumper $tss2; exit;
    }
#-- simple query call with ok parameter, negotiation failed
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'query3a?xxx=24', 'Accept' => 'text/html' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_INTERNAL_SERVER_ERROR, $AGENDA.'code');
	like ($resp->message, qr/conversion/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
#-- simple query call with triggers user exception
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'query4', 'Accept' => 'application/json' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_NOT_IMPLEMENTED, $AGENDA.'code');
	is   ($resp->message, 'lfkfsdfsd', $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
#-- simple query call with toxic parameter
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'query2?http:xxx=23', 'Accept' => 'application/json' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_BAD_REQUEST, $AGENDA.'code');
	like ($resp->message, qr/do not mess/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
#-- simple query call with triggers route exception
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'xxx', 'Accept' => 'application/json' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'response');
	is   ($resp->code, HTTP_NOT_FOUND, $AGENDA.'code');
	like ($resp->message, qr/no route/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }

#--
    if (PROFILE) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
#warn $tcpr->toString; exit;

	use IO::Async::Loop;
	my $loop = IO::Async::Loop->new;


	{
	    my ($ss, $ls) = TM2::TempleScript::PE::eval_cpr_async ([ { '$loop' => $loop, '$rest:routes' => $routes, '$uri' => 'add/3/4' }, @$ctx ], $tcpr);
	    #	$loop->watch_time( after => 15, code => sub { diag "stopping timer " if $warn; push @$ss, bless [], 'ts:collapse'; } );

	    @$tss2 = ();
	    #	use Benchmark qw(:all) ;
	    #	timethis (1000, sub {
	    DB::enable_profile();

	    foreach (1..1000) {
		push @$ss, TM2::Literal->new (1);
	    }
	    DB::disable_profile();
#warn Dumper $tss2;
	}
    }
}


if (DONE) {
    my $AGENDA = q{renderable (staging): };

    my $tm = new TM2::Materialized::TempleScript (baseuri => 'tm:',
						  inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response |->> ts:tap ($tss:result)

aaa isa bbb
    isa web:resource
    isa rest:renderable
! AAA
website : http://AAA.com

isa web:rendering:
   instance     : aaa
   web:template : xxx

ccc isa bbb
    isa web:resource
    isa rest:renderable
    isa rest:negotiable
! CCC
website : http://CCC.com

isa web:rendering:
   instance     : ccc
   web:template : xxx

xxx isa ts:tuple-function
return "XXX {{{ $web:tid / name }}} YYY {{{ $web:tid / website }}} ZZZ" ^^ lang:xml

? isa ts:converter
ts:mimetype @ ts:input  : "application/xml"
ts:mimetype @ ts:output : "text/html"
returns """
    return TM2::Literal->new ($_[0]->[0] . q|html|, q|urn:x-mime:text/html|);
""" ^^ lang:perl !


')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
		     '$ts:converters' => {},
    } ;


    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];
#warn Dumper $routes; exit;

#-- simple render access, no negotiation
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'aaa', 'Accept' => 'application/xml' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');

	isa_ok ($tss2->[0]->[0], 'HTTP::Response', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content, 'XXX AAA YYY http://AAA.com ZZZ', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content_type, 'application/xml', $AGENDA.'result mime');
#warn Dumper $tss2; exit;
    }
#-- simple render access, negotiation attempt, ignored
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'aaa', 'Accept' => 'text/html' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');

	isa_ok ($tss2->[0]->[0], 'HTTP::Response', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content, 'XXX AAA YYY http://AAA.com ZZZ', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content_type, 'application/xml', $AGENDA.'result mime');
#warn Dumper $tss2; exit;
    }
#-- simple render access, negotiation attempt, succeeds
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'ccc', 'Accept' => 'text/html' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');

	isa_ok ($tss2->[0]->[0], 'HTTP::Response', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content, 'XXX CCC YYY http://CCC.com ZZZ'.'html', $AGENDA.'result');
	is   ($tss2->[0]->[0]->content_type, 'text/html', $AGENDA.'result mime');
#warn Dumper $tss2; exit;
    }
#-- simple render access, negotiation attempt, fails
    {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'ccc', 'Accept' => 'text/xml' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);
	is ((scalar @$tss2), 1, $AGENDA.'single result');
	my $resp = $tss2->[0]->[0];
	isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
	is   ($resp->code, HTTP_INTERNAL_SERVER_ERROR, $AGENDA.'code');
	like ($resp->message, qr/no appro/, $AGENDA.'message');
#warn Dumper $tss2; exit;
    }
}

if (DONE) {
    my $AGENDA = q{rest:daemon expansion and execution: };

    my $boilerplate = q{
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

aaa isa rest:singletonic
    isa web:resource
~ urn:x-regurl:add/(?<a>\d+)/(?<b>\d+)
return
    ( $user:a + $user:b )

resty isa rest:daemon
   = http://localhost:8888/
! TestServerToken
};


    my $tm;
#--
    use TM2::Materialized::TempleScript;
    $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						inline  => $boilerplate)
        ->extend ('TM2::ObjectAble')
        ->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    use IO::Async::Loop;
    my $loop = IO::Async::Loop->new;

    $ctx = [ { '$loop' => $loop }, @$ctx ];

#-- testing structure
    my @streams = $tm->instances  ('ts:stream');
    is ((scalar @streams), 1, $AGENDA.'one generated stream');

    my (undef, $vv) = %{ $tm->object (undef, $streams[0]) };
    my ($mime, $v) = %$vv;
    isa_ok ($v,    'PE',             $AGENDA.'stream PE');
    is ($mime, 'templescript/query', $AGENDA.'MIME for generated stream');
#warn $v->toString; exit;
#    my $main = $tm->main_stream ($ctx);
#warn $main->toString; exit;
#-- testing functionality
    my $ts;
    {
        (my $ss, $ts) = $tm->execute ($ctx);
        $loop->watch_time( after => 3, code => sub { diag "stopping map" if $warn; push @$ss, bless [], 'ts:collapse'; } );
    }
    $loop->watch_time( after => 4, code => sub { diag "stopping loop" if $warn; $loop->stop; } );

    use Net::Async::HTTP;
    my $http = Net::Async::HTTP->new();
    $loop->add( $http );

    foreach my $i (1..5) {
        use HTTP::Request::Common;
        my $resp = $http->do_request(
            request => HTTP::Request::Common::GET ("http://localhost:8888/add/3/$i", 'Accept' => 'application/json'),
            )->get;
        is ($resp->code, 200, $AGENDA.'status');
        is ($resp->content, 3+$i, $AGENDA.'content');
        is ($resp->server, 'TestServerToken', $AGENDA.'header');
    }
    for (1..5) {
        use HTTP::Request::Common;
        my $resp = $http->do_request(
            request => HTTP::Request::Common::GET ('http://localhost:8888/xxx', 'Accept' => 'application/json'),
            )->get;
        is ($resp->code, 404, $AGENDA.'status');
    }
    $loop->run;
    diag "without caching done";

#-- with caching
    diag "adding caching role ... but no caching active";
    $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						inline  => $boilerplate)
        ->extend ('TM2::ObjectAble')
        ->extend ('TM2::ImplementAble')
	->extend ('TM2::CacheAble::function')
	->extend ('TM2::Executable')
	->sync_in;
	;

    $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                    TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    $ctx = [ { '$loop' => $loop }, @$ctx ];

    @$ts = ();
    {
        (my $ss, $ts) = $tm->execute ($ctx);
        $loop->watch_time( after => 3, code => sub { diag "stopping map" if $warn; push @$ss, bless [], 'ts:collapse'; } );
    }
    $loop->watch_time( after => 4, code => sub { diag "stopping loop" if $warn; $loop->stop; } );

    foreach my $i (1..5) {
        use HTTP::Request::Common;
        my $resp = $http->do_request(
            request => HTTP::Request::Common::GET ("http://localhost:8888/add/3/$i", 'Accept' => 'application/json'),
            )->get;
        is ($resp->code, 200, $AGENDA.'status');
        is ($resp->content, 3+$i, $AGENDA.'content');
        is ($resp->server, 'TestServerToken', $AGENDA.'header');
    }
    for (1..5) {
        use HTTP::Request::Common;
        my $resp = $http->do_request(
            request => HTTP::Request::Common::GET ('http://localhost:8888/xxx', 'Accept' => 'application/json'),
            )->get;
        is ($resp->code, 404, $AGENDA.'status');
    }

    $loop->run;

#-- with caching
    diag "adding caching role ... but with caching active (while ineffective)";
    $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						inline  => $boilerplate.'

bbb isa ts:function
    isa ts:cacheable
return
    (23)

%include file:chi.atm

ts:function-cache
chi:driver : Null

')
        ->extend ('TM2::ObjectAble')
        ->extend ('TM2::ImplementAble')
	->extend ('TM2::CacheAble::function')
	->sync_in
	->extend ('TM2::Executable');

    $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                    TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    $ctx = [ { '$loop' => $loop }, @$ctx ];

    @$ts = ();
    {
        (my $ss, $ts) = $tm->execute ($ctx);
        $loop->watch_time( after => 3, code => sub { diag "stopping map" if $warn; push @$ss, bless [], 'ts:collapse'; } );
    }
    $loop->watch_time( after => 4, code => sub { diag "stopping loop" if $warn; $loop->stop; } );

    foreach my $i (1..5) {
        use HTTP::Request::Common;
        my $resp = $http->do_request(
            request => HTTP::Request::Common::GET ("http://localhost:8888/add/3/$i", 'Accept' => 'application/json'),
            )->get;
        is ($resp->code, 200, $AGENDA.'status');
        is ($resp->content, 3+$i, $AGENDA.'content');
        is ($resp->server, 'TestServerToken', $AGENDA.'header');
    }
    for (1..5) {
        use HTTP::Request::Common;
        my $resp = $http->do_request(
            request => HTTP::Request::Common::GET ('http://localhost:8888/xxx', 'Accept' => 'application/json'),
            )->get;
        is ($resp->code, 404, $AGENDA.'status');
    }
    $loop->run;

}




#== bughunting ===================================================================================

our $last_before_opt;
our $last_before_stream;

our $good_before_opt;
our $good_before_stream;

if (0&&DONE) {
    my $AGENDA = q{proxyable (bughunting): };

    my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:',
						   inline => '
%include file:ontologies/web.ts

%include file:ontologies/restdaemon.ts

s isa ts:stream
return
    ( $rest:routes ) |->> ts:tap ($tss:routes)

t isa ts:function
return
    ( $req ) |->> rest:dp-response |->> ts:tap ($tss:result)

proxy1 isa web:resource
       isa rest:proxyable
       isa rest:negotiable
= http://whatever

? isa ts:converter
ts:mimetype @ ts:input  : "application/x-tm-uri"
ts:mimetype @ ts:output : "*/*"
returns """
    my $uri = $_[0];
#warn "resolve $uri";
    if ($uri eq q|http://nothing|) {
       return []; # die "no content at $uri";
    } elsif ($uri =~ qr{file://whatever/(.+)}) {
       return TM2::Literal->new ($1, q|urn:x-mime:image/png|);
    } else {
       return TM2::Literal->new (q|xxxx|, q|urn:x-mime:image/png|);
    }
""" ^^ lang:perl !

? isa ts:converter
ts:mimetype @ ts:input  : "image/png"
ts:mimetype @ ts:output : "image/jpeg"
returns """
    return TM2::Literal->new (q|yyyy|, q|urn:x-mime:image/jpeg|);
""" ^^ lang:perl !

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::Executable')
	->sync_in
	;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $tss1 = [];
    my $tss2 = [];
    unshift @$ctx, { '$tss:routes' => $tss1,
		     '$tss:result' => $tss2,
    } ;

    my $cpr = $tm->main_stream ($ctx);
    @$tss1 = ();
    TM2::TempleScript::PE::eval_cpr ($ctx, $cpr);
    my $routes = $tss1->[0]->[0];

#-- proxy call, negotiation
    for (1..10) {
	my $tcpr = $tm->code_object ($tm->tids('t'));
	@$tss2 = ();
	TM2::TempleScript::PE::eval_cpr ([ {
	    '$req'        => ( HTTP::Request::Common::GET 'proxy1', 'Accept' => 'image/jpeg' ),
	    '$rest:routes' => $routes }, @$ctx ], $tcpr);

	my $resp = $tss2->[0]->[0];
	if ($resp->content eq 'yyyy') { # ok
$good_before_opt    //= $last_before_opt;
$good_before_stream //= $last_before_stream;
	    warn "ok";
	} else { # not ok
	    warn "not ok";
# use File::Slurp;
# write_file( 'good_before_opt.log', {}, $good_before_opt ) ;
# write_file( 'last_before_opt.log', {}, $last_before_opt ) ;
# write_file( 'good_before_stream.log', {}, $good_before_stream ) ;
# write_file( 'last_before_stream.log', {}, $last_before_stream ) ;
exit;
	}

#warn Dumper $tss2; exit;
    }
}

done_testing;

__END__

{
    my $boilerplate = q{
%include file:ontologies/web.ts

%include file:ontologies/rest.atm

# %include file:ontologies/restdaemon.ts

aaa isa rest:singletonic
    isa web:resource
~ urn:x-regurl:add/(?<a>\d+)/(?<b>\d+)
return
    ( $user:a + $user:b )

resty isa rest:daemon
   = http://localhost:8888/
! TestServerToken
};

    {
	package XXX;
	use Moose::Role;

	after 'post_deserialize' => sub {
	    my $self = shift;
	    warn "XXXXXXXXXXXXXXX";
	};
	1;
    }
    {
	package YYY;
	use Moose::Role;

	after 'post_deserialize' => sub {
	    my $self = shift;
	    warn "YYYYYYYYYYYYYYYY";
	};
	1;
    }

    use TM2::Materialized::TempleScript;
    my $tm;
#-------
    $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::RESTfulNG::DynamicPages')
	->extend ('XXX')
	->extend ('YYY')
	;

    warn "map $tm";
    $tm->deserialize ($boilerplate);
#    warn "explicit post";
#    $tm->post_deserialize;
#----------------
exit;
}

# our $last_before_opt;
# our $last_before_stream;

# our $good_before_opt;
# our $good_before_stream;

#     use Benchmark qw(:all) ;
#     timethis (0000, sub {
# 	my $tcpr = $tm->code_object ($tm->tids('t'));
# 	@$tss2 = ();
# #	$last_before_opt = $last_before_stream = '';
# 	eval {
# 	    TM2::TempleScript::PE::eval_cpr ([ {
# 		'$req'        => ( HTTP::Request::Common::GET 'rrr', 'Accept' => 'text/plain' ),
# 		'$rest:routes' => 1 }, @$ctx ], $tcpr);
# 	}; if ($@) {

# 	    warn "died with '$@'";

# 	    # use Test::Differences;
# 	    # eq_or_diff $last_before_opt,    $good_before_opt,      "before opt";
# 	    # eq_or_diff $last_before_stream, $good_before_stream,   "before stream";
# use File::Slurp;
# write_file( 'good_before_opt.log', {}, $good_before_opt ) ;
# write_file( 'last_before_opt.log', {}, $last_before_opt ) ;
# write_file( 'good_before_stream.log', {}, $good_before_stream ) ;
# write_file( 'last_before_stream.log', {}, $last_before_stream ) ;

# 	    die "result ".Dumper $tss2;

# 	} elsif ((scalar @$tss2) == 1) {
# #warn $lastcpr; exit;
# warn "ok";
# #	    is ((scalar @$tss2), 1, $AGENDA.'single result');
# #	    my $resp = $tss2->[0]->[0];
# 	    #warn Dumper $resp;
# #	    isa_ok ($resp, 'HTTP::Response', $AGENDA.'result');
# #	    is ($resp->code, HTTP_OK, $AGENDA.'http resp worked');

# $good_before_opt    //= $last_before_opt;
# $good_before_stream //= $last_before_stream;

# #	    is ($resp->content, 'xxxx', $AGENDA.'http content');
# #	    is ($resp->content_type, 'text/plain', $AGENDA.'mime');

# #sleep 1;

# 	} else {
# 	    # use Test::Differences;
# 	    # eq_or_diff $last_before_opt,    $good_before_opt,      "before opt";
# 	    # eq_or_diff $last_before_stream, $good_before_stream,   "before stream";

# use File::Slurp;
# write_file( 'good_before_opt.log', {}, $good_before_opt ) ;
# write_file( 'last_before_opt.log', {}, $last_before_opt ) ;
# write_file( 'good_before_stream.log', {}, $good_before_stream ) ;
# write_file( 'last_before_stream.log', {}, $last_before_stream ) ;

# 	    die "result because of size ".Dumper $tss2;
# 	}
#     });

warn Dumper $tss;

#    my @streams = $tm->instances  ('ts:stream');
#    ok (!@streams, $AGENDA.'no stream by default');







if (0&&DONE) {
    my $AGENDA = q{raw topics: };

    use TM2::Materialized::TempleScript;
    my $tm = TM2::Materialized::TempleScript->new (inline => '
aaa

')
	->extend ('TM2::RESTfulNG');

    isa_ok  ($tm, 'TM2');
    does_ok ($tm, 'TM2::RESTfulNG');
    is ($tm->baseURL, '/', 'default baseURL');
#    ok ((grep {$_ =~ /astma/ } keys %{ $tm->formats }), 'formats defaults');

#-- with explicit
    $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

# %include file:ontologies/rest.atm

aaa                            # Get simply returns topic
return ( 42 )

bbb

')
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->sync_in
	->extend ('TM2::RESTfulNG', baseURL => q{/test/})
	;
    is ($tm->baseURL, '/test/', 'explicit baseURL');
}


__END__



    my $res = $tm->request (HTTP::Request::Common::GET '/testxxx/', 'Accept' => 'text/x-astma');
    is ($res->code, HTTP_NOT_FOUND, 'path not supported');
}

if (DONE) { # basic operation on maps
    use TM2::Materialized::TempleScript;
    my $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

aaa isa bbb

')	
	->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
        ->sync_in
        ->extend ('TM2::RESTful', baseURL => '/test/');

# GET
    my $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    is ($map->code, HTTP_OK, 'map does exist');

    like ($map->content, qr/aaa isa bbb/, 'map content back');

# PUT wrong MIME
       $map = $tm->request (HTTP::Request::Common::PUT '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-xxx', Content => 'sdsdfs');
    is ($map->code, HTTP_UNSUPPORTED_MEDIA_TYPE, 'map not accepted in this format');

# PUT invalid AsTMa
       $map = $tm->request (HTTP::Request::Common::PUT '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-astma', Content => 'xxx )');
    is ($map->code, HTTP_NOT_ACCEPTABLE, 'map not valid AsTMa');

# PUT valid AsTMa
       $map = $tm->request (HTTP::Request::Common::PUT '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-astma', Content => "xxx (yyy)\n\n");
    is ($map->code, HTTP_CREATED, 'map stored');
# reGET
       $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    is ($map->code, HTTP_OK, 'map does exist');
    like ($map->content, qr/xxx/, 'map content back');

  SKIP: {
      skip 'using storable as MIME', 2;

      $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'application/x-tmip-storable');
      is ($map->code, HTTP_OK, 'map does exist');
      use Storable qw(thaw);
#      local $Storable::Eval = sub { warn $_[0]; eval $_[0]; };
      my $tm2 = thaw ($map->content);
      ok ($tm2->mids ('xxx'), 'map content back');
    };

# PUT overwrite with broken map
       $map = $tm->request (HTTP::Request::Common::PUT '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-astma', Content => 'aaa )');
       $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    is ($map->code, HTTP_OK, 'map does exist');
    like   ($map->content, qr/xxx/, 'original map content back');
    unlike ($map->content, qr/aaa/, 'original map content back 2');

# POST with broken map
       $map = $tm->request (HTTP::Request::Common::POST '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-astma', Content => 'aaa )');
    is ($map->code, HTTP_NOT_ACCEPTABLE, 'map not valid AsTMa');

# POST with valid map
       $map = $tm->request (HTTP::Request::Common::POST '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-astma', Content => "aaa (bbb)\n\n");
    is ($map->code, HTTP_ACCEPTED, 'additional map stored');
    like ($map->headers->header('Content-Location'), qr/test/, 'Location');
       $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    like   ($map->content, qr/xxx/, 'original map content back');
    like   ($map->content, qr/aaa/, 'added    map content back');
    like   ($map->content, qr/bbb/, 'added    map content back');

# DELETE
       $map = $tm->request (HTTP::Request::Common::DELETE '/test/', 'Accept' => '*/*');
    is ($map->code, HTTP_ACCEPTED, 'map for DELETE removed');
       $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    is ($map->code, HTTP_OK, 'map for DELETE does still exist');
    unlike ($map->content, qr/xxx/, 'original map content removed');
    unlike ($map->content, qr/aaa/, 'added    map content removed');
    unlike ($map->content, qr/bbb/, 'added    map content removed');
}

if (DONE) { # multiple accept
    my $tm = new TM2::Materialized::AsTMa (inline => '
aaa (bbb)

')->sync_in;
    $tm->extend ('TM2::RESTful', baseURL => q{/test/});
#-- single accept
    my $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    like   ($map->content, qr/aaa.*bbb/s, 'added    map content back');

    $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma, text/x-astma');
    like   ($map->content, qr/aaa.*bbb/s, 'added    map content back');

}

if (DONE) { # various formats
    my $tm = new TM2::Materialized::AsTMa (inline => '
aaa (bbb)

')->sync_in;
    $tm->extend ('TM2::RESTful', baseURL => q{/test/});

    my $map;

# post AsTMa
    $map = $tm->request (HTTP::Request::Common::POST '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-astma', Content => "ccc (ddd)\n\n");
    is ($map->code, HTTP_ACCEPTED, 'fresh map stored');
    $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
    like   ($map->content, qr/aaa/, 'POST old astma    map content back');
    like   ($map->content, qr/bbb/, 'POST old astma    map content back');
    like   ($map->content, qr/ccc/, 'POST fresh astma    map content back');
    like   ($map->content, qr/ddd/, 'POST fresh astma    map content back');

# # add CTM
#     $map = $tm->request (HTTP::Request::Common::POST '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-ctm', Content => "eee isa fff .\n\n");
#     is ($map->code, HTTP_ACCEPTED, 'added map stored');
#     $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
#     like   ($map->content, qr/aaa/, 'POST old astma    map content back');
#     like   ($map->content, qr/bbb/, 'POST old astma    map content back');
#     like   ($map->content, qr/ccc/, 'POST old astma    map content back');
#     like   ($map->content, qr/ddd/, 'POST old astma    map content back');
#     like   ($map->content, qr/eee/, 'POST new ctm   map content back');
#     like   ($map->content, qr/fff/, 'POST new ctm   map content back');

# # add LTM
#     $map = $tm->request (HTTP::Request::Common::POST '/test/', 'Accept' => '*/*', 'Content-Type' => 'text/x-ltm', Content => "[ ggg: hhh ]\n\n");
#     is ($map->code, HTTP_ACCEPTED, 'added map stored');
#     $map = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => 'text/x-astma');
#     like   ($map->content, qr/aaa/, 'POST old astma    map content back');
#     like   ($map->content, qr/bbb/, 'POST old astma    map content back');
#     like   ($map->content, qr/ccc/, 'POST old astma    map content back');
#     like   ($map->content, qr/ddd/, 'POST old astma    map content back');
#     like   ($map->content, qr/eee/, 'POST old ctm   map content back');
#     like   ($map->content, qr/fff/, 'POST old ctm   map content back');
#     like   ($map->content, qr/ggg/, 'POST new ltm   map content back');
#     like   ($map->content, qr/hhh/, 'POST new ltm   map content back');

# TODO: {
#     local $TODO = 'XTM support';

#     ok (0, 'XTM shuttle');
#     }

}

if (DONE) { # basic operation on topics
    {
	package RR;
	
	use Moose;
	extends 'TM2::Materialized::TempleScript';
	with 'TM2::RESTful';
	with 'TM2::ImplementAble';
	
	sub resolver { 
	    my $self = shift;
	    my $url  = shift;
#warn "my beautiful resolver $url";
	    use HTTP::Response;
	    return HTTP::Response->new( 200, 'The World Will End', [ 'Content-Type' => 'image/png' ], 'xxxx' );
	}

	1;
    }

    my $tm = new RR (inline => '
%include file:ontologies/web.atm

aaa1                            # GET simply returns topic

aaa2
return ( 42 )

bbb1 isa web:resource           # GET /bbb1 [ .. ] => NOT_FOUND

bbb2 isa web:resource           # GET /bbb2 [ json ] => 42
return ( 42 )                   # content is only PEprs

bbb3 isa web:resource           # GET /bbb3 [ json ] => 43
return ( 42)                    # PEprs + get 43 externally into object store

bbb4 isa web:resource           # get something-xxx into the object store

bbb5 isa web:resource           # get something-xxx into the object store

bbb6 isa web:resource


ccc1 isa web:resource                             # content is PEprs => eval
return ( 44, 45 )

ccc2 isa web:resource                             # content is PEprs => eval
return ( 44 ) ++ ( 45 )

ddd1 isa web:proxyable-resource

ddd2 isa web:proxyable-resource
= $0 ~~>

ddd3 isa web:proxyable-resource
= http://whatever.com

',
		     baseURL => '/test/')
        ->extend ('TM2::ObjectAble')
	->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    my $rtopic;

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test2/aaa1', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_NOT_FOUND, 'query for aaa does not exist in wrong URI position');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa3', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_NOT_FOUND, 'topic aaa3 not found');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa1');
    is ($rtopic->code, HTTP_EXPECTATION_FAILED, 'query for aaa does not send Accept header');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa1', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic aaa1 does exist');
    use JSON;
    my $topic = from_json ($rtopic->content);
    is_deeply ($topic,
	       [
		'tm://nirvana/aaa1',
		undef,
		[]
	       ], 'toplet content');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/'.($tm->baseuri).'aaa1', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic aaa1 does exist (with baseuri)');
    $topic = from_json ($rtopic->content);
    is_deeply ($topic,
	       [
		'tm://nirvana/aaa1',
		undef,
		[]
	       ], 'toplet content');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa1', 'Accept' => 'xxx/yyy');
    is ($rtopic->code, HTTP_NOT_FOUND, 'topic aaa1 does exist, but it is no resource');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa2', 'Accept' => 'templescript/query');
    is ($rtopic->code, HTTP_NOT_FOUND, 'topic aaa2 does exist, but it is no resource');

    my $res;
#-------
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb1', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_NOT_FOUND, 'topic bbb1 does exist, but there is no object');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb2', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic bbb2 does exist, and gives results');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is ($res, 42, 'bbb2 query result');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb2', 'Accept' => 'templescript/query');
    is ($rtopic->code, HTTP_UNSUPPORTED_MEDIA_TYPE, 'topic bbb2 exists, there is a query object, but we will not serialize');
    $tm->objectify ('templescript/literal', 'tm://nirvana/bbb3', TM2::Literal->new (43));
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb3', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic bbb3 does exist, more detailled mime');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is ($res, 43, 'bbb3 object result');

    $tm->objectify ('something/something', 'tm://nirvana/bbb3', 44);
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb3', 'Accept' => 'something/something');
    is ($rtopic->code, HTTP_OK, 'topic bbb3 does exist, more detailled mime');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is ($res, 44, 'bbb3 object result');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb3', 'Accept' => 'something/*');
    is ($rtopic->code, HTTP_OK, 'topic bbb3 does exist, more detailled mime');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is ($res, 44, 'bbb3 object result');

    $tm->objectify ('ARRAY', 'tm://nirvana/bbb4', [ 21, 22, 23 ]);
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb4', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic bbb4 does exist, more detailled mime');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is_deeply ($res, [ 21, 22, 23 ], 'bbb4 object result');

    $tm->objectify ('ARRAY', 'tm://nirvana/bbb5', { aaa =>  21, bbb => 22, ccc => 23 });
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb5', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic bbb5 does exist, more detailled mime');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is_deeply ($res, { aaa =>  21, bbb => 22, ccc => 23 }, 'bbb5 object result');

    $tm->objectify (undef, 'tm://nirvana/bbb6', HTTP::Message->new ([ 'Content-Type' => 'something/else' ], 'xxxx'));
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb6', 'Accept' => 'something/else');
    is ($rtopic->code, HTTP_OK, 'topic bbb6 does exist, more detailled mime');
    is ($rtopic->content, 'xxxx', 'topic bbb6 content');
    is ($rtopic->content_type, 'something/else', 'bbb6 mime');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/bbb6', 'Accept' => 'something/*');
    is ($rtopic->code, HTTP_OK, 'topic bbb6 does exist, more detailled mime');
    is ($rtopic->content, 'xxxx', 'topic bbb6 content');
    is ($rtopic->content_type, 'something/else', 'bbb6 mime');

#-------
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/ccc1', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic ccc1 does exist, complex query');
    $res = from_json ($rtopic->content, { allow_nonref => 1 });
    is_deeply ($res, [[ 44, 45 ]], 'ccc1 object result');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/ccc2', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'topic ccc2 does exist, complex query');
    like ($rtopic->content, qr/4[45]/, 'topic ccc2 content');

#-------
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/ddd1', 'Accept' => '*/*');
    is ($rtopic->code, HTTP_SERVICE_UNAVAILABLE, 'topic ddd1 does exist, there is no address');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/ddd2', 'Accept' => '*/*');
    is ($rtopic->code, HTTP_SERVICE_UNAVAILABLE, 'topic ddd2 does exist, there is no address URL');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/ddd3', 'Accept' => '*/*');
    is ($rtopic->code, HTTP_OK, 'topic ddd3 does exist, there is address URL');
    is ($rtopic->content, 'xxxx', 'ddd3 proxy result');
#warn Dumper $rtopic;

}

if (DONE) {  #-- various object tests: image
    {
	package RR2;
	
	use Moose;
	extends 'TM2::Materialized::TempleScript';
	with 'TM2::RESTful';
	with 'TM2::ImplementAble';
	
	sub resolver { 
	    my $self = shift;
	    my $url  = shift;
#warn "my beautiful resolver $url";
	    use HTTP::Response;
	    return HTTP::Response->new( 200, 'The World Will End', [ 'Content-Type' => 'image/png' ], 'xxxx' );
	}

	1;
    }
    my $tm = new RR2 (inline => q{
%include file:ontologies/web.atm

img0

img1 isa web:resource
return <file:test.png> ~~>

img2 isa web:queryable-resource
return <file:test.png> ~~>

web:image < web:proxyable-resource .

img3 isa web:image
= file:test.png      # this needs are resolver !?

? isa ts:converter
ts:mimetype @ ts:input  : "application/x-tm-uri"
ts:mimetype @ ts:output : "*/*"
returns """
    use HTTP::Response;
    return HTTP::Response->new( 200, 'The World Will End', [ 'Content-Type' => 'image/png' ], 'xxxx' );  # this fakes a serialized image
""" ^^ lang:perl !


})
        ->extend ('TM2::ObjectAble')
        ->sync_in;
    $tm->baseURL ('/test/');

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
#    warn Dumper $tm; exit;

    {
	my $rimg = $tm->request (HTTP::Request::Common::GET '/test/img0', 'Accept' => '*/*');
	is ($rimg->code, HTTP_OK, 'img0 just a topic');
	is ($rimg->content_type, 'application/json', 'img1 JSON');
	use JSON;
	my $img = from_json ($rimg->content);
	is_deeply ($img,
		   [
		    'tm://nirvana/img0',
		    undef,
		    []
		   ], 'img0 toplet');
    }
    {
	my $rimg = $tm->request (HTTP::Request::Common::GET '/test/img1', 'Accept' => '*/*');
        is ($rimg->code, HTTP_OK, 'img1 should be an image');
	is ($rimg->content_type, 'image/png', 'img1 PNG');
	is ($rimg->content,      'xxxx', 'img1 content');
    }
    {
	my $rimg = $tm->request (HTTP::Request::Common::GET '/test/img3', 'Accept' => '*/*');
        is ($rimg->code, HTTP_OK, 'img3 should be an image');
	is ($rimg->content_type, 'image/png', 'img3 PNG');
	is ($rimg->content,      'xxxx', 'img3 content');
    }
}

if (DONE) { #-- with topic query 
    my $tm = new TM2::Materialized::TempleScript (inline => '
aaa isa xxx

bbb isa xxx

')
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::RESTful', baseURL => q{/test/})
	->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    my $rquery = $tm->request (HTTP::Request::Common::GET '/test/ aaa ~[!=878~>', 'Accept' => 'application/json');
    is ($rquery->code, HTTP_BAD_REQUEST, 'query not valid');
    like ($rquery->message, qr{not valid}, 'query still not valid');

    $rquery = $tm->request (HTTP::Request::Common::GET '/test/ aaa ~~>', 'Accept' => 'application/json');
    is ($rquery->code, HTTP_OK, 'query for aaa does exist');
#warn Dumper $rquery;
    my $res = from_json ($rquery->content);
    is_deeply ($res,  [], 'query is there, but object undef');

    $tm->objectify ('something/something', 'tm://nirvana/bbb', 43);
    $rquery = $tm->request (HTTP::Request::Common::GET '/test/ bbb ~~>', 'Accept' => 'application/json');
#warn Dumper $rquery;
    is ($rquery->code, HTTP_OK, 'query for bbb does exist');
    is_deeply (from_json ($rquery->content), [ [ 43 ] ], 'result for bbb query');

    $rquery = $tm->request (HTTP::Request::Common::GET '/test/ xxx >> instances', 'Accept' => 'application/json');
#warn Dumper $rquery;
    is ($rquery->code, HTTP_OK, 'query ok');
    $res = from_json ($rquery->content);
#warn Dumper $res;
    ok (eq_set ([ 'tm://nirvana/aaa', 'tm://nirvana/bbb' ],
		[ map { $_->[0] } @$res ]), 'query contents');
}

if (DONE) {
    {
	package Rumsti;
	use HTTP::Response;
	use HTTP::Status qw(:constants);
	
	sub request {
	    my $self = shift;
	    my $req  = shift;
	    my $path = shift;

            use Test::More;
            diag "request dispatched" if $warn;

	    my @accepts = $req->headers->header('Accept');                                             # we look at the headers, need them later
	    return undef unless grep { $_ eq 'application/json' } @accepts;

	    if ($path =~ qr{\[(?<INDEX>\d)\]}) {
#		warn Dumper \%+;
		return HTTP::Response->new (HTTP_OK, 'OK', [], $self->[ $+{INDEX} ]);
	    } else {
		return undef; # HTTP::Response->new (HTTP_BAD_REQUEST, "package Rumsti cannot interpret '$path'");
	    }
	}
	
	1;

	package Ramsti;
	use HTTP::Response;
	use HTTP::Status qw(:constants);
	
	sub request {
	    my $self = shift;
	    my $req  = shift;
	    my $path = shift;

	    if ($path =~ qr{\{(?<KEY>\w+)\}}) {
#		warn Dumper \%+;
		return HTTP::Response->new (HTTP_OK, 'OK', [], $self->{ $+{KEY} });
	    } else {
		return undef; # HTTP::Response->new (HTTP_BAD_REQUEST, "package Rumsti cannot interpret '$path'");
	    }
	}
	
	1;
    }

    my $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

isa ts:restfulness @ lang:perl :
  class : rumsti
  ts:software : urn:x-perl:Rumsti

rumsti ~ urn:x-mime:something/something

isa ts:restfulness @ lang:perl :
  class : ramsti
  ts:software : urn:x-perl:Ramsti

ramsti ~ urn:x-mime:something/else

#--

aaa isa web:resource

')
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::RESTful', baseURL => q{/test/})
	->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

#-- single object
    $tm->objectify ('something/something', 'tm://nirvana/aaa', [ 43, 44, 45 ]);
    my $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa', 'Accept' => 'something/something');
    is ($rtopic->code, HTTP_OK, 'aaa returns');
    is ($rtopic->content_type, 'application/json', 'aaa mime type');
    is_deeply ($rtopic->content, '[43,44,45]', 'aaa result');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa[2]', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'aaa[2] returns');
    is_deeply ($rtopic->content, '45', 'aaa result');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa[5]', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'aaa[5] returns');
    is_deeply ($rtopic->content, '', 'aaa result');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa[22]', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_BAD_REQUEST, 'aaa fails [22]');
    like ($rtopic->message, qr/not support/, 'fails [22]');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa[]', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_BAD_REQUEST, 'aaa fails []');
    like ($rtopic->message, qr/not support/, 'fails []');

    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa[2]', 'Accept' => 'application/xxx-json');
    is ($rtopic->code, HTTP_BAD_REQUEST, 'aaa fails xxx-json');
    like ($rtopic->message, qr/not support/, 'fails xxx-json');

#-- multiple objects
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa{xx}', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_BAD_REQUEST, 'aaa{xx} not implemented');

    $tm->objectify ('something/else', 'tm://nirvana/aaa', { xx => 43, yy => 42 });
    $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa{xx}', 'Accept' => 'application/json');
    is ($rtopic->code, HTTP_OK, 'aaa{xx} returns');
    is_deeply ($rtopic->content, '43', 'aaa result');
}

if (DONE) { # nesting
    my $tm = new TM2::Materialized::TempleScript (inline => q{
%include file:ontologies/web.atm

aaa isa web:resource
holds {
    %include file:ontologies/restful.ts

    xxx isa zzz

    yyy isa zzz

    zzz isa web:resource
    returns {
       uuu

    }

}

bbb isa web:resource  # content missing

ccc

})
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::RESTful', baseURL => q{/test/})
	->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    my $res = $tm->request (HTTP::Request::Common::GET '/test/', 'Accept' => '*/*');
    is ($res->code, HTTP_OK, '/test/ does exist');
    like ($res->content, qr/aaa /, '/test/ content back');

    $res = $tm->request (HTTP::Request::Common::GET '/test/ccc', 'Accept' => 'application/json');
    is ($res->code, HTTP_OK, 'map topic');
    is_deeply (from_json ($res->content), ["tm://nirvana/ccc",undef,[]], 'map topic data');

    $res = $tm->request (HTTP::Request::Common::GET '/test/ccc/ xxx >> instances', 'Accept' => '*/*');
    is ($res->code, HTTP_NOT_FOUND, 'toplet, but not path');
    like ($res->message, qr{not evaluatable}, 'toplet, but not path');

#--
    $res = $tm->request (HTTP::Request::Common::GET '/test/bbb', 'Accept' => 'application/json');
    is ($res->code, HTTP_NOT_FOUND, 'resource without object content');

    $res = $tm->request (HTTP::Request::Common::GET '/test/bbb/', 'Accept' => '*/*');
    is ($res->code, HTTP_NOT_FOUND, 'nested map, but no content');
    like ($res->message, qr{no object found}, 'nested map, but no content');

    $res = $tm->request (HTTP::Request::Common::GET '/test/aaa/', 'Accept' => '*/*');
#warn Dumper $res;
    is ($res->code, HTTP_OK, 'nested map');
    like ($res->content,  qr/xxx/, 'nested map content');
    like ($res->content,  qr/yyy/, 'nested map content');

    $res = $tm->request (HTTP::Request::Common::GET '/test/aaa/ zzz >> instances', 'Accept' => 'application/json');
    is ($res->code, HTTP_OK, 'nested map, query');
    ok (eq_set ([ map { $_->[0] } @{ from_json ($res->content) } ], 
		[ 'tm://nirvana/xxx' , 'tm://nirvana/yyy' ]), 'nested map, query result');

    $res = $tm->request (HTTP::Request::Common::GET '/test/aaa/xxx', 'Accept' => 'application/json');
    is ($res->code, HTTP_OK, 'submap topic');
    is_deeply (from_json ($res->content), ["tm://nirvana/xxx",undef,[]], 'submap topic data');

    $res = $tm->request (HTTP::Request::Common::GET '/test/aaa/zzz/uuu', 'Accept' => 'application/json');
    is ($res->code, HTTP_OK, 'subsubmap topic');
    is_deeply (from_json ($res->content), ["tm://nirvana/uuu",undef,[]], 'subsubmap topic data');

}

if (DONE) {
    my $AGENDA =q{nested map AND special purpose: };

    my $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

isa ts:restfulness @ lang:perl :
  class : rumsti
  ts:software : urn:x-perl:Rumsti

rumsti ~ urn:x-mime:something/something

aaa isa web:resource

')
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->sync_in
	->extend ('TM2::RESTful', baseURL => q{/test/})
        ;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    $tm->objectify ('something/something', 'tm://nirvana/aaa', [ 43, 44, 45 ]);
    $tm->objectify ('templescript/map',    'tm://nirvana/aaa', new TM2::Materialized::TempleScript (inline => "
   xxx isa zzz

   yyy isa zzz
"
		    )->extend ('TM2::ObjectAble')->extend ('TM2::ImplementAble')->extend ('TM2::RESTful')->sync_in);

    my $rtopic = $tm->request (HTTP::Request::Common::GET '/test/aaa', 'Accept' => 'something/something');
    is ($rtopic->code, HTTP_OK, 'aaa returns');
    is ($rtopic->content_type, 'application/json', 'aaa mime type');
    is_deeply ($rtopic->content, '[43,44,45]', 'aaa result');

    my $rquery = $tm->request (HTTP::Request::Common::GET '/test/aaa/ zzz >> instances', 'Accept' => 'application/json');
#warn Dumper $rquery;
    is ($rquery->code, HTTP_OK, 'query ok');
    my $res = from_json ($rquery->content);
    is ((scalar @$res), 2, 'query contents');
    ok (eq_set ([ map { $_->[0] } @$res ], [ 'tm://nirvana/xxx' , 'tm://nirvana/yyy' ]), 'query contents')
}

if (DONE) { # image rendering in implementation
    {
	package RR3;

	use Moose;
	extends 'TM2::Materialized::TempleScript';
	with 'TM2::RESTful';
	with 'TM2::ImplementAble';
	
	sub resolver { 
	    my $self = shift;
	    my $url  = shift;
	    use Test::More;
	    diag "my beautiful resolver $url" if $warn;
	    use HTTP::Response;
	    return HTTP::Response->new( 200, 'The World Will End', [ 'Content-Type' => 'image/png' ], 'xxxx' );  # this fakes a serialized image
	}

	1;

        package SpecialObject;
        use Moose;
	has 'aaa' => (is => 'ro', isa => 'Int', default => "44");
	has 'mime' => (is => 'ro', isa => 'Str', default => 'something/something');
	sub serialize {
	    return 'yyyy';
	}
	1;

	package Image::Magick;
	use Moose;
	has 'size' => (is => 'ro', isa => 'Str');
	sub ImageToBlob {
	    return 'zPNGxxxx';
	}
	sub Read {}
	1;

    }
    my $tm = new RR3 (inline => q{
%include file:ontologies/web.atm

web:image < web:proxyable-resource .

img0 isa web:image
= file:test.png

# image ~ urn:x-mime:something/something

# isa ts:serialization @ lang:perl :
#   ts:software : urn:x-perl:RestImage
#   class : image


? isa ts:converter
ts:mimetype @ ts:input  : "application/magick"
ts:mimetype @ ts:output : "image/png"
returns """
    my $image = $_[0];
#   use Image::Magick;
    my ($binary) = $image->ImageToBlob (magick=>'png');
    return new HTTP::Message ([ 'Content-Type' => 'image/png', 'Content-Length' => length ($binary) ], $binary);
""" ^^ lang:perl !

img1 isa web:resource

img2 isa web:resource

? isa ts:converter
ts:mimetype @ ts:input  : "something/something"
ts:mimetype @ ts:output : "application/json"
returns """
    my $self = $_[0];
    return new HTTP::Message ([ 'Content-Type' => 'application/json' ], $self->serialize);
""" ^^ lang:perl !

})
        ->extend ('TM2::ObjectAble')
        ->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    my $rimg = $tm->request (HTTP::Request::Common::GET '/img0', 'Accept' => '*/*');
    is ($rimg->code, HTTP_OK, 'img0 should be an image');
    is ($rimg->content_type, 'image/png', 'img0 PNG');
    is ($rimg->content,      'xxxx', 'img0 content');

#--
#   use Image::Magick;
    my $image = Image::Magick->new (size => '6x4');
    $image->Read('xc:white');

    $tm->objectify ('application/magick', 'tm://nirvana/img1', $image);
    $rimg = $tm->request (HTTP::Request::Common::GET '/img1', 'Accept' => 'image/png');

    is ($rimg->code, HTTP_OK, 'img1 converted from ImageMagick');
    is ($rimg->content_type, 'image/png', 'img1 MIME');
    like ($rimg->content, qr{^.PNG}s, 'img1 content');
    $tm->deobjectify (undef, 'tm://nirvana/img1');

#--
    $tm->objectify ('something/something', 'tm://nirvana/img1', $image);
    $rimg = $tm->request (HTTP::Request::Common::GET '/img1', 'Accept' => 'image/*');
    is ($rimg->code, HTTP_UNSUPPORTED_MEDIA_TYPE, 'img1 is not an image');

#--
    $tm->objectify ('application/magick', 'tm://nirvana/img1', $image);  # now there are 2 objects!
    $rimg = $tm->request (HTTP::Request::Common::GET '/img1', 'Accept' => 'image/*');
    is ($rimg->code, HTTP_OK, 'img1 converted from ImageMagick');
    is ($rimg->content_type, 'image/png', 'img1 MIME');
    like ($rimg->content, qr{^.PNG}s, 'img1 content');

#--
    $tm->objectify ('something/something', 'tm://nirvana/img2', SpecialObject->new);
    $rimg = $tm->request (HTTP::Request::Common::GET '/img2', 'Accept' => 'application/json');
    is ($rimg->code, HTTP_OK, 'img2 is special');
    is ($rimg->content_type, 'application/json', 'img2 mime');
    is ($rimg->content,      'yyyy', 'img2 content');
}

if (DONE) {
    my $AGENDA = q{higher context: };
    my $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

reverse isa ts:function
return
   ( $1, $0 )

echo isa web:resource
return
   ->> reverse ( $n, $m ) | ( $0 )

sleepy isa web:resource
return """
  sleep 2; return TM2::Literal->new (2);
""" ^^ lang:perl !

pushy isa web:resource
return
   ( "aaa", "bbb" ) | zigzag |->> ts:pushdown |->> ts:popup

')
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::ImplementAble')
	->extend ('TM2::RESTful')
	->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    my $res = $tm->request (HTTP::Request::Common::GET "/echo?m=5&n=3", 'Accept' => 'application/json');
    is ($res->content, 5, $AGENDA."local map");

#--
    $res = $tm->request (HTTP::Request::Common::GET "/sleepy", 'Accept' => 'application/json');
    is ($res->content, 2, $AGENDA."locally included, map");
#--
    $res = $tm->request (HTTP::Request::Common::GET "/pushy", 'Accept' => 'application/json');
    is ($res->code, 200, $AGENDA.'missing');
    is ($res->content, '[["aaa"],["bbb"]]', $AGENDA.'env map');
}

if (DONE) {
    my $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

echo isa web:resource
return
   ( $n )

echo2 isa web:resource
return
   ( $n + $m )

')
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::RESTful')
	->extend ('TM2::ImplementAble')
	->sync_in;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));

    foreach (10, 20, 30) {
        my $res = $tm->request (HTTP::Request::Common::GET "/echo?n=$_", 'Accept' => 'application/json');
#warn Dumper $res;
        is ($res->content, $_, "query parameters echo");
    }
#--
    my $res = $tm->request (HTTP::Request::Common::GET "/echo", 'Accept' => 'application/json');
    is ($res->content, '[[null]]', "echo without query parameters");
#--
    foreach (10, 20, 30) {
        my $res = $tm->request (HTTP::Request::Common::GET "/echo2?m=5&n=$_", 'Accept' => 'application/json');
#        warn Dumper $res;
        is ($res->content, $_+5, "query parameters echo2");
    }
#--
    $res = $tm->request (HTTP::Request::Common::GET "/echo2", 'Accept' => 'application/json');
    is ($res->content, 0, "echo without query parameters");
#--
    $res = $tm->request (HTTP::Request::Common::GET "/echo2?m=5", 'Accept' => 'application/json');
    is ($res->content, 5, "echo with one missing query parameters");
#--
    $res = $tm->request (HTTP::Request::Common::GET "/echo2?m=5&x=23", 'Accept' => 'application/json');
    is ($res->content, 5, "echo with one missing and one superfluous query parameters");
}

if (DONE) {
    my $AGENDA = 'routes: ';
    my $tm = new TM2::Materialized::TempleScript (inline => '
%include file:ontologies/web.atm

aaa isa web:resource
  ~ urn:x-regurl:aaa
return
   ( 1 )

bbb isa web:resource
  ~ urn:x-regurl:aaa/xxx
  ~ urn:xxx:xxx
return
   ( 2 )

ccc isa web:resource
  ~ urn:x-regurl:ddd
return
   ( 3 )

ddd isa web:resource
return
   ( 4 )

eee isa web:resource
  ~ urn:x-regurl:eee/(?<XXX>.+?)
return
   ( $XXX + 1 )

fff isa web:resource
  ~ urn:x-regurl:fff/(?<XXX>.+?)/(?<YYY>.+?)
return
   ( $XXX + $YYY )

')
        ->extend ('TM2::ObjectAble')
	->extend ('TM2::RESTful')
	->extend ('TM2::ImplementAble')
	->sync_in;

    $tm->recompute_routes;

    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm, upstream =>
                       TM2::TempleScript::Stacked->new (orig => $env)
                       ));
    my $res = $tm->request (HTTP::Request::Common::GET "/aaa", 'Accept' => 'application/json');
    is ($res->code, 200, $AGENDA.'aaa called');
    is ($res->content, '1', $AGENDA.'result');
#--
    $res = $tm->request (HTTP::Request::Common::GET "/aaa/xxx", 'Accept' => 'application/json');
    is ($res->code, 200, $AGENDA.'bbb called');
    is ($res->content, '2', $AGENDA.'result');
#--
    $res = $tm->request (HTTP::Request::Common::GET "/ddd", 'Accept' => 'application/json');
    is ($res->code, 200, $AGENDA.'ddd called');
    is ($res->content, '3', $AGENDA.'result');
#--
    $res = $tm->request (HTTP::Request::Common::GET "/eee/2/", 'Accept' => 'application/json');
    is ($res->code, 200, $AGENDA.'eee called');
    is ($res->content, '3', $AGENDA.'result');
#--
    $res = $tm->request (HTTP::Request::Common::GET "/fff/2/3", 'Accept' => 'application/json');
#warn Dumper $res->content;
    is ($res->code, 200, $AGENDA.'fff called');
    is ($res->content, '5', $AGENDA.'result');
#--
    $res = $tm->request (HTTP::Request::Common::GET "/fff/2/xxx", 'Accept' => 'application/json');
#warn Dumper $res->content;
    is ($res->code, 200, $AGENDA.'fff called with only one parameter');
    is ($res->content, '2', $AGENDA.'result');
}




#/ cotnetn-type
#/ size
# / last-mod

# use non-ex 404
# use ccc with */* -> html
#
#    my $html = $tm->request (GET '/test/ccc', 'Accept' => 'text/html');
#    is ($html->code, HTTP_OK, 'HTML topic does exist');
# check for table






__END__

