package NoMergeManager;
use base qw( Lucy::Index::IndexManager );
sub recycle { [] } # do nothing

1;

package TM2::ObjectAble::StringsViaLucy;

use strict;
use warnings;
use Data::Dumper;

use Time::HiRes qw ( time );

require Tie::Hash;
our @ISA = qw(Tie::StdHash);

my $no_merger = NoMergeManager->new;

use Lucy::Plan::Schema;
use Lucy::Plan::FullTextType;
use Lucy::Analysis::PolyAnalyzer;
use Lucy::Index::Indexer;

sub TIEHASH  {
    my $class = shift;
    my %options = @_;
#warn "TIEHASH".Dumper \%options;
    $TM2::log->logdie ("no root directory specified") unless $options{'lucy:root'};
    mkdir $options{'lucy:root'} unless -d $options{'lucy:root'};
    $options{'lucy:auto-create'}   //= 1;
    $options{'lucy:truncate'}      //= 1;
    $options{'lucy:unique-tid'}    //= 1; # by default, we are doing it (slower, but safe)
    $options{'lucy:auto-optimize'} //= 1; # by default, we are doing it (slower, but safe)
    $options{'lucy:num-wanted'}   //= 50; # by default, we are doing it (slower, but safe)

    $options{'lucy:expiry'}          = $options{'lucy:expiry'}->factor
	if ref ($options{'lucy:expiry'}) eq 'TM2::Literal::Unit';
    # otherwise kept undef => infinite expiry

    my $schema   = Lucy::Plan::Schema->new;
    my $polyanalyzer = Lucy::Analysis::PolyAnalyzer->new(language => 'en');
    my $texttype = Lucy::Plan::FullTextType->new(analyzer => $polyanalyzer);
    my $timetype = Lucy::Plan::BlobType->new( stored => 1 );

    $schema->spec_field( name => 'tid',     type => $texttype );
    $schema->spec_field( name => 'content', type => $texttype );
    $schema->spec_field( name => 'lastmod', type => $timetype );

    $options{schema} = $schema;

    { # just create it, maybe
	my $indexer = Lucy::Index::Indexer->new(
	    index      => $options{'lucy:root'},
	    schema     => $options{schema},
	    create     => $options{'lucy:auto-create'},
	    truncate   => $options{'lucy:truncate'},
	    );
    }

    return bless \%options, $class;
}

sub UNTIE {
#warn "lucy UNTIE";
    my $self  = shift;
}

sub STORE {
    my $self  = shift;
    my $key   = shift;   # only the tid
    my $value = shift;   #  TM2::Literal

#warn "STORE $key";
#warn "STORE $key $value ".Dumper $value;

    my $indexer = Lucy::Index::Indexer->new(
	index    => $self->{'lucy:root'},
	schema   => $self->{schema},
	);

    $self->delete_tid ($indexer, $key)
	if $self->{'lucy:unique-tid'};
    $indexer->add_doc ({ 'tid' => $key, 'content' => $value->[0], 'lastmod' => time() });
    $indexer->commit;
    $indexer->optimize
	if $self->{'lucy:auto-optimize'};
}

sub STORE_SLICE {
    my $self  = shift;
    my $values = shift;

#warn "STORE_SLICE (lucy): ".scalar keys %$values;
#warn "STORE_SLICE".Dumper $values;
#warn "STORE_SLICE ". scalar keys %$values;

    my $indexer = Lucy::Index::Indexer->new(
	index    => $self->{'lucy:root'},
	schema   => $self->{schema},
#	manager  => $no_merger,
	);

    $self->delete_tid ($indexer, keys %$values)
	if $self->{'lucy:unique-tid'};
#warn "deleted";

    while (my ($tid, $v) = each %$values) {
	$indexer->add_doc ({ 'tid' => $tid, 'content' => $v->[0], 'lastmod' => time() });
    }
#warn "inserted";
    $indexer->commit;
#warn "committed";
    $indexer->optimize if $self->{'lucy:auto-optimize'};
#warn "optimized";
}

sub delete_tid {
    my $self    = shift;
    my $indexer = shift;

#warn "delete tid $tid";
    my $qparser  = Lucy::Search::QueryParser->new(
	schema   => $self->{schema},
	fields   => ['tid'],
	);

    foreach (@_) {
	my $query = $qparser->parse($_);
	$indexer->delete_by_query( $query );
    }
}

sub DELETE {
    my $self = shift;
    my $key  = shift;

#warn "DELETE $key";

    my $indexer = Lucy::Index::Indexer->new(
	index    => $self->{'lucy:root'},
	schema   => $self->{schema},
	);

    $self->delete_tid ($indexer, $key);
    $indexer->commit;
}

sub DELETE_LIST {
    my $self = shift;
    my @keys = @_;

    my $indexer = Lucy::Index::Indexer->new(
	index    => $self->{'lucy:root'},
	schema   => $self->{schema},
	);
    my $qparser  = Lucy::Search::QueryParser->new(
	schema   => $self->{schema},
	fields   => ['tid'],
	);
    foreach my $tid (@keys) {
	my $query = $qparser->parse($tid);
	$indexer->delete_by_query( $query );
    }
    $indexer->commit;
}

sub _get_hits {
    my $self = shift;
    my $field = shift;
    my $text = shift;

    use Lucy::Search::IndexSearcher;
    my $searcher;
#warn "trying to get searcher";
    eval {
	$searcher = Lucy::Search::IndexSearcher->new(	index => $self->{'lucy:root'}	);
    }; if ($@) {
	$TM2::log->warn ("index empty at ".$self->{'lucy:root'});
#	$TM2::log->warn ($@);
	return undef;
    }
#warn "got searcher";
    my $query;
    if ($text eq '*') {
	$query = Lucy::Search::MatchAllQuery->new;
    } else {
	my $qparser  = Lucy::Search::QueryParser->new(
	    schema => $searcher->get_schema,
	    fields => [$field],
	    );
	$query = $qparser->parse($text);
    }
#warn "trying to get hits ".$self->{'lucy:num-wanted'};
    my $hits = $searcher->hits( query      => $query,
				offset     => 0,
				num_wanted => $self->{'lucy:num-wanted'},
           );
#warn "nr hits for $field $text ".$hits->total_hits;  # get the hit count here
    return $hits;
}

sub _lookup_by_tid {
    my $self = shift;
    my $tid = shift;

    my $hits = _get_hits ($self, 'tid', $tid)
	or return undef;
    my $hit  = $hits->next
	or return undef;

#warn "timestamp" . $hit->{lastmod};
    return _is_expired ($hit, $self->{'lucy:expiry'}, time)
	? undef
	: TM2::Literal->new ($hit->{content}, 'urn:x-mime:text/query');
}

sub _is_expired {
    my ($hit, $expiry, $now) = @_;
    if (defined $expiry) {
	if ($hit->{lastmod} + $expiry < $now) {
	    return 1;
	}
    }
}

sub EXISTS {
    my $self = shift;
    my $key  = shift;
#warn "EXISTS $key";
    return _lookup_by_tid ($self, $key) ? 1 : 0;
}

sub FETCH {
    my $self = shift;
    my $key  = shift;

#warn "FETCH $key "."from ".Dumper [ caller ];

    my $v = _lookup_by_tid ($self, $key)
	or return undef;
    return $v;
}

sub SLICEABLE {}

sub HCTEF {   # reversed FETCH, actually looking up keys based on value (fragments)
    my $self  = shift;
    my $value = shift;

#warn "HCTEF".Dumper $value;
    my $hits = _get_hits ($self, 'content', $value->[0])
	or return undef;
    my @tids;
    my $now = time;
#warn "collecting hits";
    while (my $hit = $hits->next) {
	push @tids, $hit->{tid}
	     unless _is_expired ($hit, $self->{'lucy:expiry'}, $now);
    }
#warn "returning hits";
    return @tids;
}

sub FIRSTKEY {
    my $self = shift;
#warn "FIRST";
    $self->{hits} = _get_hits ($self, 'tid', '*')
	or return undef;
    my $hit;
    do {
	$hit = $self->{hits}->next
	    or return undef;
    } until (! _is_expired ($hit, $self->{'lucy:expiry'}, time));
    return $hit->{tid};
}

sub NEXTKEY {
    my $self = shift;
    my $last = shift;
#warn "NEXT";
    my $hit;
    do {
	$hit = $self->{hits}->next
	    or return undef;
    } until (! _is_expired ($hit, $self->{'lucy:expiry'}, time));
    return $hit->{tid};
}

1;

__END__


IndexReader reader = // create IndexReader
    for (int i=0; i<reader.maxDoc(); i++) {
    if (reader.isDeleted(i))
        continue;

    Document doc = reader.document(i);
    String docId = doc.get("docId");

    // do something with docId here...
}
