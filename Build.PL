use strict;
use warnings;
use Module::Build;

$ENV{'PERL_MB_OPT'} = '--install-base=/usr'; # I could not make this work via the parameters below :/
$ENV{'DEB_BUILD_OPTIONS'}='nocheck';

use lib qw(../tm2_base/lib ../templescript/lib);

my $debian = Module::Build->subclass(
    class => "Module::Build::Debian",
    code => <<'SUBCLASS' );

sub DEBVERSION {
    return 1;
}

sub ACTION_debian {
    my $self = shift;

    $self->depends_on("distdir");

    my $version = $self->dist_version;
    my $debversion = $self->DEBVERSION;
    $self->do_system(qq{ export DEB_BUILD_OPTIONS=nocheck ; dh-make-perl --no-network --build --version=$version-$debversion TM2-Lucy-$version } );

}
sub ACTION_inject {
    my $self = shift;

    $self->depends_on("debian");

    my $version = $self->dist_version;
    my $debversion = $self->DEBVERSION;
    $self->do_system(qq{ scp libtm2-lucy-perl_${version}-${debversion}_all.deb packages.devc.at:packages/ } );
    foreach (qw(squeeze wheezy jessie stretch) ) {
        $self->do_system(qq{ ssh packages.devc.at reprepro -b /var/www/packages/data/dists/ includedeb $_ packages/libtm2-lucy-perl_${version}-${debversion}_all.deb });
    }
}
SUBCLASS

my $builder = $debian->new(
    module_name         => 'TM2::Lucy',
    license             => 'perl',
    dist_author         => q{Robert Barta <rho@devc.at>},
    dist_version_from   => 'lib/TM2/Lucy.pm',
    configure_requires => {
	'Module::Build' => 0,
    },
    build_requires => {
        'Test::More' => 0,
        'File::Temp' => 0,
        'Test::Exception' => 0,
    },
    requires => {
        'TM2::TempleScript' => 0,
        'Lucy'              => 0,
    },
    add_to_cleanup      => [ 'TM2-Lucy-*' ],
    create_makefile_pl => 'traditional',
    onto_files          => {
	'ontologies/lucy.atm'       => 'ontologies/lucy.atm',
    },
    install_path        => { 'ontologies' => '/usr/share/templescript/ontologies' },
);

$builder->add_build_element('onto');
$builder->create_build_script();
