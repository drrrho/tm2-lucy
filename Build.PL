use 5.006;
use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'TM2::Lucy',
    license             => 'artistic_2',
    dist_author         => q{rho <rho@devc.at>},
    dist_version_from   => 'lib/TM2/Lucy.pm',
    release_status      => 'stable',
    configure_requires => {
        'Module::Build' => 0,
    },
    build_requires => {
        'Test::More' => 0,
    },
    requires => {
        #'ABC'              => 1.6,
        #'Foo::Bar::Module' => 5.0401,
    },
    add_to_cleanup     => [ 'TM2-Lucy-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
