#!/usr/local/bin/perl

use strict;
use warnings;
use Time::Piece;
use WWW::Mechanize;

my $path_to_files = 'https://publicacionexterna.azurewebsites.net/publicaciones/prices';
my $mech = WWW::Mechanize->new();
$mech->get( $path_to_files );
my $fname = localtime->strftime('%Y-%m-%d-%H-%M')."-prices.xml"; 
$fname = "/home/sdmcrae/Dropbox/mexico/gasolina/data2/".$fname;
$mech->save_content( $fname );

