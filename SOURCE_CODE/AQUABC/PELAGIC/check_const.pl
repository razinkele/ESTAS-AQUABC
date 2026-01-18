#!/usr/bin/perl -w
#
# checks model constants
#
#------------------------------------------------------

use strict;

$::debug = 0;
$::error = 0;
@::names = ();
%::sections = ();
@::sections = ( "module" , "init" , "insert" , "default" );

while(<>) {

  chomp;

  #print STDERR "$_\n";

  if( /^\s*module PELAGIC_MODEL_CONSTANTS/ ) {
    read_module();
    $::sections{module} = 1;
  } elsif( /^\s*subroutine INIT_PELAGIC_MODEL_CONSTANTS/ ) {
    read_init();
    $::sections{init} = 1;
  } elsif( /^\s*subroutine INSERT_PELAGIC_MODEL_CONSTANTS/ ) {
    read_insert();
    $::sections{insert} = 1;
  } elsif( /^\s*subroutine DEFAULT_PELAGIC_MODEL_CONSTANTS/ ) {
    read_default();
    $::sections{default} = 1;
  }

}

foreach my $section (@::sections) {	#forgotten sections?
  unless( $::sections{$section} > 0 ) {
    print "*** section $section has not been checked...\n";
  }
}

print "check finished... total number of errors: $::error\n";

#-------------------------------------------------------

sub read_module
{
  my $what = "module";

  print STDERR "reading $what...\n";

  my $count = 0;
  my $error = 0;

  while(<>) {
    chomp;
    last if( /end module PELAGIC_MODEL_CONSTANTS/ );

    if( /^\s*real/i ) {
      if( /::\s*(\w+)\s*!Model constant no\s*(\d+)/ ) {
	$count++;
        my $name = $1;
	my $number = $2;
	$::names[$count] = $name;
	print STDERR "  $count  $number  $name\n" if $::debug;
	if( $number != $count ) {
	  print STDERR "  incompatible number-count: $number $count\n";
	  $error++;
	}
      } else {
        die "cannot parse: $_\n";
      }
    }
  }

  if( $error ) {
    $::error += $error;
    print STDERR  "  errors reading $what: $error\n";
  }
  print STDERR  "  a total of $count variables read\n" if $::default;
}

sub read_init
{
  my $what = "init";

  print STDERR "reading $what...\n";

  my $count = 0;
  my $error = 0;

  while(<>) {
    chomp;
    last if( /end subroutine INIT_PELAGIC_MODEL_CONSTANTS/ );

    if( /^\s*call\s*para_get_value/i ) {
      if( /\(\'(\w+)\'\s*,\s*(\w+)\)\s*!Model constant no\s*(\d+)/ ) {
	$count++;
        my $name = $1;
        my $var = $2;
	my $number = $3;
	print STDERR "  $count  $number  $name  $var\n" if $::debug;
	if( $number != $count ) {
	  print STDERR "  incompatible number-count: $number $count ($name)\n";
	  $error++;
	}
	if( $var ne $name ) {
	  print STDERR "  incompatible name and var: $name $var\n";
	  $error++;
	}
	if( $::names[$count] ne $name ) {
	  my $gname = $::names[$count];
	  print STDERR "  incompatible names: $name $gname ($count)\n";
	  $error++;
	}
      } else {
        die "cannot parse: $_\n";
      }
    }
  }

  if( $error ) {
    $::error += $error;
    print STDERR  "  errors reading $what: $error\n";
  }
  print STDERR  "  a total of $count variables read\n" if $::default;
}

sub read_insert
{
  my $what = "insert";

  print STDERR "reading $what...\n";

  my $count = 0;
  my $error = 0;

  while(<>) {
    chomp;
    last if( /end subroutine INSERT_PELAGIC_MODEL_CONSTANTS/ );

    #print "$_\n";
    if( /^\s*call\s*para_insert_value/i ) {
      if( /\(\'(\w+)\'\s*,\s*(\w+)\)\s*!Model constant no\s*(\d+)/ ) {
	$count++;
        my $name = $1;
        my $var = $2;
	my $number = $3;
	print STDERR "  $count  $number  $name  $var\n" if $::debug;
	if( $number != $count ) {
	  print STDERR "  incompatible number-count: $number $count ($name)\n";
	  $error++;
	}
	if( $var ne $name ) {
	  print STDERR "  incompatible name and var: $name $var\n";
	  $error++;
	}
	if( $::names[$count] ne $name ) {
	  my $gname = $::names[$count];
	  print STDERR "  incompatible names: $name $gname ($count)\n";
	  $error++;
	}
      } else {
        die "cannot parse: $_\n";
      }
    }
  }

  if( $error ) {
    $::error += $error;
    print STDERR  "  errors reading $what: $error\n";
  }
  print STDERR  "  a total of $count variables read\n" if $::default;
}

sub read_default
{
  my $what = "default";

  print STDERR "reading $what...\n";

  my $count = 0;
  my $error = 0;

  while(<>) {
    chomp;
    last if( /end subroutine DEFAULT_PELAGIC_MODEL_CONSTANTS/ );

    #print "$_\n";
    if( /=/ ) {
      if( /\s*(\w+)\s*=/ ) {
	$count++;
        my $name = $1;
        my $var = $2;
	print STDERR "  $count  $name  \n" if $::debug;
	if( $::names[$count] ne $name ) {
	  my $gname = $::names[$count];
	  print STDERR "  incompatible names: $name $gname ($count)\n";
	  $error++;
	}
      } else {
        die "cannot parse: $_\n";
      }
    }
  }

  if( $error ) {
    $::error += $error;
    print STDERR  "  errors reading $what: $error\n";
  }
  print STDERR  "  a total of $count variables read\n" if $::default;
}

#-------------------------------------------------------

