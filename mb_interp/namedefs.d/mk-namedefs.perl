#!/usr/bin/perl
# $Id: mk-namedefs.perl,v 1.7 2020-08-17 00:05:33-07 - - $

@mlfiles = (glob ("../*.mli"), glob ("../*.ml"));

map {s|.*/(.*)\.mli?$|$1|; $prefix{$_} = 1} @mlfiles;

sub cmd {
   print "@_\n";
   system "@_";
}

cmd "cid -is $0";
cmd "(cd ..; file *) | cut -c1-79 >file.types";

for $prefix (keys %prefix) {
   @morefiles = ();
   for $suffix (qw (mli ml)) {
      $file = "$prefix.$suffix";
      if (-e "../$file") {
         cmd "(cd ..; ocamlopt -i $file) >$file.defs";
         push @morefiles, "$file.defs";
      }
   }
   cmd "more @morefiles >$prefix.namedefs </dev/null";
   cmd "rm @morefiles";
}

cmd "mkpspdf -N Listing.namedefs.ps file.types *.namedefs";

