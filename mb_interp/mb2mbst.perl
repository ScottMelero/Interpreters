#!/usr/bin/perl
# $Id: mb2mbst.perl,v 1.3 2020-10-16 10:54:26-07 - - $

BEGIN {push @INC, "/afs/cats.ucsc.edu/courses/cse110a-wm/perl5/lib"}

$0 =~ s|.*/||;
use diagnostics;
use strict; 
use warnings;

my $status = 0;
END {exit $status}
$SIG{__WARN__} = sub {print STDERR "$0: @_"; $status = 1};
$SIG{__DIE__} = sub {warn @_; exit};

use Data::Dumper;
use Getopt::Std;
use Parse::RecDescent;

$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
#$::RD_WARN = 1;
#$::RD_HINT = 1;
#$::RD_TRACE = 1;

my %options;
getopts "g", \%options;
my $dump_grammar = $options{'g'};
my $linecount = 0;

sub floatd($) {
   my ($number) = @_;
   $number =~ s/(^|\D)\./${1}0./;
   $number =~ s/\.($|\D)/.0$1/;
   $number =~ s/e/d/;
   $number .= "d0" unless $number =~ m/d/;
   return $number;
}

sub string($) {
   my ($string) = @_;
   return "''" if $string eq '""';
   $string =~ s/""/"/g;
   $string =~ s/'/''/g;
   $string =~ s/"(.*)"/'$1'/;
   return $string;
}

my %opsub = (qw(
   !=     ~=
   ^      raisedTo:
   acos   arcCos
   asin   arcSin
   atan   arcTan
   ceil   ceiling
   log    ln
   log10  log
   round  rounded
));

sub opsub(@) {
   my ($op, @args) = @_;
   my $sub = $opsub{$op};
   return ($sub ? $sub : $op, @args);
}

sub write_syntax_file($$) {
   my ($mb_fname, $syntax) = @_;
   my $syntax_fname = $mb_fname;
   $syntax_fname =~ s|.*/||;
   $syntax_fname =~ s/\.[^.]*$//;
   $syntax_fname .= ".syntax";
   open my $syntax_file, ">$syntax_fname"
            or die "$0: $syntax_fname: $!\n";
   printf $syntax_file "%s\n", Data::Dumper->Dump ([$_]) for @$syntax;
   close $syntax_file;
}

sub flatten($);
sub flatten($) {
   my ($stmt) = @_;
   my $result = "";
   for my $item (@$stmt) {
      if (ref $item) {
         $result .= " #(" . (flatten $item) . ")";
      }else {
         $item =~ s/^[^\d']/#$&/;
         $result .= " " . $item;
      }
   }
   return $result;
}

sub write_mbst_file($$) {
   my ($mb_fname, $syntax) = @_;
   my $mbst_fname = $mb_fname;
   $mbst_fname =~ s|.*/||;
   $mbst_fname =~ s/\.[^.]*$//;
   $mbst_fname .= ".mbst";
   open my $mbst_file, ">$mbst_fname" or die "$0: $mbst_fname: $!\n";
   print $mbst_file "Object subclass: Program [\n",
                    "Program class >> get [\n",
                    "^ #(\n";
   for my $line (@$syntax) {
      my ($linenr, $label, $stmt)
         = ($line->[0], $line->[1][0], $line->[2][0]);
      printf $mbst_file "#( %s %s #(%s))\n", $linenr,
             $label ? "#$label" : "nil", flatten $stmt
             if $label || $stmt;
   }
   print $mbst_file ").]]\n";
   close $mbst_file;
}

my @grammar = <DATA>;
if ($dump_grammar) {
   printf STDERR "%3d: %s", $_, $grammar[$_] for 0..$#grammar;
}
my $grammar = join '', @grammar;
my $parser = new Parse::RecDescent ($grammar);

push @ARGV, "-" unless @ARGV;
for my $mb_fname (@ARGV) {
   open my $mb_file, "<$mb_fname"
        or warn "$mb_fname: $!" and next;
   my @source = <$mb_file>;
   my $source = join "", @source;
   close $mb_file;
   print "$0: parsing $mb_fname\n";
   my $syntax = $parser->program ($source)
              or die "$mb_fname: syntax error\n" and next;
   #write_syntax_file $mb_fname, $syntax;
   write_mbst_file $mb_fname, $syntax;
}


__DATA__

<autoaction: {[@item]}>
<skip: qr{([ \t]+|#.*)*}>

program   : NL(?) line(s?) NL(s?) EOF  {$item[2]}

line      : label(?) stmt(?) NL        {[$::linecount,@item[1,2]]}

label     : IDENT ':'                  {$item[1]}

stmt      : 'dim' IDENT '[' expr ']'   {[@item[1,2,4]]}
          | 'let' lvalue '=' expr      {[@item[1,2,4]]}
          | 'goto' IDENT               {[@item[1,2]]}
          | 'if' relexpr 'goto' IDENT  {[@item[1,2,4]]}
          | 'print' printable(s? /,/)  {[@item[1,2]]}
          | 'input' lvalue(s? /,/)     {[@item[1,2]]}

printable : /\"([^"\n]|\"\")*\"/       {::string($item[1])}
          | expr                       {$item[1]}

lvalue    : IDENT '[' expr ']'         {["aput",@item[1,3]]}
          | IDENT                      {$item[1]}

relexpr   : expr relop expr            {[::opsub(@item[2,1,3])]}

expr      : term expr2[$item[1]]                            {$item[2]}
expr2     : addop term expr2[[$item[1],$arg[0],$item[2]]]   {$item[3]}
          |                                                 {$arg[0]}

term      : factor term2[$item[1]]                          {$item[2]}
term2     : mulop factor term2[[$item[1],$arg[0],$item[2]]] {$item[3]}
          |                                                 {$arg[0]}

factor    : primary '^' factor         {[::opsub(@item[2,1,3])]}
          | primary                    {$item[1]}

primary   : '(' expr ')'               {$item[2]}
          | '+' primary                {$item[2]}
          | '-' primary                {['negated',$item[2]]}
          | NUMBER                     {$item[1]}
          | IDENT '(' expr ')'         {[::opsub(@item[1,3])]}
          | rvalue                     {$item[1]}

rvalue    : IDENT '[' expr ']'         {["aget",@item[1,3]]}
          | IDENT                      {$item[1]}

relop     : ('='|'!='|'<='|'<'|'>='|'>')     {$item[1][1]}
addop     : ('+'|'-')                        {$item[1][1]}
mulop     : ('*'|'/')                        {$item[1][1]}

IDENT     : /[a-z_][a-z_0-9]*/i              {$item[1]}
NUMBER    : /(\d+\.?\d*|\.\d+)(e[+-]?\d+)?/i {::floatd($item[1])}
NL        : (/[\n;]/)                        {++$::linecount;'<NL>'}
EOF       : /^\Z/                            {'<EOF>'}

