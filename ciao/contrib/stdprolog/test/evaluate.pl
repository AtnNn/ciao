#! /bin/sh
eval '(exit $?0)' && eval 'PERL_BADLANG=x;PATH="$PATH:.";export PERL_BADLANG\
 PATH;exec perl -x -S -- "$0" ${1+"$@"};#'if 0;eval 'setenv PERL_BADLANG x\
;setenv PATH "$PATH":.;exec perl -x -S -- "$0" $argv:q;#'.q
#!perl -w
+push@INC,'.';$0=~/(.*)/s;do(index($1,"/")<0?"./$1":$1);die$@if$@__END__+if 0
;#Don't touch/remove lines 1--7: http://www.inf.bme.hu/~pts/Magic.Perl.Header
#
# evaluate.pl -- evaluate test results
# by pts@fazekas.hu at Sat Jan 14 13:50:25 CET 2006
#
use integer;
use strict;

die "Usage: $0 <test_cases.pl> <test-results.out>\n" if @ARGV!=2;

#** Elements are 'Group:Number'.
my @tests;
#** $testh{'Group:Number'}=$how; $how is 0 for skipped, 1 for OK, 2 for not ok.
my %testh;

my $warnc=0;

my $testname;
die unless open TC, "< $ARGV[0]";
my $in_comment_p=0;
# Imp: report statistics about commented test cases !!
while (<TC>) {
  if ($in_comment_p) {
    if (s/\A.*[*]\///s) { $in_comment_p=0 }
    else { next }
  }
  if (/^\s*test_case\(\s*([a-z]\w*)\s*:\s*(\d+)\s*,/) { # Imp: better parse Prolog
    $testname="$1:$2";
    if (exists $testh{$testname}) {
      $warnc++;
      print STDERR "warning: multiple occurrence of test name: $testname\n"
    }
    $testh{$testname}=0;
    push @tests, $testname;
  } elsif (s/^\s*\/[*]//) { # Imp: proper comment parsing
    $in_comment_p=1; redo;
  } elsif (/^\s*end_of_file\s*[.]/) {
    last
  }
}
die unless close TC;
if ($in_comment_p) {
  print STDERR "warning: test cases end in comment\n";
  $warnc++;
}
if (!@tests) {
  print STDERR "error: no test cases\n";
  exit 2;
}

die unless open TR, "< $ARGV[1]";
my $I=0;
my $had_badorder_p=0;
while (<TR>) {
  if (/^(ok|not ok)\s*([a-z]\w*:\d+)$/) {
    if (!exists $testh{$testname=$2}) {
      print STDERR "warning: result for unknown test name: $testname\n";
      $warnc++;
    } elsif ($testh{$testname}!=0) {
      print STDERR "warning: multiple occurrence of test result for: $testname\n";
      $warnc++;
    } else {
      $testh{$testname}=($1 eq "ok") ? 1 : 2;
      if (!$had_badorder_p and $tests[$I] ne $testname) {
        my $J=$I;
        $J++ while $J<@tests and $tests[$J] ne $testname;
        if ($J==@tests) {
          print STDERR "warning: bad test name order: expected ($tests[$I]), got ($testname)\n";
          $had_badorder_p=1; $warnc++;
        } else { $I=$J }
      } else { $I++ }
    }
  }
}
die unless close TR;

my $okc=0;
my $skipc=0;
my $failc=0;
for $testname (@tests) {
  if ($testh{$testname}==0) {
    print STDERR "test skipped: $testname\n";
    $skipc++;
  } elsif ($testh{$testname}>1) {
    print STDERR "test failed : $testname\n"; # Imp: line number etc.
    $failc++;
  } else {
    $okc++;
  }
}
print STDERR "Statistics:\n";
my $testc=@tests;
# !! Imp: more metainfo: prolog Version, file names etc.
print STDERR "  $testc test case@{['s'x($testc!=1)]} altogether\n";
print STDERR "  $okc test@{['s'x($okc!=1)]} OK\n";
print STDERR "  $failc test@{['s'x($failc!=1)]} failed\n";
print STDERR "  $skipc test@{['s'x($skipc!=1)]} skipped\n";
my $all_ok_p=($failc==0 and $skipc==0);
print STDERR "  All OK.\n" if $all_ok_p;

exit 3 if $warnc>0;
