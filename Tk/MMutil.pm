# Copyright (c) 1995-1998 Nick Ing-Simmons. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
package Tk::MMutil;
use ExtUtils::MakeMaker;
use Cwd;
use Config;
use Carp;
use File::Basename;

use vars qw($VERSION);
$VERSION = '3.017'; # $Id: //depot/Tk8/Tk/MMutil.pm#17$

use Tk::MakeDepend;

use Tk::Config qw(!$VERSION);
use vars qw($IsWin32);

*IsWin32 = \$main::IsWin32;
$IsWin32 = ($^O eq 'MSWin32' || $Config{'ccflags'} =~ /-D_?WIN32_?/)
           unless defined $IsWin32;

@MYEXPORT = qw(perldepend cflags const_config constants installbin c_o xs_o makefile manifypods);

sub arch_prune
{
 my $hash = shift;
 foreach (keys %$hash)
  {
   if ($win_arch eq 'x') 
    {
     delete $hash->{$_} if /Win[A-Z0-9]/ or /OS2/ or /ImgUtil/ or /^x/;
    } 
   elsif ($win_arch eq 'open32') 
    {
     delete $hash->{$_} if /Unix|Mwm/ and not /tclUnix/;
     delete $hash->{$_} if /winMain|dllMain/;
    }
   elsif ($win_arch eq 'pm') 
    {
     delete $hash->{$_} if /Unix|Mwm/ and not /tclUnix/;
     delete $hash->{$_} if /os2Main|dllMain|tkOS2Dll|^xgc\./;
     delete $hash->{$_} if /ImgUtil|tkWin[A-Z0-9]/ and not /OS2/;
    }
   elsif ($win_arch eq 'MSWin32') 
    {
     delete $hash->{$_} if /Mwm/ and not /tclUnix/;
     delete $hash->{$_} if /winMain|dllMain/;
     # delete $hash->{$_} if /^Xrm/;
    }
  } 
}                                        

sub mTk_postamble
{
 my ($self) = @_;
 my $dep = "config :: \$(C_FILES) \$(H_FILES)\n\t$self->{NOECHO}\$(NOOP)\n";
 my $mTk = $self->{'MTK'};
 $dep .= "# Begin Munging dependancies\n";
 foreach my $file (sort keys %$mTk)
  {
   $dep .= "$file : ".$mTk->{$file}." \$(TKDIR)/pTk/Tcl-pTk\n";
   $dep .= "\t\$(PERL) \$(TKDIR)/pTk/Tcl-pTk ".$mTk->{$file}." $file\n";
  }
 $dep .= "# End Munging dependancies\n\n";
 return $dep;
}

sub mTk_CHO
{
 my $self = shift;
 my $mTk  = shift;
 my %c;
 my %h;
 foreach (@{$self->{H}}) { $h{$_} = 1 }
 foreach (@{$self->{C}}) { $c{$_} = 1 }
 foreach (keys %$mTk)
  {
   if (/\.c$/)
    {
     $c{$_} = 1;
    }
   elsif (/\.h$/)
    {
     $h{$_} = 1;
    }
  }
 while (@_)
  {
   my $name = shift;
   carp("No $name") unless (exists $c{$name});
   delete $c{$name}
  }
 arch_prune(\%h);
 arch_prune(\%c);
 $self->{'H'}     = [sort keys %h];
 $self->{'C'}     = [sort keys %c];
 my(@o_files)     = @{$self->{C}};
 $self->{O_FILES} = [grep s/\.c(pp|xx|c)?$/$self->{OBJ_EXT}/i, @o_files] ;
 $self->{'MTK'}   = $mTk;
 my $tk = installed_tk();
 my $perl = $self->{'PERL'};
 if ($IsWin32 && !-f $perl && -f "$perl.exe")
  {
   print "perl=$perl X=$^X\n";
   $perl = "$perl.exe";
   $self->{'PERL'} = $perl; 
  }
 foreach my $file (sort keys %$mTk)
  {
   unless (-f $file)
    {                   
     warn "Extracting $file\n";
     system($perl,"$tk/pTk/Tcl-pTk",$mTk->{$file},$file);
    }
  }
}

my %visited;          

sub abspath
{
 my $dir = shift;
 my $here = getcwd() || die "Cannot get current directory:$!";
 if (chdir($dir))
  {
   $dir = getcwd();
   chdir($here) || die "Cannot cd back to $here:$!";
  }
 return $dir;
}

sub relpath
{
 my ($path,$dir) = @_;
 unless (defined $dir)
  {
   $dir = (-d $path) ? $path : dirname($path);
  }
 if (defined $dir and -d $dir)
  {
   if ($path =~ m#^\Q$dir\E([/\\]?.*)$#)
    {
     my $base  = $1;
     my $here  = getcwd;
     if ($here =~ m#^\Q$dir\E([/\\]?.*)#)
      {
       my $depth = reverse($1);
       if ($depth)
        {
         $depth =~ s,[^/\\]+,..,g;
        }
       else
        {
         $depth = '.' ;
        }
       $depth =~ s,[/\\]+$,,;
       $base =~ s,^[/\\]+,,;
       $depth .= "/$base" if ($base);
       if (-e $depth)
        {
         # print "$path is $depth from $here\n";
         return $depth;
        }
       else
        {
         warn "Cannot find $depth\n";
        }
      }
     else
      {
       unless(exists $visited{$here})
        {                         
         $visited{$here} = 1;     
         warn "$here does not start with $dir\n";
         warn "i.e. building outside Tk itself\n";
        }
      }
    }
   else
    {
     die "'$path' not under '$dir'\n";
    }
  }
 else
  {
   die "Cannot get directory for $path\n";
  }
 return $path;
}

use strict;

sub upgrade_pic
{
 my $flags = "";
 die "upgrade_pic is obsolete";
 return $flags;
}

sub perldepend
{
 my $self = shift;
 my $str = $self->MM::perldepend;
 my $name;
 my %c;
 foreach my $file (@{$self->{'C'}})
  {
   $c{$file} = 1;
  }
 foreach my $file (keys %{$self->{'XS'}})
  {
   $c{$file} = 1;
   delete $c{$self->{'XS'}{$file}};
  }
 my @files = grep(-f $_,sort(keys %c));
 if (@files)
  {
   my @inc   = split(/\s+/,$self->{'INC'});   
   my @def   = split(/\s+/,$self->{'DEFINE'});
   push(@def,qw(-DWIN32 -D__WIN32__)) if ($IsWin32);
   $str .= Tk::MakeDepend::command_line(@inc,@def,@files);
  }
 if (0) 
  {
   $str .= "# Auto generated from GCC's .d files\n";
   foreach $name ($self->lsdir("."))
    {
     if ($name =~ /\.d$/)
      {
       local $_;
       open(DEP,"<$name") || die "Cannot open $name:$!";
       while (<DEP>)
        {
         if ($IsWin32)
          {
           s/Unix/Win/g;
          }
         elsif ($win_arch eq 'open32') {
           s/tixUnix/tixWin/g;
           s/\btkWinInt\.h\b/tkWinInt.h windows.h/g;
         }
         elsif ($win_arch eq 'pm') {
           s/tixUnix/tixWin/g;
           s/tkUnix/tkOS2/g;
         }
         s/^([^:]*)\.o\s*:/$1$self->{OBJ_EXT}:/;
         $str .= $_;
        }
       close(DEP);
      }
    }
  }
 return $str;
}

sub const_config
{
 my $self = shift;
 my $name;
 foreach $name (grep /(%|\.(old|bak))$/,keys %{$self->{PM}})
  {
   delete $self->{PM}->{$name};
  }
 my $flags = $self->{'CCCDLFLAGS'};
 $flags =~ s/(-[fK]?\s*)pic\b/${1}PIC/; 
 $self->{'CCCDLFLAGS'} = $flags;
 return $self->MM::const_config;
}

sub constants
{
 my $self = shift;
 local $_ = $self->MM::constants;
 s/(\.SUFFIXES)/$1:\n$1/;
 $_ .= "\nGCCOPT = $Tk::Config::gccopt\n";
 if ($IsWin32)
  {
   if (0) 
    {
     if ($Config::Config{cc} =~ /^bcc/i) 
      {
       $_ .= "LDDLFLAGS = -v -Tpd\n";
      }
     else 
      {
       $_ .= "!include <win32.mak>\n";
       $_ .= "LDLOADLIBS=\$(guilibsdll)\n";
       $_ .= "LDDLFLAGS=\$(linkdebug) \$(dlllflags)\n";
      }
    }
  } 
 $_;
}

sub cflags
{
 my $self = shift;
 local $_ = $self->MM::cflags;
 if ($IsWin32)
  {
   if ($Config::Config{cc} =~ /^bcc/i) {
     # s/(CCFLAGS\s*=)/$1/; 
   }
   else {
     s/(CCFLAGS\s*=)/$1 \$(cflags) \$(cvarsdll)/; 
     s/(OPTIMIZE\s*=).*/$1 \$(cdebug)/;
   }
  }
 $_;
}

sub c_o
{
 my $self = shift;
 local $_ = $self->MM::c_o;
 s/\$\(DEFINE\)/\$(DEFINE) \$(GCCOPT)/g;
 $_;
}

sub xs_o
{
 my $self = shift;
 local $_ = $self->MM::xs_o;
 s/\$\(DEFINE\)/\$(DEFINE) \$(GCCOPT)/g;
 $_;
}

sub manifypods
{
 my $self = shift;
 # Maybe always call UNIX version - we HTMLize them later
 local $_ = $self->MM::manifypods;
 if ($] >= 5.003)
  {
   s/(POD2MAN_EXE.*pod2man)/$1 -center "perl\/Tk Documentation" -release "Tk\$(VERSION)"/;
  }
 else
  {
   s/(POD2MAN_EXE.*pod2man)/$1 -center \\"perl\/Tk Documentation\\" -release \\"Tk\$(VERSION)\\"/;
  }
 s/\bpod::/Tk::/mg;
 s/\bpTk:://mg;
 $_;
}

sub findINC
{
 my $file = shift;
 my $dir;
 foreach $dir (@INC)
  {
   my $try = "$dir/$file";
   return $try if (-f $try);
  }
 die "Cannot find $file in \@INC\n";
}


sub makefile
{
 my $self = shift;
 my $str  = $self->MM::makefile;
 my $mm = findINC('Tk/MMutil.pm');
 my $cf = findINC('Tk/Config.pm');
 $str =~ s/(\$\(CONFIGDEP\))/$1 $cf $mm/;
 $str =~ s/\$\(OBJECT\)\s*:.*\n//;
 return $str;
}

sub installed_tk
{
 my $tk; 
 my $dir;
 foreach $dir (@INC)
  {
   if (-f "$dir/tkGlue.h")
    {
     $tk = relpath($dir);
     last;
    }
   my $try = "$dir/Tk";
   if (-f "$try/tkGlue.h")
    {
     $tk = relpath($try,$dir);
     last;
    }
  }
 die "Cannot find perl/Tk include files\n" unless (defined $tk);
 $tk =~ s,^(\./)+,,;
 return $tk;
}

sub installbin
{
 my ($self) = @_;
 my $str  = $self->MM::installbin;
 my $prog = 'perl'; # $self->{'MAP_TARGET'} || 'perl';
 my $inc  = findINC("Tk/MMutil.pm");
 $inc =~ s,/Tk/MMutil.pm$,,;
 $inc = relpath($inc);
 $str =~ s/^\tcp\s/\t\$(PERL) -I$inc -MTk::install -e installbin $prog /mg;
 return $str;
}

sub findpTk
{
 my $ptk;
 my $dir;
 foreach $dir (map(abspath($_),@_),@INC)
  {
   my $try = "$dir/pTk";
   if (-d $try && (-f "$try/Lang.h" || -f "$try/libpTk\$(LIB_EXT)"))
    {
     $ptk = relpath($try,$dir);
     last;
    }
  }
 confess "Cannot locate pTk\n" unless (defined $ptk); 
 return $ptk;
}

sub TkExtMakefile
{
 my (%att) = @_;
 unless (exists $att{'NAME'})
  {
   my $dir = getcwd;
   my ($pack) = $dir =~ m#/([^/]+)$#;
   if (defined $pack)
    {
     $att{NAME} = 'Tk::'.$pack;
    }
   else
    {
     warn "No Name and cannot deduce from '$dir'";
    }
  }
 my $tk = installed_tk();
 $att{'macro'} = {} unless (exists $att{'macro'});
 $att{'macro'}{'TKDIR'} = $tk;
 # 'INST_LIB' => '../blib',
 # 'INST_ARCHLIB' => '../blib',
 my @opt = ('VERSION'     => $Tk::Config::VERSION, 
            'XS_VERSION'  => $Tk::Config::VERSION);
 push(@opt,'clean' => {} ) unless (exists $att{'clean'});
 $att{'clean'}->{FILES} = '' unless (exists $att{'clean'}->{FILES});
 $att{'clean'}->{FILES} .= " *.bak";
 unless (exists($att{'linkext'}) && $att{linkext}{LINKTYPE} eq '')
  {
   my $ptk = findpTk($tk);
   my @tm = (findINC('Tk/typemap'));
   unshift(@tm,@{$att{'TYPEMAPS'}}) if (exists $att{'TYPEMAPS'});
   $att{'TYPEMAPS'} = \@tm;
   my $i = delete ($att{'INC'});
   $i = (defined $i) ? "$i $inc" : $inc;
   if (delete $att{'dynamic_ptk'})
    {
     push(@opt, 
          'MYEXTLIB' => "$ptk/libpTk\$(LIB_EXT)",
          'dynamic_lib' => {
                             INST_DYNAMIC_DEP => "$ptk/libpTk\$(LIB_EXT)"
                            }
         ); 
    }
   if ($IsWin32 && $Config{'cc'} =~ /^bcc/)
    {
     # Borland compiler is very dumb at finding files 
     $i = "-I$tk $i";
     $i = "-I$ptk $i";
    }
   if (delete $att{'ptk_include'})
    {
     $i = "-I$ptk $i" unless ($ptk eq '.');
    }
   else
    {
     $i = "-I$tk $i" unless ($tk eq '.');
    }
   push(@opt,'DEFINE' => $define, 'INC' => $i);
  }
 WriteMakefile(@opt, %att);
}

sub import
{
 no strict 'refs';
 my $class = shift;
 my @list = (@_) ? @_ : @{"${class}::MYEXPORT"};
 my $name;
 foreach $name (@list)
  {
   *{"MY::$name"} = \&{"$name"};
  }
}


1;