# -*- perl -*-
#
# tkfbox.tcl --
#
#       Implements the "TK" standard file selection dialog box. This
#       dialog box is used on the Unix platforms whenever the tk_strictMotif
#       flag is not set.
#
#       The "TK" standard file selection dialog box is similar to the
#       file selection dialog box on Win95(TM). The user can navigate
#       the directories by clicking on the folder icons or by
#       selecting the "Directory" option menu. The user can select
#       files by clicking on the file icons or by entering a filename
#       in the "Filename:" entry.
#
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# Translated to perl/Tk by Slaven Rezic <slaven@rezic.de>.
#

#----------------------------------------------------------------------
#
#                     F I L E   D I A L O G
#
#----------------------------------------------------------------------
# tkFDialog --
#
#       Implements the TK file selection dialog. This dialog is used when
#       the tk_strictMotif flag is set to false. This procedure shouldn't
#       be called directly. Call tk_getOpenFile or tk_getSaveFile instead.
#

use 5.005; # qr//

package Tk::FBox;
require Tk::Toplevel;
#use Smart::Comments;
use strict;
use vars qw($VERSION $updirImage $folderImage $fileImage);

#$VERSION = sprintf '4.%03d', q$Revision: #18 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.021';

use base qw(Tk::Toplevel);

Construct Tk::Widget 'FBox';

sub import {
    my $package = shift;
    my $i = 0;
    for my $arg (@_){
        if ( $arg eq 'as_default' ) {
            local $^W = 0;
            package Tk;
            if ($Tk::VERSION < 804) {
                *FDialog      = \&Tk::FBox::FDialog;
                *MotifFDialog = \&Tk::FBox::FDialog;
            } else {
                *tk_getOpenFile = sub {
                    Tk::FBox::FDialog("tk_getOpenFile", @_);
                };
                *tk_getSaveFile = sub {
                    Tk::FBox::FDialog("tk_getSaveFile", @_);
                };
            }
        }
        elsif( lc $arg eq 'i18n' ){
            my $i18n = $_[$i+1];
            $package->i18n_class("Tk::FBox::I18n::$i18n");
        }
        $i++;
    }
}


# i18n helper methods

# strings_defaults()  returns a complete set of default strings
# by delegating to strings() in our i18n-class
# entries provided here can be overridden using the -strings config. option
# different i18n_class can be set using the i18n_class() method,
# specifying i18n => 'Class' as arg to 'use',
# or config. option '-i18n'.
# latter two cases expand the given $class to Tk::FBox::I18n::$class

sub strings_defaults{
    my $self = shift;
    return $self->i18n_class->strings;
}
sub strings{
    return {title_open   => 'Open',
            title_save   => 'Save As',
            title_dir    => 'Choose Directory',
            dir          => 'Directory:',
            filename     => 'File name:',
            filetype     => 'Files of type:',
            btn_ok       => '_Ok',
            btn_save     => '_Save',
            btn_open     => '_Open',
            btn_cancel   => 'Cancel',
            msg_file_dne => 'File "%PATH%" does not exist.',
            msg_dir_dne  => 'Directory "%PATH%" does not exist.',
            msg_chdir    => 'Cannot change to the directory "%PATH%".'
                            ."\nPermission denied.",
            msg_invalid  => 'Invalid filename: "%PATH%"',
            msg_exists   => 'File "%PATH%" already exists.'
                            ."\nDo you want to overwrite it?",
        };
}
sub configuration_for{
    my ($w,$label) = @_;
    my $s = $w->cget('-strings')->{$label};
    my $i = index( $s, '_');
    $s =~ y/_//d;
    my @config = ( -text      => $s,
                   -underline => $i);
    return @config;
}
sub bindkey_for{
    my ($w,$label) = @_;
    my $s = $w->cget('-strings')->{$label};
    my $i = index( $s, '_');
    if($i >= 0 and $i < length $s-2){
        return lc substr($s, $i+1, 1);
    }
    return undef;
}
sub message_for{
    my ($w,$label,$path) = @_;
    my $s = $w->cget('-strings')->{$label};
    $s =~ s/%PATH%/$path/;
    return $s;
}
sub create_binding_for{
    my ($w,$label,$callback) = @_;
    my $k;
    if ($k = $w->bindkey_for($label)){
        ### creating binding for: $k
        $w->bind("<Alt-$k>", $callback );
    }
    return '';
}

{
# Tk::FBox is our default i18n_class
my $i18n_class = __PACKAGE__;
sub i18n_class{
    my $self = shift;
    if (@_){
        my $class = $_[0];
        # reset to default in case '' is passed in
        $class ||= __PACKAGE__;
        if(! $class->can('strings')) {
            eval "require $class";
            if ($@) {
                warn "i18n_class ($class) not available or does not "
                     ."implement 'strings()': $@";
                # discard $class
                $class = $i18n_class;
            }
            else{
                # require successful, check again
                if (! $class->can('strings')){
                    warn "i18n_class ($class) does not implement 'strings()'";
                    # discard $class
                    $class = $i18n_class;
                }
            }
        }
        $i18n_class = $class;
    }
    return $i18n_class;
}
}


# Note that -sortcmd is experimental and the interface is likely to change.
# Using -sortcmd is really strange :-(
# $top->getOpenFile(-sortcmd => sub { package Tk::FBox; uc $b cmp uc $a});
# or, un-perlish, but useable (now activated in code):
# $top->getOpenFile(-sortcmd => sub { uc $_[1] cmp uc $_[0]});

sub Populate {
    my($w, $args) = @_;

    require Tk::IconList;
    require File::Basename;
    require Cwd;
    # -strings option: a strings hash to override entries in the default set
    # provided by our i18n_class
    my $strings =  delete($args->{-strings});
    my $i18n = delete($args->{-i18n});
    my $i18n_class;
    if ( defined $i18n ){
        $i18n_class ="Tk::FBox::I18n::$i18n";
        $w->i18n_class( $i18n_class );
    }
    # merge str defaults and overrides
    my $str_defaults = $w->strings_defaults;
    for my $key (keys %$str_defaults){
        $strings->{$key} ||= $str_defaults->{$key};
    }

    $w->ConfigSpecs(-strings => ['PASSIVE',undef,undef,undef],
                    -i18n    => ['PASSIVE',undef,undef,$i18n_class],
                );
    $w->SUPER::Populate($args);
    $w->configure(-strings => $strings);
    $w->{'encoding'} = $w->getEncoding('iso_8859_1');

    # f1: the frame with the directory option menu
    my $f1 = $w->Frame;
    my $lab = $f1->Label($w->configuration_for('dir'));
    $w->{'dirMenu'} = my $dirMenu =
      $f1->Optionmenu(-variable => \$w->{'selectPath'},
		      -textvariable => \$w->{'selectPath'},
		      -command => ['SetPath', $w]);
    my $upBtn = $f1->Button;
    if (!defined $updirImage->{$w->MainWindow}) {
	$updirImage->{$w->MainWindow} = $w->Bitmap(-data => <<EOF);
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};
EOF
    }
    $upBtn->configure(-image => $updirImage->{$w->MainWindow});
    $dirMenu->configure(-takefocus => 1, -highlightthickness => 2);
    $upBtn->pack(-side => 'right', -padx => 4, -fill => 'both');
    $lab->pack(-side => 'left', -padx => 4, -fill => 'both');
    $dirMenu->pack(-expand => 'yes', -fill => 'both', -padx => 4);

    $w->{'icons'} = my $icons =
      $w->IconList(-command => ['OkCmd', $w, 'iconlist'],
		  );
    $icons->bind('<<ListboxSelect>>' => [$w, 'ListBrowse']);
    # $f4 a frame to hold OK/Cancel Buttons
    my $f4 = $w->Frame(-bd => 0);
    # f2: the frame with the "file name" field
    my $f2 = $w->Frame(-bd => 0);
    #XXX File name => File names if multiple
    my $f2_lab = $f2->Label($w->configuration_for( 'filename' ),
                            -anchor => 'e',
			    -width => 14,
                            -pady => 0);
    $w->{'ent'} = my $ent = $f2->Entry;

    # The font to use for the icons. The default Canvas font on Unix
    # is just deviant.
    # $w->{'icons'}{'font'} = $ent->cget(-font);
    $w->{'icons'}->configure(-font => $ent->cget(-font));

    # f3: the frame with the file types field
    my $f3 = $w->Frame(-bd => 0);

    # The "File of types:" label needs to be grayed-out when
    # -filetypes are not specified. The label widget does not support
    # grayed-out text on monochrome displays. Therefore, we have to
    # use a button widget to emulate a label widget (by setting its
    # bindtags)
    $w->{'typeMenuLab'} = my $typeMenuLab = $f3->Button
      ($w->configuration_for( 'filetype' ),
       -anchor  => 'e',
       -width => 14,
       -bd => $f2_lab->cget(-bd),
       -highlightthickness => $f2_lab->cget(-highlightthickness),
       -relief => $f2_lab->cget(-relief),
       -padx => $f2_lab->cget(-padx),
       -pady => $f2_lab->cget(-pady),
       -takefocus => 0,
      );
    $typeMenuLab->bindtags([$typeMenuLab, 'Label',
			    $typeMenuLab->toplevel, 'all']);
    $w->{'typeMenuBtn'} = my $typeMenuBtn =
      $f3->Menubutton(-indicatoron => 1, -tearoff => 0);
    $typeMenuBtn->configure(-takefocus => 1,
			    -highlightthickness => 2,
			    -relief => 'raised',
			    -bd => 2,
			    -anchor => 'w',
			   );

    # the okBtn is created after the typeMenu so that the keyboard traversal
    # is in the right order
    $w->{'okBtn'} = my $okBtn = $f4->Button
      ($w->configuration_for( 'btn_ok' ),
       -default => 'active',
       -pady => 3,
      );
    my $cancelBtn = $f4->Button
      ($w->configuration_for( 'btn_cancel' ),
       -default => 'normal',
       -pady => 3,
      );
    
    # pack the widgets in f2 and f3
    $okBtn->pack( -padx => 4, -anchor => 'e',-fill => 'x' ,-expand => 1);
    $f2_lab->pack(-side => 'left', -padx => 4);
    $ent->pack(-expand => 'yes', -fill => 'x', -padx => 2, -pady => 0);
    $cancelBtn->pack( -padx => 4, -anchor => 'w',-fill => 'x' ,-expand => 1);
    $typeMenuLab->pack(-side => 'left', -padx => 4);
    $typeMenuBtn->pack(-expand => 'yes', -fill => 'x', -side => 'right');

    # Pack all the frames together. We are done with widget construction.
    $f1->pack(-side => 'top', -fill => 'x', -pady => 4);
    $icons->pack(-expand => 'yes', -fill => 'both', -padx => 4, -pady => 1);
    $f4->pack(-side => 'right');
    $f3->pack(-side => 'bottom', -fill => 'x');
    $f2->pack(-side => 'bottom', -fill => 'x');

    # Set up the event handlers
    $ent->bind('<Return>',[$w,'ActivateEnt']);
    $upBtn->configure(-command => ['UpDirCmd', $w]);
    $okBtn->configure(-command => ['OkCmd', $w]);
    $cancelBtn->configure(-command, ['CancelCmd', $w]);
    $w->create_binding_for( 'dir', [$dirMenu,'focus'] );
    $w->create_binding_for( 'filetype',
                            sub  {
                                if ($typeMenuBtn->cget(-state) eq 'normal') {
                                    $typeMenuBtn->focus;
                                }
                            });
    $w->create_binding_for( 'filename', [$ent,'focus'] );
    $w->bind( '<KeyPress-Escape>', [$cancelBtn,'invoke'] );
    $w->create_binding_for( 'btn_cancel', [$cancelBtn,'invoke'] );
    $w->create_binding_for( 'btn_open', [$w,'InvokeBtn','btn_open'] );
    $w->create_binding_for( 'btn_save', [$w,'InvokeBtn','btn_save'] );
    $w->protocol('WM_DELETE_WINDOW', ['CancelCmd', $w]);
    $w->OnDestroy(['CancelCmd', $w]);

    # Build the focus group for all the entries
    $w->FG_Create;
    $w->FG_BindIn($ent, ['EntFocusIn', $w]);
    $w->FG_BindOut($ent, ['EntFocusOut', $w]);

    $w->SetPath(_cwd());

    $w->ConfigSpecs(-defaultextension => ['PASSIVE', undef, undef, undef],
		    -filetypes        => ['PASSIVE', undef, undef, undef],
		    -initialdir       => ['PASSIVE', undef, undef, undef],
		    -initialfile      => ['PASSIVE', undef, undef, undef],
#                   -sortcmd          => ['PASSIVE', undef, undef, sub { lc($a) cmp lc($b) }],
		    -sortcmd          => ['PASSIVE', undef, undef, sub { lc($_[0]) cmp lc($_[1]) }],
		    -title            => ['PASSIVE', undef, undef, undef],
		    -type             => ['PASSIVE', undef, undef, 'open'],
		    -filter           => ['PASSIVE', undef, undef, '*'],
		    -force            => ['PASSIVE', undef, undef, 0],
		    -multiple         => ['PASSIVE', undef, undef, 0],
		    'DEFAULT'         => [$icons],
		   );
    # So-far-failed attempt to break reference loops ...
    $w->_OnDestroy(qw(dirMenu icons typeMenuLab typeMenuBtn okBtn ent updateId));
    $w;
}

# -initialdir fix with ResolveFile
sub Show {
    my $w = shift;

    $w->configure(@_);

    # Dialog boxes should be transient with respect to their parent,
    # so that they will always stay on top of their parent window.  However,
    # some window managers will create the window as withdrawn if the parent
    # window is withdrawn or iconified.  Combined with the grab we put on the
    # window, this can hang the entire application.  Therefore we only make
    # the dialog transient if the parent is viewable.

    if (Tk::Exists($w->Parent) && $w->Parent->viewable) {
	$w->transient($w->Parent);
    }

    # set the default directory and selection according to the -initial
    # settings
    {
	my $initialdir = $w->cget(-initialdir);
	if (defined $initialdir) {
	    my ($flag, $path, $file) = ResolveFile($initialdir, 'junk');
	    if ($flag eq 'OK' or $flag eq 'FILE') {
		$w->{'selectPath'} = $path;
	    } else {
		$w->Error("\"$initialdir\" is not a valid directory");
	    }
	}
	$w->{'selectFile'} = $w->cget(-initialfile);
    }

    # Set -multiple to a one or zero value (not other boolean types
    # like "yes") so we can use it in tests more easily.
    if ($w->cget('-type') ne 'open') {
	$w->configure(-multiple => 0);
    } else {
	$w->configure(-multiple => !!$w->cget('-multiple'));
    }
    $w->{'icons'}->configure(-multiple => $w->cget('-multiple'));

    # Initialize the file types menu
    my $typeMenuBtn = $w->{'typeMenuBtn'};
    my $typeMenuLab = $w->{'typeMenuLab'};
    if (defined $w->cget('-filetypes')) {
	my(@filetypes) = GetFileTypes($w->cget('-filetypes'));
	my $typeMenu = $typeMenuBtn->cget(-menu);
	$typeMenu->delete(0, 'end');
	foreach my $ft (@filetypes) {
	    my $title  = $ft->[0];
	    my $filter = join(' ', @{ $ft->[1] });
	    $typeMenuBtn->command
	      (-label => $title,
	       -command => ['SetFilter', $w, $title, $filter],
	      );
	}
	$w->SetFilter($filetypes[0]->[0], join(' ', @{ $filetypes[0]->[1] }));
	$typeMenuBtn->configure(-state => 'normal');
	$typeMenuLab->configure(-state => 'normal');
    } else {
#XXX    $w->configure(-filter => '*');
	$typeMenuBtn->configure(-state => 'disabled',
				-takefocus => 0);
	$typeMenuLab->configure(-state => 'disabled');
    }
    $w->UpdateWhenIdle;

    {
	my $title = $w->cget(-title);
	if (!defined $title) {
	    my $type = $w->cget(-type)||'open';
            my $str  = $w->cget('-strings');
	    $title = $str->{'title_'.$type};
	}
	$w->title($title);
    }

    # Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.
    $w->withdraw;
    $w->idletasks;
    if (0)
     {
      #XXX use Tk::Wm::Popup? or Tk::PlaceWindow?
      my $x = int($w->screenwidth / 2 - $w->reqwidth / 2 - $w->parent->vrootx);
      my $y = int($w->screenheight / 2 - $w->reqheight / 2 - $w->parent->vrooty);
      $w->geometry("+$x+$y");
      $w->deiconify;
     }
    else
     {
      $w->Popup;
     }

    # Set a grab and claim the focus too.
#XXX use Tk::setFocusGrab when it's available
    my $oldFocus = $w->focusCurrent;
    my $oldGrab = $w->grabCurrent;
    my $grabStatus; $grabStatus = $oldGrab->grabStatus if ($oldGrab);
    $w->grab;
    my $ent = $w->{'ent'};
    $ent->focus;
    $ent->delete(0, 'end');
    if (defined $w->{'selectFile'} && $w->{'selectFile'} ne '') {
	$ent->insert(0, $w->{'selectFile'});
	$ent->selectionRange(0,'end');
	$ent->icursor('end');
    }

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.
    $w->waitVariable(\$w->{'selectFilePath'});
    eval {
	$oldFocus->focus if $oldFocus;
    };
    if (Tk::Exists($w)) { # widget still exists
	$w->grabRelease;
	$w->withdraw;
    }
    if (Tk::Exists($oldGrab) && $oldGrab->viewable) {
	if ($grabStatus eq 'global') {
	    $oldGrab->grabGlobal;
	} else {
	    $oldGrab->grab;
	}
    }
    return $w->{'selectFilePath'};
}

# tkFDialog_UpdateWhenIdle --
#
#       Creates an idle event handler which updates the dialog in idle
#       time. This is important because loading the directory may take a long
#       time and we don't want to load the same directory for multiple times
#       due to multiple concurrent events.
#
sub UpdateWhenIdle {
    my $w = shift;
    if (exists $w->{'updateId'}) {
	return;
    } else {
	$w->{'updateId'} = $w->after('idle', [$w, 'Update']);
    }
}

# tkFDialog_Update --
#
#       Loads the files and directories into the IconList widget. Also
#       sets up the directory option menu for quick access to parent
#       directories.
#
sub Update {
    my $w = shift;
    my $dataName = $w->name;

    # This proc may be called within an idle handler. Make sure that the
    # window has not been destroyed before this proc is called
    if (!Tk::Exists($w) || $w->class ne 'FBox') {
	return;
    } else {
	delete $w->{'updateId'};
    }
    unless (defined $folderImage->{$w->MainWindow}) {
	require Tk::Pixmap;
	$folderImage->{$w->MainWindow} = $w->Pixmap(-file => Tk->findINC('folder.xpm'));
	$fileImage->{$w->MainWindow}   = $w->Pixmap(-file => Tk->findINC('file.xpm'));
    }
    my $folder = $folderImage->{$w->MainWindow};
    my $file   = $fileImage->{$w->MainWindow};
    my $appPWD = _cwd();
    if (!ext_chdir($w->_get_select_path)) {
	# We cannot change directory to $data(selectPath). $data(selectPath)
	# should have been checked before tkFDialog_Update is called, so
	# we normally won't come to here. Anyways, give an error and abort
	# action.
	$w->messageBox(-type => 'OK',
		       -message => $w->message_for('msg_chdir',
                                                   $w->_get_select_path
                                                   ),
		       -icon => 'warning',
		      );
	ext_chdir($appPWD);
	return;
    }

    # Turn on the busy cursor. BUG?? We haven't disabled X events, though,
    # so the user may still click and cause havoc ...
    my $ent = $w->{'ent'};
    my $entCursor = $ent->cget(-cursor);
    my $dlgCursor = $w->cget(-cursor);
    $ent->configure(-cursor => 'watch');
    $w->configure(-cursor => 'watch');
    $w->idletasks;
    my $icons = $w->{'icons'};
    $icons->DeleteAll;

    # Make the dir & file list
    my $cwd = _cwd();
    local *FDIR;
    if (opendir(FDIR, $cwd)) {
	my @files;
#       my $sortcmd = $w->cget(-sortcmd);
	my $sortcmd = sub { $w->cget(-sortcmd)->($a,$b) };
	my $flt = $w->cget(-filter);
	my $fltcb;
	if (ref $flt eq 'CODE') {
	    $fltcb = $flt;
	} else {
	    $flt = _rx_to_glob($flt);
	}
	my $type_dir = $w->cget(-type) eq 'dir';
	foreach my $f (sort $sortcmd readdir(FDIR)) {
	    next if $f eq '.' or $f eq '..';
	    next if $type_dir && ! -d "$cwd/$f"; # XXX use File::Spec?
	    if ($fltcb) {
		next if !$fltcb->($w, $f, $cwd);
	    } else {
		next if !-d $f && $f !~ m!$flt!s;
	    }
	    if (-d $f) {
		$icons->Add($folder, $f);
	    } else {
		push @files, $f;
	    }
	}
	closedir(FDIR);
	$icons->Add($file, @files);
    }

    $icons->Arrange;

    # Update the Directory: option menu
    my @list;
    my $dir = '';
    foreach my $subdir (TclFileSplit($w->_get_select_path)) {
	$dir = TclFileJoin($dir, $subdir);
	push @list, $dir;
    }
    my $dirMenu = $w->{'dirMenu'};
    $dirMenu->configure(-options => \@list);

    # Restore the PWD to the application's PWD
    ext_chdir($appPWD);

    # Restore the Save label
    if ($w->cget(-type) eq 'save') {
	$w->{'okBtn'}->configure($w->configuration_for('btn_save'));
    }

    # turn off the busy cursor.
    $ent->configure(-cursor => $entCursor);
    $w->configure(-cursor =>  $dlgCursor);
}

# tkFDialog_SetPathSilently --
#
#       Sets data(selectPath) without invoking the trace procedure
#
sub SetPathSilently {
    my($w, $path) = @_;

    $w->{'selectPath'} = $path;
}

# This proc gets called whenever data(selectPath) is set
#
sub SetPath {
    my $w = shift;
    $w->{'selectPath'} = $_[0] if @_;
    $w->UpdateWhenIdle;
}

# This proc gets called whenever data(filter) is set
#
#XXX here's much more code in the tcl version ... check it out
sub SetFilter {
    my($w, $title, $filter) = @_;
    $w->configure(-filter => $filter);
    $w->{'typeMenuBtn'}->configure(-text => $title,
				   -indicatoron => 1);
    $w->{'icons'}->Subwidget('sbar')->set(0.0, 0.0);
    $w->UpdateWhenIdle;
}

# tkFDialogResolveFile --
#
#       Interpret the user's text input in a file selection dialog.
#       Performs:
#
#       (1) ~ substitution
#       (2) resolve all instances of . and ..
#       (3) check for non-existent files/directories
#       (4) check for chdir permissions
#
# Arguments:
#       context:  the current directory you are in
#       text:     the text entered by the user
#       defaultext: the default extension to add to files with no extension
#
# Return value:
#       [list $flag $directory $file]
#
#        flag = OK      : valid input
#             = PATTERN : valid directory/pattern
#             = PATH    : the directory does not exist
#             = FILE    : the directory exists but the file doesn't
#                         exist
#             = CHDIR   : Cannot change to the directory
#             = ERROR   : Invalid entry
#
#        directory      : valid only if flag = OK or PATTERN or FILE
#        file           : valid only if flag = OK or PATTERN
#
#       directory may not be the same as context, because text may contain
#       a subdirectory name
#
sub ResolveFile {
    my($context, $text, $defaultext) = @_;
    my $appPWD = _cwd();
    my $path = JoinFile($context, $text);
    # If the file has no extension, append the default.  Be careful not
    # to do this for directories, otherwise typing a dirname in the box
    # will give back "dirname.extension" instead of trying to change dir.
    if (!-d $path && $path !~ /\..+$/s && defined $defaultext) {
	$path = "$path$defaultext";
    }
    # Cannot just test for existance here as non-existing files are
    # not an error for getSaveFile type dialogs.
    # return ('ERROR', $path, "") if (!-e $path);
    my($directory, $file, $flag);
    if (-e $path) {
	if (-d $path) {
	    if (!ext_chdir($path)) {
		return ('CHDIR', $path, '');
	    }
	    $directory = _cwd();
	    $file = '';
	    $flag = 'OK';
	    ext_chdir($appPWD);
	} else {
	    my $dirname = File::Basename::dirname($path);
	    if (!ext_chdir($dirname)) {
		return ('CHDIR', $dirname, '');
	    }
	    $directory = _cwd();
	    $file = File::Basename::basename($path);
	    $flag = 'OK';
	    ext_chdir($appPWD);
	}
    } else {
	my $dirname = File::Basename::dirname($path);
	if (-e $dirname) {
	    if (!ext_chdir($dirname)) {
		return ('CHDIR', $dirname, '');
	    }
	    $directory = _cwd();
	    $file = File::Basename::basename($path);
	    if ($file =~ /[*?]/) {
		$flag = 'PATTERN';
	    } else {
		$flag = 'FILE';
	    }
	    ext_chdir($appPWD);
	} else {
	    $directory = $dirname;
	    $file = File::Basename::basename($path);
	    $flag = 'PATH';
	}
    }
    return ($flag,$directory,$file);
}

# Gets called when the entry box gets keyboard focus. We clear the selection
# from the icon list . This way the user can be certain that the input in the
# entry box is the selection.
#
sub EntFocusIn {
    my $w = shift;
    my $ent = $w->{'ent'};
    if ($ent->get ne '') {
	$ent->selectionRange(0, 'end');
	$ent->icursor('end');
    } else {
	$ent->selectionClear;
    }
#XXX is this missing in the tcl version, too???    $w->{'icons'}->Selection('clear');
    my $okBtn = $w->{'okBtn'};
    if ($w->cget(-type) ne 'save') {
	$okBtn->configure($w->configuration_for('btn_open'));
    } else {
	$okBtn->configure($w->configuration_for('btn_save'));
    }
}

sub EntFocusOut {
    my $w = shift;
    $w->{'ent'}->selectionClear;
}

# Gets called when user presses Return in the "File name" entry.
#
sub ActivateEnt {
    my $w = shift;
    if ($w->cget(-multiple)) {
	# For the multiple case we have to be careful to get the file
	# names as a true list, watching out for a single file with a
	# space in the name.  Thus we query the IconList directly.

	$w->{'selectFile'} = [];
	for my $item ($w->{'icons'}->Curselection) {
	    $w->VerifyFileName($w->_get_from_icons($item));
	}
    } else {
	my $ent = $w->{'ent'};
	my $text = $w->_encode_filename($ent->get);
	$w->VerifyFileName($text);
    }
}

# Verification procedure
#
sub VerifyFileName {
    my($w, $text) = @_;
#XXX leave this here?
#    $text =~ s/^\s+//;
#    $text =~ s/\s+$//;
    my($flag, $path, $file) = ResolveFile($w->_get_select_path, $text,
					  $w->cget(-defaultextension));
    my $ent = $w->{'ent'};
    if ($flag eq 'OK') {
	if ($file eq '') {
	    # user has entered an existing (sub)directory
	    $w->SetPath($path);
	    $ent->delete(0, 'end');
	} else {
	    $w->SetPathSilently($path);
	    if ($w->cget(-multiple)) {
		push @{ $w->{'selectFile'} }, $file;
	    } else {
		$w->{'selectFile'} = $file;
	    }
	    $w->Done;
	}
    } elsif ($flag eq 'PATTERN') {
	$w->SetPath($path);
	$w->configure(-filter => $file);
    } elsif ($flag eq 'FILE') {
	if ($w->cget(-type) eq 'open') {
	    $w->messageBox(-icon => 'warning',
			   -type => 'OK',
			   -message => $w->message_for(
                                             'msg_file_dne',
                                             TclFileJoin($path, $file)
                                            ),
                           );
#('File "' . TclFileJoin($path, $file)
		#	   . '" does not exist.');
	    $ent->selectionRange(0, 'end');
	    $ent->icursor('end');
	} elsif ($w->cget(-type) eq 'save') {
	    $w->SetPathSilently($path);
	    if ($w->cget(-multiple)) {
		push @{ $w->{'selectFile'} }, $file;
	    } else {
		$w->{'selectFile'} = $file;
	    }
	    $w->Done;
	}
    } elsif ($flag eq 'PATH') {
	$w->messageBox(-icon => 'warning',
		       -type => 'OK',
		       -message => $w->message_for(
                                                   'msg_dir_dne',
                                                   $path,
                                               ),
                       );;
	$ent->selectionRange(0, 'end');
	$ent->icursor('end');
    } elsif ($flag eq 'CHDIR') {
	$w->messageBox(-type => 'OK',
		       -message => $w->message_for('msg_chdir',
                                                   $path),
		       -icon => 'warning');
	$ent->selectionRange(0, 'end');
	$ent->icursor('end');
    } elsif ($flag eq 'ERROR') {
	$w->messageBox(-type => 'OK',
		       -message => $w->message_for('msg_invalid',
                                                   $path),
		       -icon => 'warning');
	$ent->selectionRange(0, 'end');
	$ent->icursor('end');
    }
}

# Gets called when user presses the Alt-s or Alt-o keys.
#
sub InvokeBtn {
    my($w, $key) = @_;
    my $okBtn = $w->{'okBtn'};
    my $type = $w->cget('-type');
    my $label = "btn_$type";
    ### $type
    $okBtn->invoke if ($label eq $key);
}

# Gets called when user presses the "parent directory" button
#
sub UpDirCmd {
    my $w = shift;
    $w->SetPath(File::Basename::dirname($w->_get_select_path))
      unless ($w->_get_select_path eq '/');
}

# Join a file name to a path name. The "file join" command will break
# if the filename begins with ~
sub JoinFile {
    my($path, $file) = @_;
    if ($file =~ /^~/ && -e "$path/$file") {
	TclFileJoin($path, "./$file");
    } else {
	TclFileJoin($path, $file);
    }
}

# XXX replace with File::Spec if possible
sub TclFileJoin {
    my $path = '';
    foreach (@_) {
	if (m|^/|) {
	    $path = $_;
	} elsif (m|^[a-z]:/|i) {  # DOS-ish
	    $path = $_;
	} elsif ($_ eq '~') {
	    $path = _get_homedir();
	} elsif (m|^~/(.*)|) {
	    $path = _get_homedir() . "/" . $1;
	} elsif (m|^~([^/]+)(.*)|) {
	    my($user, $p) = ($1, $2);
	    my $dir = _get_homedir($user);
	    if (!defined $dir) {
		$path = "~$user$p";
	    } else {
		$path = $dir . $p;
	    }
	} elsif ($path eq '/' or $path eq '') {
	    $path .= $_;
	} else {
	    $path .= "/$_";
	}
    }
    $path;
}

sub TclFileSplit {
    my $path = shift;
    my @comp;
    $path =~ s|/+|/|g; # strip multiple slashes
    if ($path =~ m|^/|) {
	push @comp, '/';
	$path = substr($path, 1);
    }
    push @comp, split /\//, $path;
    @comp;
}

# Gets called when user presses the "OK" button
#
sub OkCmd {
    my $w = shift;
    my $from = shift || "button";

    my $filenames = [];
    for my $item ($w->{'icons'}->Curselection) {
	push @$filenames, $w->_get_from_icons($item);
    }

    my $filename = $filenames->[0];
    $filename = "" if !defined $filename;
    if ($w->cget('-type') eq 'dir' && $from ne "iconlist") {
	my $file = $filename eq '' ? $w->_get_select_path : JoinFile($w->_get_select_path, $filename);
	$w->Done($file);
    } elsif ((@$filenames && !$w->cget('-multiple')) ||
	($w->cget('-multiple') && @$filenames == 1)) {
	my $file = JoinFile($w->_get_select_path, $filename);
	if (-d $file) {
	    $w->ListInvoke($filename);
	    return;
	}
    }

    $w->ActivateEnt;
}

# Gets called when user presses the "Cancel" button
#
sub CancelCmd {
    my $w = shift;
    undef $w->{'selectFilePath'};
}

# Gets called when user browses the IconList widget (dragging mouse, arrow
# keys, etc)
#
sub ListBrowse {
    my($w) = @_;

    my $text = [];
    for my $item ($w->{'icons'}->Curselection) {
	push @$text, $w->_get_from_icons($item);
    }
    return if @$text == 0;
    my $isDir;
    if (@$text > 1) {
	my $newtext = [];
	for my $file (@$text) {
	    my $fullfile = JoinFile($w->_get_select_path, $file);
	    if (!-d $fullfile) {
		push @$newtext, $file;
	    }
	}
	$text = $newtext;
	$isDir = 0;
    } else {
	my $file = JoinFile($w->_get_select_path, $text->[0]);
	$isDir = -d $file;
    }
    my $ent = $w->{'ent'};
    my $okBtn = $w->{'okBtn'};
    if (!$isDir) {
	$ent->delete(qw(0 end));
	$ent->insert(0, "@$text"); # XXX quote!

	if ($w->cget('-type') ne 'save') {
	    $okBtn->configure($w->configuration_for( 'btn_open' ));
	} else {
	    $okBtn->configure($w->configuration_for( 'btn_save' ));
	}
    } else {
	$okBtn->configure($w->configuration_for('btn_open'));
    }
}

# Gets called when user invokes the IconList widget (double-click,
# Return key, etc)
#
sub ListInvoke {
    my($w, @filenames) = @_;
    return if !@filenames;
    my $file = JoinFile($w->_get_select_path, $filenames[0]);
    if (-d $file) {
	my $appPWD = _cwd();
	if (!ext_chdir($file)) {
	    $w->messageBox(-type => 'OK',
			   -message => $w->message_for('msg_chdir',
                                                       $file),
			   -icon => 'warning');
	} else {
	    ext_chdir($appPWD);
	    $w->SetPath($file);
	}
    } else {
	if ($w->cget('-multiple')) {
	    $w->{'selectFile'} = [@filenames];
	} else {
	    $w->{'selectFile'} = $file;
	}
	$w->Done;
    }
}

# tkFDialog_Done --
#
#       Gets called when user has input a valid filename.  Pops up a
#       dialog box to confirm selection when necessary. Sets the
#       tkPriv(selectFilePath) variable, which will break the "tkwait"
#       loop in tkFDialog and return the selected filename to the
#       script that calls tk_getOpenFile or tk_getSaveFile
#
sub Done {
    my $w = shift;
    my $selectFilePath = (@_) ? shift : '';
    if ($selectFilePath eq '') {
	if ($w->cget('-multiple')) {
	    $selectFilePath = [];
	    for my $f (@{ $w->{'selectFile'} }) {
		push @$selectFilePath, JoinFile($w->_get_select_path, $f);
	    }
	} else {
	    $selectFilePath = JoinFile($w->_get_select_path,
				       $w->{'selectFile'});
	}
	if ($w->cget(-type) eq 'save' and
	    -e $selectFilePath and
	    !$w->cget(-force)) {
	    my $reply = $w->messageBox
	      (-icon => 'warning',
	       -type => 'YesNo',
	       -message => $w->message_for('msg_exists',
                                           $selectFilePath),
               );
	    return unless (lc($reply) eq 'yes');
	}
    }
    $w->{'selectFilePath'} = ($selectFilePath ne '' ? $selectFilePath : undef);
}

sub FDialog {
    my $cmd = shift;
    if ($cmd =~ /Save/) {
	push @_, -type => 'save';
    } elsif ($cmd =~ /Directory/) {
        push @_, -type => 'dir';
    }
    Tk::DialogWrapper('FBox', $cmd, @_);
}

# tkFDGetFileTypes --
#
#       Process the string given by the -filetypes option of the file
#       dialogs. Similar to the C function TkGetFileFilters() on the Mac
#       and Windows platform.
#
sub GetFileTypes {
    my $in = shift;
    my %fileTypes;
    foreach my $t (@$in) {
	if (@$t < 2  || @$t > 3) {
	    require Carp;
	    Carp::croak("bad file type \"$t\", should be \"typeName [extension ?extensions ...?] ?[macType ?macTypes ...?]?\"");
	}
	push @{ $fileTypes{$t->[0]} }, (ref $t->[1] eq 'ARRAY'
					? @{ $t->[1] }
					: $t->[1]);
    }

    my @types;
    my %hasDoneType;
    my %hasGotExt;
    foreach my $t (@$in) {
	my $label = $t->[0];
	my @exts;

	next if (exists $hasDoneType{$label});

	my $name = "$label (";
	my $sep = '';
	foreach my $ext (@{ $fileTypes{$label} }) {
	    next if ($ext eq '');
	    $ext =~ s/^\./*./;
	    if (!exists $hasGotExt{$label}->{$ext}) {
		$name .= "$sep$ext";
		push @exts, $ext;
		$hasGotExt{$label}->{$ext}++;
	    }
	    $sep = ',';
	}
	$name .= ')';
	push @types, [$name, \@exts];

	$hasDoneType{$label}++;
    }

    return @types;
}

# ext_chdir --
#
#       Change directory with tilde substitution
#
sub ext_chdir {
    my $dir = shift;
    if ($dir eq '~') {
	chdir _get_homedir();
    } elsif ($dir =~ m|^~/(.*)|s) {
	chdir _get_homedir() . "/" . $1;
    } elsif ($dir =~ m|^~([^/]+(.*))|s) {
	chdir _get_homedir($1) . $2;
    } else {
	chdir $dir;
    }
}

# _get_homedir --
#
#       Get home directory of the current user
#
sub _get_homedir {
    my($user) = @_;
    if (!defined $user) {
	eval {
	    local $SIG{__DIE__};
	    (getpwuid($<))[7];
	} || $ENV{HOME} || undef; # chdir undef changes to home directory, too
    } else {
	eval {
	    local $SIG{__DIE__};
	    (getpwnam($user))[7];
	};
    }
}

sub _cwd {
    #Cwd::cwd();
    Cwd::fastcwd(); # this is taint-safe
}

sub _untaint {
    my $s = shift;
    $s =~ /^(.*)$/;
    $1;
}

sub _rx_to_glob {
    my $arg = shift;
    $arg =~ s!([.+^()|\${}\[\]\\])!\\$1!g;
    $arg = join('|', map {"^$_\\z"} split(' ', $arg));
    $arg =~ s!\*!.*!g;$arg =~ s!\?!.!g;
    $arg = qr/$arg/s;
    $arg;
}

sub _get_from_icons {
    my($w, $item) = @_;
    $w->_encode_filename($w->{'icons'}->Get($item));
}

sub _get_select_path {
    my($w) = @_;
    $w->_encode_filename($w->{'selectPath'});
}

sub _encode_filename {
    my($w, $filename) = @_;
    $filename = $w->{encoding}->encode($filename);
    $filename;
}

1;

