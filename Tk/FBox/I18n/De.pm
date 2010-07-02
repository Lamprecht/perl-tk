package Tk::FBox::I18n::De;
use strict;
use warnings;

sub strings{
    return {title_open   => 'Öffnen',
            title_save   => 'Speichern als',
            title_dir    => 'Verzeichnis Auswahl',
            dir          => '_Verzeichnis:',
            filename     => 'Datei_name:',
            filetype     => 'Dateitypen:',
            btn_ok       => 'Ok',
            btn_save     => '_Speichern',
            btn_open     => 'Ö_ffnen',
            btn_cancel   => '_Abbruch',
            msg_file_dne => 'Die Datei "%PATH%" existiert nicht.',
            msg_dir_dne  => 'Das Verzeichnis "%PATH%" existiert nicht.',
            msg_chdir    => 'In das Verzeichnis "%PATH%" kann nicht gewechselt'
            ."werden.\nZugriff verweigert.",
            msg_invalid  => 'Ungültiger Dateiname: "%PATH%"',
            msg_exists   => 'Die Datei "%PATH%" exisitiert bereits.'
            ."\nWollen Sie sie überschreiben?",
        };
}
1;
