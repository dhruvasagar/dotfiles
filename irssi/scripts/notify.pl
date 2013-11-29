##
## Put me in ~/.irssi/scripts, and then execute the following in irssi:
##
##       /load perl
##       /script load notify
##

use strict;
use Irssi;
use vars qw($VERSION %IRSSI);

$VERSION = "0.02";
%IRSSI = (
    authors     => 'Luke Macken, Dhruva Sagar',
    contact     => 'lewk@csh.rit.edu, dhruva.sagar@gmail.com',
    name        => 'notify.pl',
    description => 'Uses libnotify to notify hilights and messages',
    license     => 'GNU General Public License',
    url         => 'http://lewk.org/log/code/irssi-notify',
);

sub notify {
    my ($dest, $text, $stripped) = @_;
    my $server = $dest->{server};

    return if (!$server || !($dest->{level} & MSGLEVEL_HILIGHT || $dest->{level} & MSGLEVEL_MSGS));

    $stripped =~ s/[^a-zA-Z0-9 .,!?\@:\>]//g;
    system("notify-send -i gtk-dialog-info -t 5000 '$dest->{target}' '$stripped'");
}

Irssi::settings_add_str('misc', 'notify_icon', "gtk-dialog-info");
Irssi::signal_add('print text', 'notify');
