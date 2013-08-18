#!/usr/bin/env bash

# fresh-chrome
#
# Use this script on OS X to launch a new instance of Google Chrome
# with its own empty cache, cookies, and user configuration.
#
# The first time you run this script, it will launch a new Google
# Chrome instance with a permanent user-data directory, which you can
# customize below. Perform any initial setup you want to keep on every
# new Chrome instance, such as adding bookmarks and extensions. Then
# quit this Chrome instance with Command-Q or by selecting "Quit" from
# the "Chrome" menu. (The red "close" button is not sufficient.)
#
# AFTER that, every time you run this script it will launch a new
# Google Chrome instance with a temporary user-data directory copied
# from the one you set up the first time you ran this script. Every
# new instance of Google Chrome launched by this script will be
# completely isolated from the others.



### Customize these

# Permanent directory to store the user-data directory of your 'fresh'
# Chrome configuration.
fresh_dir="$HOME/.fresh-chrome"

# Temporary directory in which to create new user-data directories for
# temporary Chrome instances.
tmp_dir="/tmp"



### Main script begins

set -e

timestamp=`date +%Y%m%d%H%M%S`

if [[ -e "$fresh_dir" ]]; then
    user_dir="$tmp_dir/chrome-$timestamp-$RANDOM"
    cp -r "$fresh_dir" "$user_dir"
    exec open -na "Google Chrome" --args "--user-data-dir=$user_dir"
else
    exec open -na "Google Chrome" --args "--user-data-dir=$fresh_dir"
fi


# The MIT License (MIT)
#
# Copyright (c) 2013 Stuart Sierra
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
