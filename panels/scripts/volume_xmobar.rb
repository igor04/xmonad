#!/usr/bin/env ruby
require '~/.xmonad/panels/scripts/volume.rb'

colors = {}
ARGV.join(' ').split('-').each do |arg|
  next if arg.empty?
  arg = arg.split(' ')
  colors[arg[0].to_sym] = arg[1]
end

$stdout << Volume.new(colors).to_format_xmobar
