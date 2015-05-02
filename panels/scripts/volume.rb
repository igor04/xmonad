class Volume
  def initialize(colors)
    @colors = {
      h: 'df0000',
      t: 'ff5f00',
      m: 'af8700',
      b: 'bcbcbc',
      l: '808080'
    }

    @colors.merge!(colors) if colors
  end

  def volume
    unless @volume
      data = `amixer sget Master`
      @volume = data.match(/([0-9]*)%/)[1]
    end

    @volume
  end

  def color
    case volume.to_i
    when 71..90 then @colors[:t]
    when 41..70 then @colors[:m]
    when 11..40 then @colors[:b]
    when 0..10 then @colors[:l]
    else @colors[:h]
    end
  end

  def to_format_xmobar ()
    "VoL: <fc=##{color}>#{volume}%</fc>"
  end

  def to_format_conky ()
    "^fg(##{color})^i(/home/xim/.xmonad/panels/icon/spkr_01.xbm) #{volume}%^fg()"
  end
end
