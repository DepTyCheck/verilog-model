-- Seed: 11635600423634195012,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity hiul is
  port (rbcrs : linkage time; vslqjcxf : buffer std_logic_vector(0 to 1); yjd : inout time);
end hiul;

architecture l of hiul is
  
begin
  -- Single-driven assignments
  yjd <= 1_4 us;
end l;

entity nbt is
  port (vomjkl : in integer);
end nbt;

library ieee;
use ieee.std_logic_1164.all;

architecture bo of nbt is
  signal nftxhqpcg : time;
  signal etreca : std_logic_vector(0 to 1);
  signal zjculn : time;
begin
  tfojm : entity work.hiul
    port map (rbcrs => zjculn, vslqjcxf => etreca, yjd => nftxhqpcg);
  
  -- Multi-driven assignments
  etreca <= ('H', 'W');
  etreca <= "LH";
  etreca <= "WX";
  etreca <= "0H";
end bo;

entity csdg is
  port (cv : out integer);
end csdg;

library ieee;
use ieee.std_logic_1164.all;

architecture wxmgcma of csdg is
  signal cmerpraklu : time;
  signal ynzulhulzp : time;
  signal qkxur : time;
  signal hngmjhggcw : std_logic_vector(0 to 1);
  signal svzftdcimh : time;
  signal kcbqcc : integer;
begin
  uqsjcr : entity work.nbt
    port map (vomjkl => kcbqcc);
  mevzgn : entity work.hiul
    port map (rbcrs => svzftdcimh, vslqjcxf => hngmjhggcw, yjd => qkxur);
  qqszuej : entity work.hiul
    port map (rbcrs => ynzulhulzp, vslqjcxf => hngmjhggcw, yjd => cmerpraklu);
  
  -- Multi-driven assignments
  hngmjhggcw <= "ZU";
  hngmjhggcw <= "0W";
  hngmjhggcw <= ('-', 'U');
  hngmjhggcw <= "X-";
end wxmgcma;



-- Seed after: 15049654633973641681,4860866131898729603
