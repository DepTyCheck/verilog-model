-- Seed: 7169589841030920781,15300320181035395489

entity bgs is
  port (cdr : buffer real);
end bgs;

architecture g of bgs is
  
begin
  -- Single-driven assignments
  cdr <= 16#C.B_1_1_B#;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (ymupd : buffer std_logic);
end d;

architecture feqedu of d is
  signal mipjsvmc : real;
  signal cgrgtjsu : real;
  signal rhtal : real;
begin
  fnkwux : entity work.bgs
    port map (cdr => rhtal);
  lpyobehoxx : entity work.bgs
    port map (cdr => cgrgtjsu);
  qzuvgzfoeg : entity work.bgs
    port map (cdr => mipjsvmc);
  
  -- Multi-driven assignments
  ymupd <= 'X';
  ymupd <= 'Z';
end feqedu;

library ieee;
use ieee.std_logic_1164.all;

entity nge is
  port (rfnvioo : inout std_logic; asye : linkage integer);
end nge;

architecture jazdcdw of nge is
  signal lbhmd : real;
begin
  eutli : entity work.bgs
    port map (cdr => lbhmd);
  
  -- Multi-driven assignments
  rfnvioo <= 'W';
  rfnvioo <= 'X';
  rfnvioo <= 'U';
end jazdcdw;

entity x is
  port (isvazt : buffer time);
end x;

architecture xrwcstzykm of x is
  signal iw : real;
begin
  qnoy : entity work.bgs
    port map (cdr => iw);
  
  -- Single-driven assignments
  isvazt <= 8#1# fs;
end xrwcstzykm;



-- Seed after: 4258568991870436678,15300320181035395489
