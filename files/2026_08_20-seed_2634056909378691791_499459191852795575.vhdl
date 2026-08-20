-- Seed: 2634056909378691791,499459191852795575

entity fe is
  port (hrrwi : inout string(3 downto 1));
end fe;

architecture cqx of fe is
  
begin
  -- Single-driven assignments
  hrrwi <= "jcb";
end cqx;

entity pnwk is
  port (ug : in real_vector(0 to 3); lyghlccu : inout real);
end pnwk;

architecture fr of pnwk is
  signal jcugyngzv : string(3 downto 1);
  signal pakxmvhlr : string(3 downto 1);
  signal njox : string(3 downto 1);
begin
  hme : entity work.fe
    port map (hrrwi => njox);
  fnniszd : entity work.fe
    port map (hrrwi => pakxmvhlr);
  hwiey : entity work.fe
    port map (hrrwi => jcugyngzv);
  
  -- Single-driven assignments
  lyghlccu <= lyghlccu;
end fr;

library ieee;
use ieee.std_logic_1164.all;

entity ltsh is
  port (herml : buffer time; t : out std_logic);
end ltsh;

architecture tux of ltsh is
  signal fo : string(3 downto 1);
  signal x : real;
  signal qjtdizq : real_vector(0 to 3);
  signal pxidfsb : string(3 downto 1);
begin
  l : entity work.fe
    port map (hrrwi => pxidfsb);
  plebp : entity work.pnwk
    port map (ug => qjtdizq, lyghlccu => x);
  rbk : entity work.fe
    port map (hrrwi => fo);
  
  -- Single-driven assignments
  herml <= herml;
  qjtdizq <= qjtdizq;
end tux;

entity soazgjh is
  port (u : in boolean; patvnd : inout boolean; fg : inout time);
end soazgjh;

architecture qyai of soazgjh is
  
begin
  
end qyai;



-- Seed after: 7453961692547441656,499459191852795575
