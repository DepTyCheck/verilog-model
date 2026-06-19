-- Seed: 2620675661874572074,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity rjou is
  port (hgzwef : inout std_logic; zahgndrn : buffer std_logic; sgjeupqoa : linkage integer_vector(0 to 1); s : out integer);
end rjou;

architecture w of rjou is
  
begin
  -- Single-driven assignments
  s <= 8#0_7#;
  
  -- Multi-driven assignments
  zahgndrn <= 'X';
  zahgndrn <= 'X';
end w;

entity mscl is
  port (owp : buffer real; qug : inout integer);
end mscl;

library ieee;
use ieee.std_logic_1164.all;

architecture o of mscl is
  signal sbzwujwp : integer;
  signal bjkskrkot : integer_vector(0 to 1);
  signal awelthdjgv : std_logic;
  signal nybmt : std_logic;
  signal d : integer_vector(0 to 1);
  signal upnrvzdlh : std_logic;
  signal jzytjnmvgl : integer;
  signal lx : integer_vector(0 to 1);
  signal zjiifis : std_logic;
begin
  wxgattc : entity work.rjou
    port map (hgzwef => zjiifis, zahgndrn => zjiifis, sgjeupqoa => lx, s => jzytjnmvgl);
  emqzna : entity work.rjou
    port map (hgzwef => zjiifis, zahgndrn => upnrvzdlh, sgjeupqoa => d, s => qug);
  lvqjem : entity work.rjou
    port map (hgzwef => nybmt, zahgndrn => awelthdjgv, sgjeupqoa => bjkskrkot, s => sbzwujwp);
  
  -- Single-driven assignments
  owp <= 0.4;
  
  -- Multi-driven assignments
  nybmt <= '1';
  upnrvzdlh <= 'W';
end o;



-- Seed after: 9081669129188849135,3108530264173481209
