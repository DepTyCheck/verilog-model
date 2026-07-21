-- Seed: 2484811320554198571,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity iwgplm is
  port (cetxfbbf : buffer std_logic_vector(2 to 0); gyu : buffer std_logic_vector(2 downto 0));
end iwgplm;

architecture stkpzvyio of iwgplm is
  
begin
  
end stkpzvyio;

entity y is
  port (ufsen : inout string(1 to 2));
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture nyf of y is
  signal hyiovcebq : std_logic_vector(2 downto 0);
  signal dd : std_logic_vector(2 to 0);
begin
  jmnspjpi : entity work.iwgplm
    port map (cetxfbbf => dd, gyu => hyiovcebq);
  
  -- Single-driven assignments
  ufsen <= ufsen;
  
  -- Multi-driven assignments
  hyiovcebq <= hyiovcebq;
  dd <= dd;
  dd <= dd;
end nyf;

entity rmekwb is
  port (l : buffer time; zummobs : linkage severity_level; fesvd : linkage integer; eov : linkage time);
end rmekwb;

library ieee;
use ieee.std_logic_1164.all;

architecture wbuifl of rmekwb is
  signal zfqc : string(1 to 2);
  signal taozm : std_logic_vector(2 downto 0);
  signal zxuhk : std_logic_vector(2 to 0);
  signal yxgdoaqj : std_logic_vector(2 downto 0);
  signal znf : std_logic_vector(2 to 0);
begin
  oqimfwtg : entity work.iwgplm
    port map (cetxfbbf => znf, gyu => yxgdoaqj);
  i : entity work.iwgplm
    port map (cetxfbbf => zxuhk, gyu => taozm);
  malmmewnb : entity work.y
    port map (ufsen => zfqc);
  
  -- Single-driven assignments
  l <= 2#11# ps;
  
  -- Multi-driven assignments
  taozm <= ('-', 'L', '1');
  taozm <= yxgdoaqj;
end wbuifl;



-- Seed after: 8606756709678872509,11481034001933599325
