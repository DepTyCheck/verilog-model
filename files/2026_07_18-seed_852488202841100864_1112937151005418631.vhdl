-- Seed: 852488202841100864,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity pltta is
  port (jvk : linkage integer; tsjdns : out real; fp : linkage real; sttaszibsy : buffer std_logic_vector(2 downto 0));
end pltta;

architecture na of pltta is
  
begin
  -- Single-driven assignments
  tsjdns <= 2#0.0_1_1#;
  
  -- Multi-driven assignments
  sttaszibsy <= ('L', 'X', '-');
  sttaszibsy <= sttaszibsy;
  sttaszibsy <= sttaszibsy;
  sttaszibsy <= sttaszibsy;
end na;

entity gjim is
  port (yyxeawyv : in boolean; nu : linkage real);
end gjim;

library ieee;
use ieee.std_logic_1164.all;

architecture qb of gjim is
  signal p : std_logic_vector(2 downto 0);
  signal np : real;
  signal pdkbq : integer;
  signal dpgjr : std_logic_vector(2 downto 0);
  signal lkcz : real;
  signal fxmb : real;
  signal wdv : integer;
begin
  hyfpk : entity work.pltta
    port map (jvk => wdv, tsjdns => fxmb, fp => lkcz, sttaszibsy => dpgjr);
  sj : entity work.pltta
    port map (jvk => pdkbq, tsjdns => np, fp => nu, sttaszibsy => p);
  
  -- Multi-driven assignments
  dpgjr <= dpgjr;
end qb;

library ieee;
use ieee.std_logic_1164.all;

entity swvfdukcu is
  port (tcvsy : linkage std_logic_vector(3 to 3); hoa : in time);
end swvfdukcu;

architecture n of swvfdukcu is
  
begin
  
end n;



-- Seed after: 3131339682325352117,1112937151005418631
