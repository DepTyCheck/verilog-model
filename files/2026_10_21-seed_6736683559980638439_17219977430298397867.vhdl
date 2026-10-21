-- Seed: 6736683559980638439,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity bkylmkzx is
  port (kdvjkangzj : buffer std_logic_vector(4 downto 2); gkuuyk : out std_logic; lzquueo : inout real);
end bkylmkzx;

architecture c of bkylmkzx is
  
begin
  -- Single-driven assignments
  lzquueo <= lzquueo;
  
  -- Multi-driven assignments
  kdvjkangzj <= kdvjkangzj;
  gkuuyk <= '0';
  kdvjkangzj <= kdvjkangzj;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity htsjok is
  port (epejwvdmnr : out std_logic; bxibilj : linkage real);
end htsjok;

library ieee;
use ieee.std_logic_1164.all;

architecture vqwlqbml of htsjok is
  signal ipnb : real;
  signal dgihik : real;
  signal bmbens : std_logic_vector(4 downto 2);
  signal uydy : real;
  signal ix : std_logic;
  signal mpd : std_logic_vector(4 downto 2);
  signal f : real;
  signal dhovjzxkq : std_logic_vector(4 downto 2);
begin
  jwyqc : entity work.bkylmkzx
    port map (kdvjkangzj => dhovjzxkq, gkuuyk => epejwvdmnr, lzquueo => f);
  kpwej : entity work.bkylmkzx
    port map (kdvjkangzj => mpd, gkuuyk => ix, lzquueo => uydy);
  awjxhtf : entity work.bkylmkzx
    port map (kdvjkangzj => bmbens, gkuuyk => epejwvdmnr, lzquueo => dgihik);
  ixtiavvndq : entity work.bkylmkzx
    port map (kdvjkangzj => bmbens, gkuuyk => epejwvdmnr, lzquueo => ipnb);
  
  -- Multi-driven assignments
  mpd <= ('U', '1', 'H');
end vqwlqbml;



-- Seed after: 6453621864793598452,17219977430298397867
