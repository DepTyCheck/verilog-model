-- Seed: 6318630878311855679,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity izrxkzinln is
  port (ppgfwjrenm : inout std_logic; tr : in std_logic; dtuabfrt : buffer bit_vector(3 downto 0));
end izrxkzinln;

architecture ljbdagca of izrxkzinln is
  
begin
  -- Single-driven assignments
  dtuabfrt <= ('0', '1', '1', '0');
  
  -- Multi-driven assignments
  ppgfwjrenm <= tr;
  ppgfwjrenm <= tr;
  ppgfwjrenm <= tr;
  ppgfwjrenm <= 'W';
end ljbdagca;

entity wj is
  port (pspol : inout real; ytarzko : out bit_vector(0 to 3); fzbgo : inout time);
end wj;

library ieee;
use ieee.std_logic_1164.all;

architecture kbiygm of wj is
  signal wv : std_logic;
  signal crfrmy : bit_vector(3 downto 0);
  signal lh : bit_vector(3 downto 0);
  signal dqjp : bit_vector(3 downto 0);
  signal qs : std_logic;
begin
  sbavphc : entity work.izrxkzinln
    port map (ppgfwjrenm => qs, tr => qs, dtuabfrt => dqjp);
  mk : entity work.izrxkzinln
    port map (ppgfwjrenm => qs, tr => qs, dtuabfrt => lh);
  nkx : entity work.izrxkzinln
    port map (ppgfwjrenm => qs, tr => qs, dtuabfrt => crfrmy);
  srrrkx : entity work.izrxkzinln
    port map (ppgfwjrenm => qs, tr => wv, dtuabfrt => ytarzko);
  
  -- Multi-driven assignments
  qs <= 'Z';
  qs <= qs;
  wv <= '-';
  qs <= 'Z';
end kbiygm;

entity uonz is
  port (ifr : in real; lrxx : in integer);
end uonz;

architecture pgaukk of uonz is
  
begin
  
end pgaukk;



-- Seed after: 13199593472500588354,6000118208082478503
